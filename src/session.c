//
// nazghul - an old-school RPG engine
// Copyright (C) 2002, 2003 Gordon McNutt
//
// This program is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 2 of the License, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
// more details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
// Suite 330, Boston, MA 02111-1307 USA
//
// Gordon McNutt
// gmcnutt@users.sourceforge.net
//

#include "session.h"

#include "../config.h"
#include "character.h"
#include "object.h"
#include "sprite.h"
#include "terrain.h"
#include "place.h"
#include "images.h"
#include "Party.h"
#include "ptable.h"
#include "file.h"
#include "player.h"
#include "sky.h"
#include "map.h"
#include "cursor.h"
#include "Arms.h"
#include "Field.h"
#include "occ.h"
#include "species.h"
#include "sched.h"
#include "Reagent.h"
#include "screen.h"
#include "vehicle.h"
#include "formation.h"
#include "combat.h"
#include "Container.h"
#include "clock.h"
#include "wind.h"
#include "foogod.h"
#include "terrain_map.h" // dbg
#include "dtable.h"
#include "wq.h"
#include "cfg.h"
#include "skill.h"
#include "skill_set.h"

#include <assert.h>
#include <ctype.h>              // isspace()
#include <errno.h>
#include <kern.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <SDL_image.h>
#include <unistd.h>
#include <sys/stat.h>
//#include <sys/mman.h>
#include <stdarg.h>
#include <SDL.h>            // for SDL_GetTicks()

//#include "scheme.h"
#include "scheme-private.h"

struct data_obj_entry {
        struct list list;
        void *obj;
        int refcount;
        void (*dtor)(void*);
        void (*save)(save_t *save, void *obj);
        void (*start)(void *obj);
};

struct session_hook_entry {
        struct list list;
        struct closure *proc;
        pointer args;
};

struct session *Session = 0;
int load_errs = 0;
int save_errs = 0;

static void session_init_hooks(struct session *session);
static void session_cleanup_hooks(struct session *session);
static void session_cleanup_queries(struct session *session);

/* Redefine the session hook macro to turn its arg into a string, then #include
 * the list of hooks directly into an array of string pointers. */
#undef SESSION_DECL_HOOK
#define SESSION_DECL_HOOK(id) #id
static const char * session_hook_str[] = {
#       include "session_hooks.h"
};

static char session_last_error[128] = { 0 };

void load_err(const char *fmt, ...)
{
        load_err_inc();
        warn("load_err: ");
        va_list args;
        va_start(args, fmt);
        vwarn(fmt, args);
        va_end(args);
        warn("\n");

        /* duplicate it to the global string error */
        va_start(args, fmt);
        vsnprintf(session_last_error, sizeof(session_last_error),
                  fmt, args);
        va_end(args);
        
}

void rt_err(const char *fmt, ...)
{
        load_err_inc();
        warn("runtime error: ");
        va_list args;
        va_start(args, fmt);
        vwarn(fmt, args);
        va_end(args);
        warn("\n");

        consolePrint("\n*** script error! ***\n");
}



void save_err(const char *fmt, ...)
{
        save_err_inc();
        warn("save_err: ");
        va_list args;
        va_start(args, fmt);
        vwarn(fmt, args);
        va_end(args);
        warn("\n");
}

static void data_obj_entry_unref(struct data_obj_entry *entry)
{
        assert(entry->refcount >= 1);
        entry->refcount--;
        if (! entry->refcount) {
                free(entry);
        }
}

void *session_add(struct session *session, void *obj, 
                  void (*dtor)(void *),
                  void (*save)(save_t *, void *),
                  void (*start)(void *)
        )
{
        struct data_obj_entry *entry;

        entry = (struct data_obj_entry *)malloc(sizeof(*entry));
        assert(entry);
        list_init(&entry->list);
        entry->obj = obj;
        entry->refcount = 1;
        entry->dtor = dtor;
        entry->save = save;
        entry->start = start;

        /* Note: it's important to always add to the tail of the list. This
         * ensures that the objects in a session will be saved in the same
         * order they are loaded. The order can matter in certain cases (e.g.,
         * include files loaded at the front of the session). */
        list_add_tail(&session->data_objects, &entry->list);
        return entry;
}

void session_rm(struct session *session, void *handle)
{
        struct data_obj_entry *entry;
        entry = (struct data_obj_entry *)handle;
        list_remove(&entry->list);
        data_obj_entry_unref(entry);
}

static void session_save_crosshair(save_t *save, struct session *session)
{
        save->write(save, "(kern-set-crosshair %s)\n", 
                session->crosshair_type->getTag());
}

static void session_save_damage_sprite(save_t *save, struct session *session)
{
        save->write(save, "(kern-set-damage-sprite %s)\n", 
                    sprite_get_tag(session->damage_sprite));
}

static void session_save_clock(save_t *save, struct session *session)
{
        save->write(save, "(kern-set-clock %d %d %d %d %d %d)\n",
                session->clock.year,
                session->clock.month,
                session->clock.week,
                session->clock.day,
                session->clock.hour,
                session->clock.min);
}

static void session_save_time_accel(save_t *save, struct session *session)
{
        save->write(save, "(kern-set-time-accel %f)\n",
                    session->time_accel);
}

static void session_save_turn_count(save_t *save, struct session *session)
{
        save->write(save, "(kern-set-turn-count %d)\n", 
                    session->turn_count);
}

struct session *session_new(void *interp)
{
        struct session *session = (struct session*)calloc(1, sizeof(*session));
        assert(session);
        list_init(&session->data_objects);        
        list_init(&session->terrains);
        session->interp = interp;
        session->status_mode = ShowParty;
        session->los = "angband";
        sky_init(&session->sky);
        magic_init(&session->magic);
        list_init(&session->tickq);
        list_init(&session->turnq);
        node_init(&session->sched_chars);
        list_init(&session->blenders);
        list_init(&session->skills);
        list_init(&session->skill_sets);
        session->time_accel = 1;
        session_init_hooks(session);
        return session;
}

void session_del(struct session *session)
{
        struct list *elem;
        struct data_obj_entry *entry;

        /* ref the player party so we can control when it gets destroyed, so we
         * know when to zero out the global */
        if (session->player) {
                obj_inc_ref(session->player);
        }

        freezer_del();

        /* stop sound so sound entries will be purged */
        sound_haltall();
        
        /* Cleanup the data objects */
        elem = session->data_objects.next;
        int count = 0;
        while (elem != &session->data_objects) {
                entry = list_entry(elem, struct data_obj_entry, list);
                elem = elem->next;
                entry->refcount++; /* keep a ref while dtor runs */
                entry->dtor(entry->obj);
                data_obj_entry_unref(entry); /* now release ref */
                count++;
        }

        /* Cleanup the terrains */
        elem = session->terrains.next;
        while (elem != &session->terrains) {
                struct terrain *terrain = list_entry(elem, struct terrain, 
                                                     session_list);
                elem = elem->next;
                list_remove(&terrain->session_list);
                terrain_del(terrain);
        }

        if (session->crosshair)
                obj_dec_ref(session->crosshair);
        sky_end_session(&session->sky);
        magic_end_session(&session->magic);
        if (session->ptable)
                ptable_del(session->ptable);
        if (session->dtable)
                dtable_del(session->dtable);

        /* Clean up the turn work queue */
        elem = session->turnq.next;
        while (elem != &session->turnq) {
                struct wq_job *job = list_entry(elem, struct wq_job, list);
                elem = elem->next;
                wq_job_del(job);
        }

        /* Clean up the tick work queue */
        elem = session->tickq.next;
        while (elem != &session->tickq) {
                struct wq_job *job = list_entry(elem, struct wq_job, list);
                elem = elem->next;
                wq_job_del(job);
        }

        session_cleanup_hooks(session);
        session_cleanup_queries(session);

        /* Ensure that nothing is referencing the player party (except perhaps
         * its vehicle, which will be cleaned up with the party). Note: if
         * we're aborting a failed load then the player may not exist. */
        if (session->player) {
                assert((1==session->player->refcount)
                       || ((2==session->player->refcount)
                           && session->player->getVehicle()));

                /* Now zilch the global player party */
                obj_dec_ref(session->player);
                session->player = 0;
        }

        /* clean up the list of skills */
        elem = session->skills.next;
        while (elem != &session->skills) {
                struct skill *skill = list_entry(elem, struct skill, list);
                elem = elem->next;
                list_remove(&skill->list);
                skill_unref(skill);
        }

        /* clean up the list of skill sets */
        elem = session->skill_sets.next;
        while (elem != &session->skill_sets) {
                struct skill_set *skset 
                        = list_entry(elem, struct skill_set, list);
                elem = elem->next;
                list_remove(&skset->list);
                skill_set_unref(skset);
        }

        /* cleanup the interpreter */
        if (session->interp) {
                scheme_deinit((scheme*)session->interp);
                free(session->interp);
        }

        /* Check for memory leaks. Character dtors should have removed
         * themselves from the sched_chars list if they were on it. */
        if (! node_list_empty(&session->sched_chars)) {
                fprintf(stderr, 
                        "warn: session sched_chars list non-empty\n");
        }

        free(session);
}

int session_load(char *filename)
{
        scheme *sc;
        FILE *file = NULL;
        struct session *old_session;
        int t1, t2;
        struct list *elem;

        /* Remember the old session in case we have to bail out. */
        old_session = Session;

        /* Clear any leftover load errors. */
        load_err_clear();

        /* Open the load file (this might come from the include dir or the save
         * dir, so the caller must pass in the complete pathname). */
        file = file_open(filename, "r");
        if (! file) {
                load_err("could not open script file '%s' for reading: %s",
                           filename, strerror(errno));
                return -1;
        }

        /* Create a new interpreter. */
        if (! (sc = kern_init())) {
                load_err("could not create interpreter");
                fclose(file);
                return -1;
        }

        /* Create a new current sesssion. */
        Session = session_new(sc);
        assert(Session);

        /* Load the file and close it. */
        t1 = SDL_GetTicks();
        scheme_load_named_file(sc, file, filename);
        t2 = SDL_GetTicks();
        info("%d ms to load\n", (t2 - t1));

        fclose(file);

        /* Check for any errors during loading. */
        if (load_err_any()) {

                /* Check if the problem is version obsolescence. */
                if (Session->major < MIN_SCRIPT_MAJOR
                    || Session->minor < MIN_SCRIPT_MINOR
                    || Session->release < MIN_SCRIPT_RELEASE) {
                        snprintf(session_last_error,
                                 sizeof(session_last_error),
                                 "The save file format is version %u.%u.%u, "
                                 "but this release of the engine requires "
                                 "version %u.%u.%u or better. You might try "
                                 "an older version of the engine with this "
                                 "file.",
                                 Session->major, Session->minor, 
                                 Session->release, MIN_SCRIPT_MAJOR, 
                                 MIN_SCRIPT_MINOR, MIN_SCRIPT_RELEASE);
                }

                session_del(Session);
                Session = old_session;
                return -1;
        }

        /* Check for stuff that needs to be there for the new session to
         * work. Start with some globals. */
        if (! Session->crosshair_type) {
                load_err("no crosshair object (use kern-set-crosshair)");
        } else {
                Session->crosshair = new Cursor();
                Session->crosshair->init(Session->crosshair_type);
                obj_inc_ref(Session->crosshair);
        }
        if (! Session->damage_sprite) {
                load_err("no damage sprite (use kern-set-cursor)");
        }
        if (! Session->clock.set) {
                load_err("clock not set (use kern-set-clock)");
        }
        if (! Session->ptable) {
                load_err("passability table not set (use kern-set-ptable)");
        }
        if (! Session->dtable) {
                load_err("diplomacy table not set (use kern-set-dtable)");
        }

        if (! Session->player) {
                load_err("no player party");
        }
        if (Session->player->getSize() == 0) {
                load_err("player party empty");
        }


        /* Check for any errors. If there are any then destroy the new session
         * and return the old one. Otherwise destroy the old session and return
         * the new one. */
        if (load_err_any()) {
                session_del(Session);
                scheme_deinit(sc);
                free(sc);
                Session = old_session;
                return -1;
        }

        /* No errors, so it's safe to delete the old session. */
        if (old_session) {
                session_del(old_session);
        }

        combat_reset_state();
        
        Session->player->startSession();

        /* Now setup stuff that with known defaults. */
        statusSetMode(Session->status_mode);
        mapSetLosStyle(Session->los);

        /* Run through all the objects in the world, initializing their
         * effects */
        list_for_each(&Session->data_objects, elem) {
                struct data_obj_entry *entry;
                entry = list_entry(elem, struct data_obj_entry, list);
                if (entry->start) {
                        entry->start(entry->obj);
                }
        }     
 
		/* need to start all the objects in the freezer too */
		freezer_start_contents();

        sky_start_session(&Session->sky, 
                          NULL != Place && ! Place->underground);


        // This is also called from place_enter(), which is called when the
        // game first starts up but apparently not necessarily on a
        // reload. Should be safe to call more than once, I think.
        //session_synch_sched_chars(Session);
 
        /* Paint all the windows for the first time in the new session. */
        screenErase(0);
	screen_repaint_frame();
	foogodRepaint();
	consoleRepaint();
	statusRepaint();

        /* show the sun, moon and wind status */
        sky_advance(&Session->sky, 
                    NULL != Place && ! Place->underground);

        windRepaint();

        session_run_hook(Session, session_start_hook, "p", Session->player);
        
        return 0;
}

#define SAVE_INDENT_WIDTH 2
static void save_vwrite(struct save *save, const char *fmt, va_list args)
{
        if (save->indent)
                fprintf(save->file, "%*c", save->indent, ' ');
        vfprintf(save->file, fmt, args);
}

static void save_write(save_t *save, const char *fmt, ...)
{
        va_list args;
        va_start(args, fmt);
        save_vwrite(save, fmt, args);
        va_end(args);        
}

static void save_append(save_t *save, const char *fmt, ...)
{
        va_list args;
        va_start(args, fmt);
        vfprintf(save->file, fmt, args);
        va_end(args);    
}

static void save_enter(save_t *save, const char *fmt, ...)
{
        va_list args;
        va_start(args, fmt);
        save_vwrite(save, fmt, args);
        va_end(args);
        save->indent += SAVE_INDENT_WIDTH;
}

static void save_exit(save_t *save, const char *fmt, ...)
{
        va_list args;
        va_start(args, fmt);
        save->indent -= SAVE_INDENT_WIDTH;
        assert(save->indent >= 0);
        save_vwrite(save, fmt, args);
        va_end(args);        
}

save_t *save_new(FILE *file)
{
        save_t *save;

        save = (save_t*)calloc(1, sizeof(*save));
        assert(save);

        save->file = file;
	// SAM: Where is indent ever initialized???
        save->write = save_write;
        save->enter = save_enter;
        save->exit = save_exit;
        save->append = save_append;

        return save;
}

void save_del(save_t *save)
{
        if (save->indent) {
                warn("save file indentation left at %d; possible "\
                     "unterminated block in save file", save->indent);
        }
        fflush(save->file);
        free(save);
}

int session_save(char *fname)
{
        FILE *file = 0;
        struct list *elem;
        save_t *save;
        int object_saves = 0;

        file = file_open_in_save_dir(fname, "w");
        if (! file) {
                warn("session_save: could not open %s: %s\n", fname,
                     file_get_error());
                return -1;
        }

        save = save_new(file);

        /* Advance the session ID each time we save so that objects can know
         * that they need to save themselves. */
        Session->session_id++;
        save->session_id = Session->session_id;

        /* Save the header. */
        save->write(save, ";; %s -- a nazghul session file\n", fname);
        /* Note: below does not work, and causes false negs with regression
         * tests: */
        /* save->write(save, ";; Created %s\n", ctime(&timep)); */

        /* Save the standard file. */
        save->write(save, ";; Load the standard definitions file\n");
        save->write(save, "(load \"naz.scm\")\n");
        save->write(save, "\n");

        /* Write the (new) version */
        save->write(save, "(kern-script-version \"%s\")\n", PACKAGE_VERSION);

        /* Generate the first part of the progress bar code. Use the number of
         * data objects which will save themselves, plus the number of load
         * files, as the limit. */
        list_for_each(&Session->data_objects, elem) {
                struct data_obj_entry *entry;
                entry = list_entry(elem, struct data_obj_entry, list);
                if (entry->save) {
                        object_saves++;
                }
        }

        save->write(save, ";; Progress bar\n");
        save->write(save, "(kern-progress-bar-start \"Loading\" %d)\n", 
                    Session->num_kern_includes + object_saves);
        
        /* Generate code to advance the progress bar as each file is loaded. */
        save->write(save, "(define original-load load)  "
                    "(define (load file) "
                    "(kern-progress-bar-advance 1) "
                    "(original-load file))\n");

        /* Save all the saveable objects. */
        list_for_each(&Session->data_objects, elem) {
                struct data_obj_entry *entry;
                entry = list_entry(elem, struct data_obj_entry, list);
                if (entry->save) {
                        entry->save(save, entry->obj);
                        if (save_err_any()) {
                                warn("Aborting save to %s due to errors\n", 
                                     fname);
                                break;
                        }

                        /* Generate code to advance the progress bar as each
                         * object finishes loading. */
                        save->write(save, "(kern-progress-bar-advance 1)\n");
                }

        }
        /* Object freezer */
        save->write(save, ";;--------------\n");
        save->write(save, ";; ObjectFreezer\n");
        save->write(save, ";;--------------\n");
        freezer_save(save);

        /* Save all the special-case stuff... */
        save->write(save, ";;--------------\n");
        save->write(save, ";; Miscellaneous\n");
        save->write(save, ";;--------------\n");
        session_save_damage_sprite(save, Session);
        session_save_crosshair(save, Session);
        session_save_clock(save, Session);
        session_save_time_accel(save, Session);
        session_save_turn_count(save, Session);
        dtable_save(Session->dtable, save);
        sky_save(&Session->sky, save);
        windSave(save);

        /* Save the flags */
        save->write(save, "(kern-add-reveal %d)\n", Session->reveal.duration);
        save->write(save, "(kern-add-quicken %d)\n", 
                    Session->quicken.duration);
        save->write(save, "(kern-add-time-stop %d)\n", 
                    Session->time_stop.duration);
        save->write(save, "(kern-add-magic-negated %d)\n", 
                    Session->magic_negated.duration);
        save->write(save, "(kern-add-xray-vision %d)\n", 
                    Session->xray.duration);

        /* save the work queues */
        /* NOTE: don't see how we can, since work queue jobs use a void
         * pointer. We could also require a save callback pointer in the job,
         * but usually the data is a C pointer which won't be valid on reload,
         * so I don't know what the save callback can do to help. */
        /*         session_save_wq(&session->turnq); */
        /*         session_save_wq(&session->tickq); */

        /* Finish progress bar code */
        save->write(save, "(kern-progress-bar-finish)\n");

        save_del(save);
        fclose(file);                
        return 0;
}

struct node *session_add_sched_char(struct session *session,
                                    class Character *npc)
{
        struct node *node = node_new(npc);
        node_add(&session->sched_chars, node);
        return node;
}

void session_rm_sched_char(struct node *node)
{
        /* beware of doing this while iterationg over the sched_chars list */
        node_remove(node);
        node_unref(node);
}

void session_synch_sched_chars(struct session *session)
{
        struct node *node = node_next(&session->sched_chars);
        while (node != &session->sched_chars) {
                class Character *npc = (class Character*)node->ptr;
                node = node_next(node);
                npc->synchronize();
                if (npc->isCharmed())
                        npc->unCharm();
        }
}

void session_intro_sched_chars(struct session *session)
{
        struct node *node = node_next(&session->sched_chars);
        while (node != &session->sched_chars) {
                class Character *npc = (class Character*)node->ptr;
                npc->introduce();
                node = node_next(node);
        }        
}

static void session_init_hooks(struct session *session)
{
        int id;

        for (id = 0; id < NUM_HOOKS; id++) {
                list_init(&session->hook_table[id]);
        }
}

void session_run_hook(struct session *session, session_hook_id_t id, const char *fmt, ...)
{
        struct list *lptr, *head;

        assert(id < NUM_HOOKS);
        head = &session->hook_table[id];

        list_for_each(head, lptr) {
                struct session_hook_entry *entry = list_entry(lptr, struct session_hook_entry, list);
                va_list args;
                va_start(args, fmt);
                closure_execvl(entry->proc, fmt, args, entry->args);
                va_end(args);
        }
        
        
}

void *session_add_hook(struct session *session, session_hook_id_t id, struct closure *proc, pointer args)
{
        struct list *head;
        struct session_hook_entry *entry;

        assert(id < NUM_HOOKS);
        head = &session->hook_table[id];

        if (!(entry = (struct session_hook_entry*)calloc(1, sizeof(*entry)))) {
                return 0;
        }

        entry->proc = proc;
        closure_ref(proc);
        entry->args = args;
        proc->sc->vptr->protect(proc->sc, args);

        list_add_tail(&session->hook_table[id], &entry->list);
        return entry;
}

void session_rm_hook(struct session *session, session_hook_id_t id, pointer code)
{
        struct list *head, *lptr;
        struct session_hook_entry *entry;

        assert(id < NUM_HOOKS);
        head = &session->hook_table[id];
        lptr = head->next;
        while (lptr != head) {
            entry = list_entry(lptr, struct session_hook_entry, list);
            struct closure *proc = entry->proc;
            if (proc->code == code) {
                list_remove(lptr);
                scheme *sc = proc->sc;
                sc->vptr->unprotect(sc, entry->args);
                closure_unref(proc);
                free(entry);
                return;
            }
            lptr = lptr->next;
        }
}

static void session_cleanup_hooks(struct session *session)
{
        int i;
        for (i = 0; i < NUM_HOOKS; i++) {
                struct list *head = &session->hook_table[i];
                struct list *lptr = head->next;
                while (lptr != head) {
                        struct session_hook_entry *entry = list_entry(lptr, struct session_hook_entry, list);
                        lptr = lptr->next;
                        list_remove(&entry->list);
                        entry->proc->sc->vptr->unprotect(entry->proc->sc, entry->args);
                        closure_unref(entry->proc);
                        free(entry);
                }
        }
}

int session_run_query(struct session *session, session_query_id_t id, const char *fmt, ...)
{
        assert(id < NUM_QUERIES);
        struct closure *proc = session->query_table[id];
        va_list args;

        if (proc) {
                pointer result;
                va_start(args, fmt);
                result = closure_execv(proc, fmt, args);
                va_end(args);
                return closure_translate_result(proc->sc, result);
        }
        
        return 0;
}

void session_add_query(struct session *session, session_query_id_t id, struct closure *proc)
{
        assert(id < NUM_QUERIES);

        /* out with the old */
        if (session->query_table[id]) {
                closure_unref(session->query_table[id]);
                session->query_table[id] = NULL;
        }
        
        /* in with the new */
        if (proc) {
                closure_ref(proc);
                session->query_table[id] = proc;
        }
}

static void session_cleanup_queries(struct session *session)
{
        int i;
        for (i = 0; i < NUM_QUERIES; i++) {
                if (session->query_table[i]) {
                        closure_unref(session->query_table[i]);
                        session->query_table[i] = NULL;
                }
        }
}

const char *session_hook_id_to_str(session_hook_id_t id)
{
        assert(id < NUM_HOOKS);
        return session_hook_str[id];
}

session_hook_id_t session_str_to_hook_id(char *str)
{
        int id;
        for (id = 0; id < NUM_HOOKS; id++) {
                if (! strcmp(session_hook_str[id], str)) {
                        break;
                }
        }
        return (session_hook_id_t)id;
}

char *session_get_last_error(void)
{
        return session_last_error;
}

void session_eval(struct session *session, char *buf)
{
        scheme_load_string((scheme *)(session->interp), (const char*)buf);
}
