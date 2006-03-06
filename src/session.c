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

#include "character.h"
#include "object.h"
#include "sprite.h"
#include "terrain.h"
#include "place.h"
#include "images.h"
#include "Party.h"
#include "ptable.h"
#include "common.h"
#include "player.h"
#include "sky.h"
#include "ascii.h"
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
        void (*dtor)(void*);
        void (*save)(save_t *save, void *obj);
        void (*start)(void *obj);
};


struct session *Session = 0;
int load_errs = 0;
int save_errs = 0;

void load_err(char *fmt, ...)
{
        load_err_inc();
        warn("load_err: ");
        va_list args;
        va_start(args, fmt);
        vwarn(fmt, args);
        va_end(args);
        warn("\n");
}

void rt_err(char *fmt, ...)
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



void save_err(char *fmt, ...)
{
        save_err_inc();
        warn("save_err: ");
        va_list args;
        va_start(args, fmt);
        vwarn(fmt, args);
        va_end(args);
        warn("\n");
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
        free(entry);
}

static void session_save_crosshair(save_t *save, struct session *session)
{
        save->write(save, "(kern-set-crosshair %s)\n", 
                session->crosshair_type->getTag());
}

static void session_save_cursor(save_t *save, struct session *session)
{
        save->write(save, "(kern-set-cursor %s)\n", 
                    session->cursor_sprite->tag);
}

static void session_save_damage_sprite(save_t *save, struct session *session)
{
        save->write(save, "(kern-set-damage-sprite %s)\n", 
                    session->damage_sprite->tag);
}

static void session_save_frame(save_t *save, struct session *session)
{
        save->write(save, "(kern-set-frame %s %s %s %s "\
                "%s %s %s %s %s "\
                "%s %s %s %s)\n",
                session->frame.ulc->tag,
                session->frame.urc->tag,
                session->frame.llc->tag,
                session->frame.lrc->tag,
                session->frame.td->tag,
                session->frame.tu->tag,
                session->frame.tl->tag,
                session->frame.tr->tag,
                session->frame.tx->tag,
                session->frame.horz->tag,
                session->frame.vert->tag,
                session->frame.endl->tag,
                session->frame.endr->tag);
}

static void session_save_ascii(save_t *save, struct session *session)
{
        save->write(save, "(kern-set-ascii %s %d)\n",
                session->ascii.images->tag,
                session->ascii.offset);
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
        save->write(save, "(kern-set-time-accel %d)\n",
                    session->time_accel);
}

struct session *session_new(void *interp)
{
        struct session *session = (struct session*)calloc(1, sizeof(*session));
        assert(session);
        list_init(&session->data_objects);        
        session->interp = interp;
        session->status_mode = ShowParty;
        session->los = "angband";
        sky_init(&session->sky);
        magic_init(&session->magic);
        list_init(&session->tickq);
        list_init(&session->turnq);
        node_init(&session->sched_chars);
        list_init(&session->blenders);
        session->time_accel = 1;
        return session;
}

void session_del(struct session *session)
{
        struct list *elem;
        struct data_obj_entry *entry;
        struct node *node;

        elem = session->data_objects.next;
        int count = 0;
        while (elem != &session->data_objects) {
                entry = list_entry(elem, struct data_obj_entry, list);
                elem = elem->next;
                entry->dtor(entry->obj);
                free(entry);
                count++;
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

        /* clean up the sched_chars list */
        node = session->sched_chars.next;
        while (node != &session->sched_chars) {
                struct node *tmp = node->next;
                node_remove(node);
                node_unref(node);
                node = tmp;
        }

        /* Clean up the closures */
        closure_unref_safe(session->start_proc);
        closure_unref_safe(session->camping_proc);

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

        /* Open the load file. */
        file = fopen(filename, "r");
	if (! file && SavedGamesDir ) {
		char *fname = dirConcat(SavedGamesDir,filename);
		if (fname) {
			file = fopen(fname, "r");
			free(fname);
		}
	}
	if (! file && IncludeDir ) {
		char *fname = dirConcat(IncludeDir,filename);
		if (fname) {
			file = fopen(fname, "r");
			free(fname);
		}
	}
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

        /* Check for any errors. If there are any then destroy the new session
         * and return the old one. Otherwise destroy the old session and return
         * the new one. */
        if (load_err_any()) {
                /* 
                 * FIXME! We're screwed here because we probably already blew
                 * away the old player party.
                 */
                session_del(Session);
                scheme_deinit(sc);
                free(sc);
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
        if (! Session->frame.llc) {
                load_err("no frame sprites (use kern-set-frame)");
        }
        if (! Session->ascii.images) {
                load_err("no ASCII sprites (use kern-set-ascii)");
        }
        if (! Session->cursor_sprite) {
                load_err("no cursor sprite (use kern-set-cursor)");
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

        if (! player_party) {
                load_err("no player party");
        }
        if (player_party->getSize() == 0) {
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

        player_party->startSession();

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

        return 0;
}

#define SAVE_INDENT_WIDTH 2
static void save_vwrite(struct save *save, char *fmt, va_list args)
{
        if (save->indent)
                fprintf(save->file, "%*c", save->indent, ' ');
        vfprintf(save->file, fmt, args);
}

static void save_write(save_t *save, char *fmt, ...)
{
        va_list args;
        va_start(args, fmt);
        save_vwrite(save, fmt, args);
        va_end(args);        
}

static void save_append(save_t *save, char *fmt, ...)
{
        va_list args;
        va_start(args, fmt);
        vfprintf(save->file, fmt, args);
        va_end(args);    
}

static void save_enter(save_t *save, char *fmt, ...)
{
        va_list args;
        va_start(args, fmt);
        save_vwrite(save, fmt, args);
        va_end(args);
        save->indent += SAVE_INDENT_WIDTH;
}

static void save_exit(save_t *save, char *fmt, ...)
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

void session_save(char *fname)
{
        FILE *file;
        struct list *elem;
        save_t *save;
	char *filename;

	filename = dirConcat(SavedGamesDir,fname);
	if (filename) {
#ifndef WIN32
                /* FIXME: cygwin build fails, saying that mkdir below has too
                 * many arguments. We don't use the save dir so not a problem */
		(void)mkdir(SavedGamesDir, 0777);
#endif
		file = fopen(filename, "w");
        	if (! file) {
                	warn("session_save: could not open %s "
			     "for writing: %s\n"
			     "session_save: falling back to current "
			     "directory.\n",
	                     filename, strerror(errno));
			file = fopen(fname, "w");
		}
	} else
		file = fopen(fname, "w");
        if (! file) {
                warn("session_save: could not open %s for writing: %s\n",
                     fname, strerror(errno));
                return;
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
                }
        }

        /* Save all the special-case stuff... */
        save->write(save, ";;--------------\n");
        save->write(save, ";; Miscellaneous\n");
        save->write(save, ";;--------------\n");
        session_save_frame(save, Session);
        session_save_cursor(save, Session);
        session_save_damage_sprite(save, Session);
        session_save_crosshair(save, Session);
        session_save_ascii(save, Session);
        session_save_clock(save, Session);
        session_save_time_accel(save, Session);
        dtable_save(Session->dtable, save);
        sky_save(&Session->sky, save);
        windSave(save);

        /* Save the flags */
        save->write(save, "(kern-add-reveal %d)\n", Session->reveal);
        save->write(save, "(kern-add-quicken %d)\n", Session->quicken);
        save->write(save, "(kern-add-time-stop %d)\n", Session->time_stop);
        save->write(save, "(kern-add-magic-negated %d)\n", 
                    Session->magic_negated);
        save->write(save, "(kern-add-xray-vision %d)\n", Session->xray);

        /* save the work queues */
        /* NOTE: don't see how we can, since work queue jobs use a void
         * pointer. We could also require a save callback pointer in the job,
         * but usually the data is a C pointer which won't be valid on reload,
         * so I don't know what the save callback can do to help. */
        /*         session_save_wq(&session->turnq); */
        /*         session_save_wq(&session->tickq); */

        save_del(save);
        fclose(file);                
}

void session_set_start_proc(struct session *session, struct closure *proc)
{
        /* out with the old */
        if (session->start_proc) {
                closure_unref(session->start_proc);
                session->start_proc = NULL;
        }
        
        /* in with the new */
        if (proc) {
                closure_ref(proc);
                session->start_proc = proc;
        }
}

void session_set_camping_proc(struct session *session, struct closure *proc)
{
        /* out with the old */
        if (session->camping_proc) {
                closure_unref(session->camping_proc);
                session->camping_proc = NULL;
        }
        
        /* in with the new */
        if (proc) {
                closure_ref(proc);
                session->camping_proc = proc;
        }
}

void session_run_start_proc(struct session *session)
{
        if (session->start_proc) {
                closure_exec(session->start_proc, "p", player_party);
        }
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
        struct node *node = session->sched_chars.next;
        while (node != &session->sched_chars) {
                class Character *npc = (class Character*)node->ptr;
                node = node->next;
                npc->synchronize();
                if (npc->isCharmed())
                        npc->unCharm();
        }
}

void session_intro_sched_chars(struct session *session)
{
        struct node *node = session->sched_chars.next;
        while (node != &session->sched_chars) {
                class Character *npc = (class Character*)node->ptr;
                npc->introduce();
                node = node->next;
        }        
}
