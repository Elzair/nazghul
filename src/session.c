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

#include <assert.h>
#include <ctype.h>              // isspace()
#include <errno.h>
#include <kern.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <SDL_image.h>
#include <unistd.h>
#include <sys/mman.h>
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

static void session_save_dtable(struct save *save, struct session *session)
{
        int rows;
        int cols;
        int level;
        hstack_t *hstack;

        hstack = hstack_new();

        save->enter(save, "(kern-mk-dtable\n");

        for (rows = 0; rows < session->dtable->n_factions; rows++) {
                save->write(save, "(list ");
                for (cols = 0; cols < session->dtable->n_factions; cols++) {
                        save->write(save, "(list ");

                        /* To save the stack in the proper order we have to pop
                         * everything into a temp stack. The dtable will refuse
                         * to pop its last entry. */
                        level = dtable_get(session->dtable, rows, cols);
                        while (0 == dtable_pop(session->dtable, rows, cols)) {
                                hstack_push(hstack, (void*)level);
                                level = dtable_get(session->dtable, rows, 
                                                   cols);
                        }

                        /* At this point 'level' holds the bottom entry of the
                         * stack. */
                        save->write(save, "%d ", level);

                        /* Now we have to deal with everything above it, and
                         * push back all the levels we just popped as we write
                         * them out. */
                        while (! hstack_empty(hstack)) {
                                level = (int)hstack_top(hstack);
                                dtable_push(session->dtable, rows, cols, 
                                            level);
                                save->write(save, "%d ", level);
                                hstack_pop(hstack);
                        }
                        save->write(save, ") ");
                }
                save->write(save, ")\n");
        }

        save->exit(save, ")\n");

        hstack_del(hstack);
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
        return session;
}

void session_del(struct session *session)
{
        struct list *elem;
        struct data_obj_entry *entry;
        struct include_file *inc;

        elem = session->data_objects.next;
        while (elem != &session->data_objects) {
                entry = list_entry(elem, struct data_obj_entry, list);
                elem = elem->next;
                entry->dtor(entry->obj);
                free(entry);
        }

        if (session->crosshair)
                delete session->crosshair;
        sky_end_session(&session->sky);
        magic_end_session(&session->magic);
        if (session->ptable)
                ptable_del(session->ptable);
        if (session->dtable)
                dtable_del(session->dtable);

        free(session);
}

void session_load(char *filename)
{
        scheme *sc;
        FILE *file = NULL;
        struct session *old_session;
        int t1, t2;
        struct list *elem;

        old_session = Session;

        /* Clear any leftover load errors. */
        load_err_clear();

        /* Open the load file. */
        file = fopen(filename, "r");
        if (! file) {
                load_err("could not open script file '%s' for reading: %s",
                           filename, strerror(errno));
                return;
        }

        /* Create a new interpreter. */
        if (! (sc = kern_init())) {
                fclose(file);
                return;
        }

        /* Create a new current sesssion. */
        Session = session_new(sc);
        assert(Session);

        /* Load the file and close it. */
        t1 = SDL_GetTicks();
        scheme_load_file(sc, file);
        t2 = SDL_GetTicks();
        info("%d ms to load\n", (t2 - t1));

        fclose(file);

        /* Check for any errors. If there are any then destroy the new session
         * and return the old one. Otherwise destroy the old session and return
         * the new one. */
        if (load_err_any()) {
                session_del(Session);
                scheme_deinit(sc);
                free(sc);
                Session = old_session;
                return;
        }

        /* Check for stuff that needs to be there for the new session to
         * work. Start with some globals. */
        if (! Session->crosshair_type) {
                load_err("no crosshair object (use kern-set-crosshair)");
        } else {
                Session->crosshair = new Cursor();
                Session->crosshair->init(Session->crosshair_type);
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
        if (! Session->clock.set) {
                load_err("clock not set (use kern-set-clock)");
        }
        if (! Session->ptable) {
                load_err("passability table ont set (use kern-set-ptable)");
        }
        if (! Session->dtable) {
                load_err("diplomacy table not set (use kern-set-dtable)");
        }

        /* Check for any errors. If there are any then destroy the new session
         * and return the old one. Otherwise destroy the old session and return
         * the new one. */
        if (load_err_any()) {
                session_del(Session);
                scheme_deinit(sc);
                free(sc);
                Session = old_session;
                return;
        }

        /* No errors, so it's safe to delete the old session. */
        if (old_session) {
                scheme *old_sc = (scheme*)old_session->interp;
                session_del(old_session);
                scheme_deinit(old_sc);
                free(old_sc);
        }

        player_party->startSession();

        /* Now setup stuff that with known defaults. */
        statusSetMode(Session->status_mode);
        mapSetLosStyle(Session->los);
	windSetDirection(NORTH, 1); /* fixme -- hardcoded */
        sky_start_session(&Session->sky, 
                          NULL != Place && ! Place->underground);

        /* Run through all the objects in the world, initializing their
         * effects */
        list_for_each(&Session->data_objects, elem) {
                struct data_obj_entry *entry;
                entry = list_entry(elem, struct data_obj_entry, list);
                if (entry->start) {
                        entry->start(entry->obj);
                }
        }        

        /* Paint all the windows for the first time in the new session. */
	screen_repaint_frame();
	foogodRepaint();
	consoleRepaint();
	statusRepaint();

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
        time_t timep;
        save_t *save;

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
        session_save_crosshair(save, Session);
        session_save_ascii(save, Session);
        session_save_clock(save, Session);
        session_save_dtable(save, Session);
        sky_save(&Session->sky, save);

        /* Save the flags */
        save->write(save, "(kern-add-reveal %d)\n", Session->reveal);
        save->write(save, "(kern-add-quicken %d)\n", Session->quicken);
        save->write(save, "(kern-add-time-stop %d)\n", Session->time_stop);
        save->write(save, "(kern-add-magic-negated %d)\n", 
                    Session->magic_negated);
        save->write(save, "(kern-add-xray-vision %d)\n", Session->xray);

        save_del(save);
        fclose(file);                
}
