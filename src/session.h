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

#ifndef session_h
#define session_h

#include "ascii.h"
#include "clock.h"
#include "list.h"
#include "magic.h"
#include "scheme.h"
#include "screen.h"
#include "sky.h"
#include "status.h"

/* Shared between session.c and kern.c */
#define load_err_any() (load_errs != 0)
#define load_err_clear() (load_errs = 0)
#define load_err_inc() (load_errs++)

/* Shared between session.c and kern.c */
#define save_err_any() (save_errs != 0)
#define save_err_clear() (save_errs = 0)
#define save_err_inc() (save_errs++)

/* Shared between session.c and kern.c */
extern void load_err(char *fmt, ...);
extern void rt_err(char *fmt, ...);
extern void save_err(char *fmt, ...);

/* Shared between session.c and kern.c */
extern int load_errs;
extern int save_errs;

/* Backwards-compatible replacements for the old global flags: */
#define Reveal (Session->reveal)
#define Quicken (Session->quicken)
#define TimeStop (Session->time_stop)
#define MagicNegated (Session->magic_negated)
#define XrayVision (Session->xray)

#define add_reveal(val) ((Reveal) += val)
#define add_quicken(val) ((Quicken) += val)
#define add_time_stop(val) ((TimeStop) += val)
#define add_magic_negated(val) ((MagicNegated) += val)
#define add_xray(val) ((XrayVision) += val)

#define dec_reveal(val) ((Reveal) -= min((Reveal), (val)))
#define dec_quicken(val) ((Quicken) -= min((Quicken), (val)))
#define dec_time_stop(val) ((TimeStop) -= min((TimeStop), (val)))
#define dec_magic_negated(val) ((MagicNegated) -= min((MagicNegated), (val)))
#define dec_xray(val) ((XrayVision) -= min((XrayVision), (val)))

/* Access to global session passability table: */
#define session_ptable() (Session->ptable)

/* Access to global session diplomacy table: */
#define session_dtable() (Session->dtable)

/* Backwards-compatible replacements for the old global work queues: */
#define TickWorkQueue (Session->tickq)
#define TurnWorkQueue (Session->turnq)

struct session {

        // This list keeps track of all loaded object types. It's private to
        // the session management code which uses it to save and delete objects
        // when tearing down a session. (aka the "orphan list").
        struct list data_objects;

        // The global crosshair type is per-session and several many places
        class ObjectType *crosshair_type;
        class Cursor *crosshair;

        // The cursor sprite is used by cmdwin.c
        struct sprite *cursor_sprite;

        // The frame sprites are used by screen.c
        struct frame frame;

        // The ascii sprites are used by screen.c
        struct ascii ascii;

        // Global clock settings
        struct clock clock;

        // Saved status window state.
        enum StatusMode status_mode;

        // Saved LOS style.
        char *los;

        // Only one palette per session will be allowed, and it will be
        // globally accessible here. This is to accomodate the creation of
        // temporary combat maps in the wilderness.
        struct terrain_palette *palette;

        // The sky is used by sky.c
        struct sky sky;

        // Magic is used in cmd.c
        struct magic magic;

        // The interpreter is private
        void *interp;

        // This starts at zero when the session is loaded and is incremented
        // each time the session is saved. A copy of its value is kept in the
        // saved struct below.
        int session_id;

        // Temporary wilderness combat place requires special handling so that
        // it is saved in the proper order.
        struct place *combat_place;
        char saved_combat_place : 1;

        int reloaded : 1; /* Old session destroyed, this one is new */

        /* The number of turns until the "reveal hidden" effect expires: */
        int reveal;

        /* The number of turns until the "quicken" effect expires: */
        int quicken;

        /* The number of turns until the "time stop" effect expires: */
        int time_stop;

        /* The number of turns until the "magic negated" effect expires: */
        int magic_negated;

        /* The number of turns until the "xray vision" effect expires: */
        int xray;

        /* The passability table */
        struct ptable *ptable;

        /* The diplomacy table */
        struct dtable *dtable;

        /* The turn/tick work queues */
        struct list turnq;
        struct list tickq;

        /* Startup script */
        struct closure *start_proc;
};

// Callback table for saving objects
typedef struct save {

        FILE *file;
        int indent;

        // This is so objects can tell if they've saved themselves already for
        // the current session.
        int session_id;

        // printf with tabs
        void (*write)(struct save *save, char *fmt, ...);

        // printf without tabs
        void (*append)(struct save *save, char *fmt, ...);

        // printf with tabs & incr tabs
        void (*enter)(struct save *save, char *fmt, ...);

        // printf with tabs & decr tabs
        void (*exit)(struct save *save, char *fmt, ...);

        

} save_t;

extern void session_load(char *filename);
extern void session_save(char *fname);

// ----------------------------------------------------------------------------
// Add a persistent object to the session. If you want an object to get saved
// and destroyed when the session exits then you should do this. Objects which
// are loaded from a script at the start of a session are automatically added,
// and most objects created at runtime will be saved as part of the place or
// inventory which contains them. I specifically made this call externally
// accessible so I could manage the temporary combat place, which is very much
// a special case.
//
// This call returns a 'handle' which can be passed to session_rm() to remove
// the object if you want to remove it from the session (NOTE: calling
// session_rm() will NOT invoke the dtor provided to session_add()).
// ----------------------------------------------------------------------------
extern void * session_add(struct session *session, void *obj, 
                          void (*dtor)(void *),
                          void (*save)(save_t *save, void *obj),
                          void (*start)(void *obj)
        );
extern void * session_add_connection(struct session *session, void *obj, 
                                     void (*dtor)(void *),
                                     void (*save)(save_t *save, void *obj)
        );
extern void session_rm(struct session *session, void *handle);

extern void session_set_start_proc(struct session *session, struct closure *proc);
extern void session_run_start_proc(struct session *session);

extern void save_err(char *fmt, ...);

// Global session object.
extern struct session *Session;

#endif
