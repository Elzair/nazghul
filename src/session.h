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
#include "node.h"
#include "Being.h"

/* File for quick save/reload with CTRL-q and CTRL-r */
#define QUICKSAVE_FNAME "save.scm"

/* Shared between session.c and kern.c */
#define load_err_any() (load_errs != 0)
#define load_err_clear() (load_errs = 0)
#define load_err_inc() (load_errs++)

/* Shared between session.c and kern.c */
#define save_err_any() (save_errs != 0)
#define save_err_clear() (save_errs = 0)
#define save_err_inc() (save_errs++)

/* Shared between session.c and kern.c */
extern void load_err(const char *fmt, ...);
extern void rt_err(const char *fmt, ...);
extern void save_err(const char *fmt, ...);

/* Shared between session.c and kern.c */
extern int load_errs;
extern int save_errs;

typedef struct {
        int duration;
        struct sprite *sprite;
} global_effect_t;

/* Indices into the hook table */
#define SESSION_DECL_HOOK(id) id
typedef enum {
#       include "session_hooks.h"
        NUM_HOOKS
} session_hook_id_t;

/* Indices into the query table */
#define SESSION_DECL_QUERY(id) id
typedef enum {
#       include "session_queries.h"
        NUM_QUERIES
} session_query_id_t;

/* Backwards-compatible replacements for the old global flags: */
#define Reload (Session->reload)
#define Reveal (Session->reveal.duration)
#define Quicken (Session->quicken.duration)
#define TimeStop (Session->time_stop.duration)
#define MagicNegated (Session->magic_negated.duration)
#define XrayVision (Session->xray.duration)

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

#define reveal_effect_sprite() (Session->reveal.sprite)
#define quicken_effect_sprite() (Session->quicken.sprite)
#define time_stop_effect_sprite() (Session->time_stop.sprite)
#define magic_negated_effect_sprite() (Session->magic_negated.sprite)
#define xray_vision_effect_sprite() (Session->xray.sprite)

/* Access to global session passability table: */
#define session_ptable() (Session->ptable)

/* Access to global session diplomacy table: */
#define session_dtable() (Session->dtable)

#define session_ticks_per_turn() (int)(place_get_scale(Place) * Session->time_accel)
#define session_set_time_accel(val) (Session->time_accel = (val))
#define session_get_time_accel(val) (Session->time_accel)

/* Backwards-compatible replacements for the old global work queues: */
#define TickWorkQueue (Session->tickq)
#define TurnWorkQueue (Session->turnq)

/* Access to the turn counter: */
#define session_inc_turn_count() (Session->turn_count++)
#define session_set_turn_count(val) (Session->turn_count=(val))
#define session_get_turn_count() (Session->turn_count)

/* Backwards-compatible replacement for the old global player_party */
#define player_party (Session->player)

struct skill;
struct skill_set;

struct session {

        // This list keeps track of all loaded object types. It's private to
        // the session management code which uses it to save and delete objects
        // when tearing down a session. (aka the "orphan list").
        struct list data_objects;

        struct list terrains; /* list of all terrains */

        // The global crosshair type is per-session and several many places
        class ObjectType *crosshair_type;
        class Cursor *crosshair;

        // The damage sprite is used in mapPaintDamage
        struct sprite *damage_sprite;

        // Global clock settings
        struct clock clock;

        // Saved status window state.
        enum StatusMode status_mode;

        // Saved LOS style.
        const char *los;

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

        /* Flag to signal a reload has been requested and is pending */
        int reload;

        /* The number of turns until the "reveal hidden" effect expires: */
        global_effect_t reveal;

        /* The number of turns until the "quicken" effect expires: */
        global_effect_t quicken;

        /* The number of turns until the "time stop" effect expires: */
        global_effect_t time_stop;

        /* During time stop, sprites for player-controlled objects should be
         * the only ones that animate. Instead of advancing the general
         * animation tick counter in sprites.c, advance a special tick counter
         * here. In object.c it will use this special tick counter for
         * advancing only player-controlled object sprites. */
        int time_stop_ticks;

        /* The number of turns until the "magic negated" effect expires: */
        global_effect_t magic_negated;

        /* The number of turns until the "xray vision" effect expires: */
        global_effect_t xray;

        /* The passability table */
        struct ptable *ptable;

        /* The diplomacy table */
        struct dtable *dtable;

        /* The turn/tick work queues */
        struct list turnq;
        struct list tickq;

        /* Startup script */
        struct closure *start_proc;

        /* A multiplier for temporarily speeding up time (used when the player
         * is camping or resting) */
        float time_accel;

        struct list hook_table[NUM_HOOKS];
        struct closure *query_table[NUM_QUERIES];

        struct node sched_chars;   /* characters with multi-place schedules */

        /* This is a flat-out hack. I decided to add some things like
         * describing if beings are hostile for the xamine command and showing
         * red or green squares around NPCs for the attack command, and these
         * require knowledge of a "who wants to know?" nature deep in the guts
         * of some call stacks. So, I added this global variable. It's
         * wrong, I know. Be careful with it. */
        Being *subject;

        struct list skills;     /* list of all skills in the game     */
        struct list skill_sets; /* list of all skill sets in the game */
        struct list blenders;   /* obsolete?                          */

        /* The turn count shown in the foogod window; incremented once per game
         * loop in play.c */
        int turn_count;

        /* The player party object for this session */
        class PlayerParty *player;

        char show_boxes : 1;  /* draw red/green/yellow boxes around npcs */
	char is_demo : 1; /* demo mode session */
	
        struct tree *freezer;

        int num_kern_includes; /* Number of times kern-include was used when
                                * loading the session. Needed for generating
                                * progress bar code in the save file. */

        unsigned int major, minor, release; /* script version */
};

// Callback table for saving objects
typedef struct save {

        FILE *file;
        int indent;

        // This is so objects can tell if they've saved themselves already for
        // the current session.
        int session_id;

        // printf with tabs
        void (*write)(struct save *save, const char *fmt, ...);

        // printf without tabs
        void (*append)(struct save *save, const char *fmt, ...);

        // printf with tabs & incr tabs
        void (*enter)(struct save *save, const char *fmt, ...);

        // printf with tabs & decr tabs
        void (*exit)(struct save *save, const char *fmt, ...);

        

} save_t;

save_t * save_new(FILE *   file);
void     save_del(save_t * save);

extern int session_load(char *filename);
extern int session_save(char *fname);
struct session *session_new(void *interp);
void session_del(struct session *session);

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

extern void session_run_hook(struct session *session, session_hook_id_t id, const char *fmt, ...);
extern void *session_add_hook(struct session *session, session_hook_id_t id, struct closure *proc, pointer args);
extern void session_rm_hook(struct session *session, session_hook_id_t id, pointer code);
extern int session_run_query(struct session *session, session_query_id_t id, const char *fmt, ...);
extern void session_add_query(struct session *session, session_query_id_t id, struct closure *proc);
extern const char *session_hook_id_to_str(session_hook_id_t id);
extern session_hook_id_t session_str_to_hook_id(char *str);

extern void save_err(const char *fmt, ...);
extern struct node *session_add_sched_char(struct session *session,
                                           class Character *npc);
extern void session_rm_sched_char(struct node *node);
extern void session_synch_sched_chars(struct session *session);
extern void session_intro_sched_chars(struct session *session);
extern char *session_get_last_error(void);
extern void session_eval(struct session *session, char *buf);

// Global session object.
extern struct session *Session;

#include "objectfreezer.h"

#endif
