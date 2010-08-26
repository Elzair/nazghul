//
// nazghul - an old-school RPG engine
// Copyright (C) 2004 Gordon McNutt
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
#ifndef effect_h
#define effect_h

#include "macros.h"
#include "closure.h"

BEGIN_DECL

/* Negative expiration codes */
#define EFFECT_PERMANENT          -1   /* never expires              */
#define EFFECT_NONDETERMINISTIC   -2   /* can't tell when it expires */

/* Runtime type-verification: */
extern const int EFFECT_ID;
#define is_effect(eff) ((eff) && ((eff)->ID == EFFECT_ID))
#define effect_will_expire(eff) ((eff)->duration > 0)

/* An effect type defines an effect like poison, protection, etc. Effects are
 * attached to objects. */
struct effect {
        int ID;                 /* for runtime type-verification */
        closure_t *exec;        /* scheme proc to execute on-hook */
        closure_t *apply;       /* scheme proc to execute on-attach */
        closure_t *rm;          /* scheme proc to execute on-removal */
        closure_t *restart;     /* scheme proc to restart effect on reload */
        char *tag;              /* identifier tag name */
        char *name;             /* short name */
        struct sprite *sprite;  /* might be used in ztats window */
        int detect_dc;          /* detection difficulty class (default zero) */
        int cumulative;         /* more then one instance can be attached */
        int duration;           /* minutes before expire (-1 for never) */
        int hook_id;            /* hook the effect attaches to */
        struct sprite *status_icon; /* 8x16 sprite for status window */
};

/* Create the closure and dup the strings. Zero out other fields (caller must
 * fill them in). */
extern struct effect *effect_new(char *tag, scheme *sc, 
                                 pointer exec, 
                                 pointer apply, 
                                 pointer rm, 
                                 pointer restart,
                                 char *name);

/* Dealloc the closure, the strings and the struct. */
extern void effect_del(struct effect *effect);

END_DECL

#endif
