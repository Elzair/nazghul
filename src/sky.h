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
#ifndef sky_h
#define sky_h

#include "macros.h"

BEGIN_DECL

#include "list.h"
#include "closure.h"
#include <SDL.h>

struct phase {
        struct sprite *sprite;
        char *name;
        int maxlight;
};

struct astral_body {
        struct list list;
        char *tag;
        char *name;
        int distance; // relative
        int minutes_per_phase;
        int minutes_per_degree;
        int initial_arc;
        int initial_phase;
        int n_phases;
        struct phase *phases;
        int arc;
        int phase;
        int light;
        closure_t *gifc;
        struct gob *gob;
        float eclipse;
};

struct sky {
        SDL_Rect screenRect;
        struct list bodies;
};


extern struct astral_body *astral_body_new(char *tag, char *name, int n_phases);
extern void astral_body_del(struct astral_body *body);

void sky_init(struct sky *sky);
extern void sky_add_astral_body(struct sky *sky, struct astral_body *body);
extern void sky_advance(struct sky *sky, int repaint);
extern int sky_get_ambient_light(struct sky *sky);
extern void sky_start_session(struct sky *sky, int visible);
extern void sky_end_session(struct sky *sky);
extern void sky_save(struct sky *sky, struct save *save);
extern void sky_for_each(int (*fx)(struct astral_body*, void *data), void *data);

//extern int astral_body_is_up(struct astral_body *body);
//extern int astral_body_is_down(struct astral_body *body);
extern int astral_body_is_visible (int arc);

        
END_DECL

#endif
