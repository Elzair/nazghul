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
#ifndef terrain_h
#define terrain_h

#ifdef __cplusplus
extern "C" {
#endif

#include "list.h"
#include "common.h"

        struct sprite;

#define terrain_combat_map(t) ((t)->combat_map)

/* Terrain effects */
#define TERRAIN_NEUTRAL 0
#define TERRAIN_POISON  EFFECT_POISON
#define TERRAIN_BURN    EFFECT_BURN

        struct terrain {
                struct list list;
                char *tag;
                char *name;
                struct sprite *sprite;
                struct terrain_map *combat_map;
                int id;
                char glyph;
                unsigned char alpha;
                unsigned int pmask; /* passability mask (sea, air land) */
		int movement_cost;	/* should not exceed
					 * PLAYER_MAX_PROGRESS */
                char effects;
                int light;
                Uint32 color;
        };

	extern struct terrain *terrain_create(char *tag,
                char *name, 
                unsigned int pmask,
                struct sprite *sprite, 
                char glyph, 
					      int id, unsigned char alpha);

        extern void terrain_destroy(struct terrain *terrain);

#ifdef __cplusplus
}
#endif

#endif
