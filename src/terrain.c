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
#include "terrain.h"
#include "sprite.h"
#include "util.h"

#include <stdlib.h>

struct terrain *terrain_create(char *tag,
                               char *name,
                               unsigned int pmask,
                               struct sprite *sprite,
                               char glyph, 
                               int  id, 
                               unsigned char alpha)
{
	struct terrain *terrain;

	CREATE(terrain, struct terrain, 0);

	terrain->tag    = strdup(tag);
	terrain->name   = strdup(name);
	terrain->pmask  = pmask;
	terrain->sprite = sprite;
	terrain->glyph  = glyph;
	terrain->id     = id;
	terrain->alpha  = alpha;

    // SAM: I notice that some fields are not initialized:
    //      How/where do these get set?
    //      Investigation finds these:
    //   list           // game.c list_add(&Terrains, &terrain->list);
    //   combat_map     // game.c terrain_combat_map(terrain) = terrain_map;
    //   movement_cost  // game.c PARSE_INT("movement_cost", terrain->movement_cost);
    //   effects        // game.c PARSE_INT("effects", terrain->effects);
    //   light          // game.c PARSE_INT("light", terrain->light);
    //   color          // game.c terrain->color = screenMapRGB(red, grn, blu);

	return terrain;
}

void terrain_destroy(struct terrain *terrain)
{
	if (terrain->tag)
		free(terrain->tag);
	if (terrain->name)
		free(terrain->name);
	free(terrain);
}
