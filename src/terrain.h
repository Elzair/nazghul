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
		unsigned char alpha;
		unsigned int pmask;	// passability mask (sea, air land)
		int movement_cost;	// should not exceed
					// PLAYER_MAX_PROGRESS
		char effects;
		int light;
		Uint32 color;

		int id;		// Used for binary maps, obsolescent
	};

	extern struct terrain *terrain_create(char *tag,
					      char *name,
					      unsigned int pmask,
					      struct sprite *sprite,
					      int id, unsigned char alpha);

	extern void terrain_destroy(struct terrain *terrain);

#define LONGEST_TERRAIN_GLYPH       4
#define NUM_QUICK_TERRAINS         10
#define PAL_TERRAIN_QUICK_DEFAULT -(NUM_QUICK_TERRAINS)
#define PAL_TERRAIN_NOT_SET       -1

	struct terrain_palette_entry {
        char           * glyph;
		struct terrain * terrain;
	};
	void palette_entry_print(FILE * fp, int indent,
				 struct terrain_palette_entry *entry);

	struct terrain_palette {
        struct list list;
		char *tag;
		int widest_glyph;
		int num_entries;
        int current_terrain_index;
		struct terrain_palette_entry *set;
        int quick_terrain[NUM_QUICK_TERRAINS];
		// "quick terrain" for quick UI access (ten number keys 0..9)
	};

	struct terrain_palette * new_terrain_palette(void);
    struct terrain_palette * palette_contains_terrain (struct terrain_palette *pp, 
                                                       struct terrain *tt);

	char * palette_glyph(struct terrain_palette *pp, int n);
    char * palette_glyph_for_terrain (struct terrain_palette * pp, 
                                      struct terrain * tt);

	struct terrain * palette_terrain(struct terrain_palette *pp, int n);
	struct terrain * palette_terrain_for_glyph(struct terrain_palette *pp, 
                                               char *glyph);
    struct terrain * palette_current_terrain(struct terrain_palette * pp);

  int palette_get_current_terrain_index(struct terrain_palette * pp);
  int palette_set_current_terrain(struct terrain_palette * pp, int n);
  int palette_prev_terrain(struct terrain_palette * pp);
  int palette_next_terrain(struct terrain_palette * pp);
  int palette_first_terrain(struct terrain_palette * pp);
  int palette_last_terrain(struct terrain_palette * pp);

  int palette_get_quick_terrain_index(struct terrain_palette * pp, int qt);
  int palette_set_quick_terrain(struct terrain_palette * pp, int qt, int index);
  struct terrain * palette_quick_terrain(struct terrain_palette *pp, int qt);

	void palette_print(FILE * fp, int indent,
                       struct terrain_palette *pp);

#ifdef __cplusplus
}
#endif

#endif // terrain_h
