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
#include <stdio.h>
#include <string.h>
#include <assert.h>

struct terrain *terrain_create(char *tag,
			       char *name,
			       unsigned int pmask,
			       struct sprite *sprite,
			       int id, unsigned char alpha)
{
	struct terrain *terrain;

	CREATE(terrain, struct terrain, 0);

	terrain->tag = strdup(tag);
	terrain->name = strdup(name);
	terrain->pmask = pmask;
	terrain->sprite = sprite;
	terrain->id = id;
	terrain->alpha = alpha;

	// SAM: I notice that some fields are not initialized:
	// How/where do these get set?
	// Investigation finds these:
	// list // game.c 
	// combat_map // game.c terrain_combat_map(terrain) = terrain_map;
	// movement_cost // game.c PARSE_INT("movement_cost",
	// terrain->movement_cost);
	// effects // game.c PARSE_INT("effects", terrain->effects);
	// light // game.c PARSE_INT("light", terrain->light);
	// color // game.c terrain->color = screenMapRGB(red, grn, blu);

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

#define BOGUS_MAX_SIZE 255	// Hack, should get a constant from lexer.h or
				// somesuch...

void palette_entry_print(FILE * fp, int indent,
			 struct terrain_palette_entry *entry)
{
	static char glyph_str[BOGUS_MAX_SIZE + 1];
	static char   tag_str[BOGUS_MAX_SIZE + 1];
	static char  name_str[BOGUS_MAX_SIZE + 1];
	assert(fp);
    assert(entry);

	snprintf(glyph_str, BOGUS_MAX_SIZE, "'%s'", entry->glyph);
	snprintf(tag_str, BOGUS_MAX_SIZE, "'%s'", entry->terrain->tag);
	snprintf(name_str, BOGUS_MAX_SIZE, "'%s'", entry->terrain->name);

	INDENT;
	INDENT;
	fprintf(fp, "glyph %-6s tag %-20s name %-20s\n", glyph_str, tag_str,
		name_str);
}				// palette_entry_print()

#ifdef HACK_GLOBAL_PALETTE
// SAM: This gets initialized in game.c game_load_ascii_terrain_map()
//      to the first palette whose tag name is 'pal_extended'.
//      This is a big, ugly hack, and a temporary work-around
//      a bug with making a global list of terrain_palette.
struct terrain_palette HACK_global_palette;
#endif // HACK_GLOBAL_PALETTE

struct terrain_palette *new_terrain_palette(void)
{
	struct terrain_palette *palette = (struct terrain_palette *)
	    malloc(sizeof(struct terrain_palette));
	assert(palette);
	memset(palette, 0, sizeof(struct terrain_palette));
	return palette;
}

char *palette_glyph(struct terrain_palette *palette, int n)
{
	return palette->set[n].glyph;
}

struct terrain *palette_terrain(struct terrain_palette *palette, int n)
{
	return palette->set[n].terrain;
}

struct terrain *palette_terrain_for_glyph(struct terrain_palette *palette,
					  char *glyph)
{
	assert(palette);
	assert(glyph);

	int num_entries = palette->num_entries;
	for (int i = 0; i < num_entries; i++) {
		char *p_glyph = palette_glyph(palette, i);
		if (!strcmp(p_glyph, glyph)) {
			struct terrain *tt = palette_terrain(palette, i);
			return tt;
		}
	}
	return 0;		// Did not find the glyph
}				// palette_terrain_for_glyph()

struct terrain *palette_quick_terrain(struct terrain_palette *palette, int n)
{
	return palette->quick_terrain[n];
}

void palette_print(FILE * fp, int indent, struct terrain_palette *palette)
{
	int i;
	assert(fp);
	INDENT;
	fprintf(fp, "palette '%s'\n", palette->tag);
	indent += INDENTATION_FACTOR;

	INDENT;
	fprintf(fp, "num_entries %d, widest_glyph %d \n",
		palette->num_entries, palette->widest_glyph);

	INDENT;
	fprintf(fp, "entries:\n");
	for (i = 0; i < palette->num_entries; i++) {
		palette_entry_print(fp, indent, &palette->set[i]);
	}
	INDENT;
	fprintf(fp, "quick terrains:\n");
	for (i = 0; i < NUM_QUICK_TERRAINS; i++) {
		struct terrain *qt = palette_quick_terrain(palette, i);
		char *tag = "(none)";
		char *name = "(none)";
		if (qt) {
			tag = qt->tag;
			name = qt->name;
		}
		INDENT;
		INDENT;
		fprintf(fp, "%d '%s' '%s'\n", i, tag, name);
	}
	fprintf(fp, "\n");
}				// palette_print()
