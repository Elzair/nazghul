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

struct terrain_palette *new_terrain_palette(void)
{
	struct terrain_palette *palette = (struct terrain_palette *)
	    malloc(sizeof(struct terrain_palette));
	assert(palette);
	memset(palette, 0, sizeof(struct terrain_palette));
    palette->current_terrain_index = PAL_TERRAIN_NOT_SET;
	return palette;
}

char *palette_glyph(struct terrain_palette *palette, int n)
{
    assert(palette);
    if (palette->num_entries < 1) {
      printf("palette_terrain_for_glyph() num_entries == 0\n");
      return 0;
    }
    if (n < 0 || n >= palette->num_entries) {
      printf("palette_terrain_for_glyph() called with out-of-bounds arg n=%d\n", n);
      return 0;
    }
	return palette->set[n].glyph;
}

char * palette_glyph_for_terrain (struct terrain_palette * pp, struct terrain * tt)
{
    assert(pp);
    assert(tt);
    
    int num_entries = pp->num_entries;
    for (int i = 0; i < num_entries; i++) {
        struct terrain * terrain = palette_terrain(pp, i);
        if (tt == terrain) {
            char *glyph = palette_glyph(pp, i);
            return glyph;
        }
    }
    return 0;  // Did not find the terrain
}

struct terrain *palette_terrain(struct terrain_palette *palette, int n)
{
    assert(palette);
    if (palette->num_entries < 1) {
      printf("palette_terrain() num_entries == 0\n");
      return 0;
    }
    if (n < 0 || n >= palette->num_entries) {
      printf("palette_terrain() called with out-of-bounds arg n=%d", n);
      return 0;
    }
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

struct terrain_palette * palette_contains_terrain (struct terrain_palette *pp, 
                                                   struct terrain *tt)
{
  // The current user of this function is 
  // combat.c create_camping_map().
  // It is used to find a palette (any palette)
  // which contains a certain fill terrain.
  // 
  // For other uses, I wonder if returning the index 
  // where it is found, or -1 for not found, 
  // would be more useful?
  int i;
  assert(pp);
  assert(tt);

  for (i = 0; i < pp->num_entries; i++) {
    struct terrain * terrain = palette_terrain(pp, i);
    if (terrain == tt)
      return pp;
  }
  return 0;  // Did not find the terrain in pp
}

struct terrain * palette_current_terrain(struct terrain_palette * pp)
{
  // Return the terrain object which is the "current" 
  // terrain for this palette.
  // If the notion of "current" terrain was not already set,
  // it is transparently set to a sensible default.
  // 
  // This function will always return a terrain when called properly;
  // this requires that the palette pp was created with 
  // new_terrain_palette() and at least one palette_entry added,
  // such as by LTP_wrapper() + load_terrain_palette_entry().
  assert(pp);
  if (pp->num_entries < 1) {
    printf("palette_current_terrain() called on an empty palette.\n");
    assert(0);  // API usage error to call on an empty palette
  }

  int index = pp->current_terrain_index;
  if (index == PAL_TERRAIN_NOT_SET) {
    // Set and return a useful value:
    // Since num_entries >= 1, we know that
    // the entry in slot zero is valid.
    pp->current_terrain_index = 0;
    return palette_terrain(pp, 0);
  }
  else if (index < 0 || index >= pp->num_entries) {
    printf("palette_current_terrain() "
           "called with out-of-bounds arg n=%d\n", index);
    assert(0);
  }

  if (!palette_terrain(pp, index)) {
    // Somehow an entry in set[] is invalid!
    printf("palette_current_terrain() "
           "palette entry %d is not valid!\n", index);
    assert(0);
  }
  return palette_terrain(pp, index);
}

int palette_get_current_terrain_index(struct terrain_palette * pp)
{
  assert(pp);
  return pp->current_terrain_index;
}

int palette_set_current_terrain(struct terrain_palette * pp, int n)
{
  assert(pp);
  if (n < 0 || n >= pp->num_entries) {
    printf("palette_set_current_terrain() called with out-of-bounds arg n=%d", n);
    return PAL_TERRAIN_NOT_SET;  // Invalid index
  }
  if (!palette_terrain(pp, n)) {
    // Is this case possible?
    printf("palette_set_current_terrain() called for empty slot n=%d", n);
    return PAL_TERRAIN_NOT_SET;  // Invalid index

  }
  pp->current_terrain_index = n;
  return n;  // Valid index
}

int palette_prev_terrain(struct terrain_palette * pp)
{
  assert(pp);
  int n = pp->current_terrain_index;
  n--;
  if (n < 0)
    n = (pp->num_entries - 1);
  return palette_set_current_terrain(pp, n);
}

int palette_next_terrain(struct terrain_palette * pp)
{
  assert(pp);
  int n = pp->current_terrain_index;
  n++;
  if (n >= pp->num_entries)
    n = 0;
  return palette_set_current_terrain(pp, n);
}

int palette_first_terrain(struct terrain_palette * pp)
{
  assert(pp);
  return palette_set_current_terrain(pp, 0);
}

int palette_last_terrain(struct terrain_palette * pp)
{
  assert(pp);
  int n = (pp->num_entries - 1);
  return palette_set_current_terrain(pp, n);
}

int palette_get_quick_terrain_index(struct terrain_palette * pp, int qt)
{
  assert(pp);
  if (qt < 0 || qt >= NUM_QUICK_TERRAINS) {
    printf("palette_get_quick_terrain_index() "
           "called with out-of-bounds arg qt=%d\n", qt);
    return PAL_TERRAIN_NOT_SET;  // Invalid index
  }
  return pp->quick_terrain[qt];
}

int palette_set_quick_terrain(struct terrain_palette * pp, int qt, int index)
{
    assert(pp);
    if (qt < 0 || qt >= NUM_QUICK_TERRAINS) {
      printf("palette_quick_terrain() "
             "called with out-of-bounds arg qt=%d\n", qt);
      return PAL_TERRAIN_NOT_SET;  // Invalid index
    }
    if (index < 0 || index >= pp->num_entries) {
      printf("palette_quick_terrain() "
             "called with out-of-bounds arg index=%d\n", index);
      return PAL_TERRAIN_NOT_SET;  // Invalid index
    }
    pp->quick_terrain[qt] = index;
    return index;  // Valid index
}

struct terrain * palette_quick_terrain(struct terrain_palette *pp, int qt)
{
  assert(pp);
  if (qt < 0 || qt >= NUM_QUICK_TERRAINS) {
    printf("palette_quick_terrain() "
           "called with out-of-bounds arg qt=%d\n", qt);
    assert(0);
  }
  int index = palette_get_quick_terrain_index(pp, qt);
  return palette_terrain(pp, index);
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
