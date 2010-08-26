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
#include "debug.h"
#include "sprite.h"
#include "common.h"
#include "object.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

//int TERRAIN_MAGIC = 0xc01dbee3;

extern struct terrain *terrain_new(const char *tag, 
                                   const char *name,
                                   struct sprite *sprite,
                                   int pclass, 
                                   int alpha, 
                                   int light)
{
	struct terrain *terrain;

        terrain = (struct terrain*)calloc(1, sizeof(*terrain));
        assert(terrain);

        terrain->magic         = TERRAIN_MAGIC;
        terrain->tag           = strdup(tag);
	terrain->name          = strdup(name);
        terrain->sprite        = sprite;
	terrain->pclass        = pclass;
	terrain->alpha         = alpha;
        terrain->light         = light;
	return terrain;
}

void terrain_del(struct terrain *terrain)
{
        if (terrain->tag) {
                free(terrain->tag);
        }
	if (terrain->name) {
		free(terrain->name);
        }
        if (terrain->effect) {
                closure_unref(terrain->effect);
        }
        free(terrain);
}

#define BOGUS_MAX_SIZE 255	// Hack, should get a constant from somewhere
// LONGEST_TERRAIN_GLYPH would be appropriate for glyph_str...

void palette_entry_print(FILE * fp, int indent,
			 struct terrain_palette_entry *entry)
{
	static char glyph_str[BOGUS_MAX_SIZE + 1];
    static char   tag_str[BOGUS_MAX_SIZE + 1];
	assert(fp);
    assert(entry);
    
    snprintf(glyph_str, BOGUS_MAX_SIZE, "\"%s\"", entry->glyph);
    snprintf(tag_str,   BOGUS_MAX_SIZE, "%s)",    entry->terrain->tag);
    
	INDENT;
    fprintf(fp, "(list  %-6s %-20s  ;; \"%s\"\n", glyph_str, tag_str, entry->terrain->name);
} // palette_entry_print()

struct terrain_palette *terrain_palette_new(const char *tag)
{
	struct terrain_palette *palette = new struct terrain_palette;
	assert(palette);
	memset(palette, 0, sizeof(struct terrain_palette));

        list_init(&palette->lookup_head);
        list_init(&palette->edit_head);
        palette->tag = strdup(tag);
        palette->widest_glyph = 0;
        palette->num_entries = 0;

	return palette;
}

struct terrain_palette_entry *
terrain_palette_entry_new(char *glyph, struct terrain *terrain)
{
        struct terrain_palette_entry *entry;
        entry = (struct terrain_palette_entry *)malloc(sizeof(*entry));
        list_init(&entry->lookup_list);
        list_init(&entry->edit_list);
        entry->glyph = strdup(glyph);
        assert(entry->glyph);
        entry->terrain = terrain;
        return entry;
}

void terrain_palette_entry_del(struct terrain_palette_entry *entry)
{
        // For each entry free the glyph (because we strdup'd our own copy) but
        // leave the terrain alone (it's a singleton and belongs to someone
        // else).
        free(entry->glyph);
        free(entry);
}

void terrain_palette_del(struct terrain_palette *pal)
{
        struct list *elem;

        elem = pal->edit_head.next;
        while (elem != &pal->edit_head) {
                struct terrain_palette_entry *entry;
                entry = outcast(elem, struct terrain_palette_entry, edit_list);
                elem = elem->next;
                terrain_palette_entry_del(entry);
        }

        if (pal->tag)
                free(pal->tag);
        delete pal;
}

void terrain_palette_add(struct terrain_palette *pal, char *glyph, 
                         struct terrain *ter)
{
        struct terrain_palette_entry *entry;
        int n = strlen(glyph);

        entry = terrain_palette_entry_new(glyph, ter);
        list_add_tail(&pal->lookup_head, &entry->lookup_list);
        list_add_tail(&pal->edit_head, &entry->edit_list);
        pal->num_entries++;
        if (pal->widest_glyph < n)
                pal->widest_glyph = n;
}

struct terrain_palette_entry *palette_entry(struct terrain_palette *palette, int n)
{
        struct list *elem;

        assert(palette);
        if (palette->num_entries < 1) {
                dbg("palette_terrain_for_glyph() num_entries == 0\n");
                return 0;
        }
        if (n < 0 || n >= palette->num_entries) {
                dbg("palette_terrain_for_glyph() called with out-of-bounds "\
                    "arg n=%d\n", n);
                return 0;
        }

        elem = palette->edit_head.next;
        while (n) {
                elem = elem->next;
                n--;
        }

        return outcast(elem, struct terrain_palette_entry, edit_list);
}

char *palette_glyph(struct terrain_palette *palette, int n)
{
        struct terrain_palette_entry *entry;

        entry = palette_entry(palette, n);
        if (entry)
                return entry->glyph;
        return 0;
}

struct terrain_palette_entry *
palette_entry_for_terrain(struct terrain_palette * pp, struct terrain * tt)
{
        struct list *elem;
        struct terrain_palette_entry *entry;

        list_for_each(&pp->lookup_head, elem) {
                entry = outcast(elem, struct terrain_palette_entry, lookup_list);
                if (tt == entry->terrain)
                        return entry;
        }

        return 0;  // Did not find the terrain
}

char * palette_glyph_for_terrain (struct terrain_palette * pp, struct terrain * tt)
{
        struct terrain_palette_entry *entry;

        entry = palette_entry_for_terrain(pp, tt);
        if (entry)
                return entry->glyph;
        return 0;  // Did not find the terrain
}

struct terrain *palette_terrain(struct terrain_palette *palette, int n)
{
        struct terrain_palette_entry *entry;

        entry = palette_entry(palette, n);
        if (entry)
                return entry->terrain;
        return 0;
}

struct terrain *palette_terrain_for_glyph(struct terrain_palette *palette,
					  char *glyph)
{
        struct list *elem;
        struct terrain_palette_entry *entry;

        list_for_each(&palette->lookup_head, elem) {
                entry = outcast(elem, struct terrain_palette_entry, lookup_list);
                if (! strcmp(glyph, entry->glyph)) {
                        /* Odds are good that we'll want this same terrain in
                         * the near future, so move it to the front of the list
                         * (if not already there) to improve performance during
                         * startup. */
                        if (elem != palette->lookup_head.next) {
                                list_remove(elem);
                                list_add(&palette->lookup_head, elem);
                        }
                        return entry->terrain;
                }
        }

        return 0;  // Did not find the terrain
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
        struct terrain_palette_entry *entry;

        entry = palette_entry_for_terrain(pp, tt);
        if (entry)
                return pp;
        return 0;  // Did not find the terrain
}

void palette_print(FILE * fp, int indent, struct terrain_palette *palette)
{
        struct list *elem;
	assert(fp);

    // (kern-mk-palette 'pal_expanded
    //     (list
    //         ;; There are 999 entries in this palette
    //         ;; The widest glyph is 4 characters
    //         (list "__" t_deep)
    //         (list ".." t_grass)
    //     )
    // )


	INDENT;
	fprintf(fp, "(kern-mk-palette '%s\n", palette->tag);
	indent += INDENTATION_FACTOR;

	INDENT;
	fprintf(fp, "(list\n");
	indent += INDENTATION_FACTOR;
    INDENT;
    fprintf(fp, ";; There are %d entries in this palette\n", palette->num_entries);
    INDENT;
    fprintf(fp, ";; The widest glyph is %d characters\n",   palette->widest_glyph);
    
    list_for_each(&palette->edit_head, elem) {
        struct terrain_palette_entry *entry;
        entry = outcast(elem, struct terrain_palette_entry, edit_list);
		palette_entry_print(fp, indent, entry);
	}

    fprintf(fp, "\n");

    indent -= INDENTATION_FACTOR;
    INDENT;
    fprintf(fp, ")\n");

    indent -= INDENTATION_FACTOR;
    INDENT;
    fprintf(fp, ") ;; palette %s\n", palette->tag);
	fprintf(fp, "\n");
} // palette_print()
