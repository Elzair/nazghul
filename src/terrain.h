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

#include "macros.h"

BEGIN_DECL

#include "list.h"
#include "object.h"
#include "ptable.h"     /* for ptable_get() */

#include <stdio.h>      /* for FILE */
#include <closure.h>

struct sprite;

#define terrain_combat_map(t) ((t)->combat_map)

/* Terrain effects */
#define TERRAIN_NEUTRAL 0
#define TERRAIN_POISON  EFFECTPOISON
#define TERRAIN_BURN    EFFECT_BURN

//extern int TERRAIN_MAGIC;
#define TERRAIN_MAGIC (0xc01dbee3)

#define terrain_pclass(t) ((t)->pclass)

struct terrain {
        int magic;
        struct list session_list; /* list of all terrains in this session */
        char *tag;
        char *name;
        struct sprite *sprite;
        struct terrain_map *combat_map;
        unsigned char alpha;
        int pclass;
        int light;
        closure_t *effect; /* when stood on */
        closure_t *renderCombat; /* closure to set up combat map */
};

extern struct terrain *terrain_new(const char *tag, const char *name, 
                                   struct sprite *sprite,
                                   int pclass, 
                                   int alpha, 
                                   int light);
extern void terrain_del(struct terrain *terrain);
extern void terrain_alloc_mmode_table(int n_mmodes);

#define LONGEST_TERRAIN_GLYPH   4

struct terrain_palette_entry {
    struct list lookup_list; /* listed by fast lookup order */
    struct list edit_list; /* listed by palette order */
    char           * glyph;
    struct terrain * terrain;
};

void palette_entry_print(FILE * fp, int indent,
                         struct terrain_palette_entry *entry);

struct terrain_palette {
    struct list list; /* list of palettes */
    char *tag;
    int widest_glyph; 
    int current_terrain_index;
    int free_index;
    int num_entries;
    struct list lookup_head; /* list of terrains for lookup */
    struct list edit_head; /* list of terrains for editor */
};

struct terrain_palette * palette_contains_terrain (struct terrain_palette *pp, struct terrain *tt);
struct terrain_palette * terrain_palette_new(const char *tag);
void terrain_palette_del(struct terrain_palette *pal);
void terrain_palette_add(struct terrain_palette *pal, char *glyph, struct terrain *ter);
char * palette_glyph(struct terrain_palette *pp, int n);
char * palette_glyph_for_terrain (struct terrain_palette * pp, struct terrain * tt);
struct terrain_palette_entry *palette_entry(struct terrain_palette *palette, int n);
struct terrain * palette_terrain(struct terrain_palette *pp, int n);
struct terrain * palette_terrain_for_glyph(struct terrain_palette *pp, char *glyph);
struct terrain * palette_current_terrain(struct terrain_palette * pp);
void palette_print(FILE * fp, int indent, struct terrain_palette *pp);

END_DECL

#endif // terrain_h
