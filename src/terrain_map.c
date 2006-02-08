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
#include "terrain_map.h"
#include "terrain.h"
#include "map.h"
#include "session.h"
#include "common.h"

#include <assert.h>

char *TERRAIN_MAP_MAGIC = "TERRAIN_MAP";

struct terrain_map *terrain_map_new(char *tag, unsigned int w,
                                    unsigned int h,
                                    struct terrain_palette * pal)
{
	struct terrain_map *terrain_map;
	CREATE(terrain_map, struct terrain_map, 0);
	if (tag) {
                terrain_map->tag = strdup(tag);
                assert(terrain_map->tag);
        }
        terrain_map->magic = TERRAIN_MAP_MAGIC;
	terrain_map->w = w;
	terrain_map->h = h;
        terrain_map->palette = pal;
	terrain_map->terrain =
                (struct terrain **) malloc(sizeof(struct terrain *) * w * h);
        terrain_map->refcount = 1;
        assert(terrain_map->terrain);
	memset(terrain_map->terrain, 0, sizeof(struct terrain *) * w * h);
	return terrain_map;
}

struct terrain_map *terrain_map_clone(struct terrain_map *orig, char *tag)
{
	struct terrain_map *map;

        if (!orig->palette) {
                err("terrain_map_clone() \n"
                    " called to clone a map (tag '%s') without a palette,\n"
                    " this may be or cause a problem elsewhere.\n", 
                    orig->tag);
        }
        
	map = terrain_map_new(tag, orig->w, orig->h, orig->palette);        
	memcpy(map->terrain, orig->terrain,
	       sizeof(struct terrain *) * orig->w * orig->h);

        map->composite = orig->composite;
        map->submap_w = orig->submap_w;
        map->submap_h = orig->submap_h;
        
	return map;
}

void terrain_map_rotate(struct terrain_map *map, int degree)
{
    // SAM:
    // One thing we will want to provide for in the future
    // is maps with terrain with different orientations.
    // The canonical example is diagonal wall sections
    // for NW,NE,SW,SE corners.
    // 
    // A map containing such pieces looks strange after rotation.
    // Presumably in future some means will exist of specifying 
    // that a terrain has an "orientation" or "facing" which must 
    // be preserved (by substitution of terrain and/or sprite) 
    // when the map is rotated.
	struct terrain **rbuf;
	int x1, y1, x2, y2;
	int w2 = map->w;
        int h2 = map->h;
     
	// Originally I tried a rotation matrix with a naive implementation,
	// but I overlooked the problem that tile coordinates do not match up
	// naturally with the point-based coordinates of the numeric axis
	// assumed by such an alg. Yes, you twiddle things to work but I don't
	// think it's worth it given the limited nature of the rotations I
	// support here. So I fell back on a set of straightforward copy
	// routines.

	rbuf =
	    (struct terrain **) malloc(sizeof(struct terrain *) * map->w *
				       map->h);
	if (rbuf == NULL) {
		err("malloc failed");
		return;
	}
	// First convert the degrees to one of the four cases.
	degree = degree % 360;
	degree = degree / 90;

	switch (degree) {
	case 0:
		// Nothing to do.
		return;
	case 1:
		// 90 degree clockwise rotation:
		// 
		// 0  1  2    9  6  3  0
		// 3  4  5 => 10 7  4  1
		// 6  7  8    11 8  5  2
		// 9 10 11
		w2 = map->h;
		h2 = map->w;
		for (y1 = 0, x2 = w2 - 1; y1 < map->h; y1++, x2--) {
			for (x1 = 0, y2 = 0; x1 < map->w; x1++, y2++) {
				rbuf[y2 * w2 + x2] =
				    map->terrain[y1 * map->w + x1];
			}
		}
		break;
	case 2:
		// 180 degree rotation:
		// 
		// 0  1  2    11 10  9 
		// 3  4  5 =>  8  7  6
		// 6  7  8     5  4  3
		// 9 10 11     2  1  0
		w2 = map->w;
		h2 = map->h;
		for (y1 = 0, y2 = h2 - 1; y1 < map->h; y1++, y2--) {
			for (x1 = 0, x2 = w2 - 1; x1 < map->w; x1++, x2--) {
				rbuf[y2 * w2 + x2] =
				    map->terrain[y1 * map->w + x1];
			}
		}
		break;
	case 3:
		// 90 degree counter-clockwise rotation:
		// 
		// 0  1  2    2  5  8 11
		// 3  4  5 => 1  4  7 10
		// 6  7  8    0  3  6  9
		// 9 10 11
		w2 = map->h;
		h2 = map->w;
		for (y1 = 0, x2 = 0; y1 < map->h; y1++, x2++) {
			for (x1 = 0, y2 = h2 - 1; x1 < map->w; x1++, y2--) {
				rbuf[y2 * w2 + x2] =
				    map->terrain[y1 * map->w + x1];
			}
		}
		break;
	default:
		assert(false);
	}

	// debug

	// Free the original copy.
	free(map->terrain);

	// Replace the original with the rotated copy.
	map->terrain = rbuf;
	map->w = w2;
	map->h = h2;

}

void terrain_map_unref(struct terrain_map *terrain_map)
{
        assert(terrain_map->refcount);
        terrain_map->refcount--;
        if (!terrain_map->refcount) {
                if (terrain_map->tag)
                        free(terrain_map->tag);
                if (terrain_map->terrain)
                        free(terrain_map->terrain);
                free(terrain_map);
        }
}

void terrain_map_blit(struct terrain_map *dest, int dest_x, int dest_y,
                      struct terrain_map *src,  int src_x,  int src_y,
                      int w, int h)
{
	int x, y;
	struct terrain **dptr, **sptr;

	// truncate dimensions if nec
	w = min(dest->w - dest_x, min(w, min(dest->w, src->w)));
	h = min(dest->h - dest_y, min(h, min(dest->h, src->h)));

	for (y = 0; y < h; y++) {
		dptr = dest->terrain + ((y + dest_y) * dest->w + dest_x);
		sptr = src->terrain + ((y + src_y) * src->w + src_x);
		for (x = 0; x < w; x++) {
			*dptr++ = *sptr++;
		}
	}
}

void terrain_map_fill(struct terrain_map *map, int x, int y, int w, int h,
		      struct terrain *fill)
{
	int x2, y2;
    assert(map);
    assert(fill);

	for (y2 = y; y2 < (y + h); y2++) {
		if (y2 < 0)
			continue;
		if (y2 >= map->h)
			break;
		for (x2 = x; x2 < (x + w); x2++) {
			if (x2 < 0)
				continue;
			if (x2 >= map->w)
				break;
			map->terrain[y2 * map->w + x2] = fill;
		}
	}
}

#define PREFER_COMPACT_MAPS 1
extern void terrain_map_print(FILE * fp, int indent, struct terrain_map *map)
{
    int x, y, compact;
    struct terrain_palette * palette;
	assert(fp);
    assert(map);

    palette = map->palette;
    if (!palette) {
        // No point in carrying on without a palette.
        printf("terrain_map_print(): No palette for map '%s'.", map->tag);
        exit(1);
    }

    // (kern-mk-map 'm_map www hhh pal_expanded
    //     (list
    //         ; 0  1  2  3
    //         ".. .. .. ..";  //  0
    //         ".. .. .. ..";  //  1
    //         ".. .. .. ..";  //  2
    //     )
    // ) ;; map m_map


	INDENT;
    // fprintf(fp, "MAP '%s' {\n", map->tag);
    fprintf(fp, "(kern-mk-map '%s %d %d %s\n", map->tag, map->w, map->h, palette->tag);
	indent += INDENTATION_FACTOR;

    compact = (PREFER_COMPACT_MAPS && palette->widest_glyph == 1);
    if (compact) {
      INDENT; fprintf(fp, ";; Compact map (glyphs are 1 char wide)\n");
    }

    INDENT; fprintf(fp, "(list\n");
    print_horizontal_guideline(fp, indent, map);

	indent += INDENTATION_FACTOR;

    int w = palette->widest_glyph;
	for (y = 0; y < map->h; y++) {
      INDENT; fprintf(fp, "\"");

      for (x = 0; x < map->w; x++) {
        int i = (y * map->w) + x;
        struct terrain * tt = map->terrain[i];
        char * glyph = palette_glyph_for_terrain(palette, tt);
        if (!glyph) {
          // SAM: 
          // There are still circumstance(s) where this can happen.
          // One example is a ship map blitted onto another combat map.
          // The ship hull terrain is not in the standard palette,
          // and each such terrain will hit this clause.
          // 
          // No point in carrying on.
          printf("terrain_map_print(): No glyph at XY=(%d,%d) map '%s'.", x, y, map->tag);
          exit(1);
        }
        fprintf(fp, "%*s%s", w, glyph, (compact) ? "" : " ");
      } // for (x)

      fprintf(fp, "\";  // %3d\n", y);
	} // for (y)

	indent -= INDENTATION_FACTOR;
    // SAM: BUG: The horizontal guide after the list of terrain produces a complaint from Scheme.  Why?
    // print_horizontal_guideline(fp, indent, map);
	INDENT;	
    fprintf(fp, ")\n");

	indent -= INDENTATION_FACTOR;
	INDENT;	
    fprintf(fp, ") ;; map %s\n", map->tag);
	fprintf(fp, "\n");
}


void print_horizontal_guideline (FILE * fp, int indent, struct terrain_map *map)
{
    // Note that the horizontal coordinates guide-lines below
    // will only be lined up correctly if INDENTATION_FACTOR == 2
    struct terrain_palette * palette;
    int compact;
	assert(fp);
    assert(map);

    palette = map->palette;
    compact = (PREFER_COMPACT_MAPS && palette->widest_glyph == 1);

    if (compact) {
      // These templates will suffice for maps of up to width 80
      int w = map->w * palette->widest_glyph;
      static char line1_template[] = 
        "          1111111111222222222233333333334444444444555555555566666666667777777777";
      static char line2_template[] =
        "01234567890123456789012345678901234567890123456789012345678901234567890123456789";
      char * line_1 = strdup(line1_template);
      char * line_2 = strdup(line2_template);
      if (w <= 80) {
        line_1[w] = '\0';
        line_2[w] = '\0';
      }
      INDENT; fprintf(fp, ";; %s\n", line_1);
      INDENT; fprintf(fp, ";; %s\n", line_2);
    }
    else if (palette->widest_glyph == 1) {
      // These templates will suffice for maps of up to width 50
      int w = map->w * (palette->widest_glyph + 1);
      static char line1_template[] = 
        "                    1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 ";
      static char line2_template[] =
        "0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 ";
      char * line_1 = strdup(line1_template);
      char * line_2 = strdup(line2_template);
      if (w <= 50*2) {
        line_1[w] = '\0';
        line_2[w] = '\0';
      }
      INDENT; fprintf(fp, ";; %s\n", line_1);
      INDENT; fprintf(fp, ";; %s\n", line_2);
    }
    else if (palette->widest_glyph == 2) {
      // These templates will suffice for maps of up to width 40
      int w = map->w * (palette->widest_glyph + 1);
      static char line1_template[] = 
        "                               1  1  1  1  1  1  1  1  1  1  2  2  2  2  2  2  2  2  2  2  3  3  3  3  3  3  3  3  3  3  ";
      static char line2_template[] =
        " 0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5  6  7  8  9  ";
      char * line_1 = strdup(line1_template);
      char * line_2 = strdup(line2_template);
      if (w < 40*3) {
        line_1[w] = '\0';
        line_2[w] = '\0';
      }
      INDENT; fprintf(fp, ";; %s\n", line_1);
      INDENT; fprintf(fp, ";; %s\n", line_2);
    }
    // TODO: width 3 and 4 palettes.
}


static void terrain_map_composite_save(struct save *save, struct terrain_map *map)
{
        int x, y, w, h, sub_y, sub_x;

        map->saved = save->session_id;

        /* write the composite map constructor */
        save->enter(save, "(kern-mk-composite-map\n");
        if (map->tag)
                save->write(save, "'%s ", map->tag);
        else
                save->write(save, "nil ");
        w = map->w/map->submap_w;
        h = map->h/map->submap_h;
        save->write(save, "%d %d\n", w, h);

        /* for each submap */
        for (y = 0; y < h; y++) {
                for (x = 0; x < w; x++) {

                        /* write the submap constructor */
                        save->enter(save, "(kern-mk-map nil %d %d %s\n",
                                    map->submap_w, map->submap_h, 
                                    map->palette->tag);

                        /* write the submap terrain list */
                        save->enter(save, "(list\n");

                        for (sub_y = y*map->submap_h; 
                             sub_y < y*map->submap_h+map->submap_h; 
                             sub_y++) {

                                save->write(save, "\"");

                                for (sub_x = x*map->submap_w; 
                                     sub_x < x*map->submap_w+map->submap_w; 
                                     sub_x++) {                        
                                        char *glyph;
                                        
                                        glyph = palette_glyph_for_terrain(
                                                map->palette,
                                                map->terrain[sub_y*map->w+sub_x]);
                                        if (! glyph) {
                                                err("map %s: no glyph in palette %s "\
                                                    "for terrain at [%d %d]\n", map->tag,
                                                    map->palette->tag, sub_x, sub_y);
                                                assert(glyph);
                                        }
                                        
                                        // print with no indentation (same line)
                                        fprintf(save->file, "%2s ", glyph);
                                }
                                fprintf(save->file, "\"\n");
                        }
                        
                        save->exit(save, ")\n");
                        save->exit(save, ")\n");
                }
        }
        
        save->exit(save, ")\n");
}

void terrain_map_save(struct save *save, void *val)
{
        struct terrain_map *map;
        int x, y, i;

        map = (struct terrain_map*)val;

        if (map->saved == save->session_id) {
                save->write(save, "%s\n", map->tag);
                return;
        }

        if (map->composite) {
                terrain_map_composite_save(save, map);
                return;
        }

        save->enter(save, "(kern-mk-map\n");
        if (map->tag)
                save->write(save, "'%s ", map->tag);
        else
                save->write(save, "nil ");
        save->write(save, "%d %d %s\n", map->w, map->h, map->palette->tag);
        save->enter(save, "(list\n");

        i = 0;
        for (y = 0; y < map->h; y++) {
                save->write(save, "\"");

                for (x = 0; x < map->w; x++) {                        
                        char *glyph;

                        glyph = palette_glyph_for_terrain(map->palette,
                                                          map->terrain[i]);
                        if (! glyph) {
                                err("map %s: no glyph in palette %s "\
                                         "for terrain at [%d %d]\n", map->tag,
                                         map->palette->tag, x, y);
                                assert(glyph);
                        }

                        // print with no indentation (same line)
                        fprintf(save->file, "%2s ", glyph);

                        i++;
                }
                fprintf(save->file, "\"\n");
        }
        save->exit(save, ")\n");
        save->exit(save, ")\n");

        map->saved = save->session_id;
}

static inline struct terrain *terrain_map_get_terrain(struct terrain_map *map, int x, int y)
{
        return map->terrain[y * map->w + x];
}

static int terrain_not_sup(struct terrain *tt, int n_not_sup, struct terrain **not_sup)
{
        int i;
        for (i = 0; i < n_not_sup; i++) {
                if (tt == not_sup[i])
                        return 1;
        }

        return 0;
}

static int terrain_map_match_pattern(struct terrain_map *map, int ox, int oy, 
                                     struct terrain *inf, 
                                     int n_not_sup,
                                     struct terrain **not_sup
                                     )
{
        int rule = 0;

        /* north */
        if (! terrain_not_sup(terrain_map_get_terrain(map, ox, oy - 1),
                              n_not_sup, not_sup))
                rule += 1;

        /* west */
        if (! terrain_not_sup(terrain_map_get_terrain(map, ox - 1, oy),
                              n_not_sup, not_sup))
                rule += 2;

        /* east */
        if (! terrain_not_sup(terrain_map_get_terrain(map, ox + 1, oy),
                              n_not_sup, not_sup))
                rule += 4;

        /* south */
        if (! terrain_not_sup(terrain_map_get_terrain(map, ox, oy + 1),
                              n_not_sup, not_sup))
                rule += 8;

        return rule;
}

void terrain_map_blend(struct terrain_map *map, 
                       struct terrain *inf,
                       int n_not_sup,
                       struct terrain **not_sup,
                       struct terrain *range[16])
{
        int x, y, rule;

        for (y = 1; y < (map->h - 1); y++) {

                for (x = 1; x < (map->w - 1); x++) {

                        if (terrain_map_get_terrain(map, x, y) == inf) {

                                rule = terrain_map_match_pattern(map, x, y, inf, n_not_sup, not_sup);

                                if (rule) {
                                        map->terrain[y*map->w+x] = range[rule];
                                }

                        }
                }
        }
}
