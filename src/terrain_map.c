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
#include "util.h"
#include "terrain.h"
#include "map.h"

#include <assert.h>

struct terrain_map *terrain_map_create(char *tag, unsigned int w,
				       unsigned int h)
{
	struct terrain_map *terrain_map;
	CREATE(terrain_map, struct terrain_map, 0);
	if (tag && !(terrain_map->tag = strdup(tag)))
		goto fail;
	terrain_map->w = w;
	terrain_map->h = h;
	if (!(terrain_map->terrain =
	      (struct terrain **) malloc(sizeof(struct terrain *) * w * h)))
		goto fail;
	memset(terrain_map->terrain, 0, sizeof(struct terrain *) * w * h);
	return terrain_map;

      fail:
	terrain_map_destroy(terrain_map);
	return 0;
}

struct terrain_map *terrain_map_clone(struct terrain_map *orig)
{
	struct terrain_map *map;

	if (!(map = terrain_map_create(NULL, orig->w, orig->h)))
		return NULL;

	memcpy(map->terrain, orig->terrain,
	       sizeof(struct terrain *) * orig->w * orig->h);

	return map;
}

void terrain_map_rotate(struct terrain_map *map, int degree)
{
	struct terrain **rbuf;
	int x1, y1, x2, y2, w2, h2;

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
		// 0 1 2 9 6 3 0
		// 3 4 5 => 10 7 4 1
		// 6 7 8 11 8 5 2
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
		// 0 1 2 11 10 9 
		// 3 4 5 => 8 7 6
		// 6 7 8 5 4 3
		// 9 10 11 2 1 0
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
		// 0 1 2 2 5 8 11
		// 3 4 5 => 1 4 7 10
		// 6 7 8 0 3 6 9
		// 9 10 11
		w2 = map->h;
		h2 = map->w;
		for (y1 = 0, x2 = 0; y1 < map->h; y1++, x2++) {
			for (x1 = 0, y2 = h2 - 1; x1 < map->w; x1++, y2--) {
				printf("%d => %d\n", y1 * map->w + x1,
				       y2 * w2 + x2);
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

void terrain_map_destroy(struct terrain_map *terrain_map)
{
	if (terrain_map->tag)
		free(terrain_map->tag);
	if (terrain_map->terrain)
		free(terrain_map->terrain);
	free(terrain_map);
}

void terrain_map_blit(struct terrain_map *dest, int dest_x, int dest_y,
		      struct terrain_map *src, int src_x, int src_y,
		      int w, int h)
{
	int x, y;
	struct terrain **dptr, **sptr;

	// truncate dimensions if nec
	w = min(dest->w - dest_x, min(w, min(dest->w, src->w)));
	h = min(dest->h - dest_y, min(h, min(dest->h, src->h)));

	printf("[%d %d %d %d] => [%d %d %d %d]\n", src_x, src_y, w, h, dest_x,
	       dest_y, w, h);

	for (y = 0; y < h; y++) {
		dptr = dest->terrain + ((y + dest_y) * dest->w + dest_x);
		sptr = src->terrain + ((y + src_y) * src->w + src_x);
		for (x = 0; x < w; x++) {
			*dptr++ = *sptr++;
		}
	}

	mapRecomputeLos(ALL_VIEWS);
	mapSetDirty();
}

void terrain_map_fill(struct terrain_map *map, int x, int y, int w, int h,
		      struct terrain *fill)
{
	int x2, y2;

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
