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
#ifndef terrain_map_h
#define terrain_map_h

#ifdef __cplusplus
extern "C" {
#endif

#include "list.h"

        struct terrain_map {
                struct list list;
                char *tag;
                int w;
                int h;
                struct terrain **terrain;
        };
        
        extern struct terrain_map *terrain_map_create(char *tag, 
                                                      unsigned int w, 
                                                      unsigned int h);
        extern void terrain_map_destroy(struct terrain_map *map);
        extern struct terrain_map *terrain_map_clone(struct terrain_map *orig);
        extern void terrain_map_rotate(struct terrain_map *map, int degree);
        extern void terrain_map_blit(struct terrain_map *dest, int dest_x,
                                     int dest_y, struct terrain_map *src,
                                     int src_x, int src_y, int w, int h);
        extern void terrain_map_fill(struct terrain_map *map, int x, int y, 
                                     int w, int h, struct terrain *fill);

#ifdef __cplusplus
}
#endif

#endif
