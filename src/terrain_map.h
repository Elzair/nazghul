/* Copyright (c) 2002 Gordon McNutt */
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
