/* Copyright (c) 2002 Gordon McNutt */
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
                int id;
                char glyph;
                unsigned char alpha;
                unsigned int pmask; /* passability mask (sea, air land) */
		int movement_cost;	/* should not exceed
					 * PLAYER_MAX_PROGRESS */
                char effects;
                int light;
                Uint32 color;
        };

	extern struct terrain *terrain_create(char *tag,
                char *name, 
                unsigned int pmask,
                struct sprite *sprite, 
                char glyph, 
					      int id, unsigned char alpha);

        extern void terrain_destroy(struct terrain *terrain);

#ifdef __cplusplus
}
#endif

#endif
