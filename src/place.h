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
/* 12/14/2002 Sam Glasby added place_get_terrain()
 */
#ifndef place_hdr
#define place_hdr

#ifdef __cplusplus
extern "C" {
#endif

#include "terrain_map.h"
#include "list.h"
#include "object.h"
#include "NpcParty.h"
#include "astar.h"
#include "common.h"
#include "sky.h"

#include <SDL/SDL.h>

#define place_w(p) ((p)->terrain_map->w)
#define place_h(p) ((p)->terrain_map->h)
#define place_index(p,x,y) ((y) * place_w(p) + (x))
#define place_name(p) ((p)->name)
#define place_is_wilderness(p) ((p)->type == wilderness_place)
#define place_get_item(p,x,y) place_get_object((p),(x),(y),item_layer)

#define PFLAG_HORZ          (1 << 0) /* matches ASTAR_HORZ */
#define PFLAG_VERT          (1 << 1) /* matches ASTAR_VERT */
#define PFLAG_IGNOREMECHS   (1 << 2)
#define PFLAG_IGNOREBEINGS  (1 << 3)
#define PFLAG_IGNOREVEHICLES (1 << 4)

        struct location;
        struct place;
        class Portal;
        struct hash;
        class Moongate;

        enum place_type {
                wilderness_place = 0,
                town_place,
                dungeon_place,
                combat_place,
        };

        struct location {
                struct place *place;
                unsigned int x;
                unsigned int y;
        };

        struct typ_npc_party_info {
                class NpcPartyType *type;
                int prob;
                int align;
        };

        struct place {
                struct list list;
                enum place_type type;
                struct location location;
                struct place *above, *below;
                char *tag;
                char *name;
                struct terrain_map *original_terrain_map;
                struct terrain_map *terrain_map;
                struct hash *objects;
                struct list vehicles;
                int n_typ_npc_parties;
                struct typ_npc_party_info *typ_npc_parties;
                int scale;
                bool wraps;
                bool dirty;
                bool underground;
        };

        extern struct place *Place;

	extern struct place *place_create(enum place_type type,
                struct place *parent,
                unsigned int x,
                unsigned int y,
                char *tag, 
                char *name, 
                int wraps, 
					  struct terrain_map *terrain_map);

	extern void place_destroy(struct place *place);

        extern int place_is_passable(struct place *place, int x, int y,
                                     unsigned char pmask, int flags);

	extern int place_is_occupied(struct place *place, int x, int y);

	extern void placePaint(struct place *place,
			       SDL_Rect * region,
			       SDL_Rect * dest,
			       unsigned char *mask, int tile_w, int tile_h);

	extern int place_visibility(struct place *place, int x, int y);

	extern unsigned int place_walking_distance(struct place *place,
                int x0, 
						   int y0, int x1, int y1);

        extern unsigned int place_flying_distance(struct place *place, 
                                                  int x0, int y0, 
                                                  int x1, int y1);

	extern class Portal *place_get_portal(struct place *place,
					      int x, int y);

	extern class NpcParty *place_get_NpcParty(struct place *place,
						  int x, int y);

	extern class Vehicle *place_get_vehicle(struct place *place,
						int x, int y);

	extern int place_eval_path(struct place *place,
				   int x0, int y0, int x1, int y1);

	extern int place_add_object(struct place *place, Object * object);

	extern void place_remove_object(struct place *place, Object * object);

	extern Object *place_get_object(struct place *place,
					int x, int y, enum layer layer);
        extern void place_add_moongate(struct place *place, 
				       class Moongate * moongate);

#if 0
	extern void place_remove_vehicle(struct place *place,
					 class Vehicle * vehicle);
#endif
	extern class NpcParty *place_search_for_NpcParty(struct place *place,
                                  int x,
                                  int y,
                                  int radius,
							 int (*criteria) (class
									  NpcParty
									  *
									  NpcParty)
                );
                
	/* Caller needs to zero out the info struct and fill in the following
	 * fields: x0, y0, x1, y1, pmask, flags. Optionally if the caller wants 
	 * to limit the depth of the search he should fill out the limit_depth
	 * and max_depth fields. */
	extern struct astar_node *place_find_path(struct place *place, struct astar_search_info
						  *info, unsigned char pmask);
                
	extern struct terrain_map *place_get_combat_terrain_map(struct place
								*place, int x,
								int y);
        
	static inline int place_off_map(struct place *place, int x, int y) {
                if (place->wraps)
                        return 0;
                return (x < 0 || x >= place_w(place) || y < 0 || 
                        y >= place_h(place));
        }
        extern void place_clip_to_map(struct place *place, int *x, int *y);

	static inline int place_wrap_x(struct place *place, int x) {
                if (place->wraps)
			return ((x +
				 place->terrain_map->w) %
				place->terrain_map->w);
                return x;
        }

	static inline int place_wrap_y(struct place *place, int y) {
                if (place->wraps)
			return ((y +
				 place->terrain_map->h) %
				place->terrain_map->h);
                return y;
        }

        extern void place_for_each_object(struct place *place, 
					  void (*fx) (class Object *,
						      void *data), void *data);

        extern int place_get_light(struct place *place, int x, int y);
        
        extern void place_set_terrain(struct place *place, int x, int y, 
                                      struct terrain *terrain);
	struct terrain *place_get_terrain(struct place *place, int x, int y);
        Uint32 place_get_color(struct place *place, int x, int y);
        int place_get_movement_cost(struct place *place, int x, int y);

	extern class NpcParty *place_random_encounter(struct place *);

        extern void placeExit(void);
        extern void placeEnter(void);
	extern class Moongate *place_get_moongate(struct place *, int x, int y);
	extern void placeAddObject(Object * object);
	extern void placeRemoveObject(Object * object);
        extern class NpcParty *placeGetNPC(int x, int y);
	extern void placeRemoveNPC(class NpcParty * NpcParty);
        extern int placeGetMovementCost(int x, int y);
        extern struct terrain *placeGetTerrain(int x, int y);
        extern int placeWrapX(int x);
        extern int placeWrapY(int y);
        extern void placeDescribe(int x, int y);
	extern void placeForEachObject(void (*fx) (class Object *, void *data),
                                       void *data);
        extern void placeInit(void);
        extern void placeAdvanceCombatTurn(void);
        // combat
        extern void placeDumpObjects(void);
//extern struct place *placeLoad(class Loader *loader);
        extern void placeAdvanceTurns(void);


#ifdef __cplusplus
}
#endif

#endif
