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

#include "macros.h"

BEGIN_DECL

#include "terrain_map.h"
#include "list.h"
#include "object.h"
#include "Party.h"
#include "astar.h"
#include "common.h"
#include "sky.h"

#ifdef TURN_LIST_NODES
#include "node.h"
#endif

#include <SDL.h>

#define place_w(p) ((p)->terrain_map->w)
#define place_h(p) ((p)->terrain_map->h)
#define place_index(p,x,y) ((y) * place_w(p) + (x))
#define place_name(p) ((p)->name)
#define place_is_wilderness(p) ((p)->wilderness)
#define place_is_town(p) (!(p)->wilderness)
#define place_is_dungeon(p) (! (p)->wilderness)
#define place_is_wrapping(p) ((p)->wraps)
#define place_get_item(p,x,y) place_get_object((p),(x),(y),item_layer)
#define place_get_parent(p) ((p)->location.place)
#define place_get_scale(p) ((p)->scale)
#define place_get_x(p) ((p)->location.x)
#define place_get_y(p) ((p)->location.y)
#define place_is_wilderness_combat(p) ((p)->is_wilderness_combat)
#define place_lock(p) ((p)->lock++)
/* place_unlock() is a function (see below) */
#define place_is_locked(p) ((p)->lock)
#define place_mark_for_death(p) ((p)->marked_for_death = 1)
#define place_is_marked_for_death(p) ((p)->marked_for_death)
#define place_max_distance(p) (place_w(p) + place_h(p))

#ifdef TURN_LIST_NODES
#define place_get_all_objects(p) (&(p)->turn_list)
#endif

#define PFLAG_HORZ             (1 << 0) /* matches ASTAR_HORZ */
#define PFLAG_VERT             (1 << 1) /* matches ASTAR_VERT */
#define PFLAG_IGNOREMECHS      (1 << 2)
#define PFLAG_IGNOREBEINGS     (1 << 3)
#define PFLAG_IGNOREVEHICLES   (1 << 4)
#define PFLAG_IGNORECOMPANIONS (1 << 5)
#define PFLAG_MOVEATTEMPT      (1 << 6)
#define PFLAG_IGNORETERRAIN    (1 << 7) /* used by Object::putOnMap */
#define PFLAG_IGNOREHAZARDS    (1 << 8) /* used by Object::putOnMap */
#define PFLAG_IGNOREFIELDS     (1 << 9) /* used by Object::putOnMap */
#define PFLAG_IGNORETFEAT      (1 << 10)

// Flags for placeDescribe:
#define PLACE_DESCRIBE_TERRAIN (1 << 0)
#define PLACE_DESCRIBE_OBJECTS (1 << 1)
#define PLACE_DESCRIBE_ALL     (~0)

struct location;
struct place;
struct hash;

struct location {
        struct place *place;
        int x;
        int y;
};

#define PLACE_MAGIC PLACE_ID

struct place {

        // 'magic' is used by the loader to type-check a pointer passed in from
        // the script.
        int magic;

#ifdef TURN_LIST_NODES
        struct node turn_list;
        struct node *turn_elem;
#else
        struct list turn_list;
        struct list *turn_elem;
#endif

        struct list list;
        struct location location;
        struct place *above, *below;
        char *tag;
        char *name;
        struct sprite *sprite;
        struct terrain_map *original_terrain_map;
        struct terrain_map *terrain_map;
        struct hash *objects;
        struct list vehicles;
        int scale;
        bool wraps;
        bool dirty;
        bool underground;
        bool is_wilderness_combat;
        bool wilderness;

        // Hack: need this so I can remove the tmp wild combat place after
        // loading it from a game that saved during combat.
        void *handle;

        // List of subplaces. Allows me to save subplaces in a separate block
        // from the other tile objects. The only reason for doing that is to
        // make the save file easier to read.
        struct list subplaces;

        // List hook used to put a subplace on the subplaces list of its
        // superplace.
        struct list container_link;

        struct closure *pre_entry_hook;

        int saved;
        int lock;
        int marked_for_death : 1;

        /* Coordinates used to position the player in a subplace when he enters
         * from the edge: */
        int edge_entrance[NUM_PLANAR_DIRECTIONS][2];
};

extern struct place *Place;

extern struct place *place_new(char *tag,
                               char *name,
                               struct sprite *sprite,
                               struct terrain_map *terrain_map,
                               int wraps,
                               int underground,
                               int wilderness,
                               int wilderness_combat
                               );

#ifdef MAP_REGIONS

/* The new region api */

typedef struct region {
        char *tag;
        char *name;
        struct location loc;
        struct terrain_map *map;
        struct list objects;
        struct list subplaces;
} region_t;

typedef struct region_map {
        int w;
        int h;
        region_t *map;
} region_map_t;

extern region_t *region_new(char *tag, char *name, struct terrain_map *map);
extern void region_del(region_t*);
extern int region_add_object(region_t *, Object *, int x, int y);
extern int region_rm_object(region_t *, Object *);
extern int region_add_subplace(region_t *, struct place*, int x int y);
extern int region_rm_subplace(region_t *, struct place*);
extern void region_paste_to_place(region_t*, struct place*, int x, int y);
extern void region_copy_from_place(region_t*, struct place*, int x, int y);
extern int regions_are_compatible(region_t *a, region_t *b);
extern struct place *place_new(char *tag,char *name, struct sprite *sprite,
                               region_map_t *rmap,
                               int wraps,
                               int underground,
                               int wilderness,
                               int wilderness_combat);
#endif

extern void place_del(struct place *place);

extern int place_is_passable(struct place *place, int x, int y,
                             class Object *passer, int flags);

extern int place_is_occupied(struct place *place, int x, int y);

extern int place_visibility(struct place *place, int x, int y);

extern unsigned int place_walking_distance(struct place *place,
                                           int x0, 
                                           int y0, int x1, int y1);

extern int place_flying_distance(struct place *place, int x0, int y0, int x1, int y1);

void place_get_direction_vector(struct place *place, int x1, int y1, 
                                int x2, int y2, int *dx, int *dy);

extern class Party *place_get_Party(struct place *place,
                                    int x, int y);

extern class Vehicle *place_get_vehicle(struct place *place,
                                        int x, int y);

extern int place_eval_path(struct place *place,
                           int x0, int y0, int x1, int y1);

extern void place_move_object(struct place *place, 
                              class Object *object, int newx, 
                              int newy);
extern int place_add_object(struct place *place, Object * object);

extern void place_remove_object(struct place *place, Object * object);

extern Object *place_get_object(struct place *place, int x, int y, enum layer layer);

#ifndef TURN_LIST_NODES
extern struct list *place_get_all_objects(struct place *place);
#endif

extern void place_add_moongate(struct place *place, 
                               class Moongate * moongate);

extern class Party *place_search_for_Party(struct place *place,
                                           int x,
                                           int y,
                                           int radius,
                                           int (*criteria) (class
                                                            Party
                                                            *
                                                            Party));
        
        
extern struct astar_node *place_find_path(struct place *place, 
                                          struct astar_search_info *info,
                                          class Object *requestor);

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

extern void place_for_each_object(struct place *place, void (*fx) (class Object *, void *data), void *data);
extern int place_get_light(struct place *place, int x, int y);
extern void place_set_terrain(struct place *place, int x, int y, struct terrain *terrain);
extern struct terrain *place_get_terrain(struct place *place, int x, int y);
extern Uint32 place_get_color(struct place *place, int x, int y);
extern int place_get_movement_cost(struct place *place, int x, int y, class Object *obj);
extern int place_adjust_turn_cost(struct place *place, int turns);
extern int place_is_hazardous(struct place *place, int x, int y);
extern class Party *place_random_encounter(struct place *);
extern void place_paint_objects(struct place *place, int mx, int my, int sx, int sy);
extern int place_los_blocked(struct place *place, int ax, int ay, int bx, int by);
extern void place_exec(struct place *place);
extern int place_contains_hostiles(struct place *place, Being *subject);
extern void place_synchronize(struct place *place);
extern void place_enter(struct place *place);
extern void place_remove_and_destroy_all_objects(struct place *place);
extern void place_save(struct save *, void *val);
extern void place_start(void *val);
extern int place_add_subplace(struct place *place, struct place *subplace, 
                              int x, int y);
extern struct place *place_get_subplace(struct place *place, int x, int y);
extern void place_remove_subplace(struct place *place, struct place *subplace);
extern void place_for_each_object_at(struct place *place, int x, int y, void (*fx)(class Object *, void *), void *);
extern void place_exit(struct place *place);
extern void place_unlock(struct place *place);
extern void place_describe(struct place *place, int x, int y, int flags);
extern int place_get_edge_entrance(struct place *place, int dir, int *x, int *y);
extern int place_set_edge_entrance(struct place *place, int dir, int x, int y);
extern class Object *place_get_filtered_object(struct place *place, int x, int y, int (*filter)(class Object*));

END_DECL

#endif
