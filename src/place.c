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
#include "place.h"
#include "sprite.h"
#include "terrain.h"
#include "hash.h"
#include "screen.h"
#include "player.h"
#include "sky.h"
#include "console.h"
#include "wq.h"
#include "Field.h"
#include "vehicle.h"
#include "session.h"
#include "log.h"
#include "factions.h"

// #define DEBUG
// #undef debug_h
#include "debug.h"

#include <stdlib.h>
#include <string.h>
#include <math.h>

#define HIGHLIGHT_W 2
#define HIGHLIGHT_H 2

#define BLOCKS_PASSABILITY -1
#define IGNORES_PASSABILITY 0
#define ALLOWS_PASSABILITY  1

#define WRAP(c,max) (((c) + (max)) % (max))
#define WRAP_DISTANCE(a,b,max) (min((a) + (max) - (b), (b) - (a)))
#define INDEX(x,y,w) ((x) + (y) * (w))
#define WRAP_COORDS(place, x, y) do { \
        if ((place)->wraps) { \
                (x) = place_wrap_x((place), (x)); \
                (y) = place_wrap_y((place), (y)); \
        } \
} while(0)

#define TERRAIN(p,x,y) ((p)->terrain_map->terrain[(y) * \
   (p)->terrain_map->w + (x)])

struct place_pathfind_context {
	struct place *place;
	int target_x;
	int target_y;
	int pflags;
        class Object *requestor;
};

struct place *Place;

/* Tile API ******************************************************************/
struct tile {
	struct olist hashlink;
	struct olist objstack;
        struct place *subplace;
	class Vehicle *vehicle;
	int objects;
	int lock;
};
static struct tile *tile_new(int hashkey)
{
	struct tile *tile;
	CREATE(tile, struct tile, 0);
	olist_init(&tile->hashlink);
	olist_init(&tile->objstack);
	tile->hashlink.key = hashkey;
	return tile;
}
static void tile_del(struct tile *tile)
{
	// Locking prevents me from destroying a tile while the place_for_each
	// algorithm is using it.
	if (!tile->lock) {
		assert(!tile->objects);
		list_remove(&tile->hashlink.list);
                //dbg("tile_del: tile=%08lx\n", tile);
		free(tile);
	}
}
static void tile_for_each_object(struct tile *tile,
                                 void (*fx)(class Object *obj, void *data),
                                 void *data)
{
        struct list *elem;

        //dbg("tile_for_each_object: tile=%08lx\n", tile);

        // Dereference all objects on the tile
        for (elem = tile->objstack.list.next; elem != &tile->objstack.list; ) {
                class Object *obj;
		obj = outcast(elem, Object, container_link.list);
                elem = elem->next;
                fx(obj, data);
	}

        if (tile->vehicle)
                fx(tile->vehicle, data);
}

static int tile_is_transparent(struct tile *tile)
{
	struct list *list, *elem, *tmp;
	class Object *obj;

	list = &tile->objstack.list;
	elem = list->next;

	while (elem != list) {
		tmp = elem->next;
		obj = outcast(elem, class Object, container_link.list);
		elem = tmp;
		if (obj->isOpaque())
			return 0;
	}
	return 1;
}

static void tile_remove_object(struct tile *tile, class Object *object)
{
	if (object->isType(VEHICLE_ID)) {
                if (tile->vehicle == object) {
                        tile->vehicle = 0;
                        tile->objects--;
                }
                // 30Jul2003 gmcnutt: otherwise this vehicle must be occupied,
                // in which case it does not occupy the tile (it's occupant
                // does).
	} else {
		list_remove(&object->container_link.list);
                tile->objects--;
	}


	if (!tile->objects) {
		tile_del(tile);
	}

        obj_dec_ref(object);
}

static void tile_add_object(struct tile *tile, class Object *object)
{
        obj_inc_ref(object);

	if (object->isType(VEHICLE_ID)) {
		if (tile->vehicle) {
                        assert(0);
                }
		tile->vehicle = (class Vehicle *) object;
		tile->objects++;
                return;
	}

	olist_add(&tile->objstack, &object->container_link);
	tile->objects++;
}

static int tile_add_subplace(struct tile *tile, struct place *place)
{
        if (tile->subplace)
                return -1;

        tile->subplace = place;
        tile->objects++;
        return 0;
}

static void tile_remove_subplace(struct tile *tile)
{
        if (tile->subplace) {
                tile->subplace = 0;
                tile->objects--;
        }

	if (!tile->objects) {
		tile_del(tile);
	}
}

static struct place *tile_get_subplace(struct tile *tile)
{        
        return tile->subplace;
}

static void tile_save(struct tile *tile, struct save *save)
{
        /* fixme: save everything */
}

static void tile_paint(struct tile *tile, int sx, int sy)
{
	struct list *l;
	Object *obj;
	struct sprite *sprite;

        /* Check for a subplace. The temp combat place won't have a sprite. */
        if (tile->subplace && tile->subplace->sprite) {
                assert(tile->subplace->sprite);
                spritePaint(tile->subplace->sprite, 0, sx, sy);
        }

	/* Check for a vehicle */
	if (tile->vehicle) {
		tile->vehicle->paint(sx, sy);
	}

	list_for_each(&tile->objstack.list, l) {
		obj = outcast(l, Object, container_link.list);
		sprite = obj->getSprite();
		if (!sprite)
			continue;

		// Handle invisible objects and alpha-blending when
		// invisible objects are revealed. If the global "reveal" flag
		// is not set then do not paint invisible objects. Otherwise
		// temporarily set the object's sprite to 'shaded' and paint
		// it. After painting I need to clear the shaded flags in case
		// other objects use this sprite, too.

		if (!obj->isVisible()) {
			if (!Reveal && !obj->isShaded())
				continue;
			sprite_fade(sprite);
		}

		obj->paint(sx, sy);

		if (obj->isSelected()) {
                        /* Highlight */
                        SDL_Rect rect;
                        rect.x = sx;
                        rect.y = sy;
                        rect.w = TILE_W;
                        rect.h = TILE_H;
                        screenHighlight(&rect);
                }

		if (sprite->faded)
			sprite_unfade(sprite);
	}
}

/*****************************************************************************/

static void place_for_each_tile(struct place *place, 
                                void (*fx)(struct tile *tile, void *data), 
                                void *data);


static void place_set_default_edge_entrance(struct place *place)
{
        /* Northwest: lower right corner */
        place->edge_entrance[NORTHWEST][0] = place_w(place) - 1;
        place->edge_entrance[NORTHWEST][1] = place_h(place) - 1;

        /* North: lower center */
        place->edge_entrance[NORTH][0] = place_w(place) / 2;
        place->edge_entrance[NORTH][1] = place_h(place) - 1;

        /* Northeast: lower left corner */
        place->edge_entrance[NORTHEAST][0] = 0;
        place->edge_entrance[NORTHEAST][1] = place_h(place) - 1;

        /* West: right center */
        place->edge_entrance[WEST][0] = place_w(place) - 1;
        place->edge_entrance[WEST][1] = place_h(place) / 2;

        /* Here: center */
        place->edge_entrance[HERE][0] = place_w(place) / 2;
        place->edge_entrance[HERE][1] = place_h(place) / 2;

        /* East: left center */
        place->edge_entrance[EAST][0] = 0;
        place->edge_entrance[EAST][1] = place_h(place) / 2;

        /* Southwest: upper right corner */
        place->edge_entrance[SOUTHWEST][0] = place_w(place) - 1;
        place->edge_entrance[SOUTHWEST][1] = 0;

        /* South: upper center */
        place->edge_entrance[SOUTH][0] = place_w(place) / 2;
        place->edge_entrance[SOUTH][1] = 0;

        /* Southeast: upper left corner */
        place->edge_entrance[SOUTHEAST][0] = 0;
        place->edge_entrance[SOUTHEAST][1] = 0;
}

struct place *place_new(char *tag,
                        char *name, 
                        struct sprite *sprite,
                        struct terrain_map *terrain_map,
                        int wraps,
                        int underground,
                        int wilderness,
                        int wild_combat)
                        
{
	struct place *place;

	CREATE(place, struct place, 0);

	place->tag = strdup(tag);
        assert(place->tag);

	place->name = strdup(name);
        assert(place->name);

	place->objects = hash_create(31);
        assert(place->objects);

        place->magic = PLACE_MAGIC;
        place->sprite = sprite;

        /* Note: turned off cloning for now. It just hasn't shown itself to be
         * necessary yet and it makes saving/loading simpler if I don't have to
         * worry about the cloned map names. In the future, if you turn it back
         * on, make the tag for the cloned map a parameter. */
	//place->terrain_map = terrain_map_clone(terrain_map);

        place->terrain_map = terrain_map;

        place->scale = wilderness ? WILDERNESS_SCALE : NON_WILDERNESS_SCALE;
	place->original_terrain_map = terrain_map;
	place->wraps = wraps;
        place->underground = underground;
        place->wilderness = wilderness;
        place->is_wilderness_combat = wild_combat;

	list_init(&place->vehicles);
        list_init(&place->turn_list);
        list_init(&place->subplaces);
        list_init(&place->container_link);
        place->turn_elem = &place->turn_list;

        place_set_default_edge_entrance(place);
        

	return place;
}

void place_del_tile_object_visitor(class Object *obj, void *data)
{
        // Called by tile_for_each_object()
        struct tile *tile = (struct tile*)data;
        tile_remove_object(tile, obj);
        if (! obj->refcount)
                delete obj;
}

void place_del_tile_visitor(struct tile *tile, void *data)
{
        // Called by place_for_each_tile()
        tile->lock++;
        tile_for_each_object(tile, place_del_tile_object_visitor, tile);
        if (tile->subplace) {
                place_del(tile->subplace);
                tile_remove_subplace(tile);
        }
        tile->lock--;
        tile_del(tile);
}

void place_del(struct place *place)
{
        // --------------------------------------------------------------------
        // If the place is locked then we cannot delete it now. Mark it for
        // death so that it will be deleted when unlocked.
        // --------------------------------------------------------------------

        if (place_is_locked(place)) {
                place_mark_for_death(place);
                return;
        }

        // Destroy all tiles, objects and subplaces recursively.
        place_for_each_tile(place, place_del_tile_visitor, 0);
        hash_destroy(place->objects);

        //dbg("place_del %s\n", place->tag);

	if (place->tag)
		free(place->tag);
	if (place->name)
		free(place->name);
	if (place->terrain_map)
		terrain_map_del(place->terrain_map);
        if (place->pre_entry_hook)
                closure_del(place->pre_entry_hook);
	free(place);
}

static int place_generic_is_passable(class Object *subject, int flags, 
                                     int pclass, struct closure *effect)
{
        // Is it passable?
        if (subject->isPassable(pclass))
                return 1;

        // Is the caller actually trying to move the subject there?
        if (0 == (flags & PFLAG_MOVEATTEMPT))
                return 0;

        // Is there an effect to apply on failed entry?
        if (effect) {
                subject->applyEffect(effect);
        }

        return 0;
}

static int place_terrain_is_passable(struct place *place, int x, int y,
                                     class Object *subject, int flags)
{
	struct terrain *terrain;

	terrain = place->terrain_map->terrain[y * place->terrain_map->w + x];

        // Can we use the generic passability test?
        if (flags & PFLAG_IGNOREVEHICLES)
                return place_generic_is_passable(subject, flags, 
                                                 terrain_pclass(terrain),
                                                 terrain->effect);

        // Is the terrain passable?
        if (subject->isPassable(terrain_pclass(terrain)))
                return 1;

        // Is there a vehicle there?
        if (place_get_vehicle(place, x, y))
                return 1;

        // Is the caller actually trying to move the subject there?
        if (0 == (flags & PFLAG_MOVEATTEMPT))
                return 0;

        // Does the terrain apply an effect on failed entry?
        if (terrain->effect) {
                subject->applyEffect(terrain->effect);
        }

        return 0;
}

static int place_field_is_passable(struct place *place, int x, int y,
                                   class Object *subject, int flags)
{
	class Field *field;

        // Is there a field there?
	field = (class Field *) place_get_object(place, x, y, field_layer);
	if (! field)
                return 1;

        return place_generic_is_passable(subject, flags,
                                         field->getPclass(),
                                         field->getObjectType()->effect);
}

static int place_obj_is_passable(class Object *obj,
                                class Object *subject, int flags)
{
        int pclass = obj->getPclass();

        // Does the object care about passability?
        if (pclass == PCLASS_NONE)
                return IGNORES_PASSABILITY;

        // Is the obj passable?
        if (subject->isPassable(obj->getPclass()))
                return ALLOWS_PASSABILITY;

        // Is the caller actually trying to move the subject there?
        if (0 == (flags & PFLAG_MOVEATTEMPT))
                return BLOCKS_PASSABILITY;

        // Does the obj run a bump handler on failed entry?
        if (obj->getObjectType()->canBump())
                obj->getObjectType()->bump(obj, subject);

        return BLOCKS_PASSABILITY;
}

int place_is_passable(struct place *place, int x, int y,
		      class Object *subject, int flags)
{
	class Object *mech;
	bool impassable_terrain;
	bool no_convenient_vehicle;
        int tfeat_pass = IGNORES_PASSABILITY;

	// For a wrapping place, wrap out-of-bounds x,y
	// For a non-wrapping place, return impassable.
	if (place->wraps) {
		x = WRAP(x, place->terrain_map->w);
		y = WRAP(y, place->terrain_map->h);
	} else if (y < 0 || y >= place->terrain_map->h ||
		   x < 0 || x >= place->terrain_map->w)
		return 0;

        // Does the caller want to check terrain features?
        if (0 == (flags & PFLAG_IGNORETFEAT)) {
                class Object *tfeat = NULL;
                
                tfeat = place_get_object(place, x, y, tfeat_layer);
                if (tfeat) {
                        tfeat_pass = place_obj_is_passable(tfeat, subject, 
                                                           flags);

                        // Does is specifically block passability?
                        if (tfeat_pass == BLOCKS_PASSABILITY)
                                return 0;
                }
        }

        // Does the caller want to check terrain, and if so is there no
        // overriding terrain feature?
        if (0 == (flags & PFLAG_IGNORETERRAIN) &&
            IGNORES_PASSABILITY == tfeat_pass) {

                // Is the terrain passable?
                if (! place_terrain_is_passable(place, x, y, subject, flags))
                        return 0;
        }
                
        // Does the caller want to check fields?
        if (0 == (flags & PFLAG_IGNOREFIELDS)) {

                // Is the field passable?
                if (! place_field_is_passable(place, x, y, subject, flags))
                        return 0;

        }

	// Does the caller want to check mechs?
	if (0 == (flags & PFLAG_IGNOREMECHS)) {
                
                // Is the mech passable?
                mech = place_get_object(place, x, y, mech_layer);
                if (mech &&
                    (place_obj_is_passable(mech, subject, flags) == 
                     BLOCKS_PASSABILITY))
                        return 0;
	}

	return 1;
}

int place_is_occupied(struct place *place, int x, int y)
{
        WRAP_COORDS(place, x, y);
	return (place_get_object(place, x, y, being_layer) != 0);
}

static struct tile *place_lookup_tile(struct place *place, int x, int y)
{
	struct olist *olist;
	if (place_off_map(place, x, y))
		return 0;
	olist = hash_lookup(place->objects, INDEX(x, y, place_w(place)));
	if (!olist)
		return 0;
	return outcast(olist, struct tile, hashlink);
}

static struct tile *place_create_and_add_tile(struct place *place, int x, 
                                              int y)
{
	struct tile *tile;
	tile = tile_new(INDEX(x, y, place_w(place)));
	if (!tile)
		return 0;
	hash_add(place->objects, &tile->hashlink);
        //dbg("place_create_and_add_tile: place=%s tile=%08lx x=%d y=%d\n", place->name, tile, x, y);
	return tile;
}

struct tile *placeGetTile(struct place *place, int x, int y)
{
	struct tile *tile = place_lookup_tile(place, x, y);
	if (tile)
		return tile;
	return place_create_and_add_tile(place, x, y);
}

void place_paint_objects(struct place *place, int mx, int my,
                         int sx, int sy)
{
	struct tile *tile;

	tile = place_lookup_tile(place, mx, my);
	if (!tile)
		return;

        tile_paint(tile, sx, sy);
}

int place_visibility(struct place *place, int x, int y)
{
	struct terrain *terrain;
	struct tile *tile;

	if (place->wraps) {
		x = WRAP(x, place->terrain_map->w);
		y = WRAP(y, place->terrain_map->h);
	} else if (x < 0 || x >= place->terrain_map->w || y < 0 ||
		   y >= place->terrain_map->h)
		return 0;

	terrain = place->terrain_map->terrain[y * place->terrain_map->w + x];
	if (!terrain || !terrain->alpha)
		return 0;

	tile = place_lookup_tile(place, x, y);
	if (tile)
		return tile_is_transparent(tile);

	return 1;
}

unsigned int place_walking_distance(struct place *place,
				    int x0, int y0, int x1, int y1)
{
	int dx;
	int dy;

	if (place->wraps) {
		dx = WRAP_DISTANCE(min(x0, x1), max(x0, x1),
				   place->terrain_map->w);
		dy = WRAP_DISTANCE(min(y0, y1), max(y0, y1),
				   place->terrain_map->h);
	} else {
		dx = max(x0, x1) - min(x0, x1);
		dy = max(y0, y1) - min(y0, y1);
	}

	return dx + dy;
}

int place_flying_distance(struct place *place,
				   int x0, int y0, int x1, int y1)
{
	int dx;
	int dy;

        place_get_direction_vector(place, x0, y0, x1, y1, &dx, &dy);
        dx = abs(dx);
        dy = abs(dy);

	// This approx comes from the angband LOS source, and overestimates
	// about one tile per fifteen tiles of distance.
	return ((dy > dx) ? (dy + (dx >> 1)) : (dx + (dy >> 1)));
}

void place_get_direction_vector(struct place *place, int x0, int y0, int x1, 
                               int y1, int *dx, int *dy)
{
        int east, west, north, south;

        // fixme: is this code assuming that (x0,y0) and (x1,y1) are already
        // wrapped?

        if (! place->wraps) {
                *dx = x1 - x0;
                *dy = y1 - y0;
                return;
        }

        // Four possibilities for dx:
        //
        // |    x0-->x1    | (a) direct east
        // |<--x0     x1<--| (b) west across map boundary
        // |    x1<--x0    | (c) direct west
        // |-->x1     x0-->| (d) east across map boundary
        //
        // Note that since west is always to the left, it is a negative vector.
        // We compute it as a positive value to make it easy to compare against
        // east, but convert it to negative before returning it as dx.

        if (x1 > x0) {
                east = x1 - x0;
                west = x0 + place->terrain_map->w - x1;
        } else {
                west = x0 - x1;
                east = x1 + place->terrain_map->w - x0;
        }

        if (west < east)
                *dx = -west;
        else
                *dx = east;

        // Four possibilities for dy:
        //
        // ---------------
        //      ^       |
        // y0   |   y1  v
        // |    y0  ^   y1
        // v        |
        // y1       y0
        //      y1      y0
        //      ^       |
        //      |       V
        // ---------------
        // (a) (b) (c) (d)
        //
        // (a) direct south
        // (b) north across map boundary
        // (c) direct north
        // (d) south across map boundary
        //
        // Note that north is always a negative vector.

        if (y1 > y0) {
                south = y1 - y0;
                north = y0 + place->terrain_map->h - y1;
        } else {
                north = y0 - y1;
                south = y1 + place->terrain_map->h - y0;
        }

        if (north < south)
                *dy = -north;
        else
                *dy = south;
}

void place_move_object(struct place *place, Object * object, 
                       int newx, int newy)
{
	struct tile *old_tile;
        struct tile *new_tile;

        if (newx == object->getX() &&
            newy == object->getY())
                return;

        old_tile = place_lookup_tile(place, object->getX(), object->getY());
	assert(old_tile);

        new_tile = place_lookup_tile(place, newx, newy);

	if (!new_tile) {
		new_tile = place_create_and_add_tile(place, newx, newy);
                assert(new_tile);
	}
        
        tile_remove_object(old_tile, object);
        tile_add_object(new_tile, object);
}

int place_add_object(struct place *place, Object * object)
{
	struct tile *tile = place_lookup_tile(place, object->getX(),
					      object->getY());

	if (!tile) {
		tile = place_create_and_add_tile(place, object->getX(),
						 object->getY());
		if (!tile)
			return -1;
	}

        tile_add_object(tile, object);
        list_add(&place->turn_list, &object->turn_list);
        mapSetDirty();
	return 0;
}

void place_remove_object(struct place *place, Object * object)
{
	struct tile *tile = place_lookup_tile(place, object->getX(),
					      object->getY());
	assert(tile);

        tile_remove_object(tile, object);

        /* Note: this is a bit subtle. If it just so happens that we are in the
         * process of running place_exec, there is a chance that the object we
         * are removing here is the next object to be processed in the
         * turn_list. In this special case we must setup the next object after
         * this to be processed instead. */
        if (place->turn_elem == &object->turn_list)
                place->turn_elem = object->turn_list.next;

        list_remove(&object->turn_list);
}

Object *place_get_object(struct place *place, int x, int y, enum layer layer)
{
	struct olist *olist;
	struct tile *tile;

        WRAP_COORDS(place, x, y);

	tile = place_lookup_tile(place, x, y);
	if (!tile)
		return 0;
	olist =
	    olist_lookup(&tile->objstack, layer, 0 /* find the top object */ );
	if (!olist)
		return 0;

	return outcast(olist, Object, container_link);
}

class Party *place_get_Party(struct place * place, int x, int y)
{
	Object *object;

        WRAP_COORDS(place, x, y);

	// fixme: this will get Character's, too... and be careful because the
	// pathfinding code appears to rely on this fact.
	object = place_get_object(place, x, y, being_layer);
	if (!object)
		return 0;

	if (!object->isType(PARTY_ID))
		return 0;

	return (class Party *) object;
}

class Vehicle *place_get_vehicle(struct place * place, int x, int y)
{
	struct tile *tile;

        WRAP_COORDS(place, x, y);
	tile = place_lookup_tile(place, x, y);
	if (!tile)
		return 0;
	return tile->vehicle;
}

struct terrain_map *place_get_combat_terrain_map(struct place *place,
						 int x, int y)
{
	struct terrain *terrain;

        WRAP_COORDS(place, x, y);
	terrain = place->terrain_map->terrain[y * place->terrain_map->w + x];
	return terrain_combat_map(terrain);
}

/* Pathfinding ***************************************************************/

static int place_pathfind_is_valid_location(
        struct place_pathfind_context *context, int x, int y)
{
	class Object *portal;


        // --------------------------------------------------------------------
	// I used to check this after passability, but it really belongs first.
	// In several cases the target location may not be passable but if the
	// seeker can get adjacent to it that will be good enough.
        // --------------------------------------------------------------------

	if (x == context->target_x && 
            y == context->target_y) {
		return 1;
        }

	if (!place_is_passable(context->place, x, y, context->requestor, 
                               context->pflags)) {
		return 0;
        }

        // --------------------------------------------------------------------
        // Check if the caller is blocked by an occupant on this tile.
        // --------------------------------------------------------------------

	if (0 == (context->pflags & PFLAG_IGNOREBEINGS)) {
                class Object *occupant;
                occupant = place_get_object(context->place, x, y, being_layer);
                if (occupant != NULL) {
                        if (0 == (context->pflags & PFLAG_IGNORECOMPANIONS) ||
                            ! context->requestor->isCompanionOf(occupant)) {
                                //printf("occupied!\n");
                                return 0;
                        }
                }
        }

	// --------------------------------------------------------------------
	// I used to penalize portals in the heuristic routine, but that was
	// back in the day when I would pathfind for the player on a
	// right-click. Any more pathfinding is used exclusively for NPCs (or
	// PC's in follow mode) and I NEVER want them to enter a portal unless
	// they explicitly want to (and currently they never do). Likewise for
	// open moongates.
        //
        // Addendum: portals are now mechs with "step" signal handlers. The
        // code below avoids any mechanism which responds to the "step" signal,
        // including non-portals. That's fine, because anything which responds
        // to "step" is probably something I want to avoid.
        // --------------------------------------------------------------------

	if ((portal = place_get_object(context->place, x, y, mech_layer)) &&
	    portal->canStep()) {
                //dbg("portal!\n");
		return 0;
        }

        //printf("ok\n");
	return 1;
}

static void place_pathfind_heuristic(struct astar_search_info *info,
                                    int *goodness, int *cost)
{
	struct terrain *terrain;
	struct place_pathfind_context *context;

	context = (struct place_pathfind_context *) info->context;

	/* The basic goodness is walking distance. Duplicate that algorithm
	 * except pay attention to the info->flags. */

	if ((info->flags & ASTAR_HORZ) == 0) {
		// Yes, we are interested in the x coordinate of the
		// destination.
		if (context->place->wraps) {
			*goodness -= WRAP_DISTANCE(min(info->x0, info->x1),
                                                  max(info->x0, info->x1),
                                               context->place->terrain_map->w);
		} else {
			*goodness -= max(info->x0, info->x1) - 
                                min(info->x0, info->x1);
		}
	}

	if ((info->flags & ASTAR_VERT) == 0) {
		// Yes, we are interested in the y coordinate of the
		// destination.
		if (context->place->wraps) {
			*goodness -= WRAP_DISTANCE(min(info->y0, info->y1),
                                                  max(info->y0, info->y1),
                                               context->place->terrain_map->h);
		} else {
			*goodness -= max(info->y0, info->y1) -
                                min(info->y0, info->y1);
		}
	}

        /* Add the terrain cost. */
        *cost += place_get_movement_cost(context->place, info->x0, info->y0, 
                                         context->requestor);

	/* And penalize tiles with hazards on them. I really should assign
	 * different penalties to different hazerds. */
	terrain = place_get_terrain(context->place, info->x0, info->y0);
	if (terrain->effect)
		*cost += 9;

	if (place_get_object(context->place, info->x0, info->y0,
			     field_layer) != NULL)
		*cost += 9;
}

struct astar_node *place_find_path(struct place *place, 
                                   struct astar_search_info *info, 
                                   class Object *requestor)
{
	struct astar_node *path;
	struct place_pathfind_context context;
        int t1;

	/* Store the target location as the context */
	context.place = place;
	context.target_x = info->x1;
	context.target_y = info->y1;
	context.pflags = info->flags;
        context.requestor = requestor;

	/* Fill out the search information */
	info->is_valid_location =
	    (int (*)(void *, int, int)) place_pathfind_is_valid_location;
	info->heuristic = place_pathfind_heuristic;
	info->width = place_w(place);
	info->height = place_h(place);
	info->wraps = place->wraps;
	info->context = &context;

	/* Run the pathfinding alg */
        t1 = SDL_GetTicks();
	path = astar_search(info);
        //dbg("place_find_path: %d msecs\n", SDL_GetTicks() - t1);

	return path;

}

int place_get_light(struct place *place, int x, int y)
{
	int light;
	struct list *l;
	Object *obj;
	struct tile *tile;

        if (place->wraps) {
                x = place_wrap_x(place, x);
                y = place_wrap_y(place, y);
        }

	/* Check if the coordinates are off-map */
	else if (place_off_map(place, x, y))
		return 0;

	/* Assign lighting from terrain */
	light =
	    place->terrain_map->terrain[y * place->terrain_map->w + x]->light;

	/* Assign lighting from tile objects */
	tile = place_lookup_tile(place, x, y);
	if (!tile)
		return light;

	/* Check for a vehicle */
	if (tile->vehicle)
		light += tile->vehicle->getLight();

	/* Check all objects */
	list_for_each(&tile->objstack.list, l) {
		obj = outcast(l, Object, container_link.list);
		light += obj->getLight();
	}

	return light;
}

static void place_obj_synchronize(class Object * obj, void *data)
{
        obj->synchronize();
}

void place_synchronize(struct place *place)
{
	place_for_each_object(place, place_obj_synchronize, 0);
}

static void myResetObjectTurns(class Object * obj, void *data)
{
	obj->synchronize();

        if (obj->isType(CHARACTER_ID)) {
                class Character *ch = (class Character*)obj;
                if (ch->isCharmed())
                        ch->unCharm();
        }
}

void place_enter(struct place *place)
{
	place_for_each_object(place, myResetObjectTurns, 0);
}

int place_get_movement_cost(struct place *place, int x, int y, 
                            class Object *obj)
{
        int cost;
        struct terrain *t;
        class Object *tfeat = NULL;
        int pclass;

        WRAP_COORDS(place, x, y);

        // Terrain features override terrain
        tfeat = place_get_object(place, x, y, tfeat_layer);
        if (tfeat) {
                return obj->getMovementCost(tfeat->getPclass());
        }

	t = TERRAIN(place, x, y);
        cost = obj->getMovementCost(terrain_pclass(t));

        // Impassable terrain must have a vehicle that makes it passable; use
        // the cost of vehicle movement
        if (PTABLE_IMPASSABLE == cost) {
                class Vehicle *vehicle;
                vehicle = place_get_vehicle(place, x, y);
                if (vehicle)
                        cost = vehicle->getMovementCost(terrain_pclass(t));
        }
        return cost;
}

int place_is_hazardous(struct place *place, int x, int y)
{
        WRAP_COORDS(place, x, y);
	struct terrain *t = TERRAIN(place, x, y);
	if (t->effect)
                return 1;        
        if (place_get_object(place, x, y, field_layer) != NULL)
                return 1;
        return 0;
}

void place_set_terrain(struct place *place, int x, int y,
		       struct terrain *terrain)
{
        WRAP_COORDS(place, x, y);
	TERRAIN(place, x, y) = terrain;
}

struct terrain *place_get_terrain(struct place *place, int x, int y)
{
        WRAP_COORDS(place, x, y);
	if (place_off_map(place, x, y))
		return NULL;
	x = place_wrap_x(place, x);
	y = place_wrap_y(place, y);
	return TERRAIN(place, x, y);
}

static void place_describe_terrain(struct place *place, int x, int y)
{
	struct terrain *t = place_get_terrain(place, x, y);
	log_continue("%s", t->name);        
}

static int place_describe_objects(struct place *place, int x, int y, 
                                  int first_thing_listed)
{

	struct list *l;
	struct tile *tile;
	Object *obj = NULL, *prev_obj = NULL;
	class ObjectType *type = NULL;
	int n_instances;
        int n_types;
        int n_described = 0;

	tile = place_lookup_tile(place, x, y);
	if (!tile)
		return n_described;
        
        // Let's make things simple. Inefficient, but simple. Efficiency is not
        // so critical here. We'll do this in two passes. Pass one will count
        // the number of things we need to list. Pass two will print the things
        // with the proper punctuation.

        // Step 1: count the number of different types of things we need to
        // list (multiple counts of one type of thing count as 1)

        type = NULL;
        n_types = 0;

        if (tile->subplace) {

                // ------------------------------------------------------------
                // FIXME: This has not been fully debugged. It works ok when
                // there are no objects on the tile, but I don't think the
                // 'ands' and commas are correct if there are.
                // ------------------------------------------------------------
                log_continue(" and the entrance to %s", tile->subplace->name);
        }

	list_for_each(&tile->objstack.list, l) {

		obj = outcast(l, Object, container_link.list);

		if (obj->container_link.key == cursor_layer)
                        // Special case: don't describe the cursor
                        continue;


		if (type == NULL) {

                        // This is the first type of thing we need to list.
			type = obj->getObjectType();
                        if (obj->isVisible() || Reveal || obj->isShaded())
                                n_types++;

		} else if (obj->getObjectType() != type) {

                        // We just found a new type of thing (we know because
                        // it's different from the last type of thing).
			type = obj->getObjectType();
                        if (obj->isVisible() || Reveal || obj->isShaded())
                                n_types++;

		}
	}

        if (tile->vehicle && (tile->vehicle->isVisible() || Reveal || 
                              obj->isShaded()))
                n_types++;

        if (n_types == 0)
                // Nothing to list so we're done.
                return n_described;


        // Step 2: now we actually list the things, using the count to help us
        // decide how to punctuate.

        n_instances = 0;
        type = NULL;
        prev_obj = NULL;

	list_for_each(&tile->objstack.list, l) {

		obj = outcast(l, Object, container_link.list);

		if (obj->container_link.key == cursor_layer)
                        // Special case: don't describe the cursor
                        continue;

		if (prev_obj == NULL) {

                        // This is the first type of thing we need to
                        // list. Don't print it until we find out how many
                        // there are.
			type = obj->getObjectType();
                        n_instances = 1;

		} else if (obj->getObjectType() != type) {

                        // We just found a new type of thing (we know because
                        // it's different from the last type of thing). Now we
                        // can print the last type of thing since we know how
                        // many there are of it.

                        if (prev_obj->isVisible() || Reveal || 
                            prev_obj->isShaded()) {
                                if (first_thing_listed) {
                                        first_thing_listed = 0;
                                } else {
                                        if (n_types == 1)
                                                log_continue(" and ");
                                        else
                                                log_continue(", ");
                                }

                                prev_obj->getObjectType()->describe(n_instances);
                                n_described++;
                                n_types--;
                        }

			type = obj->getObjectType();
                        n_instances = 1;

		} else {
                        // More of the same.
                        n_instances++;
                }

                prev_obj = obj;
	}

        // Now we have to print the last object in the stack.
        if (prev_obj && (prev_obj->isVisible()  || Reveal || 
                         prev_obj->isShaded())) {
                if (!first_thing_listed) {
                        if (n_types == 1)
                                log_continue(" and ");
                        else
                                log_continue(", ");
                }
                prev_obj->describe();
                n_described++;
                n_types--;
        }

        if (tile->vehicle && (tile->vehicle->isVisible() || Reveal || 
                              obj->isShaded())) {
                if (n_types == 1)
                        log_continue(" and ");
                else
                        log_continue(", ");
                tile->vehicle->describe();
                n_described++;
                n_types--;
        }

        return n_described;

}				// myPlaceDescribeObjects()

void place_describe(struct place *place, int x, int y, int flags)
{
        int count = 0;

        WRAP_COORDS(place, x, y);

	if (place_off_map(place, x, y)) {
		log_continue("nothing!");
		return;
	}
        if (flags & PLACE_DESCRIBE_TERRAIN) {
                place_describe_terrain(place, x, y);
                count = 1;
        }
        if (flags & PLACE_DESCRIBE_OBJECTS)
                count += place_describe_objects(place, x, y, 
                                       (flags & PLACE_DESCRIBE_TERRAIN) == 0);
        if (!count)
                log_continue("nothing!");
}

void place_for_each_tile(struct place *place, 
                         void (*fx)(struct tile *tile, void *data), void *data)
{
	int i;
	struct olist *tileList, *objList;
	struct list *tileElem, *tileTmp;
	struct tile *tile;
	class Object *obj;

	// for each bucket
	for (i = 0; i < place->objects->n && !Quit; i++) {

		tileList = &place->objects->buckets[i];
		tileElem = tileList->list.next;
		assert(tileElem->prev == &tileList->list);

		// for each tile
		while (tileElem != &tileList->list && !Quit) {

			tileTmp = tileElem->next;
			tile = outcast(tileElem, struct tile, hashlink.list);
                        
                        // invoke the function on the tile
                        tile->lock++;
                        fx(tile, data);
                        tile->lock--;
			if (!tile->objects)
				tile_del(tile);
                        

			tileElem = tileTmp;
		}
	}
}

struct forobj_tile_visitor_info {
        void (*fx) (class Object *, void *data);
        void *data;
};

void place_forobj_tile_visitor(struct tile *tile, void *data)
{
        struct forobj_tile_visitor_info *info;
        info = (struct forobj_tile_visitor_info*)data;
        tile_for_each_object(tile, info->fx, info->data);
}

void place_for_each_object(struct place *place, 
                           void (*fx) (class Object *, void *data),
			   void *data)
{
        struct forobj_tile_visitor_info info;
        info.fx = fx;
        info.data = data;
        place_for_each_tile(place, place_forobj_tile_visitor, &info);
#if 0
        // Old way:
        
	int i;
	struct olist *tileList, *objList;
	struct list *tileElem, *tileTmp, *objElem, *objTmp;
	struct tile *tile;
	class Object *obj;

	// for each bucket
	for (i = 0; i < place->objects->n && !Quit; i++) {

		tileList = &place->objects->buckets[i];
		tileElem = tileList->list.next;
		assert(tileElem->prev == &tileList->list);

		// for each tile
		while (tileElem != &tileList->list && !Quit) {

			tileTmp = tileElem->next;
			tile = outcast(tileElem, struct tile, hashlink.list);
			tile->lock++;

			objList = &tile->objstack;
			objElem = objList->list.next;

			// for each object
			while (objElem != &objList->list && !Quit) {

				objTmp = objElem->next;
				obj = outcast(objElem, class Object,
					      container_link.list);
				fx(obj, data);

				objElem = objTmp;
			}

                        // moongates & vehicles
                        if (tile->vehicle)
                                fx(tile->vehicle, data);

                        // NOTE: subplaces are not objects, so they are
                        // excused.

			// Unlock the tile. One possible consequence of the
			// above loop is that all of the objects were removed
			// from this tile, in which case we probably tried to
			// destroy it but were prevented by the lock.
			tile->lock--;
			if (!tile->objects)
				tile_del(tile);

			tileElem = tileTmp;
		}
	}
#endif
}

static void place_remove_and_destroy_object(class Object *obj, void *unused)
{
        obj->remove();
        delete obj;
}

void place_remove_and_destroy_all_objects(struct place *place)
{
        place_for_each_object(place, place_remove_and_destroy_object, NULL);
}

static void place_apply_tile_effects(struct place *place, class Object *obj)
{
        class Object *tfeat;
        class Field *field;

        assert(! obj->isDestroyed());

        // --------------------------------------------------------------------
        // First check for a terrain feature, which will override any terrain
        // effects.
        // --------------------------------------------------------------------
        tfeat = place_get_object(place, obj->getX(), obj->getY(), tfeat_layer);
        if (tfeat) {
                if (tfeat->canStep())
                        tfeat->step(obj);
        } else {
                struct terrain *terrain;                
                terrain = place_get_terrain(place, obj->getX(), obj->getY());
                if (terrain->effect) {
                        obj->applyEffect(terrain->effect);
                }
        }

        // --------------------------------------------------------------------
        // Check if the terrain or feature effect destroyed the object.
        // --------------------------------------------------------------------
        if (obj->isDestroyed())
                return;
        
        // --------------------------------------------------------------------
        // Now apply effects from any fields on that tile.
        // --------------------------------------------------------------------
        field = (class Field *)place_get_object(place, 
                                                obj->getX(),
                                                obj->getY(), 
                                                field_layer);
        if (field && 
            field != obj && 
            field->getObjectType()->effect) {
                obj->applyEffect(field->getObjectType()->effect);
        }

}

void place_exec(struct place *place, struct exec_context *context)
{
        // --------------------------------------------------------------------
        // Upon entry this should always by the current place. That may change
        // while we're in the loop, but upon entry it should always be true.
        // --------------------------------------------------------------------

        assert(Place == place);

        // --------------------------------------------------------------------
        // Lock the place to prevent destruction while running the exec loop.
        // --------------------------------------------------------------------

        place_lock(place);

        // --------------------------------------------------------------------
        // Loop over every object in the place...
        // --------------------------------------------------------------------

        place->turn_elem = place->turn_list.next;
        while (place->turn_elem != &place->turn_list 
               && ! Quit
               && ! player_party->allDead()) {
               
                class Object *obj;                

                obj = outcast(place->turn_elem, class Object, turn_list);
                place->turn_elem = place->turn_elem->next;

                if (TimeStop) {
                        if (obj->isPlayerControlled()) {
                                obj->exec(context);
                        }
                } else {
                        
                        /* 'run' the object */
                        obj->exec(context);

                        /* Apply terrain, field and any other environmental
                         * effects. */
                        if (obj->isOnMap())
                                /* Bugfix: as a result of executing its turn,
                                 * the object may now be in a different
                                 * place! */
                                place_apply_tile_effects(obj->getPlace(), obj);
                            

                }

                /* check if the session was reloaded while running the
                 * object. If so, the object, this place and everything in it
                 * has been destroyeed. Leave now. Don't touch a thing. */
                if (Session->reloaded)
                        return;                                

                /* one more time, check if the object is toast */
                if (!obj->isDestroyed())
                        continue;

        obj_destroyed:
                /* don't delete the player party */
                if (obj == player_party)
                        return;

                /* done with this object */
                delete obj;
        }

        // --------------------------------------------------------------------
        // Note: unlocking can finalize the destruction of the place if
        // somebody tried to destroy it in the above loop.
        // --------------------------------------------------------------------

        place_unlock(place);
}

void place_clip_to_map(struct place *place, int *x, int *y)
{
	if (place->wraps)
		return;

	*x = max(*x, 0);
	*x = min(*x, place_w(place) - 1);
	*y = max(*y, 0);
	*y = min(*y, place_h(place) - 1);
}

// fixme -- combine with mapAnimateProjectile(), use callback for animation
int place_los_blocked(struct place *place, int Ax, int Ay, int Bx, int By)
{
        // Should be called from source to target. Does not test for los on
        // target tile itself, but stops one short. So opaque tiles are visible
        // if they are the destination.

        // Apply the bresenhaum algorithm to walk the line from (x0, y0) to
        // (x1, y1) and check for visibility at each step. Note that the real
        // intention here is to see if I can fire an arrow from one point to
        // another. The missile flight code in Missile:animate() uses a test
        // for visibility on each tile to determine if a missile is blocked in
        // its flight path.

        int steps = 0;

        //Ax = place_wrap_x(place, Ax);
        //Ay = place_wrap_y(place, Ay);
        //Bx = place_wrap_x(place, Bx);
        //By = place_wrap_y(place, By);

        int Px = Ax;
        int Py = Ay;

        // Get the distance components
        int dX = Bx - Ax;
        int dY = By - Ay;
        int AdX = abs(dX);
        int AdY = abs(dY);

        // Moving left?
        int Xincr = (Ax > Bx) ? -1 : 1;

        // Moving down?
        int Yincr = (Ay > By) ? -1 : 1;

        // Walk the x-axis
        if (AdX >= AdY) {

                int dPr = AdY << 1;
                int dPru = dPr - (AdX << 1);
                int P = dPr - AdX;

                // For each x
                for (int i = AdX; i >= 0; i--) {

                        if (steps > 1 && i > 0) {
                                if (!place_visibility(place, Px, Py))
                                        return 1;
                        }

                        steps++;

                        if (P > 0) {
                                Px += Xincr;
                                Py += Yincr;
                                P += dPru;
                        }
                        else {
                                Px += Xincr;
                                P += dPr;
                        }
                }
        }
        // Walk the y-axis
        else {
                int dPr = AdX << 1;
                int dPru = dPr - (AdY << 1);
                int P = dPr - AdY;

                // For each y
                for (int i = AdY; i >= 0; i--) {

                        if (steps > 1 && i > 0) {
                                if (!place_visibility(place, Px, Py))
                                        return 1;
                        }

                        steps++;

                        if (P > 0) {
                                Px += Xincr;
                                Py += Yincr;
                                P += dPru;
                        }
                        else {
                                Py += Yincr;
                                P += dPr;
                        }
                }
        }

        return 0;
}

struct list *place_get_all_objects(struct place *place)
{
        return &place->turn_list;
}

int place_contains_hostiles(struct place *place, Being *subject)
{
        struct list *elem;
        class Object *obj;
        
        list_for_each(&place->turn_list, elem) {
                obj = outcast(elem, class Object, turn_list);
                if (! obj_is_being(obj))
                        continue;
                if (are_hostile((Being*)obj, subject))
                        return 1;
        }

        return 0;
}

static void place_save_object(class Object *object, void *data)
{
        struct save *save;
        save = (struct save*)data;
        save->enter(save, "(list\n");
        object->save(save);
        save->exit(save, "%d %d)\n", object->getX(), object->getY());
}

static void place_save_hooks(struct place *place, struct save *save)
{
        save->enter(save, "(list\n");
        if (place->pre_entry_hook)
                closure_save(place->pre_entry_hook, save);
        save->exit(save, ")\n");
}

static void place_save_edge_entrances(struct place *place, struct save *save)
{
        int dir;

        save->enter(save, "(list ;; edge entrances\n");
        for (dir = 0; dir < NUM_PLANAR_DIRECTIONS; dir++) {
                save->write(save, "(list %d %d %d) ;; %s\n", dir,
                            place->edge_entrance[dir][0],
                            place->edge_entrance[dir][1],
                            directionToString(dir));
        }
        save->exit(save, ")\n");
}

void place_save(struct save *save, void *val)
{
        struct place *place;
        struct list *elem;

        place = (struct place *)val;

        if (place->saved == save->session_id) {
                /* Already saved once for this session, so just write the tag
                 * to the save file. */
                save->write(save, "%s\n", place->tag);
                return;
        }

        place->saved = save->session_id;

        save->enter(save, "(kern-mk-place '%s \"%s\"\n",
                    place->tag, 
                    place->name);
        save->write(save, "%s ;; sprite\n", 
                    place->sprite ? place->sprite->tag : "nil");
        terrain_map_save(save, place->terrain_map);
        save->write(save, "%s %s %s %s\n",
                    place->wraps ? "#t" : "#f",
                    place->underground ? "#t" : "#f",
                    place->wilderness ? "#t" : "#f",
                    place->is_wilderness_combat ? "#t" : "#f"
                );

        /* Save all subplaces recursively. */

        save->write(save, ";; subplaces\n");
        if (list_empty(&place->subplaces)) {
                save->write(save, "nil\n");
        } else {
                save->enter(save, "(list\n");
                list_for_each(&place->subplaces, elem) {
                        struct place *subplace;
                        subplace = outcast(elem, struct place, container_link);
                        assert(subplace->location.place = place);
                        save->enter(save, "(list\n");
                        place_save(save, subplace);
                        save->exit(save, "%d %d) ;; coords of %s\n", 
                                   subplace->location.x, 
                                   subplace->location.y,
                                   subplace->tag);
                }
                save->exit(save, ") ; end of subplaces\n");
        }

        /* Save the neighbors recursively.  Subtle note: because neighborness
         * is a symmetric relationship, and scheme does not permit forward
         * declarations, and I don't want to use tags in lieue of recursive
         * definitions, I don't reference neighbors that have already been
         * saved. The neighborly relation was already recorded when the
         * neighbor was saved, and it will be duly restored when the neighbor
         * is loaded, so we don't need to do it here. Furthermore, we CANNOT do
         * it here because we are probably dead smack in the middle of the save
         * routine for that neighbor, so we can't refer to it yet.
         */

        save->write(save, ";; neighbors\n");
        if ((! place->above || place->above->saved == save->session_id) && 
            (! place->below || place->below->saved == save->session_id)) {
                save->write(save, "nil\n");
        } else {
                save->enter(save, "(list\n");
                if (place->above && place->above->saved != save->session_id) {
                        save->enter(save, "(list ;; begin above neighbor\n");
                        place_save(save, place->above);
                        save->exit(save, "%d) ;; end above neighbor\n", UP);
                }
                if (place->below && place->below->saved != save->session_id) {
                        save->enter(save, "(list ;; begin below neighbor\n");
                        place_save(save, place->below);
                        save->exit(save, "%d) ;; end below neighbor\n", DOWN);
                }
                save->exit(save, ")\n");
        }

        /* Save the contents */

        save->write(save, ";; contents\n");
        save->enter(save, "(list\n");
        place_for_each_object(place, place_save_object, save);
        save->exit(save, ") ;; end of objects\n");
        place_save_hooks(place, save);
        place_save_edge_entrances(place, save);
        save->exit(save, ") ;; end of place %s\n\n", place->tag);
}

static void place_start_object(class Object *object, void *data)
{
        object->start();
}

void place_start(void *val)
{
        struct place *place;
        struct list *elem;

        place = (struct place *)val;

        // --------------------------------------------------------------------
        // Start all subplaces recursively.
        // --------------------------------------------------------------------

        list_for_each(&place->subplaces, elem) {
                struct place *subplace;
                subplace = outcast(elem, struct place, container_link);
                place_start(subplace);
        }

        // --------------------------------------------------------------------
        // Save the contents.
        // --------------------------------------------------------------------

        place_for_each_object(place, place_start_object, NULL);
}

int place_add_subplace(struct place *place, struct place *subplace, 
                       int x, int y)
{
	struct tile *tile;

        if (place_off_map(place, x, y))
                return 0;

        tile = place_lookup_tile(place, x, y);

	if (!tile) {
		tile = place_create_and_add_tile(place, x, y);
		if (!tile)
			return -1;
	}
        
        if (tile_add_subplace(tile, subplace))
                return -1;

        if (subplace->handle) {
                session_rm(Session, subplace->handle);
                subplace->handle = 0;
        }

        subplace->location.place = place;
        subplace->location.x = x;
        subplace->location.y = y;

        list_add(&place->subplaces, &subplace->container_link);
	return 0;
}

struct place *place_get_subplace(struct place *place, int x, int y)
{
	struct tile *tile;

        WRAP_COORDS(place, x, y);

	tile = place_lookup_tile(place, x, y);
	if (!tile)
		return 0;
        return tile->subplace;
}

void place_remove_subplace(struct place *place, struct place *subplace)
{
	struct tile *tile;

	tile = place_lookup_tile(place, subplace->location.x, 
                                 subplace->location.y);
        assert(tile);
        tile_remove_subplace(tile);
        list_remove(&subplace->container_link);
        // FIXME: make it an orphan?
}

void place_for_each_object_at(struct place *place, int x, int y, void (*fx)(class Object *, void *), void *data)
{
        struct tile *tile;

        tile = place_lookup_tile(place, x, y);
        if (tile)
                tile_for_each_object(tile, fx, data);
}

static void place_remove_and_destroy_temporary_object(class Object *obj, void *unused)
{
        if (obj->isTemporary()) {
                obj->remove();
                delete obj;
        }
}

void place_exit(struct place *place)
{
        place_for_each_object(place, place_remove_and_destroy_temporary_object, NULL);
}

void place_unlock(struct place *place)
{
        assert(place->lock);

        place->lock--;

        if (!place->lock && place_is_marked_for_death(place))
                place_del(place);
}

int place_get_edge_entrance(struct place *place, int dir, int *x, int *y)
{
        if (dir < 0 || dir >= NUM_PLANAR_DIRECTIONS)
                return -1;

        *x = place->edge_entrance[dir][0];
        *y = place->edge_entrance[dir][1];

        return 0;
}

int place_set_edge_entrance(struct place *place, int dir, int x, int y)
{
        if (dir < 0 || dir >= NUM_PLANAR_DIRECTIONS)
                return -1;

        if (x < 0 || x >= place_w(place) ||
            y < 0 || y >= place_h(place))
                return -1;

        place->edge_entrance[dir][0] = x;
        place->edge_entrance[dir][1] = y;

        return 0;
}
