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

#include "tile.h"

#include "common.h"
#include "object.h"
#include "place.h"
#include "sprite.h"
#include "vehicle.h"

#include <assert.h>
#include <stdlib.h>

tile_t *tile_new()
{
	tile_t *tile;
        tile = (tile_t*)calloc(1, sizeof(*tile));
        assert(tile);
	olist_init(&tile->hashlink);
	olist_init(&tile->objstack);
	return tile;
}

void tile_del(tile_t *tile)
{
        assert(! tile->refcount);

        list_remove(&tile->hashlink.list);
        free(tile);
}

void tile_for_each_object(tile_t *tile, void (*fx)(class Object *obj, void *data), void *data)
{
        struct list *elem;

        tile_ref(tile);

        for (elem = tile->objstack.list.next; elem != &tile->objstack.list; ) {
                class Object *obj;
		obj = outcast(elem, Object, container_link.list);
                elem = elem->next;
                fx(obj, data);
	}

        if (tile->vehicle)
                fx(tile->vehicle, data);

        tile_unref(tile);
}

Object *tile_get_object(tile_t *tile, int (*filter)(class Object*, void *data), void *data)
{
        struct list *elem;

        for (elem = tile->objstack.list.next; elem != &tile->objstack.list; ) {
                class Object *obj;
		obj = outcast(elem, Object, container_link.list);
                elem = elem->next;
                if (filter(obj, data))
                        return obj;
	}

        if (tile->vehicle && filter(tile->vehicle, data))
                return tile->vehicle;

        return NULL;
}

int tile_is_transparent(tile_t *tile)
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

void tile_remove_object(tile_t *tile, class Object *object)
{
	if (object->isType(VEHICLE_ID)) {
                if (tile->vehicle == object) {
                        tile->vehicle = 0;
                        tile_unref(tile);
                }
                // 30Jul2003 gmcnutt: otherwise this vehicle must be occupied,
                // in which case it does not occupy the tile (it's occupant
                // does).
	} else {
		list_remove(&object->container_link.list);
                tile_unref(tile);
	}

        obj_dec_ref(object);
}

void tile_add_object(tile_t *tile, class Object *object)
{
        obj_inc_ref(object);

	if (object->isType(VEHICLE_ID)) {
		if (tile->vehicle) {
                        assert(0);
                }
		tile->vehicle = (class Vehicle *) object;
		tile_ref(tile);
                return;
	}

	olist_add(&tile->objstack, &object->container_link);
	tile_ref(tile);
}

void tile_set_subplace(tile_t *tile, struct place *place)
{
        tile->subplace = place;
        tile_ref(tile);
}

void tile_remove_subplace(tile_t *tile)
{
        if (tile->subplace) {
                tile->subplace = 0;
                tile_unref(tile);
        }
}

void tile_paint(tile_t *tile, int sx, int sy)
{
        /* Removed until place.h is sorted out */

	struct list *l;
	Object *obj;
	struct sprite *sprite;

        /* Check for a subplace. The temp combat place won't have a sprite. */
        if (tile->subplace && place_get_sprite(tile->subplace)) {
                spritePaint(place_get_sprite(tile->subplace), 0, sx, sy);
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

void tile_unref(tile_t *tile)
{
        assert(tile->refcount);

        tile->refcount--;
        if (! tile->refcount)
                tile_del(tile);
}

void tile_save_subplace(tile_t *tile, save_t *save)
{
        if (tile->subplace) {
                save->write(save, "(list %s %d %d)\n", 
                            place_get_tag(tile->subplace), 
                            place_get_x(tile->subplace), 
                            place_get_y(tile->subplace));
        }
}

static void tile_save_objects_visitor(class Object *obj, void *data)
{
        save_t *save = (save_t*)data;
        save->write(save, "(list\n");
        obj->save(save);
        save->exit(save, "%d %d)\n", obj->getX(), obj->getY());
}

void tile_save_objects(tile_t *tile, save_t *save)
{
        tile_for_each_object(tile, tile_save_objects_visitor, save);
}
