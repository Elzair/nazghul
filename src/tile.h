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

#ifndef tile_h
#define tile_h

#include "olist.h"
#include "session.h" /* for save_t */

typedef struct tile {
	struct olist hashlink;
	struct olist objstack;
        struct place *subplace;
	class Vehicle *vehicle;
	int refcount;
} tile_t;

#define tile_set_hashkey(tile, val) ((tile)->hashlink.key = (val))
#define tile_ref(tile) ((tile)->refcount++)
#define tile_get_subplace(tile) ((tile)->subplace)

extern tile_t *tile_new();
extern void tile_del(tile_t*);
extern int tile_is_transparent(tile_t *);
extern void tile_for_each_object(tile_t *, void (*fx)(class Object *obj, void *data), void *data);
extern class Object *tile_get_object(tile_t *, int (*filter)(class Object*, void *), void *);
extern void tile_rm_object(tile_t *, class Object *object);
extern void tile_add_object(tile_t *, class Object *object);
extern void tile_set_subplace(tile_t *, struct place *place);
extern void tile_rm_subplace(tile_t *);
extern void tile_paint(tile_t *, int sx, int sy);
extern void tile_unref(tile_t *);
extern void tile_save_subplace(tile_t *, save_t*);
extern void tile_save_objects(tile_t *, save_t*);

#endif
