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
#ifndef object_h
#define object_h

#include "list.h"
#include "olist.h"
#include "common.h"
#include "console.h"		// SAM: for DEBUG purposes...

#include <assert.h>

/* Note: if you change the layer's you'll probably need to change the save
 * file */
enum layer {			/* Proper rendering depends on keeping these in 
				 * order! */
	null_layer = 0,
	mech_layer = 1,
	portal_layer = 2,
	vehicle_layer = 3,
	container_layer = 4,
	item_layer = 5,
	field_layer = 6,	/* Make fields a higher priority than items and
				 * containers until transparency is more
				 * prevalent among the other sprites. This way
				 * dropped items don't hide dangerous fields
				 * from the user. */
	being_layer = 7,
	projectile_layer = 8,
	cursor_layer = 9,
};

struct inv_entry {
	struct list list;
	struct list auxlist;	// temporary auxiliary list (e.g., when
	// building
	// the list of reagents in a spell mixture)
	int count;
	int ref;
	class ObjectType *type;
};

class Loader;

class ObjectType {

      public:
	virtual bool isType(int classID) {
		return (classID == OBJECT_TYPE_ID);
	}
	virtual int getType() {
		return OBJECT_TYPE_ID;
	}

	ObjectType() {
		list_init(&this->list);
	}
	virtual ~ ObjectType();
	virtual bool init(char *tag, char *name, enum layer layer,
			  struct sprite * sprite);
	virtual char *getTag() {
		return tag;
	}
	virtual char *getName() {
		return name;
	}
	virtual struct sprite *getSprite() {
		return sprite;
	}
	virtual enum layer getLayer() {
		return layer;
	}

	virtual class Object *createInstance();

	virtual bool load(class Loader * loader);
	virtual bool bindTags(class Loader * loader) {
		return true;
	}
	virtual bool isVisible() {
		return true;
	}
	virtual void describe(int count);

	struct list list;

      protected:
	char *tag;
	char *name;
	struct sprite *sprite;
	enum layer layer;
};

class OrdnanceType:public ObjectType {
      public:
	virtual bool isType(int classID) {
		if (classID == ORDNANCE_TYPE_ID)
			return true;
		return ObjectType::isType(classID);
	}
	virtual int getType() {
		return ORDNANCE_TYPE_ID;
	}

	OrdnanceType() {
	}
	virtual ~ OrdnanceType() {
	}
	virtual bool init(char *tag, char *name, struct sprite * sprite,
			  char *fire_sound, int range, int damage,
			  class ObjectType * ammo) {
		if (!ObjectType::init(tag, name, projectile_layer, sprite))
			return false;
		this->fire_sound = fire_sound;
		this->range = range;
		this->damage = damage;
		this->ammo = ammo;
		return true;
	}
	struct sprite *getSprite() {
		return ammo->getSprite();
	}
	virtual int getRange() {
		return range;
	}
	virtual class ObjectType *getAmmo() {
		return ammo;
	}
	virtual char *getFireSound() {
		return fire_sound;
	}
	virtual int get_damage() {
		return damage;
	}
      protected:
	char *fire_sound;
	int range;
	int damage;
	class ObjectType *ammo;
};

class Object {

      public:
	virtual bool isType(int classID) {
		return (classID == OBJECT_ID);
	}
	virtual int getType() {
		return OBJECT_ID;
	}

      Object():type(NULL), x(0), y(0), place(NULL), selected(false),
	    destroyed(false), turn(0) {
		list_init(&this->container_link.list);
		script_tag = 0;
	}
	Object(class ObjectType * type) {
		this->type = type;
		list_init(&this->container_link.list);
	}
	virtual ~ Object() {
	}
	virtual void init(int x, int y, struct place *place,
			  class ObjectType * type);
	virtual void init(class ObjectType * type);

	virtual int getX() {
		return x;
	}
	virtual int getY() {
		return y;
	}
	virtual struct place *getPlace() {
		return place;
	}
	virtual struct sprite *getSprite() {
		return type->getSprite();
	}
	virtual bool isSelected() {
		return selected;
	}
	virtual enum layer getLayer(void) {
		return (enum layer) container_link.key;
	}
	virtual char *getName(void) {
		return type->getName();
	}
	virtual class ObjectType *getObjectType() {
		return type;
	}
	virtual int getTurn(void) {
		return turn;
	}
	virtual bool isDestroyed() {
		return destroyed;
	}

	virtual void setX(int x) {
		this->x = x;
	}
	virtual void setY(int y) {
		this->y = y;
	}
	virtual void changeX(int dx) {
		this->x += dx;
	}
	virtual void changeY(int dy) {
		this->y += dy;
	}
	virtual void setPlace(struct place *place) {
		this->place = place;
	}
	virtual void select(bool val) {
		selected = val;
	}
	virtual void destroy() {
		destroyed = true;
		remove();
	}
	virtual int getLight() {
		return 0;
	}
	virtual void setTurn(int turn) {
		this->turn = turn;
	}
	virtual void advanceTurn(int turn) {
		setTurn(turn);
	}
	virtual void changeTurn(int delta) {
		turn += delta;
	}
	virtual void synchronize(int turn) {
		setTurn(turn);
	}

	virtual void relocate(struct place *newplace, int newx, int newy);
	virtual void remove();
	virtual bool load(class Loader * loader);
	virtual bool isVisible() {
		return getObjectType()->isVisible();
	}
	virtual bool isShaded() {
		return false;
	}

	// SAM: Experiment shows that Object::describe() gets
	// called when I use the OPEN command, but not for the LOOK command.
	// Why is this?
	virtual void describe(int count);

	virtual bool is_opaque() {
		return false;
	}
	virtual void paint(int sx, int sy);

	struct olist container_link;

	char *script_tag;
	struct list list;	// for the loader, not the place

      protected:
	class ObjectType * type;
	int x;
	int y;
	struct place *place;
	bool selected;
	bool destroyed;
	int turn;
};

#endif				// object_h
