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

// Note: if you change the layers you'll probably need to change the save
//       file
// Proper rendering depends on keeping these in order!
enum layer {
	null_layer       = 0,
	mech_layer       = 1,
	portal_layer     = 2,
	vehicle_layer    = 3,
        bed_layer        = 4,
	container_layer  = 5,
	item_layer       = 6,
	field_layer      = 7,
	being_layer      = 8,
	projectile_layer = 9,
	cursor_layer     = 10,
};

struct inv_entry {
	struct list list;
	struct list auxlist;
	int count;
	int ref;
	class ObjectType *type;
};

class Loader;

class ObjectType {

      public:
	virtual bool isType(int classID);
	virtual int getType();
	ObjectType();
	virtual ~ObjectType();
	virtual bool init(char *tag, char *name, enum layer layer,
			  struct sprite * sprite);
	virtual char *getTag();
	virtual char *getName();
	virtual struct sprite *getSprite();
	virtual enum layer getLayer();
	virtual class Object *createInstance();
	virtual bool load(class Loader * loader);
	virtual bool bindTags(class Loader * loader);
	virtual bool isVisible();
	virtual void describe(int count);

	struct list list;

      protected:
	char *tag;
	char *name;
	struct sprite *sprite;
	enum layer layer;
};

class Object {

      public:
	virtual bool isType(int classID);
	virtual int getType();

        Object();
	Object(class ObjectType * type);
	virtual ~Object();
	virtual void init(int x, int y, struct place *place,
			  class ObjectType * type);
	virtual void init(class ObjectType * type);

	virtual int getX();
	virtual int getY();
	virtual struct place *getPlace();
	virtual struct sprite *getSprite();
	virtual bool isSelected();
	virtual enum layer getLayer(void);
	virtual char *getName(void);
	virtual class ObjectType *getObjectType();
	virtual int getTurn(void);
	virtual int getLight();
        virtual char *get_movement_sound();
        virtual int getAlignment();
        virtual int getActivity();
        virtual struct conv *getConversation();

	virtual bool isDestroyed();
	virtual bool is_opaque();

	virtual void setX(int x);
	virtual void setY(int y);
	virtual void changeX(int dx);
	virtual void changeY(int dy);
	virtual void setPlace(struct place *place);
	virtual void select(bool val);
	virtual void destroy();
	virtual void setTurn(int turn);
	virtual void advanceTurn(int turn);
	virtual void changeTurn(int delta);
	virtual void synchronize(int turn);
	virtual void relocate(struct place *newplace, int newx, int newy);
	virtual void remove();
	virtual bool load(class Loader * loader);
	virtual bool isVisible();
	virtual bool isShaded();
	virtual void describe(int count);
	virtual void paint(int sx, int sy);
        virtual class Object *clone();
	virtual bool joinPlayer(void);     
        virtual void hitByOrdnance(class ArmsType *ordnance);

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
