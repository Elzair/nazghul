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

enum control_mode {
        CONTROL_MODE_AUTO = 0,
        CONTROL_MODE_PLAYER,
        CONTROL_MODE_IDLE,
        CONTROL_MODE_FOLLOW,
};

struct inv_entry {
	struct list list;
	struct list auxlist;
	int count;
	int ref;
	class ObjectType *type;
};

struct exec_context {
        struct place *place;
        int combat:1;
        int quicken:1;
        int time_stop:1;
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
        virtual int getSpeed();
        virtual int getMaxHp();

        // This might turn out to be too vague. We'll see.
        virtual int getRequiredActionPoints();

	struct list list;

      protected:
	char *tag;
	char *name;
	struct sprite *sprite;
	enum layer layer;
        int speed;
        int required_action_points;
        int max_hp;
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

        virtual char *get_movement_sound();
        virtual int getActivity();
        virtual int getAlignment();
        virtual enum control_mode getControlMode();
        virtual struct conv *getConversation();
        virtual int getHp();
	virtual enum layer getLayer(void);
	virtual int getLight();
        virtual int getMaxHp();
	virtual char *getName(void);
	virtual class ObjectType *getObjectType();
	virtual struct place *getPlace();
        virtual int getPmask();
        virtual int getRequiredActionPoints();
        virtual int getSpeed();
	virtual struct sprite *getSprite();
        virtual struct mview *getView();
	virtual int getVisionRadius();
	virtual int getX();
	virtual int getY();


        virtual bool isCompanionOf(class Object *other);
	virtual bool isDestroyed();
	virtual bool is_opaque();
        virtual bool isOnMap();
        virtual bool isDead();
        virtual bool isHostile(int alignment);
	virtual bool isSelected();
        virtual bool isTurnEnded();
        virtual bool isCameraAttached();
        virtual bool isCharmed();
        virtual bool isNativelyHostile(int alignment);
        virtual bool isPlayerPartyMember();
        virtual bool isPlayerControlled();

        virtual void addView();
        virtual void rmView();
        virtual void updateView();
        virtual void applyExistingEffects();
        virtual bool addToInventory(class Object *object);
        virtual void attachCamera(bool val);
        virtual void charm(int alignment);
        virtual void clearAlignment(int alignment);
	virtual void heal(int amount);
	virtual void setX(int x);
	virtual void setY(int y);
	virtual void changeX(int dx);
	virtual void changeY(int dy);
	virtual void setPlace(struct place *place);
	virtual void select(bool val);
        virtual void unCharm();
	virtual void destroy();
	virtual void relocate(struct place *newplace, int newx, int newy);
	virtual void remove();
	virtual bool load(class Loader * loader);
	virtual bool isVisible();
	virtual bool isShaded();
	virtual void describe(int count);
	virtual void paint(int sx, int sy);
        virtual class Object *clone();
	virtual bool joinPlayer(void);     
	virtual void synchronize();
        virtual void exec(struct exec_context *context);
        virtual int getActionPointsPerTurn();
        virtual void applyPerTurnEffects();
        virtual int getActionPoints();
        virtual void burn();
        virtual void poison();
        virtual void sleep();
        virtual void damage(int amount);
        virtual void decActionPoints(int points);
        virtual void endTurn();
        virtual void startTurn();
        virtual void setControlMode(enum control_mode);
        virtual bool putOnMap(struct place *place, int x, int y, int r);
        virtual void setView(struct mview *view);
        virtual void changePlaceHook();

	struct olist container_link;

	char *script_tag;
	struct list list;	// for the loader, not the place
        struct list turn_list; /* for processing each object in a turn */

      protected:
        virtual void setup();


	class ObjectType * type;
	int x;
	int y;
	struct place *place;
	bool selected;
	bool destroyed;
        int action_points;
        enum control_mode control_mode;
        bool camera_attached;
	int hp;
        bool is_on_map;
	struct conv *conv;
        struct mview *view;

};


#endif				// object_h
