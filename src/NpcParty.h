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
#ifndef NpcParty_h
#define NpcParty_h

#include "object.h"
#include "list.h"
#include "common.h"
#include "character.h"
#include "moongate.h"
#include "pinfo.h"

class Loader;

struct GroupInfo {
	struct species *species;
	struct occ *occ;
	struct sprite *sprite;
	int n_max;
};

class NpcPartyType:public ObjectType {
      public:
	virtual bool isType(int classID) {
		return (classID == NPCPARTY_TYPE_ID);
	}
	virtual int getType() {
		return NPCPARTY_TYPE_ID;
	}
	NpcPartyType();
	virtual ~ NpcPartyType();
	virtual bool init(class Character * ch);
	int getPmask() {
		return pmask;
	}
	int getVisionRadius() {
		return vrad;
	}
	int getSpeed() {
		return speed;
	}
	virtual struct GroupInfo *enumerateGroups();
	virtual struct GroupInfo *getNextGroup();
	virtual class Object *createInstance();
	virtual bool load(class Loader * loader);
	virtual bool isVisible() {
		return visible;
	}

	struct formation *formation;
        struct sprite *sleep_sprite;

      protected:
	int i_group;		// group index (for silly enumeration)
	int n_groups;
	struct GroupInfo *groups;
	int pmask;
	int vrad;
	int speed;
	bool visible;
};

class NpcParty:public Object {
      public:
	virtual bool isType(int classID) {
		if (classID == NPCPARTY_ID)
			return true;
		return Object::isType(classID);
	}
	virtual int getType() {
		return NPCPARTY_ID;
	}

	NpcParty();
	~NpcParty();

	virtual class NpcPartyType *getObjectType() {
		return (class NpcPartyType *) Object::getObjectType();
	}

	virtual int getPmask();
	virtual int getVisionRadius() {
		return getObjectType()->getVisionRadius();
	}
	virtual int getSpeed();

	virtual void init(int x, int y, struct place *place,
			  class NpcPartyType * type) {
		Object::init(x, y, place, type);
	}
	virtual void init(class Character * ch);
	virtual void init(class NpcPartyType * type);
	virtual void advanceTurn(int turn);
	virtual void synchronize(int turn);
	virtual bool move(int dx, int dy);
	virtual int getAlignment() {
		return alignment;
	}
	virtual void setAlignment(int val) {
		alignment = val;
	}
	virtual bool load(class Loader *);
	virtual bool isHostile(int alignment) {
		return (!(this->alignment & alignment));
	}

	virtual void setFleeVector(int x, int y) {
		fdx = x;
		fdy = y;
	}
	virtual void getFleeVector(int *x, int *y) {
		*x = fdx;
		*y = fdy;
	}

	virtual bool isHome(struct place *place) {
		return (place == home);
	}

	virtual void setHome(struct place *place) {
		home = place;
	}

	virtual int getSize(void) {
		return size;
	}

	virtual void forEachMember(bool(*fx) (class Character *, void *),
				   void *);
	virtual void destroy();
	virtual void cleanupAfterCombat(void);
	virtual void removeMember(class Character *);
	virtual struct conv *getConversation();
	virtual bool joinPlayer(void);
	virtual bool createMembers();
	virtual void paint(int sx, int sy);
	virtual void disembark();
	virtual bool turn_vehicle();
	virtual void hit_by_ordnance(class OrdnanceType * ordnance);
	virtual void relocate(struct place *place, int x, int y);
	virtual struct formation *get_formation();
        virtual void describe(int count);
        virtual char *get_movement_sound();
        virtual struct sprite *getSprite();

	struct list members;

	struct sched *sched;
	int act;
	int appt;
	class Vehicle *vehicle;
	int dx, dy;
	struct position_info pinfo;

        // This special flag is for npc parties created during combat (for
        // example summoned parties or parties that randomly cause an ambush).
        // By default I assume this is true. But if an npc party initiates
        // combat by attacking the player then I set it to false and delete the
        // npc party in the placeAdvanceTurns() code.
        bool destroy_on_combat_exit;

      protected:
	virtual bool enter_town(class Portal * portal);
	virtual void wander();
	void work();
	bool commute();
	bool gotoSpot(int x, int y);
	bool attack_with_ordnance(int d);

	int alignment;
	int fdx, fdy;
	struct place *home;
	int size;
	struct conv *conv;
	bool isWrapper;
	int turn_cost;
	bool loitering;
	struct formation *formation;
};

#endif
