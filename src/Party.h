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
#ifndef Party_h
#define Party_h

#include "object.h"
#include "list.h"
#include "common.h"
#include "character.h"
#include "pinfo.h"

struct GroupInfo {
        struct list list;
	struct species *species;
	struct occ *occ;
	struct sprite *sprite;
        char *dice;
        struct closure *ai;
};

class PartyType : public ObjectType {
      public:
	PartyType();
        PartyType(char *tag, char *name, struct sprite *sprite);
	virtual ~ PartyType();


	virtual bool isType(int classID);
	virtual int getType();
	//virtual bool init(class Character * ch);
	virtual int getVisionRadius();
	virtual struct GroupInfo *enumerateGroups();
	virtual struct GroupInfo *getNextGroup();
	virtual class Object *createInstance();
	virtual bool isVisible();
        virtual void addGroup(struct species *species, struct occ *occ, struct sprite *sprite, char *dice, struct closure *ai);

	struct formation *formation;
        struct sprite *sleep_sprite;

      protected:
        void setup();
        struct list *i_group;
        struct list groups;
	int pmask;
	int vrad;
	bool visible;
};

class Party:public Object {
      public:

	Party();
        Party(class PartyType *type, int alignment, class Vehicle *vehicle);
	~Party();
        void setup();

	virtual bool isType(int classID);
	virtual bool isHostile(int alignment);

        virtual int getActivity();
	virtual int getAlignment();
	virtual void getFleeVector(int *x, int *y);
	virtual struct formation *get_formation();
        virtual char *get_movement_sound();
	virtual class PartyType *getObjectType();
        virtual int getMovementCost(int pclass);
	virtual int getSize(void);
	virtual int getSpeed();
        virtual struct sprite *getSprite();
	virtual int getType();
	virtual int getVisionRadius();

	virtual void setAlignment(int val);
	virtual void setFleeVector(int x, int y);
        
        virtual bool addEffect(struct effect *effect, struct gob *gob);
        virtual bool addMember(class Character *);
        virtual bool allDead();
        virtual void burn();
	virtual void cleanupAfterCombat(void);
	virtual bool createMembers();
        virtual void damage(int amount);
        virtual void describe();
	virtual void destroy();        
	virtual void disembark();
        virtual void distributeMembers();
	//virtual bool enter_town(class Portal * portal);
	virtual void exec(struct exec_context *cntxt);
        virtual char *getName(void);
	virtual bool joinPlayer(void);
        virtual bool removeEffect(struct effect *effect);
	virtual void forEachMember(bool(*fx) (class Character *, void *), 
                                   void *);
	//virtual void init(class Character * ch);
	virtual void init(class PartyType * type);
	virtual void init(int x, int y, struct place *place, 
                          class PartyType * type);
	virtual MoveResult move(int dx, int dy);
	virtual void paint(int sx, int sy);
	virtual void removeMember(class Character *);
        virtual void save(struct save *save);
        virtual void setPlace(struct place *place);
        virtual void setX(int x);
        virtual void setY(int y);
        virtual void sleep();
        virtual void start();
        virtual void startTurn();
        virtual void switchOrder(class Character *ch1, class Character *ch2);
	virtual bool turn_vehicle();
        virtual void applyEffect(closure_t *effect);

        void enterPortal(class Portal *portal);
        void removeMembers();

	bool attack_with_ordnance(int d);
	bool gotoSpot(int x, int y);

	struct list members;

	int act;
	int appt;
	class Vehicle *vehicle;
	struct position_info pinfo;

      protected:

	int alignment;
	int fdx, fdy;
	int size;
	bool isWrapper;
	int turn_cost;
	bool loitering;
	struct formation *formation;
        bool wandering;
        void (*ctrl)(class Party*);
        int n_members;
};

#endif
