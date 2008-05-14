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

#include "Being.h"
#include "common.h"
#include "character.h"
#include "list.h"
#include "node.h"
#include "pinfo.h"

class Party:public Being {
      public:

	Party();
	virtual ~Party();

	virtual bool isType(int classID);

	virtual struct formation *get_formation();
        virtual sound_t *get_movement_sound();
        virtual int getMovementCost(int pclass);
	virtual int getSize(void);
	virtual int getSpeed();
        virtual struct sprite *getSprite();
	virtual int getType();
	virtual int getVisionRadius();
        virtual bool addEffect(struct effect *effect, struct gob *gob);
        virtual bool addMember(class Character *);
        virtual bool allDead();
        virtual void burn();
        virtual void damage(int amount);
        virtual void describe();
        virtual void examine();
	virtual void destroy();        
	virtual void disembark();
        virtual void distributeMembers();
	virtual void exec();
	virtual bool joinPlayer(void);
        virtual bool removeEffect(struct effect *effect);
	virtual void forEachMember(bool (*fx) (class Character *, void *), 
                                   void *);
	virtual void forEachReverseMember(bool (*fx) (class Character *, void *), 
                                   void *);
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
        virtual Object *getSpeaker();
        virtual bool isStationary();

        // NOTE: dup of getMemberAtIndex in player.cpp
        virtual class Character *getMemberByOrder(int order);
        virtual class Container *getInventory();

        void removeMembers();

	bool attack_with_ordnance(int d);
	bool gotoSpot(int x, int y);
        void setVehicle(class Vehicle *vehicle);
        bool attackPlayer(int dx, int dy);


        class Vehicle *getVehicle();

	struct node members; // Linked list of party members

	struct position_info pinfo;
	void absorbMemberAPDebt();
	

      protected:
	class Vehicle *vehicle;
	int size;
	bool loitering;
	struct formation *formation;
        bool wandering;
        void (*ctrl)(class Party*);
};

#endif
