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

#ifndef vehicle_h
#define vehicle_h

#include "object.h"

class VehicleType:public ObjectType {
 public:
	virtual int getType() {
		return VEHICLE_TYPE_ID;
	}
        virtual bool isType(int classID) { 
                if (classID == VEHICLE_TYPE_ID)
                        return true;
                return ObjectType::isType(classID);
        }

        VehicleType();
	virtual ~ VehicleType() {
	}
	virtual bool init(char *tag, char *name, struct sprite * sprite,
                          int speed, int pmask, char *mv_desc, char *mv_sound, 
			  class ArmsType * ordnance, bool must_turn) {
                if (!ObjectType::init(tag, name, vehicle_layer, sprite))
                        return false;
                this->speed = speed;
                this->pmask = pmask;
                this->mv_desc = mv_desc;
                this->mv_sound = mv_sound;
                this->ordnance = ordnance;
                this->must_turn = must_turn;
                return true;
        }
	virtual class ArmsType *getOrdnance() {
		return ordnance;
	}
	virtual int getPmask() {
		return pmask;
	}
	virtual char *getMvDesc() {
		return mv_desc;
	}
	virtual char *getMvSound() {
		return mv_sound;
	}
	virtual bool mustTurn();
        virtual bool canFace(int facing);
        virtual class Object *createInstance();
	virtual bool load(class Loader * loader);
        virtual int getWindPenalty(int facing);

        struct terrain_map *map;
        struct formation *formation;

 protected:
        int pmask;
        char *mv_desc;
        char *mv_sound;
        class ArmsType *ordnance;
        bool must_turn;
        int tailwind_penalty;
        int headwind_penalty;
        int crosswind_penalty;
};

class Vehicle:public Object {
 public:
        virtual bool isType(int classID) { 
                if (classID == VEHICLE_ID)
                        return true;
                return Object::isType(classID);
        }
	virtual int getType() {
		return VEHICLE_ID;
	}

        Vehicle();
	virtual ~ Vehicle();
        virtual class VehicleType *getObjectType() { 
		return (class VehicleType *) Object::getObjectType();
        }
        virtual void init(int x, int y, struct place *place, 
			  class VehicleType * type, int facing) {
                Object::init(x, y, place, type);
		if (!setFacing(facing))
                        assert(0);
        }
	virtual void init(class VehicleType * type);
	virtual char *getName() {
		return getObjectType()->getName();
	}
        virtual class ArmsType *getOrdnance() { 
		return getObjectType()->getOrdnance();
	}
	virtual int getPmask() {
		return getObjectType()->getPmask();
	}
	virtual char *getMvDesc() {
		return getObjectType()->getMvDesc();
	}
	virtual char *getMvSound() {
		return getObjectType()->getMvSound();
	}
	virtual bool mustTurn();
        virtual int getFacing();
	virtual bool load(class Loader * loader);
        virtual int get_facing_to_fire_weapon(int dx, int dy);
        virtual bool fire_weapon(int dx, int dy, class Object *user);
        virtual void paint(int sx, int sy);
        virtual struct formation *get_formation();
        virtual struct place *getPlace();

        bool turn(int dx, int dy, int *cost);
        int getMovementCostMultiplier();
        void damage(int amount);
        virtual int getX();
        virtual int getY();

        class Object *occupant;
        int facing;
 protected:
        bool setFacing(int facing);

};

#endif				// vehicle_h
