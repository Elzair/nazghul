/* Copyright (c) 2003 Gordon McNutt */

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
			  class OrdnanceType * ordnance, bool must_turn) {
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
	virtual class OrdnanceType *getOrdnance() {
		return ordnance;
	}
	virtual int getPmask() {
		return pmask;
	}
	virtual int getSpeed() {
		return speed;
	}
	virtual char *getMvDesc() {
		return mv_desc;
	}
	virtual char *getMvSound() {
		return mv_sound;
	}
	virtual bool mustTurn() {
		return must_turn;
	}
        virtual bool canFace(int facing);
        virtual class Object *createInstance();

        int max_hp;
        struct terrain_map *map;
        struct formation *formation;

 protected:
        int speed;
        int pmask;
        char *mv_desc;
        char *mv_sound;
        class OrdnanceType *ordnance;
        bool must_turn;
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
        virtual class OrdnanceType *getOrdnance() { 
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
	virtual bool mustTurn() {
		return getObjectType()->mustTurn();
	}
        virtual int getFacing();
	virtual bool load(class Loader * loader);
        virtual int get_facing_to_fire_weapon(int dx, int dy);
        virtual bool fire_weapon(int dx, int dy);
        virtual void paint(int sx, int sy);
        virtual struct formation *get_formation();

        bool turn(int dx, int dy, int *cost);
        int getSpeed();
        void damage(int amount);
        void repair();

        class Object *occupant;
        int facing;
        int hp;
 protected:
        bool setFacing(int facing);

};

#endif				// vehicle_h
