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
	virtual int getType();
        virtual bool isType(int classID);
        VehicleType(char *tag, char *name, struct sprite *sprite,
                    struct terrain_map *map,
                    class ArmsType *ordnance,
                    bool vulnerable,
                    bool killsOccupants,
                    bool mustTurn,
                    char *mv_desc,
                    char *mv_sound,
                    int pmask,
                    int tailwind_penalty,
                    int headwind_penalty,
                    int crosswind_penalty,
                    int maxHp,
                    int speed
                    );
	virtual ~VehicleType();
	virtual class ArmsType *getOrdnance();
	virtual int getPmask();
	virtual char *getMvDesc();
	virtual char *getMvSound();
	virtual bool mustTurn();
        virtual bool canFace(int facing);
        virtual class Object *createInstance();
        virtual int getWindPenalty(int facing);

        bool isVulnerable();
        bool killsOccupants();

        struct formation *formation;
        struct terrain_map *map;

 protected:
        class ArmsType *ordnance;
        bool is_vulnerable;
        bool kills_occupants; /* on destroy */
        bool must_turn;
        char *mv_desc;
        char *mv_sound;
        int pmask;
        int tailwind_penalty;
        int headwind_penalty;
        int crosswind_penalty;
        int maxHp;
};

class Vehicle:public Object {
 public:
        virtual bool isType(int classID);
	virtual int getType();
        Vehicle (VehicleType*);
        Vehicle (VehicleType*, int facing, int hp);
	virtual ~Vehicle();
        virtual class VehicleType *getObjectType();
	virtual char *getName();
        virtual int getX();
        virtual int getY();
        virtual class ArmsType *getOrdnance();
	virtual int getPmask();
	virtual char *getMvDesc();
	virtual char *getMvSound();
	virtual bool mustTurn();
        virtual int getFacing();
        virtual int get_facing_to_fire_weapon(int dx, int dy);
        virtual bool fire_weapon(int dx, int dy, class Object *user);
        virtual void paint(int sx, int sy);
        virtual struct formation *get_formation();
        virtual struct place *getPlace();
        virtual void destroy();
        virtual void save(struct save *save);

        bool isVulnerable();
        bool turn(int dx, int dy, int *cost);
        int getMovementCostMultiplier();
        void damage(int amount);

        class Object *occupant;

 protected:
        bool setFacing(int);
        int facing;

};

#endif				// vehicle_h
