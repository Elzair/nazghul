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
#include "sound.h"
#include <closure.h>

class VehicleType:public ObjectType {
 public:
	virtual int getType();
        virtual bool isType(int classID);
        VehicleType(const char *tag, const char *name, struct sprite *sprite,
                    struct terrain_map *map,
                    class ArmsType *ordnance,
                    bool vulnerable,
                    bool killsOccupants,
                    bool mustTurn,
                    char *mv_desc,
                    sound_t *mv_sound,
                    int tailwind_penalty,
                    int headwind_penalty,
                    int crosswind_penalty,
                    int maxHp,
                    int speed
                    );
	virtual ~VehicleType();
	virtual class ArmsType *getOrdnance();
	virtual char *getMvDesc();
	virtual sound_t *get_movement_sound();
	virtual bool mustTurn();
        virtual class Object *createInstance();
        virtual int getWindPenalty(int facing);
        

        bool isVulnerable();
        bool killsOccupants();

        struct formation *formation;
        struct terrain_map *map;
        closure_t *renderCombat;
        struct mmode *mmode;

 protected:
        class ArmsType *ordnance;
        bool is_vulnerable;
        bool kills_occupants; /* on destroy */
        bool must_turn;
        char *mv_desc;
        sound_t *mv_sound;
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
	virtual const char *getName();
        virtual int getX();
        virtual int getY();
        virtual class ArmsType *getOrdnance();
        virtual struct mmode *getMovementMode();
	virtual char *getMvDesc();
        virtual sound_t *get_movement_sound();
	virtual bool mustTurn();
        virtual int get_facing_to_fire_weapon(int dx, int dy);
        virtual bool fire_weapon(int dx, int dy, class Object *user);
        virtual struct formation *get_formation();
        virtual struct place *getPlace();
        virtual void destroy();
        virtual void save(struct save *save);
        virtual void describe();

        bool isVulnerable();
        bool turn(int dx, int dy, int *cost);
        int getMovementCostMultiplier();
        void damage(int amount);
        void setName(char *val);
        class Object *getOccupant();
        void setOccupant(class Object *val);
        bool isNamed();

 protected:
        char *name;
        class Object *occupant;
};

#endif				// vehicle_h
