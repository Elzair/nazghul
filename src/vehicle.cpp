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

#include "vehicle.h"
#include "sprite.h"
#include "common.h"
#include "sound.h"
#include "place.h"
#include "map.h"
#include "console.h"
#include "wind.h"
#include "player.h"
#include "session.h"
#include "log.h"

/* This is the number of wilderness turns an unnamed vehicle will hang around
 * before being garbage collected. Note that wilderness turns only pass when
 * the player is actually walking around in the wilderness. Kamping, loitering
 * and combat don't count. */
#define VEHICLE_DEF_TTL 256

#include <unistd.h>

// ----------------------------------------------------------------------------
//
// VehicleType
//
// ----------------------------------------------------------------------------


VehicleType::VehicleType(const char *tag, const char *name, struct sprite *sprite,
                         struct terrain_map *_map,
                         ArmsType *_ordnance,
                         bool _vulnerable,
                         bool _killsOccupants,
                         bool _mustTurn,
                         char *_mv_desc,
                         sound_t *_mv_sound,
                         int _tailwind_penalty,
                         int _headwind_penalty,
                         int _crosswind_penalty,
                         int _max_hp,
                         int _speed
        )
        : ObjectType(tag, name, sprite, vehicle_layer)
{
        map = _map;
        formation = NULL;
        ordnance = _ordnance;
        is_vulnerable = _vulnerable;
        kills_occupants = _killsOccupants;
        must_turn = _mustTurn;
        mv_desc = strdup(_mv_desc);
        assert(mv_desc);
        mv_sound = _mv_sound;
        tailwind_penalty = _tailwind_penalty;
        headwind_penalty = _headwind_penalty;
        crosswind_penalty = _crosswind_penalty;

        max_hp = _max_hp; // override base class default
        speed  = _speed;  // override base class default
}

VehicleType::~ VehicleType() 
{
        if (mv_desc)
                free(mv_desc);
}

class Object *VehicleType::createInstance()
{
	class Vehicle *obj = new Vehicle(this);
        assert(obj);
	return obj;
}

int VehicleType::getWindPenalty(int facing)
{
	int vdx, vdy, wdx, wdy, base_speed;

	base_speed = getSpeed();

	if (!mustTurn())
                return 1;

	vdx = directionToDx(facing);
	vdy = directionToDy(facing);
	wdx = directionToDx(windGetDirection());
	wdy = directionToDy(windGetDirection());

	// take the vector dot product
        int dotprod = (vdx * wdx + vdy * wdy);
        if (dotprod < 0) {
            // with the wind
            return tailwind_penalty;
        } else if (0 == dotprod) {
            // tacking across the wind
            return crosswind_penalty;
        } else {
            // against the wind
            return headwind_penalty;
        }
}

bool VehicleType::mustTurn()
{
        return must_turn;
}

bool VehicleType::isVulnerable()
{
        return is_vulnerable;
}

bool VehicleType::killsOccupants()
{
        return kills_occupants;
}

int VehicleType::getType() 
{
        return VEHICLE_TYPE_ID;
}

bool VehicleType::isType(int classID) 
{ 
        if (classID == VEHICLE_TYPE_ID)
                return true;
        return ObjectType::isType(classID);
}

class ArmsType *VehicleType::getOrdnance() 
{
        return ordnance;
}

char *VehicleType::getMvDesc() 
{
        return mv_desc;
}

sound_t *VehicleType::get_movement_sound() 
{
        return mv_sound;
}

// ----------------------------------------------------------------------------
//
// Vehicle
//
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
// The constructor used by VehicleType::createInstance()
// ----------------------------------------------------------------------------
Vehicle::Vehicle(VehicleType *type)
        : Object(type), name(0)
{
	setFacing(NORTH);
	hp = type->getMaxHp();
}


// ----------------------------------------------------------------------------
// The constructor used by kern_mk_vehicle()
// ----------------------------------------------------------------------------
Vehicle::Vehicle(VehicleType *type, int _facing, int _hp)
        : Object(type), name(0)
{
	occupant = NULL;
        setFacing(_facing);
	hp = _hp;
}

Vehicle::~Vehicle()
{
        if (name)
                free(name);
}

int Vehicle::get_facing_to_fire_weapon(int dx, int dy)
{
	if (dx)
		return NORTH;
	if (dy)
		return EAST;
	return -1;
}

bool Vehicle::fire_weapon(int dx, int dy, class Object *user)
{
	class ArmsType *ordnance;

	ordnance = getOrdnance();
	assert(ordnance);

	// check facing
	int vdir = getFacing();
	if (((vdir == NORTH || vdir == SOUTH) && dy) ||
	    ((vdir == EAST || vdir == WEST) && dx)) {
		return false;
	}

        ordnance->fireInDirection(getPlace(), getX(), getY(), dx, dy, user);
        return true;
}

bool Vehicle::turn(int dx, int dy, int *cost)
{
	*cost = 0;

	if (getFacing() == vector_to_facing(dx, dy))
		return false;

	if (!setFacing(vector_to_facing(dx, dy)))
		return false;

	*cost = getObjectType()->getSpeed();
	return true;
}

int Vehicle::getMovementCostMultiplier()
{
        return getObjectType()->getWindPenalty(getFacing());
}

void Vehicle::damage(int amount)
{
        Object::damage(amount);
	hp -= amount;
	hp = max(0, hp);
	hp = min(hp, getObjectType()->getMaxHp());

	if (hp == 0)
		destroy();
}

struct formation *Vehicle::get_formation()
{
	return getObjectType()->formation;
}

struct place *Vehicle::getPlace()
{
        if (occupant)
                return occupant->getPlace();
        return Object::getPlace();
}

int Vehicle::getX()
{
        if (occupant != NULL)
                return occupant->getX();
        return Object::getX();
}

int Vehicle::getY()
{
        if (occupant != NULL)
                return occupant->getY();
        return Object::getY();
}

bool Vehicle::mustTurn()
{
        return getObjectType()->mustTurn();   
}

bool Vehicle::isVulnerable()
{
        return getObjectType()->isVulnerable();       
}

void Vehicle::destroy()
{
        Object::destroy();

        if (occupant != NULL &&
            getObjectType()->killsOccupants()) {                
                occupant->destroy();
                setOccupant(0);
        }
}

bool Vehicle::isType(int classID) 
{ 
        if (classID == VEHICLE_ID)
                return true;
        return Object::isType(classID);
}

int Vehicle::getType() 
{
        return VEHICLE_ID;
}

class VehicleType *Vehicle::getObjectType() 
{ 
        return (class VehicleType *) Object::getObjectType();
}

class ArmsType *Vehicle::getOrdnance() 
{ 
        return getObjectType()->getOrdnance();
}

char *Vehicle::getMvDesc() 
{
        return getObjectType()->getMvDesc();
}

sound_t *Vehicle::get_movement_sound()
{
        return getObjectType()->get_movement_sound();
}

void Vehicle::save(struct save *save)
{
        save->enter(save, "(let ((kveh (kern-mk-vehicle %s %d %d)))\n",
                    getObjectType()->getTag(), getFacing(), getHp());
        if (name) {
                save->write(save, "(kern-vehicle-set-name kveh \"%s\")\n", name);
        }
        if (getTTL() != -1) {
                save->write(save, "(kern-obj-set-ttl kveh %d)\n", getTTL());
        }
        save->exit(save, "kveh) ;; vehicle \n");
}

struct mmode *Vehicle::getMovementMode()
{
        return getObjectType()->mmode;
}

const char *Vehicle::getName()
{
        if (name)
                return name;
        return Object::getName();
}

void Vehicle::setName(char *val)
{
        name = strdup(val);
        assert(name);
}

class Object *Vehicle::getOccupant()
{
        return occupant;
}

void Vehicle::setOccupant(class Object *val)
{
        if (val) {
                assert(!occupant);
                occupant = val;
                obj_inc_ref(occupant);
                Object::setTTL(this, -1);
        } else if (occupant) {
                obj_dec_ref(occupant);
                occupant = 0;

                /* Unnamed vehicles will expire after a while. */
                if (!name) {
                        Object::setTTL(this, VEHICLE_DEF_TTL);
                }
        }
}

bool Vehicle::isNamed()
{
        return name != 0;
}

void Vehicle::describe()
{
        if (name) {
                log_continue("the %s", name);
        } else {
                Object::describe();
        }
}
