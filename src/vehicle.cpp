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
#include "Loader.h"
#include "common.h"
#include "sound.h"
#include "place.h"
#include "map.h"
#include "console.h"
#include "wind.h"
#include "player.h"
#include "Loader.h"

#include <unistd.h>

VehicleType::VehicleType()
{
	map = 0;
	mv_desc = 0;
	mv_sound = 0;
	ordnance = 0;
	formation = 0;
        tailwind_penalty = 1;
        headwind_penalty = 1;
        crosswind_penalty = 1;
        is_vulnerable     = false;
        kills_occupants   = false;
}

bool VehicleType::canFace(int facing)
{
	int bit = (1 << facing);
	struct sprite *sprite = getSprite();
	assert(sprite);
	return ((sprite->facings & bit) != 0);
}

class Object *VehicleType::createInstance()
{
	class Vehicle *obj = new Vehicle();
	if (obj)
		obj->init(this);
	return obj;
}

bool VehicleType::load(class Loader *loader)
{
        char *ordnance_tag = NULL;
        char *map_tag = NULL;
        bool result = false;

        // Parse common fields
        if (! ObjectType::load(loader))
                return false;

        // Parse type-specific fields
        if (!loader->matchWord("speed") ||
            !loader->getInt(&speed) ||
            !loader->matchWord("pmask") ||
            !loader->getBitmask(&pmask) ||
            !loader->matchWord("must_turn") ||
            !loader->getBool(&must_turn) ||
            !loader->matchWord("mv_desc") ||
            !loader->getWord(&mv_desc) ||
            !loader->matchWord("mv_sound") ||
            !loader->getString(&mv_sound) ||
            !loader->matchWord("ordnance") ||
            !loader->getWord(&ordnance_tag) ||
            !loader->matchWord("max_hp") ||
            !loader->getInt(&max_hp) ||
            !loader->matchWord("map") ||
            !loader->getWord(&map_tag)) {
                goto cleanup;
        }

        // Parse optional fields
        while (!loader->matchToken('}')) {
                if (loader->matchWord("tailwind_penalty")) {
                        if (!loader->getInt(&tailwind_penalty))
                                goto cleanup;
                }
                else if (loader->matchWord("headwind_penalty")) {
                        if (!loader->getInt(&headwind_penalty))
                                goto cleanup;
                }
                else if (loader->matchWord("crosswind_penalty")) {
                        if (!loader->getInt(&crosswind_penalty))
                                goto cleanup;
                }
                else if (loader->matchWord("is_vulnerable")) {
                        if (!loader->getBool(&is_vulnerable))
                                goto cleanup;
                }
                else if (loader->matchWord("kills_occupants")) {
                        if (!loader->getBool(&kills_occupants))
                                goto cleanup;
                }
                else {
                        loader->setError("Error parsing VEHICLE: %s is not a "
                                         "valid field name", 
                                         loader->getLexeme());
                        goto cleanup;
                }
        }

        // Bind the ordnance tag
        if (strcmp(ordnance_tag, "null")) {
                ordnance = (class ArmsType*)loader->lookupTag(ordnance_tag, 
                                                              ARMS_TYPE_ID);
                if (ordnance == NULL) {
                        loader->setError("Error loading VEHICLE %s: '%s' is "
                                         "not a valid ARMS tag", tag, 
                                         ordnance_tag);
                        goto cleanup;
                }
        }

        // Bind the map tag
        if (strcmp(map_tag, "null")) {
                map = (struct terrain_map*)loader->lookupTag(map_tag, MAP_ID);
                if (map == NULL) {
                        loader->setError("Error loading VEHICLE %s: '%s' is "
                                         "not a valid MAP tag", tag, 
                                         map_tag);
                        goto cleanup;
                }
        }

        result = true;

 cleanup:
        if (ordnance_tag)
                free(ordnance_tag);
        if (map_tag)
                free(map_tag);

        return result;
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
	switch (vdx * wdx + vdy * wdy) {
	case -1:
                // with the wind
		return tailwind_penalty;
	case 0:
		// tacking across the wind
		return crosswind_penalty;
	case 1:
		// against the wind
		return headwind_penalty;
	default:
		assert(false);
		return 0;
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

/*****************************************************************************/

Vehicle::Vehicle()
{
	occupant = 0;
	facing = NORTH;
	hp = 0;
}

Vehicle::~Vehicle()
{
}

void Vehicle::init(class VehicleType * type)
{
	Object::init(type);
	hp = type->getMaxHp();
}

int Vehicle::getFacing()
{
	return facing;
}

bool Vehicle::setFacing(int val)
{
	if (!getObjectType()->canFace(val))
		return false;
	facing = val;
	return true;
}

bool Vehicle::load(class Loader * loader)
{
	bool ret = false;
	char *facing;
	if (!Object::load(loader) ||
	    !loader->getWord(&facing) || !loader->getInt(&hp))
		return false;
	assert(facing);
	ret = setFacing(stringToDirection(facing));
	free(facing);

	// truncate hp to max allowed by type
	hp = min(hp, getObjectType()->getMaxHp());

	return ret;
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

void Vehicle::paint(int sx, int sy)
{
	struct sprite *sprite = getSprite();
	spriteSetFacing(sprite, facing);
	spritePaint(sprite, 0, sx, sy);
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
                occupant = NULL;
        }
}
