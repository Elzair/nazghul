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

#include <unistd.h>

VehicleType::VehicleType()
{
	map = 0;
	mv_desc = 0;
	mv_sound = 0;
	ordnance = 0;
	formation = 0;
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
	hp = type->max_hp;
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
	hp = min(hp, getObjectType()->max_hp);

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

bool Vehicle::fire_weapon(int dx, int dy)
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

        ordnance->fireInDirection(getPlace(), getX(), getY(), dx, dy);
        return true;
#if 0
	int i;
	Object *cannonball;

	/* Start the sound of the cannon being fired */
	soundPlay(ordnance->getFireSound(), SOUND_MAX_VOLUME);

	/* Create a cannonball */
	cannonball = new Object();
	cannonball->init(occupant->getX(), occupant->getY(),
			 occupant->getPlace(), ordnance->getAmmo());
	if (!cannonball) {
		err("object_create failed");
		return true;
	}

	usleep(50000);		/* Hack */

	/* Move the cannonball */
	for (i = 0; i <= ordnance->getRange(); i++) {

		class NpcParty *npc;

		/* paint the cannonball */
		placeAddObject(cannonball);
		mapUpdate(0);
		usleep(50000);	/* Hack */

		/* remember any npc party standing where the cannonball is */
		npc = placeGetNPC(cannonball->getX(), cannonball->getY());

		/* remove the cannonball */
		placeRemoveObject(cannonball);

		/* check if we hit an npc party (suicide doesn't count) */
		if (npc && npc != occupant) {
			consolePrint("%s hit ", ordnance->getName());
                        npc->describe(1);
                        consolePrint("!\n");
			npc->hit_by_ordnance(ordnance);
			if (npc->isDestroyed()) {
				consolePrint("%s destroyed ", 
                                             ordnance->getName());
				npc->describe(1);
				consolePrint("!\n");
				delete npc;
				mapUpdate(0);
			}
			break;
		}
		// check if we hit the player party (awful hack, but I need to
		// do a lot more work merging the player and npc party code
		// together before I can resort to a single method)
		else if (occupant != player_party &&
			 cannonball->getX() == player_party->getX() &&
			 cannonball->getY() == player_party->getY()) {
			player_party->hit_by_ordnance(ordnance);
			break;
		}

		/* move the cannonball */
		cannonball->setX(place_wrap_x(getPlace(), 
                                              cannonball->getX() + dx));
		cannonball->setY(place_wrap_y(getPlace(),
                                              cannonball->getY() + dy));
	}

	delete(cannonball);

	mapUpdate(0);

	return true;
#endif
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

int Vehicle::getSpeed()
{
	int vdx, vdy, wdx, wdy, base_speed;

	base_speed = getObjectType()->getSpeed();

	if (!mustTurn())
		return base_speed;

	vdx = directionToDx(getFacing());
	vdy = directionToDy(getFacing());
	wdx = directionToDx(windGetDirection());
	wdy = directionToDy(windGetDirection());

	// take the vector dot product
	switch (vdx * wdx + vdy * wdy) {
	case -1:
		// with the wind
		return base_speed * 2;
	case 0:
		// tacking across the wind
		return base_speed;
	case 1:
		// against the wind
		return base_speed * 4;
	default:
		assert(false);
		return 0;
	}

}

void Vehicle::damage(int amount)
{
	hp -= amount;
	hp = max(0, hp);
	hp = min(hp, getObjectType()->max_hp);

	if (hp == 0)
		destroy();
}

void Vehicle::repair(void)
{
	// Assume one hour of repairs, which is 10% of max.
	damage(-getObjectType()->max_hp / 10);
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
