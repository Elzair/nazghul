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
#include "Arms.h"
#include "character.h"
#include "screen.h"
#include "sprite.h"
#include "map.h"
#include "console.h"
#include "Field.h"
#include "place.h"
#include "Missile.h"

ArmsType::ArmsType():missile(NULL), thrown(false), field(NULL), ubiquitousAmmo(false),
                     weight(0) 
{

}

bool ArmsType::isType(int classID) 
{
        if (classID == ARMS_TYPE_ID)
                return true;
        return ObjectType::isType(classID);
}

int ArmsType::getType() 
{
        return ARMS_TYPE_ID;
}

ArmsType::~ArmsType()
{
	if (missile != NULL)
		delete missile;
}

bool ArmsType::init(char *tag, char *name, struct sprite * sprite,
                    int slotMask, int damage, int armor,
                    int numHands, int range) 
{
        if (!ObjectType::init(tag, name, item_layer, sprite))
                return false;
        this->slotMask = slotMask;
        this->damage = damage;
        this->armor = armor;
        this->numHands = numHands;
        this->range = range;
        return true;
}

class ArmsType *ArmsType::getMissileType()
{
	if (missile == NULL)
		return NULL;
	return (class ArmsType *) missile->getObjectType();
}

void ArmsType::setMissileType(class ArmsType * missileType)
{
	if (missile != NULL) {
		delete missile;
		missile = NULL;
	}

	if (missileType == NULL)
		return;

	missile = new Missile();
	if (missile == NULL)
		return;

	missile->init(missileType);
}

bool ArmsType::isMissileWeapon()
{
	return (missile != NULL && !thrown);
}

bool ArmsType::fire(class Character * target, int ox, int oy)
{
	if (isMissileWeapon() || isThrownWeapon()) {
		missile->setPlace(target->getPlace());
		missile->setX(ox);
		missile->setY(oy);
		missile->animate(ox, oy, target->getX(), target->getY());
		if (!missile->hitTarget())
			return false;
	}
	target->attack(getDamage());
	return true;
}

bool ArmsType::fire(struct place * place, int ox, int oy, int tx, int ty)
{
	if (isMissileWeapon() || isThrownWeapon()) {
		missile->setPlace(place);
		missile->setX(ox);
		missile->setY(oy);
		missile->animate(ox, oy, tx, ty);
	}
	return true;
}

void ArmsType::setThrown(bool val)
{
	if (val == thrown)
		return;
	thrown = val;

	if (!val) {
		if (missile != NULL) {
			delete missile;
			missile = NULL;
		}
		return;
	}
	// the usual case:
	setMissileType(this);
}

bool ArmsType::dropsField()
{
	if (field != NULL)
		return true;
	if (missile == NULL)
		return false;
	if (thrown)
		// Because a thrown weapon is its own missile, and I just
		// checked myself
		return false;
	return missile->getObjectType()->dropsField();
}

class ArmsType *ArmsType::getAmmoType()
{
	if (thrown)
		return this;
	if (missile == NULL)
		return NULL;
	return missile->getObjectType();
}

int ArmsType::getSlotMask()
{
        return slotMask;
}

int ArmsType::getDamage()
{
        return damage;
}

int ArmsType::getArmor()
{
        return armor;
}

int ArmsType::getNumHands()
{
        return numHands;
}

int ArmsType::getRange()
{
        return range;
}

bool ArmsType::isThrownWeapon()
{
        return thrown;
}

void ArmsType::setFieldType(class FieldType * type)
{
        field = type;
}

class FieldType *ArmsType::getFieldType()
{
        return field;
}

void ArmsType::setUbiquitousAmmo(bool val)
{
        ubiquitousAmmo = val;
}

bool ArmsType::ammoIsUbiquitous() 
{
        return ubiquitousAmmo;
}

void ArmsType::setWeight(int val) 
{
        weight = val;
}

int ArmsType::getWeight(void) 
{
        return weight;
}

