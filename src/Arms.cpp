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
#include "Loader.h"

ArmsType::ArmsType()
{
        missile        = NULL;
        thrown         = false;
        field          = NULL;
        weight         = 0;
        damage[0]      = 0;
        damage[1]      = 0;
        defend         = 0;
        hit            = 0;
        armor[0]       = 0;
        armor[1]       = 0;
        ubiquitousAmmo = false;
        layer          = item_layer;
}

ArmsType::~ArmsType()
{
	if (missile != NULL)
		delete missile;
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
	//target->attack(getDamage());
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

int ArmsType::getHit()
{
        return hit;
}

int ArmsType::getDamage()
{
        return dice_roll(1, damage[1] - damage[0]) + damage[0];
}

int ArmsType::getDamageMin()
{
        return damage[0];
}

int ArmsType::getDamageMax()
{
        return damage[1];
}

int ArmsType::getDefend()
{
        return defend;
}

int ArmsType::getArmor()
{
        return dice_roll(1, armor[1] - armor[0]) + armor[0];
}

int ArmsType::getArmorMin()
{
        return armor[0];
}

int ArmsType::getArmorMax()
{
        return armor[1];
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

bool ArmsType::load(class Loader *loader)
{
        char *sprite_tag;
        char *missile_tag;
        char *field_tag;
        class ArmsType *missile_type;
        
        if (!loader->getWord(&tag) ||
            !loader->matchToken('{') ||
            !loader->matchWord("name") ||
            !loader->getString(&name) ||
            !loader->matchWord("sprite") ||
            !loader->getWord(&sprite_tag) ||
            !loader->matchWord("hit") ||
            !loader->getInt(&hit) ||
            !loader->matchWord("damage_min") ||
            !loader->getInt(&damage[0]) ||
            !loader->matchWord("damage_max") ||
            !loader->getInt(&damage[1]) ||
            !loader->matchWord("defend") ||
            !loader->getInt(&defend) ||
            !loader->matchWord("armor_min") ||
            !loader->getInt(&armor[0]) ||
            !loader->matchWord("armor_max") ||
            !loader->getInt(&armor[1]) ||
            !loader->matchWord("slotMask") ||
            !loader->getBitmask(&slotMask) ||
            !loader->matchWord("numHands") ||
            !loader->getInt(&numHands) ||
            !loader->matchWord("range") ||
            !loader->getInt(&range) ||
            !loader->matchWord("missile") ||
            !loader->getWord(&missile_tag) ||
            !loader->matchWord("thrown") ||
            !loader->getBool(&thrown) ||
            !loader->matchWord("ubiquitousAmmo") ||
            !loader->getBool(&ubiquitousAmmo) ||
            !loader->matchWord("field") ||
            !loader->getWord(&field_tag) ||
            !loader->matchWord("weight") ||
            !loader->getInt(&weight) ||
            !loader->matchToken('}'))
                return false;
        
        if (strcmp(sprite_tag, "null")) {
                sprite = (struct sprite*)loader->lookupTag(sprite_tag, 
                                                           SPRITE_ID);
                if (!sprite) {
                        loader->setError("Error parsing ARMS: %s is not a "
                                         "valid SPRITE tag",
                                         sprite_tag);
                        free(sprite_tag);
                        return false;
                }
        }
        free(sprite_tag);

        if (strcmp(missile_tag, "null")) {
                missile_type = (class ArmsType*)loader->lookupTag(missile_tag, 
                                                                 ARMS_TYPE_ID);
                if (!missile_type) {
                        loader->setError("Error parsing ARMS: %s is not a "
                                         "valid ARMS tag for the missile",
                                         missile_tag);
                        free(missile_tag);
                        return false;
                }
                setMissileType(missile_type);
        }
        free(missile_tag);

        if (strcmp(field_tag, "null")) {
                field = (class FieldType*)loader->lookupTag(field_tag, 
                                                            FIELD_TYPE_ID);
                if (!field) {
                        loader->setError("Error parsing ARMS: %s is not a "
                                         "valid FIELD tag",
                                         field_tag);
                        free(field_tag);
                        return false;
                }
        }
        free(field_tag);

        if (thrown)
                setMissileType(this);

        return true;
}
