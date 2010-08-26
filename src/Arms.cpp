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
#include <assert.h>
#include <string.h>

#include "Arms.h"
#include "character.h"
#include "dice.h"
#include "screen.h"
#include "sprite.h"
#include "map.h"
#include "place.h"
#include "Missile.h"
#include "sound.h"
#include "player.h"
#include "log.h"

ArmsType::ArmsType(const char *tag, const char *name, struct sprite *sprite,
			int slotMask,
			char *to_hit_dice,
			char *to_defend_dice,
			int numHands,
			int range,
			int weight,
			char *damage_dice,
			char *armor_dice,
			int reqActPts,
		   int AP_mod,
			bool thrown,
			bool ubiquitousAmmo,
			sound_t *fireSound,
			class MissileType *missileType,
			class ObjectType *ammo_type,
			int strAttackMod,
			int dexAttackMod,
			int charDamageMod,
			float charAvoidMod,
			bool isBeam
			)
			: ObjectType(tag, name, sprite, item_layer),
				slotMask(slotMask),
				numHands(numHands),
				range(range),
				weight(weight),
				thrown(thrown),
				ubiquitousAmmo(ubiquitousAmmo)
{
	toHitDice = strdup(to_hit_dice);
	toDefendDice = strdup(to_defend_dice);
	damageDice = strdup(damage_dice);
	armorDice = strdup(armor_dice);
	assert(toHitDice && toDefendDice && damageDice && armorDice);
	this->fire_sound = fireSound;

	if (missileType)
	{
		missile = new Missile(missileType);
		assert(missile);
	} else {
		missile = NULL;
	}
	
	ammoType = ammo_type;
	
	if (thrown)
	{
		setAmmoType(this);
	}
	
	str_attack_mod = strAttackMod;
	dex_attack_mod = dexAttackMod;
	char_damage_mod = charDamageMod;
	char_avoid_mod = charAvoidMod;
	
	required_action_points = reqActPts;
	modifier_to_AP_of_user = AP_mod;
	beam = isBeam;
}

ArmsType::ArmsType()
{
        // Don't ever expect to call this. Defining it to override the default
        // one c++ automatically creates.
        assert(false);

        missile        = NULL;
        ammoType		  = NULL;
        thrown         = false;
        weight         = 0;
        ubiquitousAmmo = false;
        layer          = item_layer;
        fire_sound     = NULL_SOUND;
        required_action_points = 1;
	modifier_to_AP_of_user = 0;
}

ArmsType::~ArmsType()
{
	if (missile != NULL)
		delete missile;
	if (toHitDice)
	       free(toHitDice);
	if (toDefendDice)
	       free(toDefendDice);
	if (damageDice)
	       free(damageDice);
	if (armorDice)
	       free(armorDice);
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

class MissileType *ArmsType::getMissileType()
{
	if (missile == NULL)
		return NULL;
	return (class MissileType *) missile->getObjectType();
}

void ArmsType::setMissileType(class MissileType * missileType)
{
	if (missile != NULL) {
		delete missile;
		missile = NULL;
	}

	if (missileType == NULL)
		return;

	missile = new Missile(missileType);
}

class ObjectType *ArmsType::getAmmoType()
{
	return ammoType;
}

void ArmsType::setAmmoType(class ObjectType * ammo_type)
{
	ammoType = ammo_type;
}

bool ArmsType::isMissileWeapon()
{
	return (missile != NULL && !thrown);
}

bool ArmsType::isBeam()
{
	return beam;
}

/*
	triggers a hit-loc ifc event if appropriate for either weapon or missile
*/
void ArmsType::fireHitLoc(Object *attacker, Object *target, struct place *place, int x, int y, int dam)
{
	if (isMissileWeapon() || isThrownWeapon())
	{
		missile->fireHitLoc(attacker, target, place, x, y, dam);
	}
	if (canHitLocation())
		hitLocation(NULL, attacker, target, place, x, y, dam);	
}

/*
	Fires a missile at a specific target, returns true if it reaches the target's location
	updates misx and misy to be the location the missile reaches.
*/
bool ArmsType::fire(class Character * target, int ox, int oy, int *misx, int *misy)
{
	if (isMissileWeapon() || isThrownWeapon()) {
		*misx = target->getX();
		*misy = target->getY();
		missile->setPlace(target->getPlace());
		missile->setX(ox);
		missile->setY(oy);
		int frange = 0;
		if (missile->getObjectType()->isFixedRange())
		{
			frange = getRange();
		}
		missile->animate(ox, oy, misx, misy, 0, frange);
		if (!missile->hitTarget())
			return false;
	}
	return true;
}

/*
	Fires a missile at a specific tile, returns true if it reaches the target's location
	updates tx and ty to be the location the missile reaches.
*/
bool ArmsType::fire(struct place * place, int ox, int oy, int *tx, int *ty)
{
	if (isMissileWeapon() || isThrownWeapon()) {
		missile->setPlace(place);
		missile->setX(ox);
		missile->setY(oy);
		int frange = 0;
		if (missile->getObjectType()->isFixedRange())
		{
			frange = getRange();
		}
		missile->animate(ox, oy, tx, ty, 0, frange);
		if (!missile->hitTarget())
			return false;
	}
	return true;
}

bool ArmsType::fireInDirection(struct place *place, int ox, int oy, 
                               int dx, int dy, class Object *user)
{
        if (!isMissileWeapon() && !isThrownWeapon())
                return false;

        if (fire_sound)
                sound_play(fire_sound, SOUND_MAX_VOLUME);

		int misx = dx * getRange() + ox;
		int misy = dy * getRange() + oy;

        missile->setPlace(place);
        missile->setX(ox);
        missile->setY(oy);
        missile->animate(ox, oy, 
                         &misx, 
                         &misy, 
                         MISSILE_IGNORE_LOS|MISSILE_HIT_PARTY,0);

        if (!missile->hitTarget() || !missile->getStruck())
                return false;

        log_begin("%s hit ", getName());
        missile->getStruck()->describe();
        log_end("!");
        
        // Reference the object while damaging it, since damage can remove it
        // from the map.
        obj_inc_ref(missile->getStruck());

        missile->getStruck()->damage(dice_roll(damageDice));

        if (missile->getStruck()->isDestroyed()) {
                log_begin("%s destroyed ", getName());
                missile->getStruck()->describe();
                log_end("!");
                mapSetDirty();
        }
        
        // Release the reference
        obj_dec_ref(missile->getStruck());

        if (user)
        	user->decActionPoints(getRequiredActionPoints());

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
	setAmmoType(this);
}

int ArmsType::getSlotMask()
{
        return slotMask;
}

char * ArmsType::getToHitDice()
{
        return toHitDice;
}

char * ArmsType::getDamageDice()
{
        return damageDice;
}

char * ArmsType::getToDefendDice()
{
        return toDefendDice;
}

char * ArmsType::getArmorDice()
{
        return armorDice;
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

int ArmsType::modifyStrAttack(int strBonus)
{
	return strBonus *  str_attack_mod;
}

int ArmsType::modifyDexAttack(int dexBonus)
{
	return dexBonus *  dex_attack_mod;
}

int ArmsType::modifyDamageBonus(int damBonus)
{
	return damBonus *  char_damage_mod;
}

float ArmsType::modifyAvoidBonus(float avoidBonus)
{
	return avoidBonus * char_avoid_mod;
}

int ArmsType::get_AP_mod(void) {
    return modifier_to_AP_of_user;
}
