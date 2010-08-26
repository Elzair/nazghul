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
#ifndef Arms_h
#define Arms_h

#include "object.h"
#include "sound.h"

class Missile;

class ArmsType:public ObjectType {

      public:

	virtual bool isType(int classID);
	virtual int getType();
        ArmsType();
        ArmsType(const char *tag, const char *name, struct sprite *sprite,
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
			);
	virtual ~ ArmsType();

	virtual char * getArmorDice();
	virtual char * getDamageDice();
	virtual int getSlotMask();
        virtual char * getToDefendDice();
        virtual char * getToHitDice();

	virtual int getNumHands();
	virtual int getRange();
	virtual bool isMissileWeapon();
	virtual bool isBeam(); 
	virtual void setMissileType(class MissileType * missileType);
	virtual class MissileType *getMissileType();
	virtual bool fire(class Character * target, int ox, int oy, int* misx, int* misy);
	virtual bool fire(struct place *place, int ox, int oy, int* tx, int* ty);
    virtual bool fireInDirection(struct place *place, int ox, int oy, int dx, int dy, class Object *user);
	virtual void fireHitLoc(Object *attacker, Object *target, struct place *place, int x, int y, int dam);
	virtual bool isThrownWeapon();
	virtual void setThrown(bool val);
	virtual class ObjectType *getAmmoType();
	virtual void setAmmoType(ObjectType * ammo_type);
	virtual void setUbiquitousAmmo(bool val);
	virtual bool ammoIsUbiquitous();
	virtual void setWeight(int val);
	virtual int getWeight(void);
	virtual int modifyStrAttack(int strBonus);
	virtual int modifyDexAttack(int dexBonus);
	virtual int modifyDamageBonus(int damBonus);
	virtual float modifyAvoidBonus(float avoidBonus);
	virtual int get_AP_mod(void);
      
	
      protected:
	int slotMask;
	int numHands;
	int range;
	int weight;
	int modifier_to_AP_of_user;
	bool thrown;
	bool ubiquitousAmmo;
        char *armorDice;
        char *damageDice;
        char *toDefendDice;
        char *toHitDice;
        sound_t *fire_sound;
        bool beam;
	int str_attack_mod;
	int dex_attack_mod;
	int char_damage_mod;
	float char_avoid_mod;
	class Missile *missile;
	class ObjectType *ammoType;
};

#endif
