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

class Missile;

class ArmsType:public ObjectType {

      public:

	virtual bool isType(int classID);
	virtual int getType();
        ArmsType();
        ArmsType(char *tag, char *name, struct sprite *sprite,
                 int slotMask,
                 char *to_hit_dice,
                 char *to_defend_dice,
                 int numHands,
                 int range,
                 int weight,
                 char *damage_dice,
                 char *armor_dice,
                 int reqActPts,
                 bool thrown,
                 bool ubiquitousAmmo,
                 char *fire_sound,
                 class ArmsType *missileType,
                 class FieldType *fieldType);
	virtual ~ ArmsType();

	virtual char * getArmorDice();
	virtual char * getDamageDice();
	virtual int getSlotMask();
        virtual char * getToDefendDice();
        virtual char * getToHitDice();

	virtual int getNumHands();
	virtual int getRange();
	virtual bool isMissileWeapon();
	virtual void setMissileType(class ArmsType * missileType);
	virtual class ArmsType *getMissileType();
	virtual bool fire(class Character * target, int ox, int oy);
	virtual bool fire(struct place *place, int ox, int oy, int tx, int ty);
        virtual bool fireInDirection(struct place *place, int ox, int oy, int dx, int dy, class Object *user);
	virtual bool isThrownWeapon();
	virtual void setThrown(bool val);
	virtual void setFieldType(class FieldType * type);
	virtual class FieldType *getFieldType();
	virtual bool dropsField();
	virtual class ArmsType *getAmmoType();
	virtual void setUbiquitousAmmo(bool val);
	virtual bool ammoIsUbiquitous();
	virtual void setWeight(int val);
	virtual int getWeight(void);

      protected:
	int slotMask;
	int numHands;
	int range;
	int weight;
	bool thrown;
	bool ubiquitousAmmo;
        char *armorDice;
        char *damageDice;
        char *toDefendDice;
        char *toHitDice;
        char *fire_sound;
	class Missile *missile;
	class FieldType *fieldType;
};

#endif
