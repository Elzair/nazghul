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
	virtual ~ ArmsType();
	virtual bool init(char *tag, char *name, struct sprite * sprite,
			  int slotMask, int damage, int defendValue,
			  int numHands, int range);
	virtual int getSlotMask();
	virtual int getDamage();
	virtual int getArmor();
	virtual int getNumHands();
	virtual int getRange();
	virtual bool isMissileWeapon();
	virtual void setMissileType(class ArmsType * missileType);
	virtual class ArmsType *getMissileType();
	virtual bool fire(class Character * target, int ox, int oy);
	virtual bool fire(struct place *place, int ox, int oy, int tx, int ty);
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
	int damage;
	int armor;
	int numHands;
	int range;
	class Missile *missile;
	bool thrown;
	class FieldType *field;
	bool ubiquitousAmmo;
	int weight;
};

#endif
