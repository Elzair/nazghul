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

class ArmsType:public ObjectType {
      public:
	virtual bool isType(int classID) {
		if (classID == ARMS_TYPE_ID)
			return true;
		return ObjectType::isType(classID);
	}
	virtual int getType() {
		return ARMS_TYPE_ID;
	}

      ArmsType():missile(NULL), thrown(false), field(NULL), ubiquitousAmmo(false),
	    weight(0) {
	}
	virtual ~ ArmsType();
	virtual bool init(char *tag, char *name, struct sprite * sprite,
			  int slotMask, int attackValue, int defendValue,
			  int numHands, int range) {
		if (!ObjectType::init(tag, name, item_layer, sprite))
			return false;
		this->slotMask = slotMask;
		this->attackValue = attackValue;
		this->defendValue = defendValue;
		this->numHands = numHands;
		this->range = range;
		return true;
	}
	virtual int getSlotMask() {
		return slotMask;
	}
	virtual int getAttackValue() {
		return attackValue;
	}
	virtual int getDefendValue() {
		return defendValue;
	}
	virtual int getNumHands() {
		return numHands;
	}
	virtual int getRange() {
		return range;
	}

	virtual bool isMissileWeapon();
	virtual void setMissileType(class ArmsType * missileType);
	virtual class ArmsType *getMissileType();

	virtual bool fire(class Character * target, int ox, int oy);
	virtual bool fire(struct place *place, int ox, int oy, int tx, int ty);

	virtual bool isThrownWeapon() {
		return thrown;
	}
	virtual void setThrown(bool val);

	virtual void setFieldType(class FieldType * type) {
		field = type;
	}
	virtual class FieldType *getFieldType() {
		return field;
	}
	virtual bool dropsField();

	virtual class ArmsType *getAmmoType();
	virtual void setUbiquitousAmmo(bool val) {
		ubiquitousAmmo = val;
	}
	virtual bool ammoIsUbiquitous() {
		return ubiquitousAmmo;
	}
	virtual void setWeight(int val) {
		weight = val;
	}
	virtual int getWeight(void) {
		return weight;
	}

      protected:
	int slotMask;
	int attackValue;
	int defendValue;
	int numHands;
	int range;
	class Missile *missile;
	bool thrown;
	class FieldType *field;
	bool ubiquitousAmmo;
	int weight;
};

class AmmoType:public ObjectType {
      public:
	virtual bool isType(int classID) {
		if (classID == AMMO_TYPE_ID)
			return true;
		return ObjectType::isType(classID);
	}
	virtual int getType() {
		return AMMO_TYPE_ID;
	}

	AmmoType() {
	}
	virtual ~ AmmoType() {
	}
      protected:
};

class Missile:public Object {
      public:
	Missile();
	virtual ~ Missile();
	virtual class ArmsType *getObjectType() {
		return (class ArmsType *) Object::getObjectType();
	}
	virtual void animate(int Ax, int Ay, int Bx, int By);
	virtual void paint(SDL_Rect * rect);
	virtual bool hitTarget();
      protected:
	SDL_Surface * surf;	// for saving/restoring the background
	bool hit;
};

#endif
