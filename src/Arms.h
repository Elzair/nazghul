/* Copyright (c) 2002 Gordon McNutt */
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
