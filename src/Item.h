/* Copyright (c) 2002 Gordon McNutt */
#ifndef item_h
#define item_h

#include "object.h"

class Character;

// An item is an object that appears in the list of things which a player can
// (U)se,

class ItemType:public ObjectType {
      public:
	virtual bool isType(int classID);
	virtual int getType();
	 ItemType();
	 virtual ~ ItemType();
	virtual bool init(char *tag, char *name, struct sprite *sprite,
			  int effect, int amount, int duration);
	virtual void setTarget(int val);
	virtual int getEffect();
	virtual int getAmount();
	virtual int getDuration();
	virtual int getTarget();
	virtual void use(class Character * target);
	virtual bool isFood();
	virtual void setFood(bool val);
	virtual void setMessage(char *message);
	virtual bool isConsumable();
	virtual void setConsumable(bool val);
      protected:
	int effect;
	int amount;
	int duration;
	int target;
	bool food;
	char *message;
	bool consumable;
};

#endif				// item_h
