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
	virtual unsigned int getEffect();
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
