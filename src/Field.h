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
#ifndef Field_h
#define Field_h

#include "object.h"

class FieldType:public ObjectType {
      public:
	virtual bool isType(int classID) {
		if (classID == FIELD_TYPE_ID)
			return true;
		return ObjectType::isType(classID);
	}
	virtual int getType() {
		return FIELD_TYPE_ID;
	}

      FieldType():effects(0), light(0) {
	}
	virtual ~ FieldType() {
	}
	virtual int getEffects() {
		return effects;
	}
	virtual void setEffects(int effects) {
		this->effects = effects;
	}
	virtual int getLight() {
		return light;
	}
	virtual void setLight(int val) {
		light = val;
	}
	virtual void setDuration(int val) {
		duration = val;
	}
	virtual int getDuration() {
		return duration;
	}
	virtual int getPmask() {
		return pmask;
	}
	virtual void setPmask(int val) {
		pmask = val;
	}
      protected:
	int effects;
	int light;
	int duration;
	int pmask;
};

class Field:public Object {
      public:
	virtual class FieldType * getObjectType() {
		return (class FieldType *) Object::getObjectType();
	}
	Field() {
	}
	virtual ~ Field() {
	}
	virtual void init(class FieldType * type) {
		Object::init(type);
		duration = type->getDuration();
	}
	virtual int getLight() {
		return getObjectType()->getLight();
	}
	virtual void advanceTurn(int turn) {
		changeTurn(1);
		duration--;
		if (duration == 0)
			destroy();
	}
      protected:
	int duration;
};

#endif
