/* Copyright (c) 2002 Gordon McNutt */
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
