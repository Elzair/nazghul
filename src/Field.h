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
#include "closure.h"

class FieldType:public ObjectType {

      public:
        FieldType(const char *tag, const char *name, struct sprite *sprite, int light, 
                  int duration, int pclass, closure_t *effect);
	virtual ~FieldType();
	virtual bool isType(int classID);
	virtual int getType();
	virtual int getLight();
	virtual void setLight(int val);
	virtual void setDuration(int val);
	virtual int getDuration();
	virtual class Object *createInstance();

        bool isPermanent();

        closure_t *effect; /* when stepped on */
        int pclass;

      protected:
	int light;
	int duration;
};

class Field:public Object {

      public:
	virtual class FieldType * getObjectType();
	Field();
        Field(class FieldType *type);
        Field(class FieldType *type, int duration);
	virtual ~ Field();
	virtual int getLight();
	virtual void exec();
        virtual void save(struct save *save);
        virtual int getPclass();

      protected:
	int duration;
};

#endif
