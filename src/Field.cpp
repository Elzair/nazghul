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

#include "Field.h"

bool FieldType::isType(int classID) 
{
        if (classID == FIELD_TYPE_ID)
                return true;
        return ObjectType::isType(classID);
}

int  FieldType::getType()
{
        return FIELD_TYPE_ID;
}

FieldType::FieldType():effects(0), light(0)
{

}

FieldType::~FieldType()
{

}

int  FieldType::getEffects()
{
        return effects;
}

void  FieldType::setEffects(int effects)
{
        this->effects = effects;
}

int  FieldType::getLight()
{
        return light;
}

void  FieldType::setLight(int val)
{
        light = val;
}

void  FieldType::setDuration(int val)
{
        duration = val;
}

int  FieldType::getDuration()
{
        return duration;
}

int  FieldType::getPmask()
{
        return pmask;
}

void  FieldType::setPmask(int val)
{
        pmask = val;
}


class FieldType * Field::getObjectType()
{
        return (class FieldType *) Object::getObjectType();
}

Field::Field()
{

}

Field::~ Field()
{

}

void Field::init(class FieldType * type)
{
        Object::init(type);
        duration = type->getDuration();
}

int Field::getLight()
{
        return getObjectType()->getLight();
}

void Field::exec(struct exec_context *context)
{
        duration--;
        if (duration == 0)
                destroy();
}
