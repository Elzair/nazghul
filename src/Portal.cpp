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

#include "Portal.h"

#include "common.h"

int Portal::getType() 
{
        return PORTAL_ID;
}
bool Portal::isType(int classID) 
{ 
        if (classID == getType())
                return true;
        return Object::isType(classID);
}

Portal:: Portal(ObjectType * type)
        : Object(type)
{        
        automatic = false;
        open = true;
}

Portal::~ Portal() 
{
}

Portal *Portal::getDestinationPortal()
{
        return dest;
}

bool Portal::isAutomatic() 
{
        return automatic;
}

bool Portal::isOpen() 
{
        return open;
}

void Portal::setDestinationPortal(Portal *val) { dest = val; }
void Portal::setAutomatic(bool val) { automatic = val; }
void Portal::setOpen(bool val) { open = val; }

void Portal::enter(Object *obj)
{
        assert(dest);
        obj->remove();
        dest->exit(obj);
}

void Portal::exit(Object *obj)
{
        obj->relocate(getPlace(), getX(), getY());
}


