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
#ifndef portal_h
#define portal_h

#include "object.h"

class Portal:public Object {
 public:
	virtual int getType() {
		return PORTAL_ID;
	}
        virtual bool isType(int classID) { 
                if (classID == getType())
                        return true;
                return Object::isType(classID);
        }

        Portal() {
                edge_entrance = false;
                toPlace = NULL;
                toX = 0;
                toY = 0;
                toW = 0;
                toH = 0;
        }
	virtual ~ Portal() {
	}

	virtual struct place *getFromPlace() {
		return getPlace();
	}
	virtual int getFromX() {
		return getX();
	}
	virtual int getFromY() {
		return getY();
	}
	virtual struct place *getToPlace() {
		return toPlace;
	}
	virtual int getToX() {
		return toX;
	}
	virtual int getToY() {
		return toY;
	}
	virtual bool isAutomatic() {
		return automatic;
	}

        virtual void init(int fromX, int fromY, struct place *fromPlace, 
			  class ObjectType * type,
                          int toX, int toY, struct place *toPlace, 
                          bool automatic) {
                Object::init(fromX, fromY, fromPlace, type);
                this->toPlace = toPlace;
                this->toX = toX;
                this->toY = toY;
                toW = 1; // default
                toH = 1; // default
                this->automatic = automatic;
                edge_entrance = false;
        }

        bool edge_entrance;
        struct place *toPlace;
        int toX, toY, toW, toH;
        bool automatic;
};

#endif
