/* Copyright (c) 2002 Gordon McNutt */
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
