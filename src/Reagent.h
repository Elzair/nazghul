/* Copyright (c) 2002 Gordon McNutt */

#ifndef Reagent_h
#define Reagent_h

#include "object.h"

// A reagent is an object that appears in the list of things which a player can
// (M)ix to make a spell. It has absolutely no interesting or unusual
// properties other than it's type. And the fact that I have to write a new
// class to support this tells me I took a wrong turn somewhere.

class ReagentType:public ObjectType {
 public:
        virtual bool isType(int classID);
        virtual int getType();
        ReagentType();
	 virtual ~ ReagentType();
	virtual bool load(class Loader * loader);
};

#endif
