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
        ReagentType(char *tag, char *name, struct sprite *sprite);
        virtual ~ReagentType();
};

#endif
