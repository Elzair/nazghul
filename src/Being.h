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
#ifndef Being_h
#define Being_h

#include "object.h"
#include "hstack.h"

class Being:public Object {

      public:

	Being();
        Being(class ObjectType *type);
	~Being();

        int pushFaction(int faction); // returns a handle
        void popFaction();
        void rmFaction(int handle);
        int getFaction();
        int setFaction(int faction); // returns a handle
        bool hasFaction(); // false if stack empty
        bool bottomFaction(); // true iff one faction on the stack
        void restoreFaction(int handle, int faction);

      protected:

        void saveFactions(struct save *);
        void clearFactions();
        hstack_t *factions;
};

#endif
