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

#define obj_is_being(obj) ((obj)->getLayer() == being_layer)

class Being:public Object {

      public:

	Being();
        Being(class ObjectType *type);

	virtual ~Being();
        virtual int getCurrentFaction();
	virtual enum layer getLayer();
	virtual const char *getName();
        virtual void setCurrentFaction(int faction);

        int getBaseFaction();
        bool pathfindTo(struct place *place, int x, int y, int flags = 0);
        void setBaseFaction(int faction);
	void setName(const char *name);

        // These are public because player_party::rendezvous makes heavy use of
        // them...
        struct astar_node *cachedPath; // for pathfinding
        struct place *cachedPathPlace; // for pathfinding

      protected:
        void setDefaults();
        virtual void switchPlaces(class Being *);

 private:
        char *name;
        int baseFaction;
        int currentFaction;
        void clearCachedPath();
};

#endif
