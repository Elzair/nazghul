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
/* Concept and design hashed out with Sam Glasby */
#ifndef Mech_h
#define Mech_h

#include "object.h"
#include "place.h"
#include "conv.h"

struct mech_state {
	char *name;              // identifies and describes this state
	struct sprite *sprite;   // shown when rendered
	int pmask;               // passability to travelers
	int light;               // amount radiated (for lamps or things that glow)
	bool opaque;             // blocks line-of-sight
        bool invisible;          // is not shown or described
};

struct mech_transition {
	struct mech_state *from;
	int method;		// fixme: rename as 'event'
	struct mech_state *to;
	struct response *actions;
};

class MechType:public ObjectType {
      public:
	virtual bool isType(int classID) {
		return (classID == MECH_TYPE_ID);
	}
	virtual int getType() {
		return MECH_TYPE_ID;
	}
	MechType();
	~MechType();
	virtual class Object *createInstance();
	virtual bool load(class Loader * loader);
        virtual bool bindTags(class Loader*);

	struct mech_transition *MechType::load_transitions(class Loader *
							   loader, int *n);
	int n_states;
	struct mech_state *states;
	int n_transitions;
	struct mech_transition *transitions;
};

class Mech:public Object {
      public:
	virtual bool isType(int classID) {
		return (classID == MECH_ID);
	}
	virtual int getType() {
		return MECH_ID;
	}
	class MechType *getObjectType() {
		return (class MechType *) Object::getObjectType();;
	}
	Mech();
	~Mech();
	virtual bool load(class Loader * loader);
	virtual bool activate(int method);
	virtual struct sprite *getSprite();
#ifdef USE_OLD_MECH_GETNAME
	virtual char *getName();
#endif
	virtual int getPmask();
	virtual int getLight();
	virtual bool is_opaque();
	virtual bool isVisible();
	virtual void describe(int count);
	virtual void advanceTurn(int turn);

	class Mech *port;
	struct mech_state *state;
	struct conv conv;
        bool activating;
};

#endif				// Mech_h
