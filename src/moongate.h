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
#ifndef moongate_h
#define moongate_h

#include "object.h"

struct list;

enum moongate_state {
	MOONGATE_CLOSED,
	MOONGATE_OPENING,
	MOONGATE_OPENED,
	MOONGATE_CLOSING,
};

class MoongateType:public ObjectType {
      public:
        MoongateType(char *tag, char *name, struct sprite *sprite,
                     int numPhases, char *enterSound, int maxLight);
	virtual ~ MoongateType();
	virtual int getType();
	virtual bool isType(int classID);
	virtual void setSprite(int phase, struct sprite *sprite);
	virtual struct sprite *getSprite(int phase);
	virtual char *getEnterSound();
	virtual int getNumPhases();
	virtual int getMaxLight();

      protected:
	int n_phases;
	struct sprite **sprites;
	char *enter_sound;
	int maxLight;
};

class Moongate:public Object {
      public:
	virtual int getType();
	virtual bool isType(int classID);
        Moongate();
	virtual ~Moongate();
	virtual class MoongateType *getObjectType();
	virtual struct sprite *getSprite();
	virtual char *getName();
	virtual bool isOpen();
	virtual bool isClosed();
	virtual char *getEnterSound();
	virtual int getNumFrames();
	virtual void init(int x, int y, struct place *place, class MoongateType * type, int phase);
	virtual int getLight();
	virtual void paint(int sx, int sy);
	virtual void open();
	virtual void close();
	virtual void animateOpening();
	virtual void animateClosing();

      protected:
	int phase;
	int frame;
	enum moongate_state state;
};

#ifdef __cplusplus
extern "C" {
#endif

	extern class Moongate **Moongates;

	extern void moongateOpenSourceGate(int phase);
	extern void moongateCloseSourceGate(int phase);
	extern void moongateOpenDestinationGate(int phase);
	extern void moongateCloseDestinationGate(int phase);
	extern class Moongate *moongateGetDestinationGate(void);
	extern void moongateSetAnimationWorkQueue(struct list *wq);

#ifdef __cplusplus
}
#endif
#endif
