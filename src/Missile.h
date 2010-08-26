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
#ifndef Missile_h
#define Missile_h

#include "object.h"
#include "Arms.h"

#define MISSILE_IGNORE_LOS (1 << 0)
#define MISSILE_HIT_PARTY  (1 << 1)

class MissileType:public ObjectType
{
		public:
	MissileType();
	MissileType(const char *tag, const char *name, struct sprite *sprite, bool isBeam, bool isFixedRange, struct mmode *mmode);
	virtual ~ MissileType();
	
	virtual bool isType(int classID);
	virtual int getType();

	virtual bool isBeam();
	virtual bool isFixedRange();
	virtual void fireHitLoc(Object *attacker, Object *target, struct place *place, int x, int y, int dam);
	virtual bool fireEnterTile(Missile* missile, struct place *place, int x, int y);
		
		protected:
	bool beam;
	bool fixedrange;
};

class Missile:public Object
{
		public:
	Missile(MissileType*);
	virtual ~Missile();
	
	virtual class MissileType *getObjectType();
	virtual void animate(int Ax, int Ay, int *Bx, int *By, int flags, float fixedrange);
	virtual void fireHitLoc(Object *attacker, Object *target, struct place *place, int x, int y, int dam);
	virtual bool hitTarget();
	virtual class Object *getStruck();
	virtual bool enterTile(struct place *place, int x, int y);
	
		protected:
	bool hit;
	class Object *struck;
	int flags;
};

#endif
