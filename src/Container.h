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
#ifndef Container_h
#define Container_h

#include "object.h"

class TrapType;

class Container:public Object {
      public:
	Container();
	virtual ~ Container();
	virtual void add(class ObjectType * type, int quantity);
	virtual void open();
	virtual void subtract(class ObjectType * type, int quantity);
	virtual struct inv_entry *search(class ObjectType * type);
	virtual bool isTrapped();
	virtual void setTrap(class TrapType * trap);
	virtual class TrapType *getTrap();
	void forEach(void (*fx) (struct inv_entry *, void *), void *);
	virtual bool load(class Loader * loader);
      protected:
	struct list contents;
	class TrapType *trap;
};

#endif
