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
#include "closure.h"

struct filter {
        bool (*fx)(struct inv_entry *ie, void *cookie);
        void *cookie;
};

class TrapType;

class Container:public Object {
      public:
	Container();
        Container(class ObjectType *type);
	virtual ~ Container();
	virtual bool add(class ObjectType * type, int quantity);
	virtual void open();
	virtual bool takeOut(class ObjectType * type, int quantity);
	virtual struct inv_entry *search(class ObjectType * type);
	virtual bool isTrapped();
	virtual void setTrap(closure_t *trap);
	virtual closure_t *getTrap();
        virtual void save(struct save *save);

        int filter_count(struct filter *);
	struct inv_entry *first(struct filter *);
	struct inv_entry *next(struct inv_entry *ie, struct filter*);
	struct inv_entry *prev(struct inv_entry *ie, struct filter*);

	void forEach(void (*fx) (struct inv_entry *, void *), void *);

      protected:
        void saveContents(struct save *save);
	struct list contents;
	closure_t *trap;
};

#endif
