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

class Container:public Object {
      public:
	Container();
	virtual ~ Container();
	void open();
	virtual struct inv_entry *search(class ObjectType * type);
        bool isEmpty();

        // Virtual methods from base class
	virtual bool add(class ObjectType * type, int quantity);
        virtual void save(struct save *save);
	virtual bool takeOut(class ObjectType * type, int quantity);
	virtual void relocate(struct place *newplace, int newx, int newy, 
                              int flags = 0,
                              struct closure *place_switch_hook = NULL);
        int numAvail(class ObjectType * type);

        int filter_count(struct filter *);
	struct inv_entry *first(struct filter *);
	struct inv_entry *next(struct inv_entry *ie, struct filter*);
	struct inv_entry *prev(struct inv_entry *ie, struct filter*);
        void moveToFront(struct inv_entry *ie);

	void forEach(void (*fx) (struct inv_entry *, void *), void *);

      protected:
        void saveContents(struct save *save);
	struct list contents;
};

#endif
