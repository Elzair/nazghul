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

#include "Container.h"
#include "Loader.h"

static void move_to_map(struct inv_entry *ie, void *data)
{
	class Container *box = (class Container *) data;
	while (ie->count) {
		class Object *obj = new Object();
		if (obj) {
			obj->init(ie->type);
			obj->relocate(box->getPlace(), box->getX(),
				      box->getY());
		}
		ie->count--;
	}
	box->subtract(ie->type, ie->count);
}

static void destroy_content(struct inv_entry *ie, void *data)
{
	list_remove(&ie->list);
	delete ie;
}

Container::Container():trap(NULL)
{
	list_init(&contents);
}

Container::~Container()
{
	forEach(destroy_content, NULL);
}

void Container::add(class ObjectType * type, int quantity)
{
	struct inv_entry *ie;

	assert(quantity > 0);

	ie = search(type);
	if (ie) {
		ie->count += quantity;
		return;
	}

	ie = new struct inv_entry;
	if (ie) {
		list_add(&contents, &ie->list);
		ie->ref = 0;
		ie->count = quantity;
		ie->type = type;
	}

}

void Container::subtract(class ObjectType * type, int quantity)
{
	struct inv_entry *ie;

	ie = search(type);
	assert(ie);
	ie->count -= quantity;
	assert(ie->count >= 0);

	if (!ie->count) {
		list_remove(&ie->list);
		delete ie;
	}
}

void Container::open()
{
	forEach(move_to_map, this);
	assert(list_empty(&contents));
}

void Container::forEach(void (*fx) (struct inv_entry *, void *), void *data)
{
	struct list *elem = contents.next;
	while (elem != &contents) {
		struct list *tmp = elem->next;
		struct inv_entry *ie = outcast(elem, struct inv_entry, list);
		fx(ie, data);
		elem = tmp;
	}
}

struct inv_entry *Container::search(class ObjectType * type)
{
	struct list *elem = contents.next;
	while (elem != &contents) {
		struct list *tmp = elem->next;
		struct inv_entry *ie = outcast(elem, struct inv_entry, list);
		if (ie->type == type)
			return ie;
		elem = tmp;
	}
	return NULL;
}

bool Container::isTrapped()
{
	return trap != NULL;
}

class TrapType *Container::getTrap()
{
	return trap;
}

void Container::setTrap(class TrapType * val)
{
	trap = val;
}

bool Container::load(class Loader *loader)
{
        if (!Object::load(loader))
                return false;

        if (!loader->matchToken('{'))
                return false;

        while (!loader->matchToken('}')) {

                class ObjectType *type;
                char *tag;
                int quantity;

                if (!loader->getWord(&tag) ||
                    !loader->getInt(&quantity))
                        return false;

                type = (class ObjectType*)loader->lookupTag(tag, 
                                                            OBJECT_TYPE_ID);
                if (!type) {
                        loader->setError("Error parsing container content "
                                         "list: %s it not a valid object "
                                         "type tag", tag);
                        free(tag);
                        return false;
                }

                free(tag);
                add(type, quantity);
        }

        return true;
}
