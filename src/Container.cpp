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

#include "common.h"
#include "Container.h"
#include "session.h"

static void move_to_map(struct inv_entry *ie, void *data)
{
	class Container *box = (class Container *) data;
        assert(ie->count);

        class Object *obj = new Object();
        assert(obj);

        obj->init(ie->type);
        obj->setCount(ie->count);
        obj->relocate(box->getPlace(), box->getX(), box->getY());

	box->takeOut(ie->type, ie->count);
}

static void destroy_content(struct inv_entry *ie, void *data)
{
	list_remove(&ie->list);
	delete ie;
}

Container::Container()
{
	list_init(&contents);
}

Container::~Container()
{
        return;
	forEach(destroy_content, NULL);
}

bool Container::add(class ObjectType * type, int quantity)
{
	struct inv_entry *ie;

	if (quantity <= 0)
                return false;

	ie = search(type);
	if (!ie) {
                ie = new struct inv_entry;                
		list_add(&contents, &ie->list);
		ie->ref = 0;
		ie->count = 0;
		ie->type = type;
	}

        ie->count += quantity;

        return true;
}

int Container::numAvail(class ObjectType *type)
{
	struct inv_entry *ie;

	ie = search(type);
        if (!ie)
                return 0;
        return ie->count;

}

bool Container::takeOut(class ObjectType * type, int quantity)
{
	struct inv_entry *ie;

	ie = search(type);
        if (!ie)
                return false;

        if (ie->count < quantity)
                return false;

	ie->count -= quantity;
	assert(ie->count >= 0);

	if (!ie->count) {
		list_remove(&ie->list);
		delete ie;
	}

        return true;
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

void Container::saveContents(struct save *save)
{
        struct list *elem;
        struct inv_entry *ie;
        int count;

        save->enter(save, "(list ");

        // Iterate backwards to save/reload in the same order
        for (elem = contents.prev; elem != &contents; elem = elem->prev) {
                ie = outcast(elem, struct inv_entry, list);
                count = ie->count - ie->ref;
                if (count) {
                        save->write(save, "(list %d %s)\n", 
                                    count,
                                    ie->type->getTag());
                }
        }
        save->exit(save, ")\n");
}

void Container::save(struct save *save)
{
        assert(saved < save->session_id);

        saved = save->session_id;

        save->enter(save, "(kern-mk-inventory\n");
        save->write(save, ";; contents\n");
        if (list_empty(&contents)) {
                save->write(save, "nil\n");
        } else {
                saveContents(save);
	}
        
        Object::saveHooks(save);

        save->exit(save, ")\n");
}

int Container::filter_count(struct filter *filter)
{
        struct inv_entry *ie;
        int q = 0;

        ie = first(filter);
        while (ie) {
                q++;
                ie = next(ie, filter);
        }

        return q;
}

struct inv_entry *Container::first(struct filter *filter)
{
        struct list *elem;
        struct inv_entry *ie;

        elem = contents.next;
        while (elem != &contents) {
                ie = outcast(elem, struct inv_entry, list);
                if (!filter ||
                    filter->fx(ie, filter->fdata))
                        return ie;
                elem = elem->next;
        }
        return NULL;
}

struct inv_entry *Container::next(struct inv_entry *ie, struct filter *filter)
{
        struct list *elem;

        if (ie == NULL)
                return first(filter);

        elem = ie->list.next;
        while (elem != &contents) {
                ie = outcast(elem, struct inv_entry, list);
                if (!filter ||
                    filter->fx(ie, filter->fdata))
                        return ie;
                elem = elem->next;
        }
        return NULL;
}

struct inv_entry *Container::prev(struct inv_entry *ie, struct filter *filter)
{
        struct list *elem;

        elem = ie->list.prev;
        while (elem != &contents) {
                ie = outcast(elem, struct inv_entry, list);
                if (!filter ||
                    filter->fx(ie, filter->fdata))
                        return ie;
                elem = elem->prev;
        }
        return NULL;
}

bool Container::isEmpty()
{
        return list_empty(&contents);
}

void Container::moveToFront(struct inv_entry *ie)
{
        list_remove(&ie->list);
        list_add(&contents, &ie->list);
}

void Container::relocate(struct place *newplace, int newx, int newy, int flags,
                         struct closure *place_switch_hook)
{
        // Spill the contents but don't try to put this down on the map as a
        // physical object, because it's not.
        setPlace(newplace);
        setX(newx);
        setY(newy);
        open();
}
