/* Copyright (c) 2002 Gordon McNutt */
#include "Container.h"

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
