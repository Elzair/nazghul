/* Copyright (c) 2002 Gordon McNutt */
#include "object.h"
#include "util.h"
#include "place.h"
#include "character.h"
#include "map.h"
#include "sprite.h"
#include "screen.h"
#include "Loader.h"
#include "console.h"

#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*****************************************************************************/

ObjectType::~ObjectType()
{
	if (tag)
		free(tag);
	if (name)
		free(name);
}

bool ObjectType::init(char *tag, char *name, enum layer layer,
		      struct sprite *sprite)
{
	this->tag = strdup(tag);
	this->name = strdup(name);
	this->sprite = sprite;
	this->layer = layer;
	return (this->tag != 0 && this->name != 0);
}

class Object *ObjectType::createInstance()
{
	class Object *obj = new Object();
	if (obj)
		obj->init(this);
	return obj;
}

bool ObjectType::load(class Loader * loader)
{
	char *sprite_tag = 0;

	if (!loader->getWord(&tag) ||
	    !loader->matchToken('{') ||
	    !loader->matchWord("name") ||
	    !loader->getString(&name) ||
	    !loader->matchWord("sprite") || !loader->getWord(&sprite_tag))
		return false;

	sprite = (struct sprite *) loader->lookupTag(sprite_tag, SPRITE_ID);
	if (!sprite) {
		loader->setError("Invalid SPRITE tag '%s'", sprite_tag);
		free(sprite_tag);
		return false;
	}
	free(sprite_tag);

	// layer is set by subclass
	// '}' is parsed by subclass

	return true;
}

void ObjectType::describe(int count)
{
	char *name = getName();
	if (count == 1) {
		if (isvowel(name[0]))
			consolePrint("an ");
		else
			consolePrint("a ");
		consolePrint(name);
	} else {
		consolePrint("some ");
		consolePrint("%ss (%d)", name, count);
	}
}

/*****************************************************************************/

void Object::init(int x, int y, struct place *place, class ObjectType * type)
{
	setX(x);
	setY(y);
	container_link.key = type->getLayer();
	setPlace(place);
	this->type = type;
}

void Object::init(class ObjectType * type)
{
	container_link.key = type->getLayer();
	this->type = type;
}

void Object::relocate(struct place *newplace, int newx, int newy)
{
	if (getPlace())
		place_remove_object(getPlace(), this);
	setPlace(newplace);
	setX(newx);
	setY(newy);
	place_add_object(getPlace(), this);
	mapSetDirty();
}

void Object::remove()
{
	if (getPlace()) {
		place_remove_object(getPlace(), this);
		setPlace(0);
	}
}

bool Object::load(class Loader * loader)
{
	loader->getWord(&script_tag);

	if (!loader->getInt(&x) || !loader->getInt(&y))
		return false;
	return true;
}

void Object::paint(int sx, int sy)
{
	struct sprite *sprite = getSprite();
	if (sprite)
		spritePaint(sprite, 0, sx, sy);
}
