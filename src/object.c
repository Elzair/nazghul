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
#include "object.h"
#include "util.h"
#include "place.h"
#include "character.h"
#include "map.h"
#include "sprite.h"
#include "screen.h"
#include "Loader.h"
#include "console.h"
#include "sound.h"
#include "player.h"

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

bool ObjectType::isType(int classID) 
{
        return (classID == OBJECT_TYPE_ID);
}

int ObjectType::getType()
{
        return OBJECT_TYPE_ID;
}

ObjectType::ObjectType()
{
        list_init(&this->list);
}

char *ObjectType::getTag()
{
        return tag;
}

char *ObjectType::getName()
{
        return name;
}

struct sprite *ObjectType::getSprite()
{
        return sprite;
}

enum layer ObjectType::getLayer()
{
        return layer;
}

bool ObjectType::bindTags(class Loader * loader)
{
        return true;
}

bool ObjectType::isVisible()
{
        return true;
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
        int volume;
        int distance;
        struct place *foc_place;
        int foc_x, foc_y;

	if (getPlace())
		place_remove_object(getPlace(), this);
	setPlace(newplace);
	setX(newx);
	setY(newy);
	place_add_object(getPlace(), this);
	mapSetDirty();

        volume = SOUND_MAX_VOLUME;

        // Only apply distance attenuation in towns and wilderness.
        if (player_party->context != CONTEXT_COMBAT) {
                mapGetCameraFocus(&foc_place, &foc_x, &foc_y);
                distance = place_flying_distance(foc_place, foc_x, foc_y,
                                                 getX(), getY());

                // reduce volume proportionally to distance
                if (distance > 1)
                        volume /= (distance/2);
        }

        soundPlay(get_movement_sound(), volume);
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

	if (!loader->getInt(&x) || 
            !loader->getInt(&y))
		return false;
	return true;
}

void Object::paint(int sx, int sy)
{
	struct sprite *sprite = getSprite();
	if (sprite)
		spritePaint(sprite, 0, sx, sy);
}

void Object::describe(int count)
{
        getObjectType()->describe(count);
        if (!isVisible())
                consolePrint(" (invisible)");
}

char *Object::get_movement_sound()
{
        return 0;
}

class Object *Object::clone()
{
        // gmcnutt: added support for an optional quantity field for placed
        // objects.

        class Object *obj;

        obj = getObjectType()->createInstance();
        obj->setX(getX());
        obj->setY(getY());

        // FIXME: should assign the new object a unique script tag

        return obj;
}

//////////////////////////////////////////////////

bool Object::isType(int classID) 
{
        return (classID == OBJECT_ID);
}
int Object::getType() 
{
        return OBJECT_ID;
}

Object::Object():type(NULL), x(0), y(0), place(NULL), selected(false),
                 destroyed(false), turn(0) 
{
        list_init(&this->container_link.list);
        script_tag = 0;
}

Object::Object(class ObjectType * type) 
{
        this->type = type;
        list_init(&this->container_link.list);
}

Object::~Object()
{
}

int Object::getX()
{
        return x;
}

int Object::getY()
{
        return y;
}

struct place *Object::getPlace()
{
        return place;
}

struct sprite *Object::getSprite()
{
        return type->getSprite();
}

bool Object::isSelected()
{
        return selected;
}

enum layer Object::getLayer(void)
{
        return (enum layer) container_link.key;
}

char *Object::getName(void)
{
        return type->getName();
}

class ObjectType *Object::getObjectType()
{
        return type;
}

int Object::getTurn(void)
{
        return turn;
}

bool Object::isDestroyed()
{
        return destroyed;
}


void Object::setX(int x)
{
        this->x = x;
}

void Object::setY(int y)
{
        this->y = y;
}

void Object::changeX(int dx)
{
        this->x += dx;
}

void Object::changeY(int dy)
{
        this->y += dy;
}

void Object::setPlace(struct place *place)
{
        this->place = place;
}

void Object::select(bool val)
{
        selected = val;
}

void Object::destroy()
{
        destroyed = true;
        remove();
}

int Object::getLight()
{
        return 0;
}

void Object::setTurn(int turn)
{
        this->turn = turn;
}

void Object::advanceTurn(int turn)
{
        setTurn(turn);
}

void Object::changeTurn(int delta)
{
        turn += delta;
}

void Object::synchronize(int turn)
{
        setTurn(turn);
}

bool Object::isVisible()
{
        return getObjectType()->isVisible();
}

bool Object::isShaded()
{
        return false;
}

bool Object::is_opaque()
{
        return false;
}

int Object::getAlignment()
{
        return 0;
}

bool Object::joinPlayer()
{
        return false;
}

int Object::getActivity()
{
        return 0;
}

struct conv *Object::getConversation()
{
        return NULL;
}
