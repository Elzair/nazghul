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
#include "Item.h"
#include "character.h"
#include "wq.h"
#include "player.h"
#include "console.h"
#include "place.h"
#include "play.h"
#include "event.h"

bool ItemType::isType(int classID)
{
	if (classID == ITEM_TYPE_ID)
		return true;
	return ObjectType::isType(classID);
}

int ItemType::getType()
{
	return ITEM_TYPE_ID;
}

ItemType::ItemType():target(TARG_NONE), food(false), message(NULL),
consumable(true)
{
}

ItemType::~ItemType()
{
	if (message != NULL)
		free(message);
}

bool ItemType::init(char *tag, char *name, struct sprite *sprite,
		    int effect, int amount, int duration)
{
	if (!ObjectType::init(tag, name, item_layer, sprite))
		return false;
	this->effect = effect;
	this->amount = amount;
	this->duration = duration;
	return true;
}

void ItemType::setTarget(int val)
{
	target = val;
}

int ItemType::getEffect()
{
	return effect;
}

int ItemType::getAmount()
{
	return amount;
}

int ItemType::getDuration()
{
	return duration;
}

int ItemType::getTarget()
{
	return target;
}

void ItemType::use(class Character * target)
{
	switch (getEffect()) {

	case EFFECT_LIGHT:
		effectLight(getName(), getAmount(), getDuration(), target);
		break;

	case EFFECT_POISON:
		target->setPoison(true);
		break;

	case EFFECT_CURE:
		if (target->isPoisoned())
			target->setPoison(false);
		break;

	case EFFECT_AWAKEN:
		if (target->isAsleep())
			target->awaken();
		break;

	case EFFECT_HEAL:
		target->changeHp(getAmount());
		break;

        case EFFECT_SLEEP:
                target->changeSleep(true);
                break;

	case EFFECT_NONE:
	default:
		break;
	}

	if (message != NULL) {
		struct KeyHandler kh;
		kh.fx = scroller;
		kh.data = 0;

		statusSetPageText(getName(), message);
		statusSetMode(Page);

		eventPushKeyHandler(&kh);
		eventHandle();
		eventPopKeyHandler();

		statusSetMode(ShowParty);
	}
}

bool ItemType::isFood()
{
	return food;
}

void ItemType::setFood(bool val)
{
	food = val;
}
void ItemType::setMessage(char *msg)
{
	message = strdup(msg);
}

bool ItemType::isConsumable()
{
	return consumable;
}

void ItemType::setConsumable(bool val)
{
	consumable = val;
}
