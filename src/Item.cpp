/* Copyright (c) 2002 Gordon McNutt */
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
