/* Copyright (c) 2002 Gordon McNutt */

#include "Reagent.h"
#include "Loader.h"

bool ReagentType::isType(int classID)
{
	if (classID == REAGENT_TYPE_ID)
		return true;
	return ObjectType::isType(classID);
}

int ReagentType::getType()
{
	return REAGENT_TYPE_ID;
}

ReagentType::ReagentType()
{
}

ReagentType::~ReagentType()
{
}

bool ReagentType::load(class Loader * loader)
{
	if (!ObjectType::load(loader) || !loader->matchToken('}'))
		return false;

	layer = item_layer;
	return true;
}
