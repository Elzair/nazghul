/* Copyright (c) 2002 Gordon McNutt */
#include "Trap.h"

bool TrapType::isType(int classID)
{
	if (classID == TRAP_TYPE_ID)
		return true;
	return ObjectType::isType(classID);
}

int TrapType::getType()
{
	return TRAP_TYPE_ID;
}

TrapType::TrapType():effects(0), amount(0)
{
}

TrapType::~TrapType()
{
}
void TrapType::setEffects(int val)
{
	effects = val;
}
void TrapType::setAmount(int val)
{
	amount = val;
}

int TrapType::getEffects()
{
	return effects;
}

int TrapType::getAmount()
{
	return amount;
}
