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
