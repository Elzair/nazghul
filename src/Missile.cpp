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

#include "Missile.h"
#include "character.h"
#include "screen.h"
#include "sprite.h"
#include "map.h"
#include "console.h"
#include "Field.h"
#include "place.h"
#include "player.h"

Missile::Missile()
{
}

Missile::~Missile()
{
}

class ArmsType *Missile::getObjectType() 
{
        return (class ArmsType *) Object::getObjectType();
}

bool Missile::enterTile(struct place *place, int x, int y)
{
        // kind of a hack... cannonballs aren't blocked by forest
        if (! (flags & MISSILE_IGNORE_LOS))
                return place_visibility(place, x, y);

        if (! (flags & MISSILE_HIT_PARTY))
                return true;

        struck = place_get_NpcParty(place, x, y);

        if (struck != NULL) {
                hit = true;
                return false;
        }

        // fugly hack...
        if (player_party->getPlace() == place &&
            player_party->getX() == x &&
            player_party->getY() == y) {
                struck = player_party;
                hit = true;
                return false;
        }

        return true;
}

void Missile::animate(int Ax, int Ay, int Bx, int By, int _flags)
{
        int origBx = Bx;
        int origBy = By;
        
        hit = false;
        struck = NULL;
        flags = _flags;

        mapAnimateProjectile(Ax, Ay, &Bx, &By, getSprite(), getPlace(), this);
        
        hit = (hit || (origBx == Bx && origBy == By));

	// If this missile/thrown weapon is supposed to leave behind a field
	// then create a field object and drop it on the final target
	// location. This is how burning oil leaves behind a fire field, for
	// example.
	class FieldType *fieldType = getObjectType()->getFieldType();
	if (fieldType == NULL)
		return;
	class Field *field = new Field();
	if (field == NULL)
		return;
	field->init(fieldType);
	field->relocate(getPlace(), Bx, By);
}

bool Missile::hitTarget()
{
	return hit;
}

class Object * Missile::getStruck()
{
        return struck;
}
