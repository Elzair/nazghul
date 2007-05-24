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
#include "dice.h"
#include "character.h"
#include "screen.h"
#include "mmode.h"
#include "sprite.h"
#include "map.h"
#include "console.h"
#include "Field.h"
#include "place.h"
#include "player.h"
#include "vehicle.h"
#include "session.h"

Missile::Missile(ArmsType*type)
        : Object(type)
{

}

Missile::~Missile()
{
}

class ArmsType *Missile::getObjectType() 
{
        return (class ArmsType *) Object::getObjectType();
}

/* set hit=true if a party or object has been struck
return true if the missile has not been interupted */
bool Missile::enterTile(struct place *place, int x, int y)
{
        if (! (flags & MISSILE_IGNORE_LOS))
			{
				int obstruction = place_get_movement_cost(place, x, y, this,0);
				 //int opacity = place_visibility(place, x, y);
				 return ((obstruction != 20) && (dice_roll_numeric(1,100,0)>obstruction));
			}	
			
        if (! (flags & MISSILE_HIT_PARTY))
                return true;

        struck = place_get_Party(place, x, y);

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

        /* Allow wilderness-scale weapons to destroy empty vehicles. */
        struck = place_get_vehicle(place, x, y);
        if (struck != NULL) {
                hit = true;
                return false;
        }

        return true;
}

/*
	triggers a hit-loc ifc event if appropriate
*/
void Missile::fireHitLoc(Object *attacker, Object *target, struct place *place, int x, int y, int dam)
{
	if (getObjectType()->canHitLocation())
		getObjectType()->hitLocation(this, attacker, target, place, x, y, dam);	
}

/*
	Calculates & animates trajectory, returns true if the missile reached the target location
	alters Bx, By to be where it reached (so you can tell where it wound up if blocked)
*/
void Missile::animate(int Ax, int Ay, int *Bx, int *By, int _flags)
{
        int origBx = *Bx;
        int origBy = *By;
        
        hit = false;
        struck = NULL;
        flags = _flags;

        struct sprite *tmpSprite = sprite_clone(getSprite(), 0);
        mapAnimateProjectile(Ax, Ay, Bx, By, tmpSprite, getPlace(), this);
        sprite_del(tmpSprite);

        hit = (hit || (origBx == *Bx && origBy == *By));
}

bool Missile::hitTarget()
{
	return hit;
}

class Object * Missile::getStruck()
{
        return struck;
}
