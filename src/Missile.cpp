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

bool Missile::enterTile(struct place *place, int x, int y)
{
        // kind of a hack... cannonballs aren't blocked by forest
        if (! (flags & MISSILE_IGNORE_LOS))
                return place_visibility(place, x, y);

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

void Missile::animate(int Ax, int Ay, int Bx, int By, int _flags)
{
        int origBx = Bx;
        int origBy = By;
        
        hit = false;
        struck = NULL;
        flags = _flags;

        struct sprite *tmpSprite = sprite_clone(getSprite(), 0);
        mapAnimateProjectile(Ax, Ay, &Bx, &By, tmpSprite, getPlace(), this);
        sprite_del(tmpSprite);

        hit = (hit || (origBx == Bx && origBy == By));

        // New system: check if this object type has a "hit" procedure. If so
        // then run it here. Objects which drop fields will drop the field in
        // their "hit" procedure.
        if (getObjectType()->canHitLocation())
                getObjectType()->hitLocation(this, getPlace(), Bx, By);

}

bool Missile::hitTarget()
{
	return hit;
}

class Object * Missile::getStruck()
{
        return struck;
}
