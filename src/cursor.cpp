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
#include "cursor.h"
#include "place.h"
#include "map.h"

class Cursor *Cursor = NULL;

Cursor::Cursor():range(0), bounded(0), originX(0), originY(0), active(false)
{
}

Cursor::~Cursor()
{
}

void Cursor::init(class ObjectType * type)
{
	Object::init(type);
}

enum MoveResult Cursor::move(int dx, int dy)
{
  // SAM: Found a few things, noted below.
  // 
  // -- Cursor sometimes not drawn:
  // If the cursor is moved out of LOS, it is not drawn.
  // That is not desirable, methinks.
  // 
  // -- Cursor range "any range within viewport"
  // Cursor::setViewportBounded() makes this possible.
  // The caller need only set the range to some large value,
  // and turn on 'bounded'.
	int newx = getX() + dx;
	int newy = getY() + dy;

    newx = place_wrap_x(getPlace(), newx);
    newy = place_wrap_y(getPlace(), newy);
    
    // this works on wrapping maps
    int d = place_flying_distance(getPlace(), originX, originY, newx, 
                                  newy);
    
	// Is the new location off the map?
	if (place_off_map(getPlace(), newx, newy))
      return OffMap;
    
    // Is the new location out of the current viewport (without scrolling)?
    if (bounded && !mapTileIsWithinViewport(newx,newy))
      return OutOfRange;

	// Is the new location out of range?
	if (d > range)
      return OutOfRange;
    
	// move the cursor
	relocate(getPlace(), newx, newy);
    
	return MovedOk;
}

void Cursor::setViewportBounded(int bounded)
{
  this->bounded = !!bounded;
}

void Cursor::setRange(int range)
{
	this->range = range;
}

void Cursor::setOrigin(int x, int y)
{
	originX = x;
	originY = y;
}

void Cursor::relocate(struct place *newplace, int newx, int newy)
{
        Object::relocate(newplace, newx, newy);
        active = true;
}

void Cursor::remove()
{
        Object::remove();
        active = false;
}

bool Cursor::is_active(void)
{
        return active;
}
