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

class Cursor *Cursor = NULL;

Cursor::Cursor():range(0), originX(0), originY(0)
{
}

Cursor::~Cursor()
{
}

void Cursor::init(class ObjectType * type)
{
	Object::init(type);
}

bool Cursor::move(int dx, int dy)
{
  // SAM: Found a few things, noted below.
  // 
  // If the cursor is moved out of LOS, it is not drawn.
  // That is not desirable, methinks.
  // 
  // Currently, it is possible to specify a cursor range 
  // via Cursor::setRange() which is compared with d (calculated below).
  // This allows specification of a roughly circular area
  // within range of cursor selection.
  // 
  // Another common use case (for Look, and such) however, is
  // to be able to specify "any range within the viewport".
  // I see no means of setting such a range currently.
  // 
  // It is desirable in combat to be able to select a point,
  // within range, but outside of the viewport.
  // (automatically scrolling as this is done would be nice)
  // That can be done today (the range, not the scrolling).
  // 
  // Perhaps, then, the answer is another function
  //     Cursor::setViewportOnly()
  // which when set, disallows cursor movement past the viewport.
  // This would allow for both uses, with the "any range within viewport"
  // either setting a range of 999 or similar, or perhaps 
  // the special value -1, to be tested for below as in:
  //     if ( (range != -1) && (d > range) ) {
  // 
	int newx = getX() + dx;
	int newy = getY() + dy;

        newx = place_wrap_x(getPlace(), newx);
        newy = place_wrap_y(getPlace(), newy);

        // this works on wrapping maps
        int d = place_flying_distance(getPlace(), originX, originY, newx, 
                                      newy);

	// Is the new location off the map?
	if (place_off_map(getPlace(), newx, newy))
		return false;

	// Is the new location out of range?
	if (d > range)
		return false;

	// move the cursor
	relocate(getPlace(), newx, newy);

	return true;
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
