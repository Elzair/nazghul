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
	int newx = getX() + dx;
	int newy = getY() + dy;

	dx = newx - originX;
	dy = newy - originY;

	dx = (dx < 0) ? -dx : dx;
	dy = (dy < 0) ? -dy : dy;

	int d = ((dy > dx) ? (dy + (dx >> 1)) : (dx + (dy >> 1)));

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
