/* Copyright (c) 2002 Gordon McNutt */
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
