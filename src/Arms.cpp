/* Copyright (c) 2002 Gordon McNutt */
#include "Arms.h"
#include "character.h"
#include "screen.h"
#include "sprite.h"
#include "map.h"
#include "console.h"
#include "Field.h"
#include "place.h"

/*****************************************************************************/
ArmsType::~ArmsType()
{
	if (missile != NULL)
		delete missile;
}

class ArmsType *ArmsType::getMissileType()
{
	if (missile == NULL)
		return NULL;
	return (class ArmsType *) missile->getObjectType();
}

void ArmsType::setMissileType(class ArmsType * missileType)
{
	if (missile != NULL) {
		delete missile;
		missile = NULL;
	}

	if (missileType == NULL)
		return;

	missile = new Missile();
	if (missile == NULL)
		return;

	missile->init(missileType);
}

bool ArmsType::isMissileWeapon()
{
	return (missile != NULL && !thrown);
}

bool ArmsType::fire(class Character * target, int ox, int oy)
{
	if (isMissileWeapon() || isThrownWeapon()) {
		missile->setPlace(target->getPlace());
		missile->setX(ox);
		missile->setY(oy);
		missile->animate(ox, oy, target->getX(), target->getY());
		if (!missile->hitTarget())
			return false;
	}
	target->attack(getAttackValue());
	return true;
}

bool ArmsType::fire(struct place * place, int ox, int oy, int tx, int ty)
{
	if (isMissileWeapon() || isThrownWeapon()) {
		missile->setPlace(place);
		missile->setX(ox);
		missile->setY(oy);
		missile->animate(ox, oy, tx, ty);
	}
	return true;
}

void ArmsType::setThrown(bool val)
{
	if (val == thrown)
		return;
	thrown = val;

	if (!val) {
		if (missile != NULL) {
			delete missile;
			missile = NULL;
		}
		return;
	}
	// the usual case:
	setMissileType(this);
}

bool ArmsType::dropsField()
{
	if (field != NULL)
		return true;
	if (missile == NULL)
		return false;
	if (thrown)
		// Because a thrown weapon is its own missile, and I just
		// checked myself
		return false;
	return missile->getObjectType()->dropsField();
}

class ArmsType *ArmsType::getAmmoType()
{
	if (thrown)
		return this;
	if (missile == NULL)
		return NULL;
	return missile->getObjectType();
}

/*****************************************************************************/

Missile::Missile():surf(NULL)
{
}

Missile::~Missile()
{
	if (surf != NULL)
		SDL_FreeSurface(surf);
}

void Missile::paint(SDL_Rect * rect)
{
	// The rect coordinates are in SCREEN coordinates (not map) so I need
	// to do some clipping here to make sure we don't paint off the map
	// viewer.
	if (rect->x < MAP_X || rect->y < MAP_Y ||
	    ((rect->x + rect->w) > (MAP_X + MAP_W)) ||
	    ((rect->y + rect->h) > (MAP_Y + MAP_H)))
		return;

	// Save the backdrop of the new location
	screenCopy(rect, NULL, surf);

	// Paint the missile at the new location
	spritePaint(getSprite(), 0, rect->x, rect->y);
	screenUpdate(rect);

	// Pause. Doing nothing is too fast, usleep and SDL_Delay are both too
	// slow, so use the custom calibrated busywait.
	busywait(1);

	// Erase the missile by blitting the background
	screenBlit(surf, NULL, rect);
	screenUpdate(rect);
}

void Missile::animate(int Ax, int Ay, int Bx, int By)
{
	// 
	// Derived from Kenny Hoff's Bresenhaum impl at
	// http://www.cs.unc.edu/~hoff/projects/comp235/bresline/breslin1.txt
	// (no license or copyright noted)
	// 

	// Note: what happens if the missile goes or comes in from off-screen?

	hit = false;

	// Create a scratch surface if we don't have one yet
	if (surf == NULL) {
		surf = screenCreateSurface(TILE_W, TILE_H);
		if (surf == NULL)
			return;
	}
	// Get the map coordinates of the view origin (upper left corner)
	int Ox, Oy;
	mapGetMapOrigin(&Ox, &Oy);

	// Get the screen coordinates of the map viewer origin
	int Sx, Sy;
	mapGetScreenOrigin(&Sx, &Sy);

	// Copy the place coordinates of the origin of flight. I'll walk these
	// along as the missile flies and check for obstructions.
	int Px, Py;
	Px = Ax;
	Py = Ay;

	// Convert to screen coordinates. (I need to keep the original
	// B-coordinates for field effects at the bottom of this routine).
	Ax = (Ax - Ox) * TILE_W + Sx;
	Ay = (Ay - Oy) * TILE_H + Sy;
	int sBx = (Bx - Ox) * TILE_W + Sx;
	int sBy = (By - Oy) * TILE_H + Sy;

	// Create the rect which bounds the missile's sprite (used to update
	// that portion of the screen after blitting the sprite).
	SDL_Rect rect;
	rect.x = Ax;
	rect.y = Ay;
	rect.w = TILE_W;
	rect.h = TILE_H;

	// Get the distance components
	int dX = sBx - rect.x;
	int dY = sBy - rect.y;
	int AdX = abs(dX);
	int AdY = abs(dY);

	// Select the sprite orientation based on direction of travel
	struct sprite *sprite = getSprite();
	assert(sprite);
	spriteSetFacing(sprite, vector_to_dir(dX, dY));

	// Moving left?
	int Xincr = (rect.x > sBx) ? -1 : 1;

	// Moving down?
	int Yincr = (rect.y > sBy) ? -1 : 1;

	// Walk the x-axis?
	if (AdX >= AdY) {

		int dPr = AdY << 1;
		int dPru = dPr - (AdX << 1);
		int P = dPr - AdX;

		// For each x
		for (int i = AdX; i >= 0; i--) {

			Px = ((rect.x - Sx) / TILE_W + Ox);
			Py = ((rect.y - Sy) / TILE_H + Oy);
			if (!place_visibility(getPlace(), Px, Py))
				goto done;

			paint(&rect);

			if (P > 0) {
				rect.x += Xincr;
				rect.y += Yincr;
				P += dPru;
			} else {
				rect.x += Xincr;
				P += dPr;
			}
		}
	}
	// Walk the y-axis
	else {
		int dPr = AdX << 1;
		int dPru = dPr - (AdY << 1);
		int P = dPr - AdY;

		// For each y
		for (int i = AdY; i >= 0; i--) {

			Px = ((rect.x - Sx) / TILE_W + Ox);
			Py = ((rect.y - Sy) / TILE_H + Oy);
			if (!place_visibility(getPlace(), Px, Py))
				goto done;

			paint(&rect);

			if (P > 0) {
				rect.x += Xincr;
				rect.y += Yincr;
				P += dPru;
			} else {
				rect.y += Yincr;
				P += dPr;
			}
		}
	}

	hit = true;
      done:
	// erase the missile
	// mapRepaintView(NULL, REPAINT_ACTIVE);
	mapUpdate(0);

	// restore the missile sprite to the default facing
	spriteSetFacing(sprite, SPRITE_DEF_FACING);

	// This object's type is either Ammo (for missile weapons) or Arms (for
	// thrown weapons). If it's Ammo then just bailout here, because I
	// don't have field effects for Ammo right now. But if it's thrown then
	// fall on through and check if we need to leave behind a field.
	if (getObjectType()->isType(AMMO_TYPE_ID))
		return;

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
	field->relocate(getPlace(), Px, Py);
}

bool Missile::hitTarget()
{
	return hit;
}
