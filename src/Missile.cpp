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

Missile::Missile():surf(NULL)
{
}

Missile::~Missile()
{
	if (surf != NULL)
		SDL_FreeSurface(surf);
}

class ArmsType *Missile::getObjectType() 
{
        return (class ArmsType *) Object::getObjectType();
}

void Missile::paint(SDL_Rect * rect)
{
        if (!getSprite())
                return;
                
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
        if (sprite) {
                spriteSetFacing(sprite, vector_to_dir(dX, dY));
        }

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
        if (sprite) {
                spriteSetFacing(sprite, SPRITE_DEF_FACING);
        }

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
