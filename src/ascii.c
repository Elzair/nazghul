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
#include "ascii.h"
#include "images.h"
#include "screen.h"
#include "common.h"
#include "session.h"

#include <assert.h>

static struct {
	Uint32 whiteIndex;
	Uint32 blackIndex;

} Ascii;

void asciiPaint(char c, int x, int y, SDL_Surface * surf)
{
	SDL_Rect dest;
	SDL_Rect src;
	int row;
	int col;

	assert(Session->ascii.images);

	if (c == '\t')
		c = ' ';

	assert(c >= ' ');

	/* fixme -- put these calcs in a table or something. Don't need to do
	 * it every time. */

	c = c - ' ' + Session->ascii.offset;

	col = c % Session->ascii.images->cols;
	row = c / Session->ascii.images->cols;

	src.x = (col * ASCII_W) + Session->ascii.images->offx;
	src.y = (row * ASCII_H) + Session->ascii.images->offy;
	src.w = ASCII_W;
	src.h = ASCII_H;

	dest.x = x;
	dest.y = y;
	dest.w = ASCII_W;
	dest.h = ASCII_H;

	SDL_BlitSurface(Session->ascii.images->images, &src, surf, &dest);
}

void asciiInvert(void)
{
	int wIndex, bIndex;

	/* Get the index of white */
	wIndex = SDL_MapRGB(Session->ascii.images->images->format, 0xff, 0xff, 0xff);

	/* Get the index of black */
	bIndex = SDL_MapRGB(Session->ascii.images->images->format, 0x00, 0x00, 0x00);

	/* Set the white index to black */
	SDL_SetPalette(Session->ascii.images->images, SDL_LOGPAL, &fontBlack, wIndex, 1);

	/* Set the black index to white */
	SDL_SetPalette(Session->ascii.images->images, SDL_LOGPAL, &fontWhite, bIndex, 1);

}

void asciiUninvert(void)
{
	int wIndex, bIndex;

	/* Get the index of white */
	wIndex = SDL_MapRGB(Session->ascii.images->images->format, 0xff, 0xff, 0xff);

	/* Get the index of black */
	bIndex = SDL_MapRGB(Session->ascii.images->images->format, 0x00, 0x00, 0x00);

	/* Set the white index to white */
	SDL_SetPalette(Session->ascii.images->images, SDL_LOGPAL, &fontWhite, wIndex, 1);

	/* Set the black index to black */
	SDL_SetPalette(Session->ascii.images->images, SDL_LOGPAL, &fontBlack, bIndex, 1);

}
