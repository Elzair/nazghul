/* Copyright (c) 2002 Gordon McNutt */
#include "ascii.h"
#include "images.h"
#include "screen.h"
#include "common.h"

#include <SDL/SDL.h>
#include <assert.h>

static struct {
	struct images *images;
	int offset;
	Uint32 whiteIndex;
	Uint32 blackIndex;

} Ascii;

void asciiSetImages(struct images *images, int offset)
{
	assert(images->w == ASCII_W);
	assert(images->h == ASCII_H);
	assert(offset <= ((images->cols * images->rows) - (128 - ' ')));

	Ascii.images = images;
	Ascii.offset = offset;
}

void asciiPaint(char c, int x, int y, SDL_Surface * surf)
{
	SDL_Rect dest;
	SDL_Rect src;
	int row;
	int col;

	assert(Ascii.images);

	if (c == '\t')
		c = ' ';

	assert(c >= ' ');

	/* fixme -- put these calcs in a table or something. Don't need to do
	 * it every time. */

	c = c - ' ' + Ascii.offset;

	col = c % Ascii.images->cols;
	row = c / Ascii.images->cols;

	src.x = (col * ASCII_W) + Ascii.images->offx;
	src.y = (row * ASCII_H) + Ascii.images->offy;
	src.w = ASCII_W;
	src.h = ASCII_H;

	dest.x = x;
	dest.y = y;
	dest.w = ASCII_W;
	dest.h = ASCII_H;

	SDL_BlitSurface(Ascii.images->images, &src, surf, &dest);
}

void asciiInvert(void)
{
	int wIndex, bIndex;

	/* Get the index of white */
	wIndex = SDL_MapRGB(Ascii.images->images->format, 0xff, 0xff, 0xff);

	/* Get the index of black */
	bIndex = SDL_MapRGB(Ascii.images->images->format, 0x00, 0x00, 0x00);

	/* Set the white index to black */
	SDL_SetPalette(Ascii.images->images, SDL_LOGPAL, &fontBlack, wIndex, 1);

	/* Set the black index to white */
	SDL_SetPalette(Ascii.images->images, SDL_LOGPAL, &fontWhite, bIndex, 1);

}

void asciiUninvert(void)
{
	int wIndex, bIndex;

	/* Get the index of white */
	wIndex = SDL_MapRGB(Ascii.images->images->format, 0xff, 0xff, 0xff);

	/* Get the index of black */
	bIndex = SDL_MapRGB(Ascii.images->images->format, 0x00, 0x00, 0x00);

	/* Set the white index to white */
	SDL_SetPalette(Ascii.images->images, SDL_LOGPAL, &fontWhite, wIndex, 1);

	/* Set the black index to black */
	SDL_SetPalette(Ascii.images->images, SDL_LOGPAL, &fontBlack, bIndex, 1);

}
