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
#include "cfg.h"
#include "dimensions.h"

#include <assert.h>

#define ASCII_DEF_CLR White /* default printing color is white */
#define ASCII_CLR_STK_SZ 32

/* State machine states for embedded control sequences. */
enum ascii_ctrl_states {
        ASCII_STATE_DEF = 0,
        ASCII_STATE_ESC,
        ASCII_STATE_CLR
};


static struct ascii {
        struct images *images;
        int offset;
        ascii_ctrl_states state;
        Uint32 color_stack[ASCII_CLR_STK_SZ];
        int i_color;  /* index onto stack */
        Uint32 color; /* active color */
} Ascii;

static Uint32  asciiDecodeColor(char clr)
{
        switch (clr) {
        case 'w': return White;
        case 'B': return Black;
        case 'r': return Red;
        case 'g': return Green;
        case 'b': return Blue;
        case 'y': return Yellow;
        case 'c': return Cyan;
        case 'm': return Magenta;
        case 'G': return Gray;
        default:
                warn("Color '%c' unknown\n", clr);
                return ASCII_DEF_CLR;
        }
}

static void asciiPushColor()
{
        /* Check for overrun. */
        if (array_sz(Ascii.color_stack) == Ascii.i_color) {
                warn("Ascii color stack overrun\n");
                return;
        }

        /* Push the new color. */
        Ascii.color_stack[Ascii.i_color] = Ascii.color;
        Ascii.i_color++;
}

static void asciiPopColor()
{
        /* Check for underrun. */
        if (0 == Ascii.i_color) {
                warn("Ascii color stack already at bottom\n");
                return;
        }
        
        Ascii.i_color--;
        Ascii.color = Ascii.color_stack[Ascii.i_color];
        return;
}

static void asciiSetColor(char clr)
{
        /* Check for a pop. */
        switch (clr) {
        case '+': 
                asciiPushColor();
                break;
        case '-':
                asciiPopColor();
                break;
        default:
                Ascii.color = asciiDecodeColor(clr);
                break;
        }
}

static void asciiBlitColored(SDL_Surface *dst, SDL_Rect *dstrect, 
                             SDL_Surface *src, SDL_Rect *srcrect,
                             Uint32 color)
{
        Uint16 mask = color;
        Uint16 *srcpix, *dstpix;
        int x=0, y=0;

        assert(dst->format->BitsPerPixel==src->format->BitsPerPixel);
        assert(dst->format->BitsPerPixel==16);
        assert(dstrect->w==srcrect->w);
        assert(dstrect->h==srcrect->h);

        for (y=0; y<dstrect->h; y++) {

                srcpix = (Uint16*)src->pixels 
                        + (srcrect->y+y)*src->w 
                        + srcrect->x;
                dstpix = (Uint16*)dst->pixels 
                        + (dstrect->y+y)*dst->w 
                        + dstrect->x;

                for (x=0; x<dstrect->w; x++) {
                        *dstpix = *srcpix&mask;
                        srcpix++;
                        dstpix++;
                }
        }
}

static void asciiPaintColored(char c, int x, int y, SDL_Surface *surf, Uint32 color)
{
	SDL_Rect dest;
	SDL_Rect src;
	int row;
	int col;

	assert(Ascii.images);

	if (c == '\t')
		c = ' ';

	assert(c >= ' ');

        if (c<' ') {
                warn("c==%d\n", c);
                c='?';
        }

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

        asciiBlitColored(surf, &dest, 
                         Ascii.images->images, &src, 
                         color);
}

static void asciiPaintDefault(char c, int x, int y, SDL_Surface * surf)
{
	SDL_Rect dest;
	SDL_Rect src;
	int row;
	int col;

	assert(Ascii.images);

	if (c == '\t')
		c = ' ';

	assert(c >= ' ');

        if (c<' ') {
                warn("c==%d\n", c);
                c='?';
        }

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

int asciiInit(void)
{
        char *fname = cfg_get("ascii-image-filename");
        assert(fname);
        Ascii.images = images_new(0, 8, 16, 8, 16, 0, 0, fname);
        assert(Ascii.images);
        Ascii.offset = 32;
        Ascii.state = ASCII_STATE_DEF;
        Ascii.i_color = 0;
        Ascii.color = ASCII_DEF_CLR;
        return 0;
}

int asciiPaint(char c, int x, int y, SDL_Surface * surf)
{
        int ret = 0;

        switch (Ascii.state) {
                        
        case ASCII_STATE_CLR:
                asciiSetColor(c);
                Ascii.state = ASCII_STATE_DEF;
                break;
                
        case ASCII_STATE_ESC:
                if (c == 'c') {
                        Ascii.state = ASCII_STATE_CLR;
                }
                break;
                
        case ASCII_STATE_DEF:
        default:
                if (c == '^') {
                        Ascii.state = ASCII_STATE_ESC;
                } else {
                        if (ASCII_DEF_CLR==Ascii.color) {
                                asciiPaintDefault(c, x, y, surf);
                        } else {
                                asciiPaintColored(c, x, y, surf, Ascii.color);
                        }
                        ret = 1;
                }
        }

        return ret;
}
