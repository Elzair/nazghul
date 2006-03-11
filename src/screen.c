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
#include "screen.h"
#include "common.h"
#include "ascii.h"
#include "sprite.h"
#include "status.h"
#include "foogod.h"
#include "session.h"

#include <unistd.h>
#include <stdarg.h>
#include <assert.h>

#include <SDL_image.h>

#define N_SHADERS 3
#define MAX_SHADER (N_SHADERS - 1)
#define SHADER_W STAT_W
#define SHADER_H STAT_H_MAX
#define HIGHLIGHT_THICKNESS 2

static SDL_Surface *Screen;
static SDL_Surface *Shaders[N_SHADERS];

// ---------------------------------------------------------
// The Highlight surface is used to highlight specific tiles.
// ---------------------------------------------------------

static SDL_Surface *Highlight;

static int Zoom;

Uint32 Black;
Uint32 Blue;
Uint32 White;
Uint32 Green;
Uint32 Red;
Uint32 Yellow;

SDL_Color fontWhite = { 0xff, 0xff, 0xff, 0x00 };
SDL_Color fontBlack = { 0, 0, 0, 0 };

void screenInitColors(void)
{

	Black = SDL_MapRGB(Screen->format, 0, 0, 0);
	Blue = SDL_MapRGB(Screen->format, 0, 0, 0xff);
	White = SDL_MapRGB(Screen->format, 0xff, 0xff, 0xff);
	Green = SDL_MapRGB(Screen->format, 0x00, 0xff, 0x00);
	Red = SDL_MapRGB(Screen->format, 0xff, 0x00, 0x00);
        Yellow = SDL_MapRGB(Screen->format, 0xff, 0xff, 0x00);
}

void screenInitScreen(void)
{
	Uint32 flags = 0;
	const SDL_VideoInfo *fmt;

	if (SDL_Init(SDL_INIT_VIDEO) < 0) {
		perror_sdl("SDL_Init");
		exit(-1);
	}
	atexit(SDL_Quit);

	fmt = SDL_GetVideoInfo();
	if (!fmt) {
		perror_sdl("SDL_GetVideoInfo");
		exit(-1);
	}

	if (fmt->blit_hw_CC &&
	    fmt->blit_fill &&
	    ((fmt->video_mem * 1024) >
	     (Uint32) (SCREEN_W * SCREEN_H * SCREEN_BPP / 8))) {
		flags |= SDL_HWSURFACE;
		flags |= SDL_DOUBLEBUF;
	}

	Screen = SDL_SetVideoMode(SCREEN_W, SCREEN_H, SCREEN_BPP, flags);
	if (!Screen) {
		perror_sdl("SDL_SetVideoMode");
		exit(-1);
	}
	// printf("Set video mode: [%dx%d] %d bpp (%s)\n", 
	// Screen->w, Screen->h,
	// Screen->format->BitsPerPixel, 
	// flags & SDL_HWSURFACE ? "hw" : "sw");

	SDL_WM_SetCaption(APPLICATION_NAME, APPLICATION_NAME);
}

void screen_fade_surface(SDL_Surface * surf, int fade_level)
{
	int x, y;
	Uint8 *pix;
	Uint8 trans;
	int base;
	int toggle;

	assert(surf->format->BitsPerPixel == 8);

	if (fade_level == 0)
		return;

	pix = (Uint8 *) surf->pixels;
	trans = (Uint8) surf->format->colorkey;

	for (y = 0; y < surf->h; y++) {
		base = y * surf->pitch;
		toggle = y % 2;
		for (x = 0; x < surf->w; x++) {
			int i = base + x;
			if (toggle) {
				if (pix[i] != trans) {
					toggle = 0;
					pix[i] = trans;
				}
			} else if (pix[i] != trans) {
				toggle = 1;
			}
		}		// for (x)
	}			// for (y)

	assert(surf->format->BitsPerPixel == 8);

	screen_fade_surface(surf, fade_level - 1);
}

static SDL_Surface *create_shader(int fade_level)
{
	SDL_Surface *shader;

	shader = screenCreateSurface(SHADER_W, SHADER_H);
	if (shader == NULL)
		return NULL;

	SDL_FillRect(shader, NULL, SDL_MapRGBA(shader->format, 0, 0, 0, 0));

	if (shader->format->palette != NULL) {
		SDL_LockSurface(shader);
		screen_fade_surface(shader, fade_level);
		SDL_UnlockSurface(shader);
	}

	return shader;
}

void screenInitShader(void)
{
	int n, i;

	n = (Screen->format->BitsPerPixel == 8) ? N_SHADERS : 1;

	for (i = 0; i < n; i++) {
		Shaders[i] = create_shader(i);
	}
}

void screenInitHighlight(void)
{
	Highlight = screenCreateSurface(SHADER_W, SHADER_H);
        assert(Highlight != NULL);

	SDL_FillRect(Highlight, NULL, SDL_MapRGBA(Highlight->format, 255, 255, 255, 0));

	if (Highlight->format->palette != NULL) {
		SDL_LockSurface(Highlight);
		screen_fade_surface(Highlight, 4);
		SDL_UnlockSurface(Highlight);
	}
}

int screenInit(void)
{
	screenInitScreen();
	screenInitColors();
	screenInitShader();
        screenInitHighlight();
	Zoom = 1;

	SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY,
			    SDL_DEFAULT_REPEAT_INTERVAL);

        return 0;
}

void screenErase(SDL_Rect * rect)
{
	SDL_FillRect(Screen, rect, Black);
}

void screenFill(SDL_Rect * rect, Uint32 color)
{
	SDL_Rect tmp = *rect;
	rect->w /= Zoom;
	rect->h /= Zoom;
	SDL_FillRect(Screen, rect, color);
}

void screenUpdate(SDL_Rect * rect)
{
	if (rect) {
		SDL_UpdateRect(Screen, rect->x, rect->y, rect->w, rect->h);
	} else {
		SDL_Flip(Screen);
	}
}

/* bpp-independent macro to test if a pixel is magenta */
#define isTransparent(ff,pp) \
        (((ff)->Rmask&(pp))==(ff)->Rmask \
          && ((ff)->Gmask&(pp))==0 \
          && ((ff)->Bmask&(pp))==(ff)->Bmask)

static void scaled_blit_32bpp(SDL_Surface * source, SDL_Rect * from,
			      SDL_Surface * dest, SDL_Rect * to,
			      int spitch, int dpitch)
{
	int dx, dy, di, sx, sy, si;
	Uint32 *d, *s;

	d = (Uint32 *) dest->pixels;
	s = (Uint32 *) source->pixels;

	for (dy = 0; dy < to->h; dy++) {
		sy = dy * Zoom;
		for (dx = 0; dx < to->w; dx++) {
			sx = dx * Zoom;
			di = (dy + to->y) * dpitch + (dx + to->x);
			si = (sy + from->y) * spitch + (sx + from->x);
                        if (!isTransparent(dest->format, s[si]))
                                d[di] = s[si];
		}		// for (dx)
	}			// for (dy)
}

static void scaled_blit_16bpp(SDL_Surface * source, SDL_Rect * from,
			      SDL_Surface * dest, SDL_Rect * to,
			      int spitch, int dpitch)
{
	int dx, dy, di, sx, sy, si;
	Uint16 *d, *s;

	d = (Uint16 *) dest->pixels;
	s = (Uint16 *) source->pixels;

	for (dy = 0; dy < to->h; dy++) {
		sy = dy * Zoom;
		for (dx = 0; dx < to->w; dx++) {
			sx = dx * Zoom;
			di = (dy + to->y) * dpitch + (dx + to->x);
			si = (sy + from->y) * spitch + (sx + from->x);
                        if (! isTransparent(dest->format, s[si]))
                                d[di] = s[si];
		}		// for (dx)
	}			// for (dy)
}

static void scaled_blit_8bpp(SDL_Surface * source, SDL_Rect * from,
			     SDL_Surface * dest, SDL_Rect * to,
			     int spitch, int dpitch)
{
	int dx, dy, di, sx, sy, si;
	Uint8 *d, *s;

	d = (Uint8 *) dest->pixels;
	s = (Uint8 *) source->pixels;

	for (dy = 0; dy < to->h; dy++) {
		sy = dy * Zoom;
		for (dx = 0; dx < to->w; dx++) {
			sx = dx * Zoom;
			di = (dy + to->y) * dpitch + (dx + to->x);
			si = (sy + from->y) * spitch + (sx + from->x);
			d[di] = s[si];
		}		// for (dx)
	}			// for (dy)
}

static void scaled_blit(SDL_Surface * source, SDL_Rect * from,
			SDL_Surface * dest, SDL_Rect * to)
{
	int dpitch, spitch;

	// This is not a general-purpose blitting routine, and I assume that
	// the source and destination surfaces are compatible.

	assert(source->format->BitsPerPixel == dest->format->BitsPerPixel);
	assert(Zoom > 0);

	to->w /= Zoom;
	to->h /= Zoom;

	dpitch = dest->pitch / dest->format->BytesPerPixel;
	spitch = source->pitch / source->format->BytesPerPixel;

	if (SDL_LockSurface(dest) < 0)
		return;

	switch (dest->format->BitsPerPixel) {
	case 32:
		scaled_blit_32bpp(source, from, dest, to, spitch, dpitch);
		break;
	case 16:
		scaled_blit_16bpp(source, from, dest, to, spitch, dpitch);
		break;
	case 8:
		scaled_blit_8bpp(source, from, dest, to, spitch, dpitch);
		break;
	default:
		assert(0);
		break;
	}

	SDL_UnlockSurface(dest);
}

void screenBlit(SDL_Surface * source, SDL_Rect * from, SDL_Rect * to)
{
	/* Clipping is really only needed for wave sprites right now. If the
	 * following proves to be too expensive on slow machines... */
	if (to) {
		SDL_Rect _to = *to;
		SDL_SetClipRect(Screen, &_to);
		if (Zoom > 1) {

                        // Clients are allowed to pass a NULL from rect,
                        // indicating they want to blit the whole source
                        // area. But the scaled blits require a non-NULL
                        // from rect.
                        SDL_Rect _from;
                        if (from == NULL) {
                                _from.x = 0;
                                _from.y = 0;
                                _from.w = source->w;
                                _from.h = source->h;
                                from = &_from;
                        }

			scaled_blit(source, from, Screen, &_to);
                } else {
			if (SDL_BlitSurface(source, from, Screen, &_to) < 0)
				perror_sdl("SDL_BlitSurface");
		}
		SDL_SetClipRect(Screen, 0);
	} else {
		SDL_SetClipRect(Screen, to);
		if (SDL_BlitSurface(source, from, Screen, NULL) < 0)
			perror_sdl("SDL_BlitSurface");
		SDL_SetClipRect(Screen, 0);
	}
}

int screenWidth(void)
{
	return Screen->w;
}

int screenHeight(void)
{
	return Screen->h;
}

void screenFlash(SDL_Rect * rect, int mdelay, Uint32 color)
{
	screenFill(rect, color);
	screenUpdate(rect);
	//usleep(mdelay * 1000);
        SDL_Delay(mdelay);
}

void screenPrint(SDL_Rect * rect, int flags, char *fmt, ...)
{
	va_list args;
	char buf[128];
	int i;
	int x = rect->x;
	int y = rect->y;
	int len;

        flags |= SP_INVERTED; // dbg hack

	va_start(args, fmt);
	vsnprintf(buf, sizeof(buf), fmt, args);
	va_end(args);

	len = strlen(buf);
	if (len > rect->w)
		// fixme: shouldn't it be len * ASCII_W?
		len = rect->w;

	// If painting on the border then first fill the line with the border
	// image.
	if (flags & SP_ONBORDER) {
		for (x = rect->x; x < rect->x + rect->w; x += BORDER_W)
			spritePaint(Session->frame.horz, 0, x, rect->y);
	}

	if (flags & SP_CENTERED) {
		int w = len * ASCII_W;
		if (w > rect->w) {
			w = rect->w;
			len = w / ASCII_W;
		}
		x = (rect->w - w) / 2 + rect->x;
	} else if (flags & SP_RIGHTJUSTIFIED) {
		int w = len * ASCII_W;
		if (w > rect->w) {
			w = rect->w;
			len = w / ASCII_W;
		}
		x = (rect->w - w) + rect->x;
	}

	if (flags & SP_INVERTED)
		asciiInvert();

	// If painting on the border, then paint the right stub 
	// to the left of the text.
	if (flags & SP_ONBORDER)
		spritePaint(Session->frame.endr, 0, x - BORDER_W, rect->y);

	for (i = 0; i < len; i++) {
		asciiPaint(buf[i], x, y, Screen);
		x += ASCII_W;
	}

	// If painting on the border, then paint the left stub 
	// to the right of the text.
	if (flags & SP_ONBORDER)
		spritePaint(Session->frame.endl, 0, x, rect->y);

	if (flags & SP_INVERTED)
		asciiInvert();
}

void screen_repaint_frame(void)
{
	int i;

#ifdef MAP_LEFT

	// First draw the top and bottom horizontal bars. Leave gaps for the
	// sky and wind windows. Originally I went ahead and painted over them
	// here, relying on their update routines to black out their
	// backgrounds. But when I started using the tall/short mode for the
	// status window I found that this was no longer good enough. The
	// backgrounds of these windows tended to flash when switching mode.

	// Draw the top bar from the top left corner to the sky window.
	for (i = 0; i < SKY_X - BORDER_W; i += BORDER_W)
		spritePaint(Session->frame.horz, 0, i, 0);

	// Draw the top bar from the sky window to the left edge of the status
	// window's title.
	for (i = SKY_X + SKY_W + BORDER_W; i < STAT_X; i += BORDER_W)
		spritePaint(Session->frame.horz, 0, i, 0);

	// Draw the bottom of the map from the left edge to the wind window.
	for (i = 0; i < (int) (WIND_X - BORDER_W); i += BORDER_W)
		spritePaint(Session->frame.horz, 0, i, MAP_X + MAP_H);

	// Draw the bottom of the map from the wind window to the left edge of
	// the console window.
	for (i = WIND_X + WIND_W + BORDER_W; i < CONS_X - BORDER_W;
	     i += BORDER_W)
		spritePaint(Session->frame.horz, 0, i, MAP_X + MAP_H);

	// Draw the bar across the bottom of the screen.
	for (i = 0; i < SCREEN_W; i += BORDER_W)
		spritePaint(Session->frame.horz, 0, i, SCREEN_H - BORDER_H);

	// Next draw the bottom of the status and food/gold window.
	for (i = (MAP_X + MAP_W); i < SCREEN_W; i += BORDER_W) {
		spritePaint(Session->frame.horz, 0, i, STAT_Y + status_get_h());
		spritePaint(Session->frame.horz, 0, i,
			    foogod_get_y() + FOOGOD_H);
	}

	// Next rough in all the vertical lines.
	for (i = 0; i < SCREEN_H; i += BORDER_H) {
		spritePaint(Session->frame.vert, 0, 0, i);
		spritePaint(Session->frame.vert, 0, MAP_X + MAP_W, i);
		spritePaint(Session->frame.vert, 0, SCREEN_W - BORDER_W, i);
	}

	// Now paint the four corner pieces
	spritePaint(Session->frame.ulc, 0, 0, 0);
	spritePaint(Session->frame.urc, 0, SCREEN_W - BORDER_W, 0);
	spritePaint(Session->frame.llc, 0, 0, SCREEN_H - BORDER_H);
	spritePaint(Session->frame.lrc, 0, SCREEN_W - BORDER_W,
		    SCREEN_H - BORDER_H);

	// Then all the right-facing tee-joints
	spritePaint(Session->frame.tr, 0, 0, MAP_Y + MAP_H);
	spritePaint(Session->frame.tr, 0, MAP_X + MAP_W,
		    STAT_Y + status_get_h());
	spritePaint(Session->frame.tr, 0, MAP_X + MAP_W,
		    foogod_get_y() + FOOGOD_H);

	// Then all the left-facing tee-joints
	spritePaint(Session->frame.tl, 0, MAP_X + MAP_W, MAP_Y + MAP_H);
	spritePaint(Session->frame.tl, 0, SCREEN_W - BORDER_W,
		    STAT_Y + status_get_h());
	spritePaint(Session->frame.tl, 0, SCREEN_W - BORDER_W,
		    foogod_get_y() + FOOGOD_H);

	// Then the downward and upward-facing tee-joints
	spritePaint(Session->frame.td, 0, MAP_X + MAP_W, 0);
	spritePaint(Session->frame.tu, 0, MAP_X + MAP_W, SCREEN_H - BORDER_H);

	// And then the stubs around the sky section
	spritePaint(Session->frame.endr, 0, SKY_X - BORDER_W, 0);
	spritePaint(Session->frame.endl, 0, SKY_X + SKY_W, 0);

	// And finally stubs around the wind section
	spritePaint(Session->frame.endr, 0, WIND_X - BORDER_W, MAP_X + MAP_H);
	spritePaint(Session->frame.endl, 0, WIND_X + WIND_W, MAP_X + MAP_H);

        // And some stubs around the status title section
	spritePaint(Session->frame.endr, 0, STAT_X, 0);
	spritePaint(Session->frame.endl, 0, STAT_X + STAT_W - BORDER_W,   0);

	screenUpdate(0);

#else /* ! MAP_LEFT */
	// First draw the top and bottom horizontal bars. Leave gaps for the
	// sky and wind windows. Originally I went ahead and painted over them
	// here, relying on their update routines to black out their
	// backgrounds. But when I started using the tall/short mode for the
	// status window I found that this was no longer good enough. The
	// backgrounds of these windows tended to flash when switching mode.

        // Bar across the top to sky window
        for (i = 0; i < (SKY_X - BORDER_W); i += BORDER_W)
		spritePaint(Session->frame.horz, 0, i, 0);

	// Bar from sky window to right edge
	for (i = SKY_X + SKY_W + BORDER_W; i < SCREEN_W; i += BORDER_W)
		spritePaint(Session->frame.horz, 0, i, 0);

	// Draw the bottom of the map from the left edge to the wind window.
	for (i = MAP_X; i < (int) (WIND_X - BORDER_W); i += BORDER_W)
		spritePaint(Session->frame.horz, 0, i, MAP_Y + MAP_H);

	// Draw the bottom of the map from the wind window to screen edge
	for (i = WIND_X + WIND_W + BORDER_W; i < SCREEN_W; i += BORDER_W)
		spritePaint(Session->frame.horz, 0, i, MAP_Y + MAP_H);

	// Draw the bar across the bottom of the screen.
	for (i = 0; i < SCREEN_W; i += BORDER_W)
		spritePaint(Session->frame.horz, 0, i, SCREEN_H - BORDER_H);

	// Next draw the bottom of the status and food/gold window.
	for (i = STAT_X; i < (STAT_W + BORDER_W); i += BORDER_W) {
		spritePaint(Session->frame.horz, 0, i, STAT_Y + 
                            status_get_h());
		spritePaint(Session->frame.horz, 0, i,
			    foogod_get_y() + FOOGOD_H);
	}

	// Next rough in all the vertical lines.
	for (i = 0; i < SCREEN_H; i += BORDER_H) {
		spritePaint(Session->frame.vert, 0, 0, i);
		spritePaint(Session->frame.vert, 0, MAP_X - BORDER_W, i);
		spritePaint(Session->frame.vert, 0, SCREEN_W - BORDER_W, i);
	}

	// Now paint the four corner pieces
	spritePaint(Session->frame.ulc, 0, 0, 0);
	spritePaint(Session->frame.urc, 0, SCREEN_W - BORDER_W, 0);
	spritePaint(Session->frame.llc, 0, 0, SCREEN_H - BORDER_H);
	spritePaint(Session->frame.lrc, 0, SCREEN_W - BORDER_W,
		    SCREEN_H - BORDER_H);

	// Then all the right-facing tee-joints
	spritePaint(Session->frame.tr, 0, 0, MAP_Y + MAP_H);
	spritePaint(Session->frame.tr, 0, MAP_X + MAP_W,
		    STAT_Y + status_get_h());
	spritePaint(Session->frame.tr, 0, MAP_X + MAP_W,
		    foogod_get_y() + FOOGOD_H);

	// Then all the left-facing tee-joints
	spritePaint(Session->frame.tl, 0, MAP_X + MAP_W, MAP_Y + MAP_H);
	spritePaint(Session->frame.tl, 0, SCREEN_W - BORDER_W,
		    STAT_Y + status_get_h());
	spritePaint(Session->frame.tl, 0, SCREEN_W - BORDER_W,
		    foogod_get_y() + FOOGOD_H);

	// Then the downward and upward-facing tee-joints
	spritePaint(Session->frame.td, 0, MAP_X + MAP_W, 0);
	spritePaint(Session->frame.tu, 0, MAP_X + MAP_W, SCREEN_H - BORDER_H);

	// And then the stubs around the sky section
	spritePaint(Session->frame.endr, 0, SKY_X - BORDER_W, 0);
	spritePaint(Session->frame.endl, 0, SKY_X + SKY_W, 0);

	// And finally stubs around the wind section
	spritePaint(Session->frame.endr, 0, WIND_X - BORDER_W, MAP_X + MAP_H);
	spritePaint(Session->frame.endl, 0, WIND_X + WIND_W, MAP_X + MAP_H);

	screenUpdate(0);
#endif /* ! MAP_LEFT */
}

SDL_Surface *screenCreateSurface(int w, int h)
{
	SDL_Surface *surf = NULL, *tmp;

	tmp = SDL_CreateRGBSurface(Screen->flags,
				   w, h,
				   Screen->format->BitsPerPixel,
				   Screen->format->Rmask,
				   Screen->format->Gmask,
				   Screen->format->Bmask,
				   Screen->format->Amask);

	// surf->format->palette = Screen->format->palette;

	if (tmp == NULL) {
		perror_sdl("SDL_CreateRGBSurface");
		return NULL;
	}

	surf = SDL_DisplayFormat(tmp);
	SDL_FreeSurface(tmp);

	if (surf == NULL) {
		perror_sdl("SDL_DisplayFormat");
		return NULL;
	}

	if (surf->format->palette) {
		SDL_SetColorKey(surf, SDL_SRCCOLORKEY,
				SDL_MapRGB(surf->format, 0xFF, 0x00, 0xFF));
	}

	return surf;
}

void screenCopy(SDL_Rect * from, SDL_Rect * to, SDL_Surface * dest)
{
	if (SDL_BlitSurface(Screen, from, dest, to) < 0)
		perror_sdl("SDL_BlitSurface");

        assert(from);
        assert(dest);

        SDL_Rect _from = *from;

        if (Zoom > 1) {
                // Clients are allowed to pass a NULL 'to' rect,
                // indicating they want to blit the whole dest
                // area. But the scaled blits require a non-NULL
                // 'to' rect.
                SDL_Rect _to;
                if (to == NULL) {
                        _to.x = 0;
                        _to.y = 0;
                        _to.w = dest->w;
                        _to.h = dest->h;
                        to = &_to;
                }
                scaled_blit(Screen, &_from, dest, to);
        } else {
                if (SDL_BlitSurface(Screen, &_from, dest, to) < 0)
                        perror_sdl("SDL_BlitSurface");
        }
}

void screenShade(SDL_Rect * area, unsigned char amount)
{
	SDL_Surface *shade;

	assert(area->w <= SHADER_W);
	assert(area->h <= SHADER_H);

	if (amount == 0)
		return;

	if (Screen->format->BitsPerPixel == 8) {
		shade = Shaders[MAX_SHADER - (amount * MAX_SHADER) / 255];
	} else {
		shade = Shaders[0];
		SDL_SetAlpha(shade, SDL_SRCALPHA, amount);
	}
	screenBlit(shade, NULL, area);
}

void screenHighlightColored(SDL_Rect * area, Uint32 color)
{
        SDL_Rect edge;

        // ---------------------------------------------------------------------
        // Top edge
        // ---------------------------------------------------------------------

        edge.x = area->x;
        edge.y = area->y;
        edge.w = area->w;
        edge.h = HIGHLIGHT_THICKNESS;

        screenFill(&edge, color);

        // ---------------------------------------------------------------------
        // Bottom edge
        // ---------------------------------------------------------------------

        edge.x = area->x;
        edge.y = area->y + (area->h/Zoom) - HIGHLIGHT_THICKNESS;
        edge.w = area->w;
        edge.h = HIGHLIGHT_THICKNESS;

        screenFill(&edge, color);

        // ---------------------------------------------------------------------
        // Left edge
        // ---------------------------------------------------------------------

        edge.x = area->x;
        edge.y = area->y;
        edge.w = HIGHLIGHT_THICKNESS;
        edge.h = area->h;

        screenFill(&edge, color);

        // ---------------------------------------------------------------------
        // Right edge
        // ---------------------------------------------------------------------

        edge.x = area->x + (area->w/Zoom) - HIGHLIGHT_THICKNESS;
        edge.y = area->y;
        edge.w = HIGHLIGHT_THICKNESS;
        edge.h = area->h;

        screenFill(&edge, color);
}

void screenHighlight(SDL_Rect *area)
{
        screenHighlightColored(area, White);
}

int screenLock()
{
	return SDL_LockSurface(Screen);
}

void screenUnlock()
{
	SDL_UnlockSurface(Screen);
}

void screenSetPixel(int x, int y, Uint32 color)
{
	Uint32 *pix;

	assert(Screen->format->BitsPerPixel == 32);
	pix = (Uint32 *) (Screen->pixels);
	pix += (y * Screen->pitch) / Screen->format->BytesPerPixel + x;

#if 0
	red = (color >> 16) & 0xFF;
	grn = (color >> 8) & 0xFF;
	blu = color & 0xFF;

	red >>= Screen->format->Rloss;
	grn >>= Screen->format->Gloss;
	blu >>= Screen->format->Bloss;

	red &= (Screen->format->Rmask >> Screen->format->Rshift);
	grn &= (Screen->format->Gmask >> Screen->format->Gshift);
	blu &= (Screen->format->Bmask >> Screen->format->Bshift);

	red <<= Screen->format->Rshift;
	grn <<= Screen->format->Gshift;
	blu <<= Screen->format->Bshift;

	*pix = red | grn | blu;
#else
	*pix = color;
#endif				// 0

}

Uint32 screenMapRGB(Uint8 red, Uint8 grn, Uint8 blu)
{
	return SDL_MapRGB(Screen->format, red, grn, blu);
}

void screenZoomOut(int factor)
{
	if (factor)
		Zoom *= factor;
}

void screenZoomIn(int factor)
{
	if (factor)
		Zoom /= factor;
}
