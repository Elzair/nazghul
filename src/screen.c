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
#include "Loader.h"
#include "status.h"
#include "foogod.h"

#include <unistd.h>
#include <stdarg.h>
#include <assert.h>

#include <SDL/SDL_image.h>

#define N_SHADERS 3
#define MAX_SHADER (N_SHADERS - 1)
#define SHADER_W STAT_W
#define SHADER_H STAT_H

static SDL_Surface *Screen;
static SDL_Surface *Shaders[N_SHADERS];
static int Zoom;

Uint32 Black;
Uint32 Blue;
Uint32 White;
Uint32 Green;
Uint32 Red;

SDL_Color fontWhite = { 0xff, 0xff, 0xff, 0x00 };
SDL_Color fontBlack = { 0, 0, 0, 0 };

static struct {
	int bound:1;
	struct sprite *ulc, *urc, *llc, *lrc, *td, *tu, *tl, *tr, *tx, *horz,
	    *vert, *endl, *endr;
} frame_sprites;

void screenInitColors(void)
{

	Black = SDL_MapRGB(Screen->format, 0, 0, 0);
	Blue = SDL_MapRGB(Screen->format, 0, 0, 0xff);
	White = SDL_MapRGB(Screen->format, 0xff, 0xff, 0xff);
	Green = SDL_MapRGB(Screen->format, 0x00, 0xff, 0x00);
	Red = SDL_MapRGB(Screen->format, 0xff, 0x00, 0x00);
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
#if 1
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
#else
	SDL_Surface *tmp;
	SDL_Surface *shader;

	tmp = SDL_CreateRGBSurface(SDL_SWSURFACE, SHADER_W, SHADER_H,
				   Screen->format->BitsPerPixel,
				   0x0000F000, 0x00000F00, 0x000000F0,
				   0x000000FF);
	if (!tmp) {
		perror_sdl("SDL_CreateRGBSurface");
		exit(-1);
	}

	shader = SDL_DisplayFormat(tmp);
	SDL_FreeSurface(tmp);

	if (shader->format->palette == NULL) {
		SDL_FillRect(shader, NULL,
			     SDL_MapRGBA(shader->format, 0, 0, 0, 0));
	} else {
		SDL_FillRect(shader, NULL, SDL_MapRGB(shader->format, 0, 0, 0));
		SDL_SetColorKey(shader, SDL_SRCCOLORKEY,
				SDL_MapRGB(shader->format, 0xFF, 0x00, 0xFF));
		SDL_LockSurface(shader);
		screen_fade_surface(shader, fade_level);
		SDL_UnlockSurface(shader);
	}

	return shader;
#endif				// 1
}

void screenInitShader(void)
{
	int n, i;

	n = (Screen->format->BitsPerPixel == 8) ? N_SHADERS : 1;

	for (i = 0; i < n; i++) {
		Shaders[i] = create_shader(i);
	}
}

void screenInit(void)
{
	screenInitScreen();
	screenInitColors();
	screenInitShader();
	Zoom = 1;

	SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY,
			    SDL_DEFAULT_REPEAT_INTERVAL);
	memset(&frame_sprites, 0, sizeof(frame_sprites));
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

static void scaled_blit_32bpp(SDL_Surface * source, SDL_Rect * from,
			      SDL_Surface * dest, SDL_Rect * to,
			      int spitch, int dpitch)
{
	int dx, dy, di, sx, sy, si;
	Uint32 *d, *s;

	d = (Uint32 *) dest->pixels;
	s = (Uint32 *) source->pixels;

	for (dy = 0; dy < to->w; dy++) {
		sy = dy * Zoom;
		for (dx = 0; dx < to->h; dx++) {
			sx = dx * Zoom;
			di = (dy + to->y) * dpitch + (dx + to->x);
			si = (sy + from->y) * spitch + (sx + from->x);
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

	for (dy = 0; dy < to->w; dy++) {
		sy = dy * Zoom;
		for (dx = 0; dx < to->h; dx++) {
			sx = dx * Zoom;
			di = (dy + to->y) * dpitch + (dx + to->x);
			si = (sy + from->y) * spitch + (sx + from->x);
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

	for (dy = 0; dy < to->w; dy++) {
		sy = dy * Zoom;
		for (dx = 0; dx < to->h; dx++) {
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
		SDL_Rect tmp = *to;
		SDL_SetClipRect(Screen, &tmp);
		if (Zoom > 1)
			scaled_blit(source, from, Screen, &tmp);
		else {
			if (SDL_BlitSurface(source, from, Screen, &tmp) < 0)
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
	usleep(mdelay * 1000);
}

void screenPrint(SDL_Rect * rect, int flags, char *fmt, ...)
{
	va_list args;
	char buf[128];
	int i;
	int x = rect->x;
	int y = rect->y;
	int len;

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
			spritePaint(frame_sprites.horz, 0, x, rect->y);
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
		spritePaint(frame_sprites.endr, 0, x - BORDER_W, rect->y);

	for (i = 0; i < len; i++) {
		asciiPaint(buf[i], x, y, Screen);
		x += ASCII_W;
	}

	// If painting on the border, then paint the left stub 
	// to the right of the text.
	if (flags & SP_ONBORDER)
		spritePaint(frame_sprites.endl, 0, x, rect->y);

	if (flags & SP_INVERTED)
		asciiInvert();
}

void screen_repaint_frame(void)
{
	int i;

	// First draw the top and bottom horizontal bars. Leave gaps for the
	// sky and wind windows. Originally I went ahead and painted over them
	// here, relying on their update routines to black out their
	// backgrounds. But when I started using the tall/short mode for the
	// status window I found that this was no longer good enough. The
	// backgrounds of these windows tended to flash when switching mode.

	// Draw the top bar from the top left corner to the sky window.
	for (i = 0; i < SKY_X - BORDER_W; i += BORDER_W)
		spritePaint(frame_sprites.horz, 0, i, 0);

	// Draw the top bar from the sky window to the left edge of the status
	// window's title.
#if 1
	for (i = SKY_X + SKY_W + BORDER_W; i < STAT_X; i += BORDER_W)
		spritePaint(frame_sprites.horz, 0, i, 0);
#else
	for (i = SKY_X + SKY_W + BORDER_W; i < SCREEN_W; i += BORDER_W)
		spritePaint(frame_sprites.horz, 0, i, 0);
#endif				// 1

	// Draw the bottom of the map from the left edge to the wind window.
	for (i = 0; i < (int) (WIND_X - BORDER_W); i += BORDER_W)
		spritePaint(frame_sprites.horz, 0, i, MAP_X + MAP_H);

	// Draw the bottom of the map from the wind window to the left edge of
	// the console window.
	for (i = WIND_X + WIND_W + BORDER_W; i < CONS_X - BORDER_W;
	     i += BORDER_W)
		spritePaint(frame_sprites.horz, 0, i, MAP_X + MAP_H);

	// Draw the bar across the bottom of the screen.
	for (i = 0; i < SCREEN_W; i += BORDER_W)
		spritePaint(frame_sprites.horz, 0, i, SCREEN_H - BORDER_H);

	// Next draw the bottom of the status and food/gold window.
	for (i = (MAP_X + MAP_W); i < SCREEN_W; i += BORDER_W) {
		spritePaint(frame_sprites.horz, 0, i, STAT_Y + status_get_h());
		spritePaint(frame_sprites.horz, 0, i,
			    foogod_get_y() + FOOGOD_H);
	}

	// Next rough in all the vertical lines.
	for (i = 0; i < SCREEN_H; i += BORDER_H) {
		spritePaint(frame_sprites.vert, 0, 0, i);
		spritePaint(frame_sprites.vert, 0, MAP_X + MAP_W, i);
		spritePaint(frame_sprites.vert, 0, SCREEN_W - BORDER_W, i);
	}

	// Now paint the four corner pieces
	spritePaint(frame_sprites.ulc, 0, 0, 0);
	spritePaint(frame_sprites.urc, 0, SCREEN_W - BORDER_W, 0);
	spritePaint(frame_sprites.llc, 0, 0, SCREEN_H - BORDER_H);
	spritePaint(frame_sprites.lrc, 0, SCREEN_W - BORDER_W,
		    SCREEN_H - BORDER_H);

	// Then all the right-facing tee-joints
	spritePaint(frame_sprites.tr, 0, 0, MAP_Y + MAP_H);
	spritePaint(frame_sprites.tr, 0, MAP_X + MAP_W,
		    STAT_Y + status_get_h());
	spritePaint(frame_sprites.tr, 0, MAP_X + MAP_W,
		    foogod_get_y() + FOOGOD_H);

	// Then all the left-facing tee-joints
	spritePaint(frame_sprites.tl, 0, MAP_X + MAP_W, MAP_Y + MAP_H);
	spritePaint(frame_sprites.tl, 0, SCREEN_W - BORDER_W,
		    STAT_Y + status_get_h());
	spritePaint(frame_sprites.tl, 0, SCREEN_W - BORDER_W,
		    foogod_get_y() + FOOGOD_H);

	// Then the downward and upward-facing tee-joints
	spritePaint(frame_sprites.td, 0, MAP_X + MAP_W, 0);
	spritePaint(frame_sprites.tu, 0, MAP_X + MAP_W, SCREEN_H - BORDER_H);

	// And then the stubs around the sky section
	spritePaint(frame_sprites.endr, 0, SKY_X - BORDER_W, 0);
	spritePaint(frame_sprites.endl, 0, SKY_X + SKY_W, 0);

	// And finally stubs around the wind section
	spritePaint(frame_sprites.endr, 0, WIND_X - BORDER_W, MAP_X + MAP_H);
	spritePaint(frame_sprites.endl, 0, WIND_X + WIND_W, MAP_X + MAP_H);

	screenUpdate(0);
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

int screenLoadFrame(class Loader * loader)
{
	char *tags[] = {
		"ulc", "llc", "urc", "lrc",
		"horz", "vert",
		"td", "tu", "tr", "tl", "tx",
		"endl", "endr"
	};
	struct sprite **sprites[] = {
		&frame_sprites.ulc,
		&frame_sprites.llc,
		&frame_sprites.urc,
		&frame_sprites.lrc,
		&frame_sprites.horz,
		&frame_sprites.vert,
		&frame_sprites.td,
		&frame_sprites.tu,
		&frame_sprites.tr,
		&frame_sprites.tl,
		&frame_sprites.tx,
		&frame_sprites.endl,
		&frame_sprites.endr
	};
	unsigned int i;
	char *tag;

	if (!loader->matchToken('{'))
		return -1;

	for (i = 0; i < array_sz(tags); i++) {
		if (!loader->matchWord(tags[i]) || !loader->getWord(&tag))
			return -1;
		*sprites[i] =
		    (struct sprite *) loader->lookupTag(tag, SPRITE_ID);
		if (*sprites[i] == NULL) {
			loader->setError("Error in FRAME: invalid SPRITE tag "
					 "%s", tag);
			return -1;
		}
	}

	return 0;
}
