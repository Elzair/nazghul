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
#ifndef screen_h
#define screen_h

#ifdef __cplusplus
extern "C" {
#endif

#include <SDL.h>

#define MAX_SHADE 0
#define MIN_SHADE 255

        extern Uint32 Blue;
        extern Uint32 Black;
        extern Uint32 White;
        extern Uint32 Green;
        extern Uint32 Red;

        extern SDL_Color fontWhite;
        extern SDL_Color fontBlack;

#define SP_CENTERED (1 << 0)
#define SP_INVERTED (1 << 1)
#define SP_RIGHTJUSTIFIED (1 << 2)
#define SP_ONBORDER (1 << 4)

        extern int screenInit(void);
	extern void screenErase(SDL_Rect * rect);
	extern void screenUpdate(SDL_Rect * rect);
	extern void screenFill(SDL_Rect * rect, Uint32 color);
	extern void screenBlit(SDL_Surface * source, SDL_Rect * from,
			       SDL_Rect * to);
        extern int screenWidth(void);
        extern int screenHeight(void);
	extern void screenFlash(SDL_Rect * rect, int mdelay, Uint32 color);
	extern void screenPrint(SDL_Rect * rect, int flags, char *fmt, ...);

        // Added for missile animations
        extern SDL_Surface *screenCreateSurface(int w, int h);
	extern void screenCopy(SDL_Rect * from, SDL_Rect * to,
			       SDL_Surface * dest);
	extern void screenShade(SDL_Rect * area, unsigned char amount);
	extern void screenHighlight(SDL_Rect * area);

        // Added for peer effect
        extern int screenLock();
        extern void screenUnlock();
        extern void screenSetPixel(int x, int y, Uint32 color);
        extern Uint32 screenMapRGB(Uint8 red, Uint8 grn, Uint8 blu);
        extern void screenZoomOut(int factor);
        extern void screenZoomIn(int factor);

	extern int screenLoadFrame(class Loader * loader);
	extern void screen_fade_surface(SDL_Surface * surf, int transparency);
        extern void screen_repaint_frame(void);

#ifdef __cplusplus
}
#endif

#endif
