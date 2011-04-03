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

#include "glyph.h"
#include "macros.h"

BEGIN_DECL

#include <SDL.h>

#define MAX_SHADE 0
#define MIN_SHADE 255

extern Uint32 Blue;
extern Uint32 Black;
extern Uint32 White;
extern Uint32 Green;
extern Uint32 Red;
extern Uint32 Yellow;
extern Uint32 Cyan;
extern Uint32 Magenta;
extern Uint32 Gray;

extern Uint32 TextRed;
extern Uint32 TextGreen;
extern Uint32 TextBlue;
extern Uint32 TextYellow;
extern Uint32 TextCyan;
extern Uint32 TextMagenta;

extern SDL_Color fontWhite;
extern SDL_Color fontBlack;

#define SP_CENTERED (1 << 0)
#define SP_INVERTED (1 << 1)
#define SP_RIGHTJUSTIFIED (1 << 2)
#define SP_ONBORDER (1 << 4)

#define SP_ESC '^'
#define SP_CLR 'c'

extern int screen_init(void);
extern void screen_erase(SDL_Rect * rect);
extern void screen_update(SDL_Rect * rect);
extern void screen_fill(SDL_Rect * rect, Uint32 color);
extern void screen_blit(SDL_Surface * source, SDL_Rect * from, SDL_Rect * to);
extern int screen_width(void);
extern int screen_height(void);
extern SDL_PixelFormat *screen_format(void);
extern void screen_flash(SDL_Rect * rect, int mdelay, Uint32 color);
extern void screen_print(SDL_Rect * rect, int flags, const char *fmt, ...);
extern void screen_print_glyph_buf(SDL_Rect *rect, int flags, glyph_buf_t *gbuf);

// Added for missile animations
extern SDL_Surface *screen_create_surface(int w, int h);
extern void screen_copy(SDL_Rect * from, SDL_Rect * to, SDL_Surface * dest);
extern void screen_shade(SDL_Rect * area, unsigned char amount);
extern void screen_highlight(SDL_Rect * area);
extern void screen_highlightColored(SDL_Rect * area, Uint32 color);

// Added for peer effect
extern int screen_lock(void);
extern void screen_unlock(void);
extern void screen_set_pixel(int x, int y, Uint32 color);
extern Uint32 screen_map_rgb(Uint8 red, Uint8 grn, Uint8 blu);
extern void screen_zoom_out(int factor);
extern void screen_zoom_in(int factor);

extern void screen_fade_surface(SDL_Surface * surf, int transparency);
extern void screen_repaint_frame(void);
extern void screen_capture(char *fname, SDL_Rect *rect);

END_DECL

#endif
