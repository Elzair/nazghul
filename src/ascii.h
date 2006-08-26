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
#ifndef ascii_h
#define ascii_h

#include "macros.h"

#include <SDL.h>

BEGIN_DECL

extern int asciiInit(void);

/* asciiPaint -- in the usual case this prints the character to the surface
 * using the current font and color. In the general case, 'c' may be part of a
 * control sequence, where the format is:
 *
 *       <SEQ> := ^c<CMD><COLOR>
 *     <COLOR> := B|w|r|g|b|c|y|m|G|!
 *       <CMD> := +|-|<NIL>
 *       <NIL> :=
 *
 * The '+' CMD pushes the current color before setting the new color. The '-'
 * CMD pops the last pushed color and makes it the new color.
 */
extern int asciiPaint(char c, int x, int y, SDL_Surface * surface);

END_DECL

#endif
