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

/**
 * Print a character using a build-in font and color. In the usual case this
 * prints the character to the surface using the current font and color. In the
 * general case, 'c' may be part of a control sequence, where the format is:
 *
 *       <SEQ> := ^c<CMD><COLOR>
 *     <COLOR> := B|w|r|g|b|c|y|m|G|!
 *       <CMD> := +|-|<NIL>
 *       <NIL> :=
 *
 * The '+' CMD pushes the current color before setting the new color. The '-'
 * CMD pops the last pushed color and makes it the new color.
 *
 * The color codes:
 *
 *   w White
 *   B Black
 *   r Red
 *   g Green
 *   b Blue
 *   y Yellow
 *   c Cyan
 *   m Magenta
 *   G Gray
 *
 * @param c is either a character to print or part of a control sequence.
 * @param x is the pixel x-coord of the upper left corner to paint to.
 * @param y is the pixel y-coord of the upper left corner to paint to.
 * @param surface is the surface to paint to.
 * @returns 1 if a character was painted, 0 if it was part of a control
 * sequence
 */
extern int asciiPaint(char c, int x, int y, SDL_Surface * surface);

/**
 * Get the length of a string NOT including the font and color control
 * characters. This is useful for determining how much screen space the string
 * will take when printed with asciiPaint().
 *
 * The function assumes that the string applies to the current internal state
 * of the ascii painting engine. This is only important if the last character
 * to asciiPaint was part of an unfinished control sequence. In that case,
 * asciiStrlen() assumes that this string continues where that one left off.
 *
 * @param s is the null-terminated string to check.
 * @returns strlen(s) minus the number of control characters
 */
extern int asciiStrlen(char *s);

END_DECL

#endif
