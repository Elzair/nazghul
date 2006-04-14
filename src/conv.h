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
#ifndef conv_h
#define conv_h

extern void conv_enter(class Object *npc, class Object *pc, struct closure *conv);
extern void conv_end(void);

static int isprintable(int c)
{
        /* Looks like ctype's isprint() doesn't always behave the same way. On
         * some systems it was letting c<32 go through, causing an assert in
         * ascii.c. */
        return (c>=32&&c<127);
}

#endif
