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
#ifndef vmask_h
#define vmask_h

#include "dimensions.h"

#define VMASK_W    (MAP_TILE_W * 2 + 1)
#define VMASK_H    (VMASK_W)
#define VMASK_SZ   (VMASK_W * VMASK_H)

#ifdef __cplusplus
extern "C" {
#endif

        // --------------------------------------------------------------------
        // Call this once to initialize the library on startup.
        // --------------------------------------------------------------------

        extern int vmask_init(void);

        // --------------------------------------------------------------------
        // Fetch the vmask corresponding to the given location. The vmask is
        // gauranteed to be valid until the next call to vmask_get(), at which
        // point all bets are off. I expect the typical usage will be for
        // callers to only use one at a time, and to make their own copy in the
        // rare cases where they need to deal with more than one at a time. 
        // --------------------------------------------------------------------

        extern char *vmask_get(struct place *place, int x, int y);


        // --------------------------------------------------------------------
        // Invalidate all vmasks in the area surrounding the given
        // location. You should call this whenever you do something that will
        // change the line-of-sight properties of a tile. It will force the
        // surrounding vmasks to recompute their line-of-sight the next time
        // somebody tries to fetch them.
        //
        // Note that the 'w' and 'h' indicate a rectangle of tiles whose
        // line-of-sight property have changed. For a single tile they would
        // each be 1. Don't worry about trying to evaluate the extent of the
        // damage: the function will automatically figure out which vmasks are
        // affected.
        // --------------------------------------------------------------------

        extern void vmask_invalidate(struct place *place, int x, int y, int w, int h);

        // --------------------------------------------------------------------
        // Invalidate all vmasks everywhere.
        // --------------------------------------------------------------------

        extern void vmask_flush_all(void);

#ifdef __cplusplus
}
#endif
#endif
