/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2008 Gordon McNutt
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 *
 * Gordon McNutt
 * gmcnutt@users.sourceforge.net
 */

#ifndef ztats_pane_h
#define ztats_pane_h

#include "list.h"
#include "Party.h"
#include "status.h"

#include <SDL.h>

struct ztats_pane_ops {
        void (*enter)(struct ztats_pane *pane, class Party *party, enum StatusScrollDir via, SDL_Rect *dims);
        /* Scroll returns non-zero iff it handled the scroll request */
        int (*scroll)(struct ztats_pane *pane, enum StatusScrollDir dir);
        void (*paint)(struct ztats_pane *pane);
        void (*select)(struct ztats_pane *pane);
};

struct ztats_pane {
        struct list list;
        SDL_Rect dims;
        class Party *party;
        struct ztats_pane_ops *ops;
};

extern void ztats_pane_enter(struct ztats_pane *pane, class Party *party, enum StatusScrollDir via, SDL_Rect *dims);

#endif
