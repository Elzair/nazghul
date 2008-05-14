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

#ifndef ztats_h
#define ztats_h

#include "ztats_pane.h"
#include "Party.h"
#include "status.h"

#include <SDL.h>

extern void ztats_init();
extern void ztats_enter(class Party *party, SDL_Rect *rect);
extern void ztats_scroll(enum StatusScrollDir dir);
extern void ztats_paint(void);
extern void ztats_add_pane(struct ztats_pane *pane);
extern void ztats_rm_pane(struct ztats_pane *pane);

#endif
