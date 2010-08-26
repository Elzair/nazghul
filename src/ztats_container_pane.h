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

#ifndef ztats_container_pane_h
#define ztats_container_pane_h

#include "Container.h"
#include "object.h"
#include "ztats_pane.h"

#include <SDL.h>

struct ztats_container_pane_ops {
        void (*paint_item)(struct inv_entry *ie, SDL_Rect *rect);
};

struct ztats_container_pane {
        struct ztats_pane base;
        const char *title;
        struct ztats_container_pane_ops *ops;
        struct filter *filter;
        int top_line;
        int max_line;
        int num_lines;
};

extern void ztats_container_pane_init(struct ztats_container_pane *zcp, const char *title, struct filter *filter, struct ztats_container_pane_ops *ops);
extern void ztats_container_paint_item_generic(struct inv_entry *ie, SDL_Rect *rect);

#endif
