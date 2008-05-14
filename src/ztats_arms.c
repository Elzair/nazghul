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

#include "ztats_arms.h"

#include "Arms.h"
#include "screen.h"
#include "sprite.h"
#include "ztats.h"
#include "ztats_container_pane.h"


static void ztats_arms_paint_item(struct inv_entry *ie, SDL_Rect *rect)
{
	ArmsType *arms = (ArmsType*)ie->type;
	
	assert(ie->count);
	
	/* sprite */
	sprite_paint(arms->getSprite(), 0, rect->x, rect->y);
	rect->x += TILE_W;
	
	/* quantity and name */
	if (ie->ref) {
		screenPrint(rect, 0, "%2d %s ^c+%c[%d in use]^c-", ie->count, arms->getName(), STAT_INUSE_CLR, ie->ref);
	} else {
		screenPrint(rect, 0, "%2d %s", ie->count, arms->getName());
	}
	rect->y += ASCII_H;
	
	/* stats */
	status_show_arms_stats(rect, arms);
	rect->x -= TILE_W;
}

static bool ztats_arms_filter_cb(struct inv_entry *ie, void *fdata)
{
	return (ie->type->isReadyable());
}


void ztats_arms_init(void)
{
        static struct ztats_container_pane pane;
        static struct ztats_container_pane_ops ops = {
                ztats_arms_paint_item
        };
        static struct filter filter = {
                ztats_arms_filter_cb,
                NULL
        };

        ztats_container_pane_init(&pane, "Armaments", &filter, &ops);
        ztats_add_pane(&pane.base);
}
