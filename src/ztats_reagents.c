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

#include "ztats_reagents.h"

#include "screen.h"
#include "sprite.h"
#include "ztats.h"
#include "ztats_container_pane.h"


static void ztats_reagents_paint_item(struct inv_entry *ie, SDL_Rect *rect)
{
	if (ie->type->getSprite()) {
		sprite_paint(ie->type->getSprite(), 0, rect->x, rect->y);
	}
	
	/* Indent past the sprite column. */
	rect->x += TILE_W;
	
	/* This is a single-line entry in a two-line rect, so center it
         * vertically. */
	rect->y += TILE_H / 4;

	/* During mixing, if the ref field is set that means the reagent has
         * been selected to be part of the mixture (see cmdMix() in cmd.c, this
         * is something of a hack). Show an asterisk to mark selected
         * reagents. */
	screenPrint(rect, 0, "%2d%c%s", ie->count, (ie->ref ? '*':' '), ie->type->getName());

	/* Carriage-return line-feed */
	rect->y += (TILE_H * 3) / 4;
	rect->x -= TILE_W;
}

static bool ztats_reagents_filter_cb(struct inv_entry *ie, void *fdata)
{
	return (ie->type->isMixable());
}


void ztats_reagents_init(void)
{
        static struct ztats_container_pane pane;
        static struct ztats_container_pane_ops ops = {
                ztats_reagents_paint_item
        };
        static struct filter filter = {
                ztats_reagents_filter_cb,
                NULL
        };

        ztats_container_pane_init(&pane, "Reagents", &filter, &ops);
        ztats_add_pane(&pane.base);
}
