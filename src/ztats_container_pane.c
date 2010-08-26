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

#include "ztats_container_pane.h"

#include "screen.h"
#include "sprite.h"

#include <string.h>

static int ztats_container_pane_count_items(struct ztats_container_pane *zcp)
{
        class Container *container = zcp->base.party->getInventory();
        if (! container) {
                return 0;
        }

        assert(zcp->ops);
        assert(zcp->filter);

        return container->filter_count(zcp->filter);
}

void ztats_container_pane_enter(struct ztats_pane *pane, class Party *party, enum StatusScrollDir via, SDL_Rect *dims)
{
        struct ztats_container_pane *zcp = (struct ztats_container_pane*)pane;
        ztats_pane_enter(pane, party, via, dims);
        assert(zcp->title);
        status_set_title(zcp->title);
        zcp->top_line = 0;
        zcp->num_lines = dims->h / TILE_H;
        zcp->max_line = ztats_container_pane_count_items(zcp) - zcp->num_lines;
}

int ztats_container_pane_scroll(struct ztats_pane *pane, enum StatusScrollDir dir)
{
        struct ztats_container_pane *zcp = (struct ztats_container_pane*)pane;

	switch (dir) {
	case ScrollUp:
		if (zcp->top_line > 0) {
			zcp->top_line--;
                }
		break;
	case ScrollDown:
		if (zcp->top_line < zcp->max_line) {
			zcp->top_line++;
                }
		break;
	case ScrollPageUp:
		zcp->top_line -= zcp->num_lines;
		if (zcp->top_line < 0) {
			zcp->top_line = 0;
                }
		break;
	case ScrollPageDown:
		zcp->top_line = min(zcp->max_line, zcp->top_line + zcp->num_lines);
		break;
	case ScrollLeft:
	case ScrollRight:
        default:
                return 0;
	}

        return 1;
}

void ztats_container_pane_paint(struct ztats_pane *pane)
{
        struct ztats_container_pane *zcp = (struct ztats_container_pane*)pane;
	SDL_Rect rect = pane->dims;
	int top = zcp->top_line;
        class Container *container = pane->party->getInventory();
	struct inv_entry *ie;
	int line = 0;

        if (! container) {
                screenPrint(&rect, 0, "No inventory!");
                return;
        }

	rect.h = ASCII_H;

	for (ie = container->first(zcp->filter); ie != NULL; ie = container->next(ie, zcp->filter)) {

		if (top) {
			top--;
			continue;
		}
	
                zcp->ops->paint_item(ie, &rect);
		line++;
	
		/* Don't print outside the status window. */
		if (line >= zcp->num_lines) {
			break;
                }
	}

}

void ztats_container_pane_init(struct ztats_container_pane *zcp, const char *title, struct filter *filter, struct ztats_container_pane_ops *ops)
{
        static struct ztats_pane_ops base_ops = {
                ztats_container_pane_enter,
                ztats_container_pane_scroll,
                ztats_container_pane_paint
        };

        memset(zcp, 0, sizeof(*zcp));
        zcp->base.ops = &base_ops;
        zcp->title = title;
        zcp->filter = filter;
        zcp->ops = ops;
}

void ztats_container_paint_item_generic(struct inv_entry *ie, SDL_Rect *rect)
{
	if (ie->type->getSprite()) {
                sprite_paint(ie->type->getSprite(), 0, rect->x, rect->y);
	}
	
	/* Indent past the sprite column. */
	rect->x += TILE_W;

	/* This is a single-line entry in a two-line rect, so center it
		* vertically. */
	rect->y += TILE_H / 4;

	screenPrint(rect, 0, "%2d %s", ie->count - ie->ref, ie->type->getName());

	/* Carriage-return line-feed */
	rect->y += (TILE_H * 3) / 4;
	rect->x -= TILE_W;
}
