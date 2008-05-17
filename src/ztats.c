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

#include "ztats.h"

#include "event.h"

#include <string.h>

struct ztats {
        struct list panes;
        struct ztats_pane *current;
        class Party *party;
        SDL_Rect dims;
};

static struct ztats ztats;


static int ztats_key_handler(struct KeyHandler * handler, int key, int keymod)
{
	switch (key) {
	case KEY_NORTH:
		ztats_scroll(ScrollUp);
		break;
	case KEY_SOUTH:
		ztats_scroll(ScrollDown);
		break;
	case KEY_EAST:
		ztats_scroll(ScrollRight);
		break;
	case KEY_WEST:
		ztats_scroll(ScrollLeft);
		break;
	case SDLK_PAGEUP:
		ztats_scroll(ScrollPageUp);
		break;
	case SDLK_PAGEDOWN:
		ztats_scroll(ScrollPageDown);
		break;
	case SDLK_RETURN:
	case SDLK_SPACE:
        case KEY_HERE:
	case '\n':
                if (ztats.current->ops->select) {
                        ztats.current->ops->select(ztats.current);
                }
		return 0;
	case SDLK_ESCAPE:
	case 'q':
		return 1;
	default:
		break;
	}

	return 0;
}

void ztats_init(void)
{
        memset(&ztats, 0, sizeof(&ztats));
        list_init(&ztats.panes);
}

void ztats_enter(class Party *party, SDL_Rect *dims)
{
        if (list_empty(&ztats.panes)) {
                return;
        }

        ztats.party = party;
        ztats.dims = *dims;

        ztats.current = list_entry(ztats.panes.next, struct ztats_pane, list);
        if (ztats.current->ops->enter) {
                ztats.current->ops->enter(ztats.current, party, ScrollRight, dims);
        }

        eventRunKeyHandler(ztats_key_handler, NULL);
}

void ztats_scroll(enum StatusScrollDir dir)
{
        struct list *list = 0;

        if (! ztats.current) {
                return;
        }

        /* let the pane have first crack at handling it */
        if (ztats.current->ops->scroll 
            && ztats.current->ops->scroll(ztats.current, dir)) {
                statusRepaint();
                return;
        }

        switch (dir) {
        case ScrollRight:
                list = ztats.current->list.next;
                if (list == &ztats.panes) {
                        list = list->next;
                        assert(list != &ztats.panes);
                }
                break;
        case ScrollLeft:
                list = ztats.current->list.prev;
                if (list == &ztats.panes) {
                        list = list->prev;
                        assert(list != &ztats.panes);
                }
                break;
        default:
                /* ignore non-horizontal scrolling */
                return;
        }

        ztats.current = list_entry(list, struct ztats_pane, list);
        if (ztats.current->ops->enter) {
                ztats.current->ops->enter(ztats.current, ztats.party, dir, &ztats.dims);
        }

        statusRepaint();

}

void ztats_paint(void)
{
        if (! ztats.current) {
                return;
        }

        ztats.current->ops->paint(ztats.current);
}

void ztats_add_pane(struct ztats_pane *pane)
{
        list_add_tail(&ztats.panes, &pane->list);
}

void ztats_rm_pane(struct ztats_pane *pane)
{
        if (pane == ztats.current) {
                ztats_scroll(ScrollRight);
                if (pane == ztats.current) {
                        /* last pane in the list */
                        ztats.current = NULL;
                }
        }

        list_remove(&pane->list);
}
