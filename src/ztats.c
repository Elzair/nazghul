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

#include "applet.h"
#include "cmdwin.h"
#include "event.h"
#include "foogod.h"
#include "macros.h"
#include "player.h"
#include "screen.h"
#include "session.h"

#include <string.h>

struct ztats {
        struct applet base;
        struct list panes;
        struct ztats_pane *current;
};

/**
 * For now there's a global singleton for the ztats applet, but whenever
 * possible try to write functions that get passed in a pointer.
 */
static struct ztats ztats;

static void ztats_op_paint(struct applet *applet)
{
        DECL_CAST(struct ztats, ztats, applet);

        screenErase(&applet->dims);
        ztats->current->ops->paint(ztats->current);
        screenUpdate(&applet->dims);

        status_repaint_title();
}

static void ztats_scroll(struct ztats *ztats, enum StatusScrollDir dir)
{
        struct list *list = 0;

        if (! ztats->current) {
                return;
        }

        /* let the pane have first crack at handling it */
        if (ztats->current->ops->scroll 
            && ztats->current->ops->scroll(ztats->current, dir)) {
                ztats_op_paint(&ztats->base);
                return;
        }

        switch (dir) {
        case ScrollRight:
                list = ztats->current->list.next;
                if (list == &ztats->panes) {
                        list = list->next;
                        assert(list != &ztats->panes);
                }
                break;
        case ScrollLeft:
                list = ztats->current->list.prev;
                if (list == &ztats->panes) {
                        list = list->prev;
                        assert(list != &ztats->panes);
                }
                break;
        default:
                /* ignore non-horizontal scrolling */
                return;
        }

        ztats->current = list_entry(list, struct ztats_pane, list);
        if (ztats->current->ops->enter) {
                ztats->current->ops->enter(ztats->current, ztats->base.session->player, dir, &ztats->base.dims);
        }

        ztats_op_paint(&ztats->base);

}

static int ztats_key_handler(struct KeyHandler * handler, int key, int keymod)
{
        DECL_CAST(struct ztats, ztats, handler->data);

	switch (key) {
	case KEY_NORTH:
		ztats_scroll(ztats, ScrollUp);
		break;
	case KEY_SOUTH:
		ztats_scroll(ztats, ScrollDown);
		break;
	case KEY_EAST:
		ztats_scroll(ztats, ScrollRight);
		break;
	case KEY_WEST:
		ztats_scroll(ztats, ScrollLeft);
		break;
	case SDLK_PAGEUP:
	case SDLK_KP9:
		ztats_scroll(ztats, ScrollPageUp);
		break;
	case SDLK_PAGEDOWN:
	case SDLK_KP3:
		ztats_scroll(ztats, ScrollPageDown);
		break;
	case SDLK_RETURN:
	case SDLK_SPACE:
	case SDLK_KP_ENTER:
        case KEY_HERE:
	case '\n':
                if (ztats->current->ops->select) {
                        ztats->current->ops->select(ztats->current);
                        ztats_op_paint(&ztats->base); /* in case applet ran */
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

static void ztats_op_run(struct applet *applet, SDL_Rect *dims, struct session *session)
{
        DECL_CAST(struct ztats, ztats, applet);

        if (list_empty(&ztats->panes)) {
                return;
        }

	cmdwin_clear();
	cmdwin_spush("Stats");
	cmdwin_spush("<ESC to exit>");
        foogodSetHintText("\200\201=scroll ESC=exit");
        foogodSetMode(FOOGOD_HINT);        
        
        applet->dims = *dims;
        applet->session = session;

        ztats->current = list_entry(ztats->panes.next, struct ztats_pane, list);
        ztats->current->ops->enter(ztats->current, session->player, ScrollRight, dims);
        ztats_op_paint(applet);

        eventRunKeyHandler(ztats_key_handler, ztats);

        foogodSetMode(FOOGOD_DEFAULT);
	cmdwin_pop();
	cmdwin_spush("ok");

        /* Reset to NULL; else if the current pane removes itself during a
         * session teardown it will trigger a scroll action, which will
         * probably crash. */
        ztats->current = NULL;

}

void ztats_init(void)
{
        static struct applet_ops ztats_ops = {
                ztats_op_run,
                ztats_op_paint
        };

        memset(&ztats, 0, sizeof(&ztats));
        list_init(&ztats.panes);
        ztats.base.ops = &ztats_ops;
}

void ztats_add_pane(struct ztats_pane *pane)
{
        list_add_tail(&ztats.panes, &pane->list);
}

void ztats_rm_pane(struct ztats_pane *pane)
{
        if (pane == ztats.current) {
                ztats_scroll(&ztats, ScrollRight);
                if (pane == ztats.current) {
                        /* last pane in the list */
                        ztats.current = NULL;
                }
        }

        list_remove(&pane->list);
}

struct applet *ztats_get_applet(void)
{
        return &ztats.base;
}
