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

#include "ztats_misc.h"

#include "screen.h"
#include "sprite.h"
#include "ztats.h"
#include "ztats_container_pane.h"


static bool ztats_misc_filter_cb(struct inv_entry *ie, void *fdata)
{
	return (! ie->type->isReadyable()
		&& ! ie->type->isMixable()
		&& ! ie->type->isCastable()
		&& ! ie->type->isUsable());
}

void ztats_misc_init(void)
{
        static struct ztats_container_pane pane;
        static struct ztats_container_pane_ops ops = {
                ztats_container_paint_item_generic,
        };
        static struct filter filter = {
                ztats_misc_filter_cb,
                NULL
        };

        ztats_container_pane_init(&pane, "Miscellaneous", &filter, &ops);
        ztats_add_pane(&pane.base);
}
