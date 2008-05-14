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

#include "ztats_spells.h"

#include "magic.h"
#include "screen.h"
#include "session.h"
#include "sprite.h"
#include "ztats.h"
#include "ztats_container_pane.h"

static void ztats_spells_paint_item(struct inv_entry *ie, SDL_Rect *rect)
{
	char code[MAX_SYLLABLES_PER_SPELL+1] = { 0 };
	struct spell *spell = 0;

	/* This assumes the type name matches the spelled-out code name, and
         * doesn't include extra stuff like " spell" at the end. Eg, "Vas Flam"
         * is great but "Vas Flam spell" will come back as "Vas Flam Sanct" or
         * possibly an error. */
	if (! magic_spell_name_to_code(&Session->magic, code, sizeof(code), ie->type->getName())) {
		spell = magic_lookup_spell(&Session->magic, code);
	}

	/* Blit the sprite on the left */
	if (spell && spell->sprite) {
		sprite_paint(spell->sprite, 0, rect->x, rect->y);
	}
	rect->x += TILE_W;

	/* Print basic info available in the type. */
	screenPrint(rect, 0, "%2d %s", ie->count, ie->type->getName());
	rect->y += ASCII_H;

	/* Print info only available in the spell struct. */
	if (spell) {
		screenPrint(rect, 0, 
			"^c+GLvl:^c+y%d^c- MP:^c+b%d^c- AP:^c+r%d^c-^c-",
			spell->level, 
			spell->cost, spell->action_points);
	}

	/* Carriage-return line-feed */
	rect->y += ASCII_H;
	rect->x -= TILE_W;
}

static bool ztats_spells_filter_cb(struct inv_entry *ie, void *fdata)
{
	return (ie->type->isCastable());
}


void ztats_spells_init(void)
{
        static struct ztats_container_pane pane;
        static struct ztats_container_pane_ops ops = {
                ztats_spells_paint_item
        };
        static struct filter filter = {
                ztats_spells_filter_cb,
                NULL
        };

        ztats_container_pane_init(&pane, "Spells", &filter, &ops);
        ztats_add_pane(&pane.base);
}
