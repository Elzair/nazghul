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

#include "ztats_pm.h"

#include "effect.h"
#include "mmode.h"
#include "occ.h"
#include "screen.h"
#include "sprite.h"
#include "ztats.h"

#include <string.h>


struct ztats_pm {
        struct ztats_pane base;
        int order;
};

/* status_show_member_arms -- called during Ztats when showing Party Members,
 * this shows individual arms held by the member */
static void ztats_pm_paint_arms(SDL_Rect * rect, ArmsType *arms)
{
	sprite_paint(arms->getSprite(), 0, rect->x, rect->y);
	rect->x += TILE_W;

	/* name */
	// SAM: I don't like cluttering the name line, but the range and AP 
	//      are essential information, and previous attempts at multi-line 
	//      status entries foundered.  So, until the thing can be re-written...
	screenPrint(rect, 0, "%s  (Rng:%d, AP:%d, Spd:%d)", 
		    arms->getName(), arms->getRange(), arms->getRequiredActionPoints(), arms->get_AP_mod() );
	rect->y += ASCII_H;

	/* stats */
	status_show_arms_stats(rect, arms);
	rect->x -= TILE_W;
}

static int ztats_pm_paint_effect(hook_entry_t *entry, void *data)
{
	SDL_Rect *rect = (SDL_Rect*)data;
	struct effect *effect = entry->effect;

	/* No name implies invisible to UI. */
	if (!effect->name) {
		return 0;
	}

	/* Blit the effect icon, if it has one */
	if (effect->sprite) {
                sprite_paint(effect->sprite, 0, rect->x, rect->y);
	}

	rect->x += ASCII_W;

	if (EFFECT_NONDETERMINISTIC == entry->effect->duration) {
		screenPrint(rect, 0, " %s", entry->effect->name);
	} else if (EFFECT_PERMANENT == entry->effect->duration) {
		screenPrint(rect, 0, " %s (permanent)", entry->effect->name);
	} else {
		screenPrint(rect, 0, " %s [%d min]", entry->effect->name, 
			clock_alarm_remaining(&entry->expiration));
	}
	rect->x -= ASCII_W; /* back up to start next effect at column 0 */
	rect->y += ASCII_H;

	return 0;
}


void ztats_pm_enter(struct ztats_pane *pane, class Party *party, enum StatusScrollDir via, SDL_Rect *dims)
{
        struct ztats_pm *pm = (struct ztats_pm*)pane;

        ztats_pane_enter(pane, party, via, dims);

        if (! pane->party->getSize()) {
                status_set_title("Nobody");
                return;
        }

        if (ScrollLeft == via) {
                pm->order = pane->party->getSize() - 1;
        } else {
                pm->order = 0;
        }

        class Character *ch = pane->party->getMemberByOrder(pm->order);
        assert(ch);
        status_set_title(ch->getName());
}

int ztats_pm_scroll(struct ztats_pane *pane, enum StatusScrollDir dir)
{
        struct ztats_pm *pm = (struct ztats_pm*)pane;

        if (! pane->party->getSize()) {
                return 0;
        }

        switch (dir) {
        case ScrollRight:
                if (pm->order == (pane->party->getSize() - 1)) {
                        return 0;
                }
                pm->order++;
                break;
        case ScrollLeft:
                if (pm->order == 0) {
                        return 0;
                }
                pm->order--;
                break;
        default:
                return 0;
        }

        class Character *ch = pane->party->getMemberByOrder(pm->order);
        assert(ch);
        status_set_title(ch->getName());
        return 1;
}

void ztats_pm_paint(struct ztats_pane *pane)
{
        struct ztats_pm *pm = (struct ztats_pm*)pane;
	struct mmode *mmode;
	int i;

        SDL_Rect rect = pane->dims;
	
        if (! pane->party->getSize()) {
                screenPrint(&rect, 0, "Empty party!");
                return;
        }

        class Character *ch = pane->party->getMemberByOrder(pm->order);
        assert(ch);

	/* Push the current color. */
	screenPrint(&rect, 0, "^c+=");

	// Show experience level and XP information:
	screenPrint(&rect, 0, 
		    "^c%cLevel:^cw%d "
		    "^c%cXP:^cw%d "
		    "^c%cNext Level:^cw%d ",

		    STAT_LABEL_CLR,ch->getLevel(),
		    STAT_LABEL_CLR,ch->getExperience(),
		    STAT_LABEL_CLR,ch->getXpForLevel(ch->getLevel()+1)
	    );
	rect.y += ASCII_H;

	// Show the basic character attributes:
	screenPrint(&rect, 0, 
		    "^c%cStr:^cw%d "
		    "^c%cInt:^cw%d "
		    "^c%cDex:^cw%d ",

		    STAT_LABEL_CLR, ch->getStrength(),
		    STAT_LABEL_CLR, ch->getIntelligence(),
		    STAT_LABEL_CLR, ch->getDexterity()
		);
	rect.y += ASCII_H;

	// Show highly variable information such as HP/max, MP/max, and AP/max
	screenPrint(&rect, 0, 
		    "^c%cHP:^c%c%d^cw/%d "
		    "^c%cMP:^c%c%d^cw/%d "
		    "^c%cAP:^c%c%d^cw/%d ",

		    STAT_LABEL_CLR, 
		    status_range_color(ch->getHp(), ch->getMaxHp()),
		    ch->getHp(), ch->getMaxHp(),

		    STAT_LABEL_CLR, 
		    status_range_color(ch->getMana(), ch->getMaxMana()),
		    ch->getMana(), ch->getMaxMana(),

		    STAT_LABEL_CLR, 
		    status_range_color(ch->getActionPoints(), ch->getActionPointsPerTurn()), 
		    ch->getActionPoints(), ch->getActionPointsPerTurn()
		);
	rect.y += ASCII_H;

	// Show species, class, and movement mode:
	mmode = ch->getMovementMode();
	assert(mmode);
	screenPrint(&rect, 0, 
		    "^c%cSpecies:    ^cw%s", 
		    STAT_LABEL_CLR, ch->species ? ch->species->name:"Unknown");
	rect.y += ASCII_H;

	screenPrint(&rect, 0, 
		    "^c%cOccupation: ^cw%s", 
		    STAT_LABEL_CLR, ch->occ ? ch->occ->name : "None");
	rect.y += ASCII_H;

	screenPrint(&rect, 0, 
		    "^c%cMovement:   ^cw%s", 
		    STAT_LABEL_CLR, mmode->name);
	rect.y += ASCII_H;
	rect.y += ASCII_H;

	/* Show effects */
	screenPrint(&rect, 0 /*SP_CENTERED*/ , "^c%c*** Effects ***^cw", 
						STAT_LABEL_CLR);
	rect.y += ASCII_H;
	for (i = 0; i < OBJ_NUM_HOOKS; i++) {
                ch->hookForEach(i, ztats_pm_paint_effect, &rect);
	}
	rect.y += ASCII_H;

	/* Show arms */
	screenPrint(&rect, 0 /*SP_CENTERED*/ , "^c%c*** Arms ***^cw", STAT_LABEL_CLR);
	rect.y += ASCII_H;

#if 1
	int armsIndex=0;
	class ArmsType *arms = ch->enumerateArms(&armsIndex);
	while (arms != NULL) {
		ztats_pm_paint_arms(&rect, arms);
		arms = ch->getNextArms(&armsIndex);
	}
#else
	/* This was an experiment with enumerating the slots instead of the
         * readied arms. I couldn't get the formatting to look very good, so I
         * punted. */
	for (i = 0; i < ch->species->n_slots; i++) {
		class ArmsType *arms = ch->getArmsInSlot(i);
		if (arms) {
			status_show_member_arms(&rect, i, arms);
		} else {
			rect.x += TILE_W;
			screenPrint(&rect, 0, "^c+y%d:^cG(empty)^c-", i);
			rect.x -= TILE_W;
			rect.y += ASCII_H;
		}
	}
#endif

	/* Pop the saved current color. */
	screenPrint(&rect, 0, "^c-");


	// fixme: currently this will overprint and it doesn't support
	// scrolling. These may be necessary if the status window is not large
	// enough.


}

void ztats_pm_init(void)
{
        static struct ztats_pane_ops ztats_pm_ops = {
                ztats_pm_enter,
                ztats_pm_scroll,
                ztats_pm_paint
        };

        static struct ztats_pm ztats_pm;

        memset(&ztats_pm, 0, sizeof(ztats_pm));
        ztats_pm.base.ops = &ztats_pm_ops;
        ztats_add_pane(&ztats_pm.base);
}
