//
// nazghul - an old-school RPG engine
// Copyright (C) 2002, 2003 Gordon McNutt
//
// This program is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 2 of the License, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
// more details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
// Suite 330, Boston, MA 02111-1307 USA
//
// Gordon McNutt
// gmcnutt@users.sourceforge.net
//
#include "status.h"

#include "applet.h"
#include "screen.h"
#include "sprite.h"
#include "common.h"
#include "player.h"
#include "object.h"
#include "Arms.h"
#include "ascii.h"
#include "console.h"
#include "sky.h"
#include "wind.h"
#include "foogod.h"
#include "mmode.h"
#include "cmdwin.h"
#include "dice.h"
#include "occ.h"
#include "effect.h"
#include "session.h"
#include "magic.h"
#include "clock.h"

#include "ztats.h"
#include "ztats_arms.h"
#include "ztats_items.h"
#include "ztats_misc.h"
#include "ztats_pm.h"
#include "ztats_reagents.h"
#include "ztats_spells.h"

#include <stdio.h>
#include <assert.h>
#include <ctype.h>

#define LINE_H TILE_H
#define Y_TO_LINE(Y) (((Y) - Status.screenRect.y) / TILE_H)
#define N_LINES Status.numLines	/* (STAT_H / TILE_H) */

#define TALL_H (SCREEN_H - 4 * BORDER_H - 8 * ASCII_H)
#define MAX_TITLE_LEN (STAT_CHARS_PER_LINE-2)
#define STAT_MAX_H TALL_H


#define STAT_MODE_STACK_DEPTH 10

static struct status {
	SDL_Rect titleRect;
	SDL_Rect screenRect;
	SDL_Rect lineRect;
	void (*paint) (void);
	void (*scroll) (enum StatusScrollDir);
	int pcIndex;
	struct inv_entry *selectedEntry;
	enum StatusMode mode;

	/**
         * The index of the list entry that appears at the top of the status
         * window.
         */
	int topLine;

	/**
         * Not sure what this is.
         */
	int maxLine;

	/**
         * The number of lines in the status window.
         */
	int numLines;

	/**
         * The index of the list entry that is currently highlighted in the
         * status window.
         */
	int curLine;

	/**
         * The text that appears in the title bar at the top of the status
         * window.
         */
	char title[MAX_TITLE_LEN+1];

	const char *pg_title;
        char *pg_text;
	SDL_Surface *pg_surf;
	SDL_Rect pg_rect;
	int pg_max_y;

	const char *list_title;
	int list_sz;
	struct trade_info *trades;
	struct stat_list_entry *list;
	const char **strlist;
	
	Container *container;
	struct filter *filter;
	void (*show_thing)(SDL_Rect * rect, void *thing);

	/**
         * Sometimes I just don't want to repaint until I'm done doing more
         * stuff. Repaints will be suppressed unless this counter is
         * zero. Limited to internal use only for now.
         */
	int suppressRepaint;

	/**
         * New experimental super-generic stuff
         */
	struct stat_super_generic_data *super_generic;

	enum StatusMode stack[STAT_MODE_STACK_DEPTH];
	int top;
        struct list applet_stack;

} Status;

/* filtering functions used with status_show_container() */
static bool stat_filter_ready_arms(struct inv_entry *ie, void *fdata);
static bool stat_filter_drop(struct inv_entry *ie, void *fdata);

/* functions to show specific types of things from status_show_containe() */
static void status_show_generic_object_type(SDL_Rect *rect, void *thing);
static void status_show_mix_reagent(SDL_Rect *rect, void *thing);

/* super-generic functions */
static void stat_super_generic_paint();
static void stat_super_generic_scroll(enum StatusScrollDir dir);

/* Filter for the player inventory during the R)eady UI. */
static struct filter stat_ready_arms_filter = {
	stat_filter_ready_arms, 0
};

static struct filter stat_drop_filter = {
	stat_filter_drop, 0
};

static bool stat_filter_usable(struct inv_entry *ie, void *fdata)
{
	return (ie->type->isUsable());
}

static struct filter stat_use_filter = {
	stat_filter_usable, 0
};

static bool stat_filter_mixable(struct inv_entry *ie, void *fdata)
{
	return (ie->type->isMixable());
}


static struct filter stat_mix_filter = {
	stat_filter_mixable, 0
};

static bool stat_filter_ready_arms(struct inv_entry *ie, void *fdata)
{
	if (! ie->type->isReadyable()) {
		return false;
	}
				
	/* Are any available? */
	if (ie->count > ie->ref)
		return true;
				
	/* Is one already readied by the current party member? */
	if (ie->ref && 
			player_party->getMemberAtIndex(Status.pcIndex)->
				hasReadied((class ArmsType *)ie->type))
	{
		return true;
	}

	return false;
}

static bool stat_filter_drop(struct inv_entry *ie, void *fdata)
{
	return (! ie->type->isCastable()
		&& (ie->ref < ie->count));
}

static void switch_to_tall_mode(void)
{
	if (Status.screenRect.h == TALL_H)
		return;

	Status.screenRect.h = TALL_H;
	Status.numLines = Status.screenRect.h / LINE_H;

	foogod_set_y(STAT_Y + Status.screenRect.h + BORDER_H);

	foogodRepaint();
	consoleRepaint();
	screen_repaint_frame();
}

static void status_set_line_height(int lines)
{
	int height = lines * ASCII_H;
	
	if (height > STAT_MAX_H) {
		height = STAT_MAX_H;
	}
	
	Status.screenRect.h = height;
	Status.numLines = Status.screenRect.h / ASCII_H;
	
	foogod_set_y(STAT_Y + Status.screenRect.h + BORDER_H);
	foogodRepaint();
	consoleRepaint();
	screen_repaint_frame();
}

static void switch_to_short_mode(void)
{
	int num_in_party = player_party->getSize();
	int party_height = (num_in_party * TILE_H);

	if (Status.screenRect.h == party_height)
		return;

	Status.numLines	= num_in_party;
	Status.screenRect.h = party_height;

	foogod_set_y(STAT_Y + Status.screenRect.h + BORDER_H);
	//console_set_y(foogod_get_y() + FOOGOD_H);

	foogodRepaint();
	consoleRepaint();
	screen_repaint_frame();
}

int statusInit()
{
	memset(&Status, 0, sizeof(Status));

	Status.screenRect.x = STAT_X;
	Status.screenRect.y = STAT_Y;
	Status.screenRect.w = STAT_W;
	Status.screenRect.h = TILE_H;
					
	Status.titleRect.x = STAT_X;
	Status.titleRect.y = 0;
	Status.titleRect.w = STAT_W;	// - (2 * BORDER_W);
	Status.titleRect.h = BORDER_H;

	Status.lineRect.x = Status.screenRect.x + TILE_W;
	Status.lineRect.y = Status.screenRect.y;
	Status.lineRect.w = Status.screenRect.w - TILE_W;
	Status.lineRect.h = LINE_H;

	Status.numLines = Status.screenRect.h / LINE_H;

	foogod_set_y(STAT_Y + Status.screenRect.h + BORDER_H);

        list_init(&Status.applet_stack);

#ifdef ztats_h
        ztats_init();
#endif
#ifdef ztats_pm_h
        ztats_pm_init();
#endif
#ifdef ztats_arms_h
        ztats_arms_init();
#endif
#ifdef ztats_reagents_h
        ztats_reagents_init();
#endif
#ifdef ztats_spells_h
        ztats_spells_init();
#endif
#ifdef ztats_items_h
        ztats_items_init();
#endif
#ifdef ztats_misc_h
        ztats_misc_init();
#endif

	return 0;
}

void status_set_title(const char *title)
{
	strncpy(Status.title, title, MAX_TITLE_LEN);
	Status.title[MAX_TITLE_LEN]=0;
}

void status_repaint_title(void)
{
	screenErase(&Status.titleRect);
	screenPrint(&Status.titleRect, SP_CENTERED | SP_ONBORDER, "%s", 
						Status.title);
	screenUpdate(&Status.titleRect);
}

static char status_arms_stat_color(char *dice)
{
	int avg = dice_average(dice);
	if (avg < 0)
		return STAT_PENALTY_CLR;
	if (avg > 0)
		return STAT_BONUS_CLR;
	return STAT_NULL_CLR;
}

/* status_show_arms_stats -- helper function to print the arms stats the same
 * way for all viewers. */
void status_show_arms_stats(SDL_Rect *rect, ArmsType *arms)
{
	char *hit   = arms->getToHitDice();
	char *dmg   = arms->getDamageDice();

	char *def   = arms->getToDefendDice();
	char *armor = arms->getArmorDice();

	screenPrint(rect, 0, 
		    "^c+%cHit:^c%c%s ^c%cDmg:^c%c%s ^c%cDef:^c%c%s ^c%cArm:^c%c%s^c-", 

		    STAT_LABEL_CLR,
		    status_arms_stat_color(hit), hit,
		    STAT_LABEL_CLR,
		    status_arms_stat_color(dmg), dmg,
		    STAT_LABEL_CLR,
		    status_arms_stat_color(def), def,
		    STAT_LABEL_CLR,
		    status_arms_stat_color(armor), armor
		);
	rect->y += (TILE_H - ASCII_H);
}

/* status_show_ready_arms -- called during the R)eady command, this shows
 * individual arms, indicating if they are available or already held by the
 * character in question */
static void status_show_ready_arms(SDL_Rect * rect, void *thing)
{
	struct inv_entry *ie = (struct inv_entry*)thing;
	ArmsType *arms = (ArmsType*)ie->type;
	int inUse = 0;
	int avail = ie->count - ie->ref;
	
	assert(ie->count);
	assert(avail >= 0);
	
	/* sprite */
	sprite_paint(arms->getSprite(), 0, rect->x, rect->y);
	rect->x += TILE_W;
	
	if (ie->ref && 
		player_party->getMemberAtIndex(Status.pcIndex)->
		hasReadied(arms)) {
			inUse = 1;
	}
	
	/* quantity and name */
	if (avail) {
		screenPrint(rect, 0, "^c+%c%2d%c%s^c-",
			(inUse?STAT_INUSE_CLR:STAT_NULL_CLR),
			avail,
			(inUse?'*':' '),
			arms->getName());
	} else {
		screenPrint(rect, 0, "^c+%c--%c%s^c-",
			(inUse?STAT_INUSE_CLR:STAT_UNAVAIL_CLR),
			(inUse?'*':' '),
			arms->getName());
	}
	rect->y += ASCII_H;
	
	/* stats */
	status_show_arms_stats(rect, arms);
	rect->x -= TILE_W;
}

/* status_range_color -- return red, green or yellow to reflect the relative
 * level of a statistic */
char status_range_color(int cur, int max)
{
	if (cur > max/2)
	{
		return STAT_OK_CLR;
	} 
	else if (cur > max/4)
	{
		return STAT_WARNING_CLR;
	} 
	else
	{
		return STAT_CRITICAL_CLR;
	}
}

static void status_show_character_var_stats(SDL_Rect *rect, 
                                            class Character *pm)
{
	/* Show the xp, hp and mp */
	/* Note that getXpForLevel(2) - getXpForLevel(1) != getXpForLevel(1)*/
	screenPrint(rect, 0, 
			"^c+%cHP:^c%c%d^cw/%d "
			"^c%cMP:^c%c%d^cw/%d "
			"^c%cAP:^c%c%d^cw/%d "
			"^c%cLvl:^cw%d^c%c(%d%%)^c-"
			, STAT_LABEL_CLR
			, status_range_color(pm->getHp(), pm->getMaxHp())
			, pm->getHp(), pm->getMaxHp()

			, STAT_LABEL_CLR
			, status_range_color(pm->getMana(), pm->getMaxMana())
			, pm->getMana(), pm->getMaxMana()

			, STAT_LABEL_CLR
			, status_range_color(pm->getActionPoints(), 
										pm->getActionPointsPerTurn())
			, pm->getActionPoints()
			, pm->getActionPointsPerTurn()

			, STAT_LABEL_CLR
			, pm->getLevel()
			, STAT_LABEL_CLR
			, (100 * (pm->getExperience()-pm->getXpForLevel(pm->getLevel()))/(pm->getXpForLevel(pm->getLevel()+1)-pm->getXpForLevel(pm->getLevel())))
		);
	rect->y += ASCII_H;
}

static void myShadeLines(int line, int n)
{
	SDL_Rect rect;
	rect.x = Status.screenRect.x;
	rect.y = Status.screenRect.y + line * TILE_H;
	rect.w = STAT_W;
	rect.h = ASCII_H * n;
	screenShade(&rect, 128);
}

static void myShadeHalfLines(int line, int n)
{
	SDL_Rect rect;
	rect.x = Status.screenRect.x;
	rect.y = Status.screenRect.y + line * ASCII_H;
	rect.w = STAT_W;
	rect.h = ASCII_H * n;
	screenShade(&rect, 128);
}

/* status_show_generic_object_type -- show a generic object type (just name and
 * quantity) */
static void status_show_generic_object_type(SDL_Rect *rect, void *thing)
{
	struct inv_entry *ie = (struct inv_entry*)thing;
	if (ie->type->getSprite()) {
				sprite_paint(ie->type->getSprite(), 0, rect->x, rect->y);
	}
	
	/* Indent past the sprite column. */
	rect->x += TILE_W;

	/* This is a single-line entry in a two-line rect, so center it
		* vertically. */
	rect->y += LINE_H / 4;

	screenPrint(rect, 0, "%2d %s", ie->count - ie->ref, 
					ie->type->getName());

	/* Carriage-return line-feed */
	rect->y += (LINE_H * 3) / 4;
	rect->x -= TILE_W;
}

/* status_show_mix_reagent -- show a reagent during the M)ix UI. Marks reagents
 * which have been selected for mixing. */
static void status_show_mix_reagent(SDL_Rect *rect, void *thing)
{
	struct inv_entry *ie = (struct inv_entry*)thing;
	if (ie->type->getSprite()) {
		sprite_paint(ie->type->getSprite(), 0, rect->x, rect->y);
	}
	
	/* Indent past the sprite column. */
	rect->x += TILE_W;
	
	/* This is a single-line entry in a two-line rect, so center it
		* vertically. */
	rect->y += LINE_H / 4;

	/* During mixing, if the ref field is set that means the reagent has
		* been selected to be part of the mixture (see cmdMix() in cmd.c, this
		* is something of a hack). Show an asterisk to mark selected
		* reagents. */
	screenPrint(rect, 0, "%2d%c%s", 
			ie->count, 
			(ie->ref ? '*':' '), 
			ie->type->getName());

	/* Carriage-return line-feed */
	rect->y += (LINE_H * 3) / 4;
	rect->x -= TILE_W;
}

/* stat_show_container -- generic function for showing the contents of a
 * container. Uses a filter to select certain types of contents for display,
 * and a specialized function for that type of content. */
static void stat_show_container()
{
	SDL_Rect rect;
	struct inv_entry *ie;
	int top = Status.topLine;
	int line = 0;

	rect = Status.screenRect;
	rect.h = LINE_H;

	for (ie = Status.container->first(Status.filter);
			ie != NULL; 
			ie = Status.container->next(ie, Status.filter))
	{

		/* Check the scrolling window */
		if (top) {
			top--;
			continue;
		}
	
		/* Use a specific function to show whatever it is. This should
		* advance the rect to the next entry position before it
		* returns. */
		Status.show_thing(&rect, ie);
	
		/* Highlight the selected item by shading all the other
		* entries. */
		if (Status.selectedEntry && ie != Status.selectedEntry) {
			myShadeLines(line, 2);
		}
	
		line++;
	
		/* Don't print outside the status window. */
		if (line >= N_LINES)
			break;
	}

}

static void stat_scroll_container(enum StatusScrollDir dir)
{
	struct inv_entry *tmp;
	int n_lines;
	int i;

	n_lines = Status.container->filter_count(Status.filter);

	if (!n_lines)
		return;

	switch (dir) {
	case ScrollUp:
		tmp = Status.container->prev(Status.selectedEntry, 
															Status.filter);
		if (!tmp)
			break;
		Status.selectedEntry = tmp;
		if (Status.topLine &&
			Status.curLine < (n_lines - Status.numLines / 2))
			Status.topLine--;
		Status.curLine--;
		break;
	case ScrollDown:
		tmp = Status.container->next(Status.selectedEntry, 
															Status.filter);
		if (!tmp)
			break;
		Status.selectedEntry = tmp;
		if (Status.topLine < Status.maxLine &&
			Status.curLine >= (Status.numLines / 2))
			Status.topLine++;
		Status.curLine++;
		break;
	case ScrollPageUp:
		for (i = 0; i < Status.numLines; i++) {
			stat_scroll_container(ScrollUp);
		}
		break;
	case ScrollPageDown:
		for (i = 0; i < Status.numLines; i++) {
			stat_scroll_container(ScrollDown);
		}
		break;
	default:
		break;
	}
}

/* status_show_effect_icon -- visitor function for
 * status_show_party_view_character_effects(), called for each effect on a
 * character and paints the status icons if the effect has one */
static int status_show_effect_icon(hook_entry_t *entry, void *data)
{
	SDL_Rect *rect = (SDL_Rect*)data;
	struct effect *eff = entry->effect;

	/* Skip effects which have no icon. */
	if (!eff->sprite)
		return 0;

	/* Blit the effect sprite. */
	sprite_paint(eff->sprite, 0, rect->x, rect->y);
	
	/* Shift the rectangle one left. */
	rect->x -= ASCII_W;
	
	/* If we hit the left edge abort. */
	if (rect->x == (Status.lineRect.x + BORDER_W)) {
		return -1;
	}

	return 0;
}

/* status_show_party_view_character_effects -- shows the party member's effects
 * as little mini-icons on the right side of the status line during Party View
 * mode. */
static void status_show_party_view_character_effects(class Character *pm, 
																	SDL_Rect *rect)
{
	int hook;

	/* remember the left edge for the limit check in the loop */
	int left_edge = rect->x;

	/* Start the rectangle on the far right */
	rect->x = rect->x + rect->w - ASCII_W;

	/* for each effect */
	for (hook = 0; hook < OBJ_NUM_HOOKS; hook++) {
		pm->hookForEach(hook, status_show_effect_icon, rect);
	}

	/* restore the left edge for the caller */
	rect->x = left_edge;
}

/* status_show_party_view_character_arms -- shows the party member's readied
 * arms as little mini-icons on the right side of the status line during Party
 * View mode. */
static void status_show_party_view_character_arms(class Character *pm, 
																SDL_Rect *rect)
{
	class ArmsType *arms = NULL;

	/* Use half the normal width for the arms icons. */
	const int ICON_W = TILE_W/2;

	/* remember the left edge for the limit check in the loop */
	int left_edge = rect->x;

	/* Start the rectangle on the far right */
	rect->x = rect->x + rect->w - ICON_W;

	/* Tell the sprite lib to scale down by 2x */
	sprite_zoom_out(2);
	screenZoomOut(2);

		/* for each readied armament */
	int armsIndex=0;
	for (arms = pm->enumerateArms(&armsIndex); arms != NULL; 
				arms = pm->getNextArms(&armsIndex))
	{

		/* blit it */
		sprite_paint(arms->getSprite(), 0, rect->x, rect->y);
		
		/* shift the rectangle one left */
		rect->x -= ICON_W;
		
		/* if we hit the left edge abort */
		if (rect->x == left_edge) {
					break;
		}
	}

	/* Tell the sprite lib to go back to unscaled sprites */
	screenZoomIn(2);
	sprite_zoom_in(2);
	
	/* restore the left edge for the caller */
	rect->x = left_edge;
}

/* status_show_party_view_character -- show a party member during Party View
 * mode. */
static bool status_show_party_view_character(class Character * pm, void *data)
{
	// check if we've scrolled too far
	if (Status.lineRect.y >= (Status.screenRect.y + Status.screenRect.h))
		return true;

	/* Paint the sprite. Using the time_stop_ticks as the frame arg to
         * animate during Time Stop. */
	sprite_paint(pm->getSprite(), Session->time_stop_ticks, Status.screenRect.x, 
		Status.lineRect.y);

	/* Paint the name on line 1 */
	screenPrint(&Status.lineRect, 0, "%-*s", MAX_NAME_LEN,
			pm->getName());

	/* Show the readied arms as scaled-down icons right-justified on line
		* 1 */
	status_show_party_view_character_arms(pm, &Status.lineRect);

	/* Go to line 2 */
	Status.lineRect.y += ASCII_H;

	/* Show character stats on line 2, left-justified. */
	status_show_character_var_stats(&Status.lineRect, pm);

        /* the above auto-advances; backup to show the condition codes on the
         * same line */
	Status.lineRect.y -= ASCII_H;

	/* Show the character effects as mini-icons right-justified on line
         * 2 */
	status_show_party_view_character_effects(pm, &Status.lineRect);
	Status.lineRect.y += ASCII_H;

	if (Status.pcIndex != -1 && pm->getOrder() != Status.pcIndex) {
		/* Highlight the selected party member by shading all the other
		* entries. */
		myShadeLines(Y_TO_LINE(Status.lineRect.y - 2 * ASCII_H), 2);
	}

	return false;
}

static void myShowParty(void)
{
        /* This can happen on reload. */
        if (! player_party) {
                return;
        }

	/* Setup the text rectangle */
	Status.lineRect.y = Status.screenRect.y;
	Status.lineRect.w = Status.screenRect.w - (2 * BORDER_W);

        /* Show members */
	player_party->forEachMember(status_show_party_view_character, 0);

}

static void myScrollParty(enum StatusScrollDir dir)
{
	switch (dir) {
	case ScrollDown:
	case ScrollRight:
	case ScrollPageDown:
		Status.pcIndex = (Status.pcIndex + 1) % player_party->getSize();
		break;
	case ScrollUp:
	case ScrollLeft:
	case ScrollPageUp:
		Status.pcIndex =
			(Status.pcIndex + player_party->getSize() - 1) % player_party->getSize();
		break;
	default:
		break;
	}
}

static void myPaintPage(void)
{
	screenBlit(Status.pg_surf, &Status.pg_rect, &Status.screenRect);
}

static void myScrollPage(enum StatusScrollDir dir)
{

	switch (dir) {
	case ScrollLeft:
	case ScrollRight:
		break;
	case ScrollUp:
		Status.pg_rect.y = max(Status.pg_rect.y - ASCII_H, 0);
		break;
	case ScrollDown:
		Status.pg_rect.y = min(Status.pg_max_y,
				Status.pg_rect.y + ASCII_H);
		break;
	case ScrollPageUp:
		Status.pg_rect.y = max(Status.pg_rect.y -
				(Status.pg_rect.h - ASCII_H), 0);
		break;
	case ScrollPageDown:
		Status.pg_rect.y = min(Status.pg_rect.y +
				(Status.pg_rect.h - ASCII_H), Status.pg_max_y);
		break;
	default:
		break;
	}
}

static int myFormatPgText()
{
	char *ptr;
	int n;
	int lines = 0;
	int normal_lines = 0;
	int added_lines = 0;
	int natural_breaks = 0;

	assert(Status.pg_text);

	ptr = Status.pg_text;
	n = 0;

	// Pass 1: wrap words
	if (*ptr)
		lines = 1;

	while (1)
	{
		n = 0;
		while (*ptr && n < STAT_CHARS_PER_LINE) {
			if (*ptr == '\n') {
				n = 0;
				lines++;
				normal_lines++;
			} else
				n++;
			ptr++;
		}

		/* End of message? */
		if (!*ptr)
			break;

		/* At this point ptr is at the next character after the last
		* one we're trying to fit on the line. If this character is a
		* space or the last character was a space then this is a
		* natural break in the line (i.e., we're not going to wrap a
		* word by breaking the line here). */
		if (isspace(*ptr) || isspace(*(ptr - 1))) {
			lines++;
			natural_breaks++;
			continue;
		}

		/* At this point we know that we do not have a natural break
		* and we're trying to wrap a word. We need to back up to find
		* the beginning of the word and insert a newline just prior.
		* But if the word is too long to fit on a single line then we
		* give up and just wrap the word. */
		ptr -= 2;
		n = 2;
		while (!isspace(*ptr) && n < STAT_CHARS_PER_LINE) {
			ptr--;
			n++;
		}

		if (n != STAT_CHARS_PER_LINE)
		{
			*ptr = '\n';
			added_lines++;
		}
		else
		{
			ptr += n;
		}
	}

	printf("lines=%d normal_lines=%d added_lines=%d natural_breaks=%d\n",
			lines, normal_lines, added_lines, natural_breaks);

	return lines;
}

static void mySetPageMode(void)
{
	int h, rows, x, y, c;
	char *ptr;

	assert(Status.pg_title);
	assert(Status.pg_text);

	rows = myFormatPgText();

	// Calculate how much space we need to hold all the text in a scratch
	// buffer.
	h = rows * ASCII_H;

	// If we can reuse the existing scratch buffer then do so. Otherwise
	// make a new one. If this fails then silently abort this
	// request. Fixme: need to adjust the status ifc to return errors.
	if (Status.pg_surf == NULL || Status.pg_surf->h < h)
	{
		if (Status.pg_surf) {
			SDL_FreeSurface(Status.pg_surf);
		}
		Status.pg_surf = screenCreateSurface(STAT_W, h);
		if (!Status.pg_surf)
			return;
	}
	// Render the text to the scratch surface.
	SDL_FillRect(Status.pg_surf, 0, Black);
	ptr = Status.pg_text;
	for (y = 0; y < rows && *ptr; y++)
	{
		for (x = 0, c = 0; x < (STAT_W / ASCII_W) && *ptr; x++)
		{
			if (*ptr == '\n')
			{
				ptr++;
				break;
			}
			if (!c && *ptr == ' ')
			{
				ptr++;
				continue;
			}
			if (asciiPaint(*ptr++, c * ASCII_W, y * ASCII_H,
					Status.pg_surf))
			{
				c++;
			}
		}
	}

	// Position the paging rect at the top of the window.
	Status.pg_rect.x = 0;
	Status.pg_rect.y = 0;
	Status.pg_rect.w = STAT_W;
	Status.pg_rect.h = TALL_H;

	// Set the srolling limit.
	Status.pg_max_y = max(0, h - Status.pg_rect.h);

	// Setup for viewing and scrolling.
	status_set_title(Status.pg_title);
	Status.paint = myPaintPage;
	Status.scroll = myScrollPage;

	// Clear the cmdwin and print instructions for exiting page mode.
	cmdwin_clear();
	cmdwin_push("(Hit ESC when done reading)");
}

static void myPaintTrade(void)
{
	SDL_Rect srect, nrect, prect, qrect;
	int i, top, line;

	if (!Status.list_sz)
		return;

	// *** Setup sprite, name and price rects ***

	srect = Status.screenRect;
	srect.w = TILE_W;
	srect.h = TILE_H;

	nrect = Status.screenRect;
	if (Status.trades[0].show_sprite) {
		// shift name left to leave room for sprite
		nrect.x += TILE_W;
		nrect.w -= TILE_W;
	}
	nrect.h = ASCII_H;

	qrect = Status.screenRect;
	if (Status.trades[0].show_sprite)
	{
		// shift quantity left to leave room for sprite
		qrect.x += TILE_W;
		qrect.w -= TILE_W;
	}
	
	qrect.y += ASCII_H;
	qrect.h = ASCII_H;

	prect = Status.screenRect;
	prect.x += TILE_W;
	prect.w -= TILE_W;
	prect.y += ASCII_H;
	prect.h = ASCII_H;

	// *** Show the entries in the current scroll window ***

	// The top line is the index at the top of the scrolled window. All
	// entries prior to this index are above the window and therefore
	// unseen.
	top = Status.topLine;

	line = 0;

	for (i = 0; i < Status.list_sz && line < N_LINES; i++)
	{
		// Skip entries until we encounter the index which is at the
		// top of our scrolled window.
		if (top) {
			top--;
			continue;
		}
		// sprite
		if (Status.trades[i].show_sprite) {
			sprite_paint(Status.trades[i].sprite, 0, srect.x,
					srect.y);
		}
		// name
		screenPrint(&nrect, 0, "%s", Status.trades[i].name);

		// quantity
		if (Status.trades[i].show_quantity) {
			screenPrint(&qrect, 0, "[You have %d]", 
												Status.trades[i].quantity);
		}
		// price
		screenPrint(&prect, SP_RIGHTJUSTIFIED, "%dgp",
				Status.trades[i].cost);

		// Shade unselected items.
		if (i != Status.curLine) {
			myShadeLines(line, 2);
		}
		line++;

		// Advance all the rectangles to the next line.
		srect.y += TILE_H;
		nrect.y += TILE_H;
		qrect.y += TILE_H;
		prect.y += TILE_H;
	}
}

static void statusPaintGenericList(void)
{
	SDL_Rect srect, l1rect, l2rect;
	int i, top, line;

	if (!Status.list_sz)
		return;

	// Setup sprite rect
	srect = Status.screenRect;
	srect.w = TILE_W;
	srect.h = TILE_H;

		// Setup line 1 rect1
	l1rect = Status.screenRect;
	if (Status.list[0].sprite) {
		l1rect.x += TILE_W;
		l1rect.w -= TILE_W;
	}
	l1rect.h = ASCII_H;

	// Setup line 2 rect
	l2rect = l1rect;
	l2rect.y += ASCII_H;

	// The top line is the index at the top of the scrolled window. All
	// entries prior to this index are above the window and therefore
	// unseen.
	top = Status.topLine;

	line = 0;

	for (i = 0; i < Status.list_sz && line < N_LINES; i++)
	{
		// Skip entries until we encounter the index which is at the
		// top of our scrolled window.
		if (top) {
			top--;
			continue;
		}

		// paint sprite (if applicable)
		if (Status.list[i].sprite) {
			sprite_paint(Status.list[i].sprite, 0, srect.x,
					srect.y);
		}

		// print line 1
		screenPrint(&l1rect, 0, "%s", Status.list[i].line1);

		// print line 2
		screenPrint(&l2rect, 0, "%s", Status.list[i].line2);


		// Shade unselected items.
		if (i != Status.curLine) {
			myShadeLines(line, 2);
		}
		line++;

		// Advance all the rectangles to the next line.
		srect.y += TILE_H;
		l1rect.y += TILE_H;
		l2rect.y += TILE_H;
	}
}

static void statusPaintStringList(void)
{
	SDL_Rect srect, l1rect;
	int i, top, line;

	if (!Status.list_sz)
		return;

	// Setup sprite rect
	srect = Status.screenRect;
	srect.w = TILE_W;
	srect.h = TILE_H;

	// Setup line rect
	l1rect = Status.screenRect;
	l1rect.h = ASCII_H;

	// The top line is the index at the top of the scrolled window. All
	// entries prior to this index are above the window and therefore
	// unseen.
	top = Status.topLine;

	line = 0;

	for (i = 0; i < Status.list_sz && line < N_LINES; i++)
	{
		// Skip entries until we encounter the index which is at the
		// top of our scrolled window.
		if (top) {
			top--;
			continue;
		}

		// print line 1
		screenPrint(&l1rect, 0, "%s", Status.strlist[i]);

		// Shade unselected items.
		if (i != Status.curLine) {
			myShadeHalfLines(line, 1);
		}
		line++;

		// Advance all the rectangles to the next line.
		srect.y += ASCII_H;
		l1rect.y += ASCII_H;
	}
}

static void myScrollGeneric(enum StatusScrollDir dir)
{
	int i;

	switch (dir)
	{
	case ScrollUp:
		// If the window is not at the top of the list and the current
		// line is centered in the window then move the window up.
		if (Status.topLine &&
				Status.curLine < (Status.list_sz - Status.numLines / 2))
		{
			Status.topLine--;
		}
		// Move up the currently selected line.
		if (Status.curLine)
			Status.curLine--;
		break;
		
	case ScrollDown:
		// If the window is not at the bottom of the list and the
		// current line is centered in the window then move the window
		// down.
		if (Status.topLine < Status.maxLine &&
			Status.curLine >= (Status.numLines / 2))
		{
			Status.topLine++;
		}
		if (Status.curLine < (Status.list_sz - 1))
			Status.curLine++;
		break;
		
	case ScrollTop:
		Status.topLine = 0;
		Status.curLine = 0;
		break;
		
	case ScrollBottom:
		Status.curLine = Status.list_sz - 1;
		if (Status.list_sz > Status.numLines)
		{
			Status.topLine = Status.list_sz - Status.numLines;
		} 
		else
		{
			Status.topLine = 0;
		}
		break;
		
	case ScrollPageUp:
		for (i = 0; i < Status.numLines; i++)
		{
			myScrollGeneric(ScrollUp);
		}
		break;
		
	case ScrollPageDown:
		for (i = 0; i < Status.numLines; i++)
		{
			myScrollGeneric(ScrollDown);
		}
		break;
		
	default:
		break;
	}
}

void statusRepaint(void)
{
	static int repainting = 0;
        struct applet *applet;

	if (Status.suppressRepaint)
		return;

	// Prevent recursive entry since it messes up the coordinate counters
	if (repainting)
		return;
		
	repainting = 1;

	screenErase(&Status.screenRect);

        if (Status.paint) {
                Status.paint();
        } else if ((applet = statusGetCurrentApplet())) {
                applet->ops->paint(applet);
        }


	status_repaint_title();
	screenUpdate(&Status.screenRect);

	repainting = 0;
}

void statusFlash(int line, unsigned int color)
{
	enum StatusMode omode = Status.mode;
	statusSetMode(ShowParty);

	Status.lineRect.y = Status.screenRect.y + line * TILE_H;
	if (Status.lineRect.y >= (Status.screenRect.y + Status.screenRect.h))
		return;
	Status.lineRect.w = Status.screenRect.w - TILE_W;
	screenFlash(&Status.lineRect, 50, color);
	statusRepaint();

	statusSetMode(omode);
	statusRepaint();
}

void statusScroll(enum StatusScrollDir dir)
{
	assert(Status.scroll);
	Status.scroll(dir);
	statusRepaint();
}

void statusSetMode(enum StatusMode mode)
{
	/* Unref the old super generic struct if applicable */
	if (Status.mode == SuperGeneric)
	{
		assert(Status.super_generic);
		if (Status.super_generic->unref)
		{
			Status.super_generic->unref(Status.super_generic);
		}
		Status.super_generic = 0;
	}

	Status.mode = mode;

	/* note: must always repaint title AFTER switching mode because
		* switching modes repaints the border, and the * title must be painted
		* over the border. */

	switch (mode)
	{
		
	case DisableStatus:
		Status.paint=0;
		break;
		
	case ShowParty:
		switch_to_short_mode();
		status_set_title("Party");	
		Status.pcIndex = -1;
		Status.scroll = 0;
		Status.paint = myShowParty;
		break;
		
	case SelectCharacter:
		switch_to_tall_mode();
		status_set_title("Select Member");
		Status.scroll = myScrollParty;
		Status.paint = myShowParty;
		Status.pcIndex = 0;
		break;
		
	case Ready:
		switch_to_tall_mode();
		status_set_title(player_party->
				getMemberAtIndex(Status.pcIndex)->getName());
		Status.topLine = 0;
		Status.curLine = 0;
		Status.container = player_party->inventory;
		Status.filter = &stat_ready_arms_filter;
		Status.maxLine = Status.container->
				filter_count(Status.filter) - Status.numLines;
		Status.paint = stat_show_container;
		Status.scroll = stat_scroll_container;
		Status.show_thing = status_show_ready_arms;
		Status.selectedEntry = Status.container->first(Status.filter);
		break;
		
	case Use:
		switch_to_tall_mode();
		status_set_title("Use");
		Status.topLine = 0;
		Status.curLine = 0;
		Status.container = player_party->inventory;
		Status.filter = &stat_use_filter;
		Status.maxLine = Status.container->
				filter_count(Status.filter) - Status.numLines;
		Status.paint = stat_show_container;
		Status.scroll = stat_scroll_container;
		Status.show_thing = status_show_generic_object_type;
		Status.selectedEntry = Status.container->first(Status.filter);
		break;
		
	case Page:
		switch_to_tall_mode();
		mySetPageMode();
		break;
		
	case Trade:
		switch_to_tall_mode();
		status_set_title("Trade");
		Status.topLine = 0;
		Status.curLine = 0;
		Status.maxLine = Status.list_sz - Status.numLines;
		Status.paint = myPaintTrade;
		Status.scroll = myScrollGeneric;
		Status.selectedEntry = 0;
		break;
		
	case MixReagents:
		switch_to_tall_mode();
		status_set_title("Select Reagents");
		Status.topLine = 0;
		Status.curLine = 0;
		Status.container = player_party->inventory;
		Status.filter = &stat_mix_filter;
		Status.maxLine = Status.container->
				filter_count(Status.filter) - Status.numLines;
		Status.paint = stat_show_container;
		Status.scroll = stat_scroll_container;
		Status.show_thing = status_show_mix_reagent;
		Status.selectedEntry = Status.container->first(Status.filter);
		break;
		
	case GenericList:
		switch_to_tall_mode();
		status_set_title(Status.list_title);
		Status.topLine = 0;
		Status.curLine = 0;
		Status.maxLine = Status.list_sz - Status.numLines;
		Status.paint = statusPaintGenericList;
		Status.scroll = myScrollGeneric;
		Status.selectedEntry = 0;					
		break;
		
	case StringList:
		status_set_line_height(max(Status.list_sz, 5));
		status_set_title(Status.list_title);
		Status.topLine = 0;
		Status.curLine = 0;
		Status.maxLine = Status.list_sz - Status.numLines;
		Status.paint = statusPaintStringList;
		Status.scroll = myScrollGeneric;
		Status.selectedEntry = 0;					
		break;
		
	case SuperGeneric:
		assert(Status.super_generic);
		switch_to_tall_mode();
		status_set_title(Status.super_generic->title);
		Status.topLine = 0;
		Status.curLine = 0;
		Status.maxLine = node_list_len(&Status.super_generic->list) 
				- Status.numLines;
		Status.paint = stat_super_generic_paint;
		Status.scroll = stat_super_generic_scroll;
                Status.super_generic->last_shown = 0; /* unknown */
                Status.super_generic->first_to_selected = 0;
                Status.super_generic->first_to_last = 0; /* unknown */
		if (node_list_empty(&Status.super_generic->list)) {
			Status.super_generic->first_shown = 0;
		} else {
			Status.super_generic->first_shown = node_next(&Status.super_generic->list);
		}
		Status.super_generic->selected = Status.super_generic->first_shown;
		break;
	}

	statusRepaint();
}

const void *statusGetSelected(enum StatusSelection sel)
{
	switch (sel)
	{
	case Character:
		return player_party->getMemberAtIndex(Status.pcIndex);
		
	case InventoryItem:
	case Reagents:
		return Status.selectedEntry;
		
	case Generic:
		return Status.list_sz ? Status.list[Status.curLine].data : 0;
		
	case TradeItem:
		return Status.list_sz ? &Status.trades[Status.curLine] : 0;
		
	case String:
		return Status.list_sz ? Status.strlist[Status.curLine] : 0;
		
	case SelectSuperGeneric:
		assert(Status.super_generic);
		return Status.super_generic->selected;
		break;
		
	default:
		return 0;
	}
}

int statusGetSelectedIndex(enum StatusSelection sel)
{
	switch (sel)
	{
	case Character:
		return Status.pcIndex;
		
	case Generic:
	case TradeItem:
	case String:
		return Status.list_sz ? Status.curLine : -1;
		
	default:
		return -1;
	}
}

void statusSetSelectedIndex(int index)
{
	switch (Status.mode)
	{
	case StringList:
	case Trade:
	case GenericList:
		while (index < Status.curLine) {
					Status.scroll(ScrollUp);
		}
		while (index > Status.curLine) {
					Status.scroll(ScrollDown);
		}
		/*Status.curLine = index;*/
		statusRepaint();
		break;
		
	default:
		assert(0);
		break;
	}
}

void statusSelectCharacter(int partyOrderIndex)
{
	Status.pcIndex = partyOrderIndex;
}

void statusSetPageText(const char *title, const char *text)
{
	Status.pg_title = title;
        if (Status.pg_text) {
                free(Status.pg_text);
                Status.pg_text = NULL;
        }
        /* Make a writable copy so we can insert newlines during formatting. */
	Status.pg_text = strdup(text);
}

void statusSetTradeInfo(int list_sz, struct trade_info *trades)
{
	Status.list_sz = list_sz;
	Status.trades = trades;
}

void statusUpdateTradeInfo(int list_sz, struct trade_info *trades)
{
	Status.list_sz = list_sz;
	Status.trades = trades;
	Status.maxLine = Status.list_sz - Status.numLines;
	if (Status.curLine >= list_sz)
		Status.curLine = max(0, list_sz - 1);
	statusRepaint();
}

void statusSetGenericList(const char *title, int list_sz, 
                          struct stat_list_entry *list)
{
	Status.list_title = title;
	Status.list_sz = list_sz;
	Status.list	= list;
}

void statusSetStringList(const char *title, int list_sz, const char **strings)
{
	Status.list_title = title;
	Status.list_sz = list_sz;
	Status.strlist = strings;
}

enum StatusMode statusGetMode(void)
{
	return Status.mode;
}

int status_get_h(void)
{
	return Status.screenRect.h;
}

void statusFlashSelected(unsigned int color)
{
	SDL_Rect rect;
	switch (Status.mode)
	{
	case StringList:
	case Trade:
	case GenericList:
		rect.x = Status.screenRect.x;
		rect.y = (Status.screenRect.y 
					+ ((Status.curLine - Status.topLine) 
						* ASCII_H));
		if (rect.y >= (Status.screenRect.y 
							+ Status.screenRect.h))
					return;
		rect.w = Status.screenRect.w;
		rect.h = ASCII_H;
		screenFlash(&rect, 50, color);
		statusRepaint();
		break;
		
	default:
		break;
	}
}

void statusDisableRepaint()
{
	Status.suppressRepaint++;
}

void statusEnableRepaint()
{
	assert(Status.suppressRepaint);
	Status.suppressRepaint--;		
}

void statusPushMode(enum StatusMode mode)
{
	assert(Status.top < STAT_MODE_STACK_DEPTH);
	Status.stack[Status.top] = Status.mode;
	Status.top++;
	statusSetMode(mode);
}

void statusPopMode(void)
{
	assert(Status.top > 0);
	Status.top--;
	statusSetMode(Status.stack[Status.top]);
}

static void stat_super_generic_paint()
{
	SDL_Rect rect = Status.screenRect;
	struct node *node = Status.super_generic->first_shown;
	struct node *end = &Status.super_generic->list;
	int window_bottom = Status.screenRect.y + Status.screenRect.h;
        Status.super_generic->first_to_last = 0;

	/* check for empty list */
	if (!node)
	{
		screenPrint(&rect, 0, "No Skills!");
		return;
	}

	for (;;)
	{
		/* check for end-of-list */
		if (node == end) {
                        break;
		}
		
		/* check for bottom of window */
		if (rect.y >= window_bottom) {
                        break;
		}

		/* Clip the rect to fit */
		rect.h = window_bottom - rect.y;

		/* Paint the entry. If it won't fit it will return non-zero,
                 * and we won't advance any further. */
		if (Status.super_generic->paint(Status.super_generic,
                                                  node,
                                                  &rect)) {
                        break;
                }

                /* Remember the last one painted */
                Status.super_generic->last_shown = node;
                Status.super_generic->first_to_last++;
                
                /* Advance the list */
                node = node_next(node);
	}

        Status.super_generic->first_to_last--; /* overcounted by 1 */
}

static void stat_super_generic_scroll(enum StatusScrollDir dir)
{
	struct stat_super_generic_data *gen = Status.super_generic;
	struct node *first = node_next(&gen->list);
	struct node *last = node_prev(&gen->list);

	/* Check for trivial case: empty list */
	if (node_list_empty(&gen->list)) {
		return;
	}

	switch (dir)
	{
	case ScrollPageUp:
	case ScrollUp:
		if (gen->selected != first) {
			gen->selected = node_prev(gen->selected);
                        
                        /* If the selected item has reached the middle of the
                         * window, and there is more stuff to see above the
                         * window, then move the window up the list. */
                        int selected_to_last = gen->first_to_last - gen->first_to_selected;
                        if ((gen->first_shown != first)
                            && (selected_to_last > gen->first_to_selected)) {
                                gen->first_shown = node_prev(gen->first_shown);
                        } else {
                                gen->first_to_selected--;
                        }
		}
		break;
		
	case ScrollPageDown:
	case ScrollDown:
		if (gen->selected != last) {
			gen->selected = node_next(gen->selected);
                        
                        /* If the selected item has reached the middle of the
                         * window, and there is more stuff to see below the
                         * window, then move the window down the list. */
                        int selected_to_last = gen->first_to_last - gen->first_to_selected;
                        if ((gen->last_shown != last)
                            && (selected_to_last <= gen->first_to_selected)) {
                                gen->first_shown = node_next(gen->first_shown);
                        } else {
                                gen->first_to_selected++;
                        }
		}
		break;
		
	case ScrollTop:
		gen->selected = first;
		break;
		
	case ScrollBottom:
		gen->selected = last;
		break;
		
	default:
		break;
	}
}

void statusSetSuperGenericData(struct stat_super_generic_data *data)
{
	if (data == Status.super_generic) {
		return;
	}
	
	if (Status.super_generic
		&& Status.super_generic->unref) {
			Status.super_generic->unref(Status.super_generic);
	}
	
	Status.super_generic = data;
	data->refcount++;
}

void statusBrowseContainer(class Container *container, const char *title)
{
	switch_to_tall_mode();
	status_set_title(title);
	Status.topLine = 0;
	Status.curLine = 0;
	Status.container = container;
	Status.filter = &stat_drop_filter;
	Status.maxLine = container->filter_count(Status.filter) - Status.numLines;
	Status.paint = stat_show_container;
	Status.scroll = stat_scroll_container;
	Status.show_thing = status_show_generic_object_type;
	Status.selectedEntry = container->first(Status.filter);
        statusRepaint();
}

void statusRunApplet(struct applet *applet)
{
        /* set the window height */
        if (! applet->ops->get_desired_height) {
                switch_to_tall_mode();
        } else {
                int hpix = applet->ops->get_desired_height(applet);
                hpix = min(hpix, TALL_H);
                hpix = max(hpix, LINE_H);
                status_set_line_height(hpix/ASCII_H);
        }

        list_add(&Status.applet_stack, &applet->list); /* push */
        Status.paint = NULL; /* so statusRepaint will use applet->ops->paint */
        applet->ops->run(applet, &Status.screenRect, Session); /* returns when applet done */
        list_remove(&applet->list); /* pop */
}

struct applet *statusGetCurrentApplet(void)
{
        if (! list_empty(&Status.applet_stack)) {
                return list_entry(Status.applet_stack.next, struct applet, list);
        }
        return NULL;
}
