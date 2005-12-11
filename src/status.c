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
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
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

#include <stdio.h>
#include <assert.h>
#include <ctype.h>

#define LINE_H TILE_H
#define N_CHARS_PER_LINE STAT_MAX_CHARS_PER_LINE
#define Y_TO_LINE(Y) (((Y) - Status.screenRect.y) / TILE_H)
#define N_LINES Status.numLines	/* (STAT_H / TILE_H) */

#define TALL_H (SCREEN_H - 4 * BORDER_H - 6 * ASCII_H)

/*
 * Armaments - R)eady-player
 * Reagents  - M)ix
 * Spells    - C)ast
 * Items     - U)se
 */

enum ZtatsView {
	ViewMember = 0,
	ViewArmaments,
	ViewReagents,
	ViewSpells,
	ViewItems,
        ViewMisc,
	NumViews,
};

static bool stat_filter_arms(struct inv_entry *ie, void *cookie);
static bool stat_filter_ready_arms(struct inv_entry *ie, void *cookie);
static bool stat_filter_reagents(struct inv_entry *ie, void *cookie);
static bool stat_filter_spells(struct inv_entry *ie, void *cookie);
static bool stat_filter_items(struct inv_entry *ie, void *cookie);
static bool stat_filter_misc(struct inv_entry *ie, void *cookie);

static char *ZtatsTitles[] = {
	"Party Member",
	"Armaments",
	"Reagents",
	"Spells",
	"Usable Items",
        "Misc"
};

static struct filter ZtatsFilters[] = {
        { 0, 0 },
        { stat_filter_arms, 0 },
        { stat_filter_reagents, 0 },
        { stat_filter_spells, 0 },
        { stat_filter_items, 0 },
        { stat_filter_misc, 0 },
};

static struct filter stat_ready_arms_filter = {
        stat_filter_ready_arms, 0
};

static struct status {
	SDL_Rect titleRect;
	SDL_Rect screenRect;
	SDL_Rect lineRect;
	void (*paint) (void);
	void (*scroll) (enum StatusScrollDir);
	int pcIndex;
	struct inv_entry *selectedEntry;
	enum ZtatsView ztatsView;
	enum StatusMode mode;
	int topLine;
	int maxLine;
	int numLines;
	int curLine;

	char *pg_title, *pg_text;
	SDL_Surface *pg_surf;
	SDL_Rect pg_rect;
	int pg_max_y;

	int list_sz;
	struct trade_info *trades;
        struct stat_list_entry *list;
        char **strlist;

        Container *container;
        struct filter *filter;

} Status;

static bool stat_filter_arms(struct inv_entry *ie, void *cookie)
{
        return (ie->type->isReadyable());
}

static bool stat_filter_ready_arms(struct inv_entry *ie, void *cookie)
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
            hasReadied((class ArmsType *)ie->type)) {
                return true;
        }

        return false;
}

static bool stat_filter_reagents(struct inv_entry *ie, void *cookie)
{
        return ie->type->isMixable();
}

static bool stat_filter_spells(struct inv_entry *ie, void *cookie)
{
        return ie->type->isCastable();
}

static bool stat_filter_items(struct inv_entry *ie, void *cookie)
{
        return ie->type->isUsable();
}

static bool stat_filter_misc(struct inv_entry *ie, void *cookie)
{
        /* Things that don't fall into any of the other categories */
        return (! ie->type->isReadyable()
                && ! ie->type->isMixable()
                && ! ie->type->isCastable()
                && ! ie->type->isUsable());
}

static void switch_to_tall_mode(void)
{
	if (Status.screenRect.h == TALL_H)
		return;

	Status.screenRect.h = TALL_H;
	Status.numLines     = Status.screenRect.h / LINE_H;

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

    Status.numLines     = num_in_party;
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
	Status.screenRect.h = player_party->getSize() * TILE_H;

	Status.titleRect.x = STAT_X;
	Status.titleRect.y = 0;
	Status.titleRect.w = STAT_W;	// - (2 * BORDER_W);
	Status.titleRect.h = BORDER_H;

	Status.lineRect.x = Status.screenRect.x + TILE_W;
	Status.lineRect.y = Status.screenRect.y;
	Status.lineRect.w = Status.screenRect.w - TILE_W;
	Status.lineRect.h = LINE_H;

	Status.numLines = Status.screenRect.h / LINE_H;

        return 0;
}

static void myRepaintTitle(char *title)
{
	screenErase(&Status.titleRect);
	screenPrint(&Status.titleRect, SP_CENTERED | SP_ONBORDER, "%s", title);
	screenUpdate(&Status.titleRect);
}

static void myShowMemberArms(SDL_Rect * rect, class ArmsType * arms, char *slot)
{
	rect->y += ASCII_H;
	spritePaint(arms->getSprite(), 0, rect->x, rect->y);
	rect->x += TILE_W;
	rect->y += TILE_H / 4;
	screenPrint(rect, 0, "%s", arms->getName());
	rect->y += (TILE_H * 3) / 4;
	rect->x -= TILE_W;
}

static void myShowMember(void)
{
	SDL_Rect rect;
	int pad;
	class Character *pm;
        struct mmode *mmode;
        
	// screenErase(&Status.screenRect);

	rect = Status.screenRect;
	pad = (STAT_W / ASCII_W) - 17;
	assert(pad >= 1);
	pm = player_party->getMemberAtIndex(Status.pcIndex);
	assert(pm);

	/* Show the sex symbol */

	/* Show the level and class */
	screenPrint(&rect, 0, "Lvl=%3d%*cXP:%7d", pm->getLevel(), pad, ' ',
		    pm->getExperience());

	/* Show strength and hp */
	rect.y += ASCII_H;
	screenPrint(&rect, 0, "Str=%3d%*cHP:%3d/%3d", pm->getStrength(), pad,
		    ' ', pm->getHp(), pm->getMaxHp());

	/* Show intelligence and mana */
	rect.y += ASCII_H;
	screenPrint(&rect, 0, "Int=%3d%*cMP:%3d/%3d", pm->getIntelligence(),
		    pad, ' ', pm->getMana(), pm->getMaxMana());

	/* Show dexterity and armour class */
	rect.y += ASCII_H;
	screenPrint(&rect, 0, "Dex=%3d%*cAC:%3d", pm->getDexterity(), pad,
		    ' ', pm->getArmourClass());

        // Show Movement Modes (pmask for now)
        rect.y += ASCII_H;
        // SAM: What is wanted here is a function to turn a pmask 
        //      into one or more words of text (names of movement modes).
        mmode = pm->getMovementMode();
        if (mmode)
                screenPrint(&rect, 0, "Move:%s", mmode->name);

	/* Show arms */
	rect.y += ASCII_H;
	rect.y += ASCII_H;
	screenPrint(&rect, SP_CENTERED, "*** Arms ***");

	class ArmsType *arms = pm->enumerateArms();
	while (arms != NULL) {
		myShowMemberArms(&rect, arms, "null");
		arms = pm->getNextArms();
	}

	// fixme: currently this will overprint and it doesn't support
	// scrolling. These may be necessary if the status window is not large
	// enough.
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

static void stat_show_container()
{
	SDL_Rect rect;
	struct inv_entry *ie;
	int top = Status.topLine;
	int flags, line = 0;

	rect = Status.screenRect;
	rect.x += TILE_W;
	rect.h = LINE_H;

        for (ie = Status.container->first(Status.filter);
             ie != NULL; 
             ie = Status.container->next(ie, Status.filter)) {

		int inUse = 0;
		int avail = 0;

		/* Check the scrolling window */
		if (top) {
			top--;
			continue;
		}

		avail = ie->count;

		/* For Ready mode, check if any of the items are worn by the
		 * selected member */
		if (Status.mode == Ready) {
			avail -= ie->ref;
			assert(avail >= 0);
			if (ie->ref && 
                            player_party->getMemberAtIndex(Status.pcIndex)->
                            hasReadied((class ArmsType *)ie->type)) {
				inUse = 1;
			}
		}
		// If mixing reagents then check if this reagent has already
		// been selected. I'm going to cheat a bit here and reuse the
		// 'ref' field of the inv_entry to mean that the reagent has
		// been selected. Must clear this field when done mixing!
		else if (Status.mode == MixReagents) {
			inUse = ie->ref;
		}

		flags = 0;

		/* If there are no free items and none are worn by the current
		 * player then don't list them. */
		if (!avail && !inUse)
			continue;

                if (ie->type->getSprite()) {
                        spritePaint(ie->type->getSprite(), 0, 
                                    Status.screenRect.x,
                                    rect.y);
                }

                rect.y += LINE_H / 4;

		if (avail)
			screenPrint(&rect, flags, "%2d%c%s", avail,
				    (inUse ? '*' : ' '), ie->type->getName());
		else
			screenPrint(&rect, flags, "--%c%s",
				    (inUse ? '*' : ' '), ie->type->getName());

                rect.y += (LINE_H * 3) / 4;

		if (Status.selectedEntry && ie != Status.selectedEntry) {
			/* Highlight the selected item by shading all the other
			 * entries. */
			myShadeLines(line, 2);
		}

		line++;

		// Don't print outside the status window
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

static bool myShowPCInPartyView(class Character * pm, void *data)
{
	int flags;
	char condition[32];

	// flags = (pm->getOrder() == Status.pcIndex ? SP_INVERTED : 0);
	flags = 0;

	// check if we've scrolled too far
	if (Status.lineRect.y >= (Status.screenRect.y + Status.screenRect.h))
		return true;

	/* Paint the sprite */
	spritePaint(pm->getSprite(), 0, Status.screenRect.x, Status.lineRect.y);

	/* Paint the name */
	screenPrint(&Status.lineRect, flags, "%-*s", MAX_NAME_LEN,
		    pm->getName());
	Status.lineRect.y += ASCII_H;

	/* Paint the condition */
        strncpy(condition, pm->getCondition(), array_sz(condition) - 1);
	screenPrint(&Status.lineRect, SP_RIGHTJUSTIFIED, "%d%s",
		    pm->getHp(), condition);
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
	/* Setup the text rectangle */
	Status.lineRect.y = Status.screenRect.y;
	Status.lineRect.w = Status.screenRect.w - TILE_W;

	player_party->forEachMember(myShowPCInPartyView, 0);

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
		    (Status.pcIndex + player_party->getSize() -
		     1) % player_party->getSize();
		break;
	}
}

static int myScrollMemberZtatsHorz(int d)
{

	if (d > 0 && Status.pcIndex < (player_party->getSize() - 1)) {
		Status.pcIndex++;
		return 0;
	}

	else if (d < 0 && Status.pcIndex > 0) {
		Status.pcIndex--;
		return 0;
	}

	return 1;
}

static void myScrollZtatsHorz(int d)
{
	if (Status.ztatsView != ViewMember || myScrollMemberZtatsHorz(d)) {
		Status.ztatsView = (enum ZtatsView) ((NumViews +
						      Status.ztatsView + d) %
						     NumViews);
		Status.topLine = 0;

		/* init new view */
		switch (Status.ztatsView) {

		case ViewMember:
			Status.pcIndex = (d > 0 ? 0 : player_party->getSize() - 1);
			break;

		default:
                        Status.container = player_party->inventory;
                        Status.filter = &ZtatsFilters[Status.ztatsView];
                        Status.maxLine = 
                                Status.container->filter_count(Status.filter) -
                                Status.numLines;
			break;
		}
	}

	Status.maxLine = max(Status.maxLine, 0);

	if (Status.ztatsView == ViewMember)
		myRepaintTitle(player_party->getMemberAtIndex(Status.pcIndex)->getName());
	else
		myRepaintTitle(ZtatsTitles[Status.ztatsView]);
}

static void myScrollZtats(enum StatusScrollDir dir)
{
	switch (dir) {
	case ScrollLeft:
		myScrollZtatsHorz(-1);
		break;
	case ScrollRight:
		myScrollZtatsHorz(1);
		break;
	case ScrollUp:
		if (Status.topLine)
			Status.topLine--;
		break;
	case ScrollDown:
		if (Status.topLine < Status.maxLine)
			Status.topLine++;
		break;
	case ScrollPageUp:
		Status.topLine -= Status.numLines;
		if (Status.topLine < 0)
			Status.topLine = 0;
		break;
	case ScrollPageDown:
		Status.topLine = min(Status.maxLine,
				     Status.topLine + Status.numLines);
		break;
	}
}

static void myShowZtats(void)
{
	switch (Status.ztatsView) {
	case ViewMember:
		myShowMember();
		break;
	default:
                stat_show_container();
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
				       (Status.pg_rect.h - ASCII_H),
				       Status.pg_max_y);
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

	while (1) {

		n = 0;
		while (*ptr && n < N_CHARS_PER_LINE) {
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
		while (!isspace(*ptr) && n < N_CHARS_PER_LINE) {
			ptr--;
			n++;
		}

		if (n != N_CHARS_PER_LINE) {
			*ptr = '\n';
			added_lines++;
		} else
			ptr += n;
	};

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
	if (Status.pg_surf == NULL || Status.pg_surf->h < h) {
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
	for (y = 0; y < rows && *ptr; y++) {
		for (x = 0, c = 0; x < (STAT_W / ASCII_W) && *ptr; x++) {
			if (*ptr == '\n') {
				ptr++;
				break;
			}
			if (!c && *ptr == ' ') {
				ptr++;
				continue;
			}
			asciiPaint(*ptr++, c * ASCII_W, y * ASCII_H,
				   Status.pg_surf);
			c++;
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
	myRepaintTitle(Status.pg_title);
	Status.paint = myPaintPage;
	Status.scroll = myScrollPage;

        // Clear the cmdwin and print instructions for exiting page mode.
        cmdwin_clear();
        cmdwin_print("(Hit ESC when done reading)");
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
	if (Status.trades[0].show_sprite) {
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

	for (i = 0; i < Status.list_sz && line < N_LINES; i++) {

		// Skip entries until we encounter the index which is at the
		// top of our scrolled window.
		if (top) {
			top--;
			continue;
		}
		// sprite
		if (Status.trades[i].show_sprite) {
			spritePaint(Status.trades[i].sprite, 0, srect.x,
				    srect.y);
		}
		// name
		screenPrint(&nrect, 0, "%s", Status.trades[i].name);

		// quantity
		if (Status.trades[i].show_quantity) {
			screenPrint(&qrect, 0, "[%d]", 
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

	for (i = 0; i < Status.list_sz && line < N_LINES; i++) {

		// Skip entries until we encounter the index which is at the
		// top of our scrolled window.
		if (top) {
			top--;
			continue;
		}

		// paint sprite (if applicable)
		if (Status.list[i].sprite) {
			spritePaint(Status.list[i].sprite, 0, srect.x,
				    srect.y);
		}

		// print line 1
		screenPrint(&l1rect, 0, "%s", Status.list[i].line1);

		// print line 2
		screenPrint(&l1rect, 0, "%s", Status.list[i].line1);


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

	for (i = 0; i < Status.list_sz && line < N_LINES; i++) {

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
	switch (dir) {
	case ScrollUp:
		// If the window is not at the top of the list and the current
		// line is centered in the window then move the window up.
		if (Status.topLine &&
		    Status.curLine < (Status.list_sz - Status.numLines / 2))
			Status.topLine--;
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
			Status.topLine++;
		if (Status.curLine < (Status.list_sz - 1))
			Status.curLine++;
		break;
	default:
		break;
	}
}

void statusRepaint(void)
{
        static int repainting = 0;

        // Check if we're early in startup and haven't set the paint function
        // yet.
        if (! Status.paint)
                return;

        // Prevent recursive entry since it messes up the coordinate counters
        if (repainting)
                return;
        repainting = 1;

	screenErase(&Status.screenRect);
	Status.paint();
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
	Status.mode = mode;

        /* note: must always repaint title AFTER switching mode because
         * switching modes repaints the border, and the * title must be painted
         * over the border. */

	switch (mode) {
	case ShowParty:
		switch_to_short_mode();
		myRepaintTitle("Party");	
		Status.pcIndex = -1;
		Status.scroll = 0;
		Status.paint = myShowParty;
		break;
	case SelectCharacter:
		switch_to_tall_mode();
		myRepaintTitle("select");
		Status.scroll = myScrollParty;
		Status.paint = myShowParty;
		Status.pcIndex = 0;
		break;
	case Ztats:
		switch_to_tall_mode();
		myRepaintTitle(player_party->
                               getMemberAtIndex(Status.pcIndex)->getName());
		Status.ztatsView = ViewMember;
		Status.selectedEntry = 0;
		Status.topLine = 0;
		Status.paint = myShowZtats;
		Status.scroll = myScrollZtats;
		break;
	case Ready:
		switch_to_tall_mode();
		myRepaintTitle(player_party->
                               getMemberAtIndex(Status.pcIndex)->getName());
		Status.topLine = 0;
		Status.curLine = 0;
		Status.container = player_party->inventory;
                Status.filter = &stat_ready_arms_filter;
		Status.maxLine = Status.container->
                        filter_count(Status.filter) - Status.numLines;
		Status.paint = stat_show_container;
                Status.scroll = stat_scroll_container;
		Status.selectedEntry = Status.container->first(Status.filter);
		break;
	case Use:
		switch_to_tall_mode();
		myRepaintTitle("select");
		Status.topLine = 0;
		Status.curLine = 0;
		Status.container = player_party->inventory;
                Status.filter = &ZtatsFilters[ViewItems];
		Status.maxLine = Status.container->
                        filter_count(Status.filter) - Status.numLines;
		Status.paint = stat_show_container;
                Status.scroll = stat_scroll_container;
		Status.selectedEntry = Status.container->first(Status.filter);
		break;
	case Page:
		switch_to_tall_mode();
		mySetPageMode();
		break;
	case Trade:
		switch_to_tall_mode();
		myRepaintTitle("select");
		Status.topLine = 0;
		Status.curLine = 0;
		Status.maxLine = Status.list_sz - Status.numLines;
		Status.paint = myPaintTrade;
		Status.scroll = myScrollGeneric;
		Status.selectedEntry = 0;
		break;
	case MixReagents:
		switch_to_tall_mode();
		myRepaintTitle("reagents");
		Status.topLine = 0;
		Status.curLine = 0;
		Status.container = player_party->inventory;
                Status.filter = &ZtatsFilters[ViewReagents];
		Status.maxLine = Status.container->
                        filter_count(Status.filter) - Status.numLines;
		Status.paint = stat_show_container;
                Status.scroll = stat_scroll_container;
		Status.selectedEntry = Status.container->first(Status.filter);
		break;
        case GenericList:
		switch_to_tall_mode();
                myRepaintTitle("select");
		Status.topLine = 0;
		Status.curLine = 0;
		Status.maxLine = Status.list_sz - Status.numLines;
		Status.paint = statusPaintGenericList;
		Status.scroll = myScrollGeneric;
		Status.selectedEntry = 0;                
                break;
        case StringList:
		switch_to_tall_mode();
                myRepaintTitle("select");
		Status.topLine = 0;
		Status.curLine = 0;
		Status.maxLine = Status.list_sz - Status.numLines;
		Status.paint = statusPaintStringList;
		Status.scroll = myScrollGeneric;
		Status.selectedEntry = 0;                
                break;
	}

	statusRepaint();
}

void *statusGetSelected(enum StatusSelection sel)
{
	switch (sel) {
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
	default:
		return 0;
	}
}

void statusSelectCharacter(int partyOrderIndex)
{
	Status.pcIndex = partyOrderIndex;
}

void statusSetPageText(char *title, char *text)
{
	Status.pg_title = title;
	Status.pg_text = text;
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

void statusSetGenericList(int list_sz, struct stat_list_entry *list)
{
	Status.list_sz = list_sz;
	Status.list    = list;
}

void statusUpdateGenericList(int list_sz, struct stat_list_entry *list)
{
	Status.list_sz = list_sz;
	Status.list    = list;
	Status.maxLine = Status.list_sz - Status.numLines;
	if (Status.curLine >= list_sz)
		Status.curLine = max(0, list_sz - 1);
	statusRepaint();
}

void statusSetStringList(int list_sz, char **strings)
{
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
