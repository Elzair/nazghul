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
#include "Spell.h"
#include "ascii.h"
#include "console.h"
#include "sky.h"
#include "wind.h"
#include "foogod.h"

#include <stdio.h>
#include <assert.h>
#include <ctype.h>

#define LINE_H TILE_H
#define N_CHARS_PER_LINE (STAT_W / ASCII_W)
#define Y_TO_LINE(Y) (((Y) - Status.screenRect.y) / TILE_H)
#define N_LINES Status.numLines	/* (STAT_H / TILE_H) */
#define SHORT_H STAT_H
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
	ViewAmmo,
	ViewReagents,
	ViewSpells,
	ViewItems,
	NumViews,
};

static char *ZtatsTitles[] = {
	"Party Member",
	"Armaments",
	"Ammo",
	"Reagents",
	"Spells",
	"Items",
};

static int ZtatsTypes[] = {
	OBJECT_TYPE_ID,
	ARMS_TYPE_ID,
	AMMO_TYPE_ID,
	REAGENT_TYPE_ID,
	SPELL_TYPE_ID,
	ITEM_TYPE_ID,
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

	int n_trades;
	struct trade_info *trades;

} Status;

static void switch_to_tall_mode(void)
{
	if (Status.screenRect.h == TALL_H)
		return;

	Status.screenRect.h = TALL_H;
	Status.numLines = Status.screenRect.h / LINE_H;
	foogod_set_y(foogod_get_y() + (TALL_H - SHORT_H));
	console_set_y(console_get_y() + (TALL_H - SHORT_H));
	foogodRepaint();
	consoleRepaint();
	screen_repaint_frame();
}

static void switch_to_short_mode(void)
{
	if (Status.screenRect.h == SHORT_H)
		return;

	Status.screenRect.h = SHORT_H;
	Status.numLines = Status.screenRect.h / LINE_H;
	foogod_set_y(foogod_get_y() - (TALL_H - SHORT_H));
	console_set_y(console_get_y() - (TALL_H - SHORT_H));
	foogodRepaint();
	consoleRepaint();
	screen_repaint_frame();
}

void statusInit()
{
	memset(&Status, 0, sizeof(Status));

	Status.screenRect.x = STAT_X;
	Status.screenRect.y = STAT_Y;
	Status.screenRect.w = STAT_W;
	Status.screenRect.h = SHORT_H;

	Status.titleRect.x = STAT_X;
	Status.titleRect.y = 0;
	Status.titleRect.w = STAT_W;	// - (2 * BORDER_W);
	Status.titleRect.h = BORDER_H;

	Status.lineRect.x = Status.screenRect.x + TILE_W;
	Status.lineRect.y = Status.screenRect.y;
	Status.lineRect.w = Status.screenRect.w - TILE_W;
	Status.lineRect.h = LINE_H;

	Status.numLines = Status.screenRect.h / LINE_H;

	statusSetMode(ShowParty);
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

	// screenErase(&Status.screenRect);

	rect = Status.screenRect;
	pad = (STAT_W / ASCII_W) - 17;
	assert(pad >= 1);
	pm = player_party->pc[Status.pcIndex];
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

	/* Show rest credits */
	rect.y += ASCII_H;
	screenPrint(&rect, 0, "Slp:%2d", pm->getRestCredits());

    // Show Movement Modes (pmask for now)
    rect.y += ASCII_H;
    // SAM: What is wanted here is a function to turn a pmask 
    //      into one or more words of text (names of movement modes).
    screenPrint(&rect, 0, "Pmask:%1d", pm->getPmask() );

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

static void myShade2Lines(int line)
{
	SDL_Rect rect;
	rect.x = Status.screenRect.x;
	rect.y = Status.screenRect.y + line * TILE_H;
	rect.w = STAT_W;
	rect.h = ASCII_H * 2;
	screenShade(&rect, 128);
}

static void myShowInventory(int type)
{
	SDL_Rect rect;
	struct list *list;
	struct inv_entry *ie;
	int top = Status.topLine;
	int flags, line = 0;

	rect = Status.screenRect;
	rect.x += TILE_W;
	rect.h = LINE_H;

	list_for_each(&player_party->inventory, list) {

		int inUse = 0;
		int avail = 0;
		ie = outcast(list, struct inv_entry, list);

		/* Check the type */
		if (!ie->type->isType(type))
			continue;

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
			    player_party->pc[Status.pcIndex]->
			    hasReadied((class ArmsType *) ie->type)) {
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

		spritePaint(ie->type->getSprite(), 0, Status.screenRect.x,
			    rect.y);

		if (type != ARMS_TYPE_ID)
			/* Not an arms type, so it has no weight, so we only
			 * use one line. */
			rect.y += LINE_H / 4;

		if (avail)
			screenPrint(&rect, flags, "%2d%c%s", avail,
				    (inUse ? '*' : ' '), ie->type->getName());
		else
			screenPrint(&rect, flags, "--%c%s",
				    (inUse ? '*' : ' '), ie->type->getName());

		if (type != ARMS_TYPE_ID) {
			/* No weight, so advance to the start of the next line
			 * pair */
			rect.y += (LINE_H * 3) / 4;
		} else {
			/* Show the weight on the second line */
			rect.y += LINE_H / 2;
			rect.w -= TILE_W;
			screenPrint(&rect, flags | SP_RIGHTJUSTIFIED,
				    "%ds",
				    ((class ArmsType *) ie->type)->getWeight());
			rect.w += TILE_W;
			rect.y += LINE_H / 2;
		}

		if (Status.selectedEntry && ie != Status.selectedEntry) {
			/* Highlight the selected item by shading all the other
			 * entries. */
			myShade2Lines(line);
		}

		line++;

		// Don't print outside the status window
		if (line >= N_LINES)
			break;
	}
}

static struct inv_entry *myGetFirstEntry(int type)
{
	struct list *list;
	struct inv_entry *ie;

	list_for_each(&player_party->inventory, list) {
		ie = outcast(list, struct inv_entry, list);
		if (!ie->type->isType(type))
			continue;
		if (type == ARMS_TYPE_ID && Status.mode == Ready &&
		    ie->ref == ie->count &&
		    !player_party->pc[Status.pcIndex]->
		    hasReadied((class ArmsType *) ie->type))
			continue;
		return ie;
	}
	return 0;
}

static struct inv_entry *myGetNextEntry(int type, int d)
{
	struct list *list;
	struct inv_entry *ie;

	list = &Status.selectedEntry->list;

	for (;;) {
		list = (d > 0 ? list->next : list->prev);
		if (list == &player_party->inventory)
			break;
		ie = outcast(list, struct inv_entry, list);
		if (!ie->type->isType(type))
			continue;
		if (type == ARMS_TYPE_ID && Status.mode == Ready &&
		    ie->ref == ie->count &&
		    !player_party->pc[Status.pcIndex]->
		    hasReadied((class ArmsType *) ie->type))
			continue;
		return ie;
	}
	return 0;
}

static void myScrollInventory(enum StatusScrollDir dir, int type, int nLines)
{
	struct inv_entry *tmp;

	if (!nLines)
		return;

	switch (dir) {
	case ScrollUp:
		tmp = myGetNextEntry(type, -1);
		if (!tmp)
			break;
		Status.selectedEntry = tmp;
		if (Status.topLine &&
		    Status.curLine < (nLines - Status.numLines / 2))
			Status.topLine--;
		Status.curLine--;
		break;
	case ScrollDown:
		tmp = myGetNextEntry(type, 1);
		if (!tmp)
			break;
		Status.selectedEntry = tmp;
		if (Status.topLine < Status.maxLine &&
		    Status.curLine >= (Status.numLines / 2))
			Status.topLine++;
		Status.curLine++;
		break;
	default:
		break;
	}
}

static void myPaintReady(void)
{
	myShowInventory(ARMS_TYPE_ID);
}

static void myScrollReady(enum StatusScrollDir dir)
{
	myScrollInventory(dir, ARMS_TYPE_ID, player_party->nArms);
}

static void myPaintUse(void)
{
	myShowInventory(ITEM_TYPE_ID);
}

static void myScrollUse(enum StatusScrollDir dir)
{
	myScrollInventory(dir, ITEM_TYPE_ID, player_party->nItems);
}

static void myPaintMixReagents(void)
{
	myShowInventory(REAGENT_TYPE_ID);
}

static void myScrollMixReagents(enum StatusScrollDir dir)
{
	myScrollInventory(dir, REAGENT_TYPE_ID, player_party->nReagents);
}

static bool myShowPCInPartyView(class Character * pm, void *data)
{
	int flags;
	char condition[32] = { 0 };
	char *c = condition;

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

	/* Paint the condition */
	condition[0] = 'G';
	Status.lineRect.y += ASCII_H;
	if (pm->isDead())
		condition[0] = 'D';
	else {
		if (pm->isPoisoned())
			*c++ = 'P';
		if (pm->isAsleep())
			*c++ = 'S';
		if (pm->isHostile(player_party->alignment))
			*c++ = 'C';
	}

	screenPrint(&Status.lineRect, SP_RIGHTJUSTIFIED, "%d%s",
		    pm->getHp(), condition);

	Status.lineRect.y += ASCII_H;

	if (Status.pcIndex != -1 && pm->getOrder() != Status.pcIndex) {
		/* Highlight the selected party member by shading all the other
		 * entries. */
		myShade2Lines(Y_TO_LINE(Status.lineRect.y - 2 * ASCII_H));
	}

	return false;
}

static void myShowParty(void)
{
	/* Setup the text rectangle */
	Status.lineRect.y = Status.screenRect.y;
	Status.lineRect.w = Status.screenRect.w - TILE_W;

	player_party->for_each_member(myShowPCInPartyView, 0);

}

static void myScrollParty(enum StatusScrollDir dir)
{
	switch (dir) {
	case ScrollDown:
	case ScrollRight:
	case ScrollPageDown:
		Status.pcIndex = (Status.pcIndex + 1) % player_party->n_pc;
		break;
	case ScrollUp:
	case ScrollLeft:
	case ScrollPageUp:
		Status.pcIndex =
		    (Status.pcIndex + player_party->n_pc -
		     1) % player_party->n_pc;
		break;
	}
}

static int myScrollMemberZtatsHorz(int d)
{

	if (d > 0 && Status.pcIndex < (player_party->n_pc - 1)) {
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
			Status.pcIndex = (d > 0 ? 0 : player_party->n_pc - 1);
			break;

		case ViewArmaments:
			Status.maxLine = player_party->nArms - Status.numLines;
			break;

		case ViewAmmo:
			Status.maxLine = player_party->nAmmo - Status.numLines;
			break;

		case ViewReagents:
			Status.maxLine =
			    player_party->nReagents - Status.numLines;
			break;

		case ViewSpells:
			Status.maxLine =
			    player_party->nSpells - Status.numLines;
			break;

		case ViewItems:
			Status.maxLine = player_party->nItems - Status.numLines;
			break;

		default:
			break;
		}
	}

	Status.maxLine = max(Status.maxLine, 0);

	if (Status.ztatsView == ViewMember)
		myRepaintTitle(player_party->pc[Status.pcIndex]->getName());
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
		myShowInventory(ZtatsTypes[Status.ztatsView]);
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
}

static void myPaintTrade(void)
{
	SDL_Rect srect, nrect, prect, qrect;
	int i, top, line;

	if (!Status.n_trades)
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

	for (i = 0; i < Status.n_trades && line < N_LINES; i++) {

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
			screenPrint(&qrect, 0, "%d", Status.trades[i].quantity);
		}
		// price
		screenPrint(&prect, SP_RIGHTJUSTIFIED, "%dgp",
			    Status.trades[i].cost);

		// Shade unselected items.
		if (i != Status.curLine) {
			myShade2Lines(line);
		}
		line++;

		// Advance all the rectangles to the next line.
		srect.y += TILE_H;
		nrect.y += TILE_H;
		qrect.y += TILE_H;
		prect.y += TILE_H;
	}
}

static void myScrollTrade(enum StatusScrollDir dir)
{
	switch (dir) {
	case ScrollUp:
		// If the window is not at the top of the list and the current
		// line is centered in the window then move the window up.
		if (Status.topLine &&
		    Status.curLine < (Status.n_trades - Status.numLines / 2))
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
		if (Status.curLine < (Status.n_trades - 1))
			Status.curLine++;
		break;
	default:
		break;
	}
}

void statusRepaint(void)
{
	screenErase(&Status.screenRect);
	Status.paint();
	screenUpdate(&Status.screenRect);
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

	switch (mode) {
	case ShowParty:
		switch_to_short_mode();
		myRepaintTitle("Party");	/* note: must always repaint
						 * title AFTER switching mode
						 * because switching modes
						 * repaints the border, and the 
						 * * title must be painted over
						 * the border. */
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
		myRepaintTitle(player_party->pc[Status.pcIndex]->getName());
		Status.ztatsView = ViewMember;
		Status.selectedEntry = 0;
		Status.topLine = 0;
		Status.paint = myShowZtats;
		Status.scroll = myScrollZtats;
		break;
	case Ready:
		switch_to_tall_mode();
		myRepaintTitle(player_party->pc[Status.pcIndex]->getName());
		Status.topLine = 0;
		Status.curLine = 0;
		Status.maxLine = player_party->nArms - Status.numLines;
		Status.paint = myPaintReady;
		Status.scroll = myScrollReady;
		Status.selectedEntry = myGetFirstEntry(ARMS_TYPE_ID);
		break;
	case Use:
		switch_to_tall_mode();
		myRepaintTitle("select");
		Status.topLine = 0;
		Status.curLine = 0;
		Status.maxLine = player_party->nItems - Status.numLines;
		Status.paint = myPaintUse;
		Status.scroll = myScrollUse;
		Status.selectedEntry = myGetFirstEntry(ITEM_TYPE_ID);
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
		Status.maxLine = Status.n_trades - Status.numLines;
		Status.paint = myPaintTrade;
		Status.scroll = myScrollTrade;
		Status.selectedEntry = 0;
		break;
	case MixReagents:
		switch_to_tall_mode();
		myRepaintTitle("reagents");
		Status.topLine = 0;
		Status.curLine = 0;
		Status.maxLine = player_party->nReagents - Status.numLines;
		Status.paint = myPaintMixReagents;
		Status.scroll = myScrollMixReagents;
		Status.selectedEntry = myGetFirstEntry(REAGENT_TYPE_ID);
		break;
	}

	statusRepaint();
}

void *statusGetSelected(enum StatusSelection sel)
{
	switch (sel) {
	case Character:
		return player_party->pc[Status.pcIndex];
	case InventoryItem:
	case Reagents:
		return Status.selectedEntry;
	case TradeItem:
		return Status.n_trades ? &Status.trades[Status.curLine] : 0;
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

void statusSetTradeInfo(int n_trades, struct trade_info *trades)
{
	Status.n_trades = n_trades;
	Status.trades = trades;
}

void statusUpdateTradeInfo(int n_trades, struct trade_info *trades)
{
	Status.n_trades = n_trades;
	Status.trades = trades;
	Status.maxLine = Status.n_trades - Status.numLines;
	if (Status.curLine >= n_trades)
		Status.curLine = max(0, n_trades - 1);
	statusRepaint();
}

enum StatusMode statusGetMode(void)
{
	return Status.mode;
}

int status_get_h(void)
{
	return Status.screenRect.h;
}
