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
/* 12/14/2002  Added HANDLE command by Sam Glasby.
 */
#include "play.h"
#include "util.h"
#include "game.h"
#include "place.h"
#include "constants.h"
#include "images.h"
#include "sprite.h"
#include "los.h"
#include "astar.h"
#include "common.h"
#include "screen.h"
#include "console.h"
#include "status.h"
#include "player.h"
#include "sky.h"
#include "map.h"
#include "moongate.h"
#include "wq.h"
#include "foogod.h"
#include "combat.h"
#include "cursor.h"
#include "Arms.h"
#include "event.h"
#include "wind.h"
#include "Item.h"
#include "Container.h"
#include "Trap.h"
#include "conv.h"
#include "Mech.h"
#include "Spell.h"
#include "Mech.h"
#include "dup_constants.h"
#include "cmdwin.h"
#include "vehicle.h"

#define DEBUG
#include "debug.h"

#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>

enum cmdstate {
	CMD_IDLE,
	CMD_GET,
	CMD_INV,
	CMD_DROP,
	CMD_FIRE,
	CMD_LOOK,
	CMD_ZTATS,
	CMD_ZTATS2,
	CMD_RDY,
	CMD_RDY2,
	CMD_USE,
	CMD_DEAD,
};

struct play {
	char *savefile;
	struct game *game;
	char *los_name;
	enum cmdstate cmdstate;
};

struct play *Play;		/* hack */

bool Quit;

struct LightEffectContext {
	char *name;
	int amount;
	class Character *owner;
};

static void myExpireLightEffect(struct wq_job *job, struct list *wq)
{
	struct LightEffectContext *context;
	context = (struct LightEffectContext *) job->data;
	context->owner->changeLight(-context->amount);
	player_party->recompute_los();
	consolePrint("Light from %s expired\n", context->name);
	delete context;
	free(job);		// fixme -- use delete
}

void effectLight(char *name, int amount, int duration, class Character * target)
{
	struct LightEffectContext *context;
	context = new struct LightEffectContext;
	if (context) {
		context->name = name;
		context->amount = amount;	// careful...
		context->owner = target;
		target->changeLight(amount);
		if (Place->type != combat_place)
			// Right now combat is the only "small-scale"
			// place, where the Player structure's location
			// is invalid, and where everything relates to
			// the individual party members and not the
			// party as a whole.
			player_party->recompute_los();
		wqCreateJob(&TurnWorkQueue,
			    Turn + duration, 0, context, myExpireLightEffect);
	}
}

static void myExpireRevealEffect(struct wq_job *job, struct list *wq)
{
	char *cause = (char *) job->data;
	Reveal = false;
	consolePrint("%s expired\n", cause);
	free(job);
}

void effectReveal(char *name, int duration)
{
	Reveal = true;
	wqCreateJob(&TurnWorkQueue, Turn + duration,
		    0, name, myExpireRevealEffect);
}

static void myExpireQuickenEffect(struct wq_job *job, struct list *wq)
{
	char *cause = (char *) job->data;
	Quicken = 0;
	consolePrint("%s expired\n", cause);
	free(job);
}

void effectQuicken(char *name, int duration)
{
	Quicken = 2;		// Setting it to a multiple of two will make
	// sure that the
	// player sees the effects right away
	wqCreateJob(&TurnWorkQueue, Turn + duration,
		    0, name, myExpireQuickenEffect);
}

static void myExpireNegateMagicEffect(struct wq_job *job, struct list *wq)
{
	char *cause = (char *) job->data;
	MagicNegated--;
	consolePrint("%s expired\n", cause);
	free(job);
}

void effectNegateMagic(char *name, int duration)
{
	MagicNegated++;
	wqCreateJob(&TurnWorkQueue, Turn + duration,
		    0, name, myExpireNegateMagicEffect);
}

static void myExpireShowTerrainEffect(struct wq_job *job, struct list *wq)
{
	char *cause = (char *) job->data;
	ShowAllTerrain--;
	consolePrint("%s expired\n", cause);
	free(job);
}

void effectShowTerrain(char *name, int duration)
{
	ShowAllTerrain++;
	wqCreateJob(&TurnWorkQueue, Turn + duration,
		    0, name, myExpireShowTerrainEffect);
}

bool dirkey(struct KeyHandler *kh, int key)
{
	int *dir = (int *) kh->data;

	if (key >= KEY_SOUTHWEST && key <= KEY_NORTHEAST) {
		*dir = keyToDirection(key);
		return true;
	}

	if (key == SDLK_ESCAPE) {
		*dir = key;
		return true;
	}

	return false;
}

bool yesnokey(struct KeyHandler * kh, int key)
{
	int *yesno = (int *) kh->data;

	switch (key) {
	case 'y':
	case 'Y':
		*yesno = 'y';
		return true;
	case 'n':
	case 'N':
	case CANCEL:
		*yesno = 'n';
		return true;
	default:
		return false;
	}
}

enum get_number_state {
	GN_ALL,
	GN_ZERO,
	GN_SOME,
	GN_CANCEL
};

struct get_number_info {
	int digit;
	int erase;
	int state;
	char *prompt;
};

static inline void getnum_erase_prompt(struct get_number_info *info)
{
	if (info->erase) {
		cmdwin_backspace(info->erase);
		// info->erase = 0;
	}
}

bool getnum(struct KeyHandler *kh, int key)
{
	struct get_number_info *info;

	info = (struct get_number_info *) kh->data;

	switch (info->state) {
	case GN_ALL:
		if (key == CANCEL) {
			getnum_erase_prompt(info);
			info->digit = 0;
			info->state = GN_CANCEL;
			return true;
		}
		if (key == '\n') {
			getnum_erase_prompt(info);
			return true;
		}
		if (key == '0') {
			getnum_erase_prompt(info);
			cmdwin_print("0");
			info->digit = 0;
			info->state = GN_ZERO;
			return false;
		}
		if (isdigit(key)) {
			getnum_erase_prompt(info);
			info->digit = info->digit * 10 + key - '0';
			cmdwin_print("%c", key);
			info->state = GN_SOME;
			return false;
		}
		break;
	case GN_ZERO:
		if (key == CANCEL) {
			info->digit = 0;
			info->state = GN_CANCEL;
			return true;
		}
		if (key == '\n') {
			return true;
		}
		if (key == '\b') {
			cmdwin_backspace(1);
			if (info->prompt)
				cmdwin_print(info->prompt);
			info->state = GN_ALL;
			return false;
		}
		if (key == '0')
			return false;
		if (isdigit(key)) {
			cmdwin_backspace(1);
			info->digit = info->digit * 10 + key - '0';
			cmdwin_print("%c", key);
			info->state = GN_SOME;
			return false;
		}
		break;
	case GN_SOME:
		if (key == CANCEL) {
			info->digit = 0;
			info->state = GN_CANCEL;
			return true;
		}
		if (key == '\n') {
			return true;
		}
		if (key == '\b') {
			info->digit = info->digit - (info->digit % 10);
			info->digit /= 10;
			cmdwin_backspace(1);
			if (info->digit == 0) {
				info->state = GN_ALL;
				if (info->prompt)
					cmdwin_print(info->prompt);
			}
			return false;
		}
		if (isdigit(key)) {
			info->digit = info->digit * 10 + key - '0';
			cmdwin_print("%c", key);
			return false;
		}
		break;
	}

	return false;
}

#if 0

if (key == '\n') {
	if (info->digit == 0)
		info->digit = -1;	/* return with no number selects all */
	return true;
}

if (key == '\b' && info->digit > 0) {
	info->digit = info->digit - (info->digit % 10);
	info->digit /= 10;
	cmdwin_backspace(1);
	return false;
}

if (isdigit(key)) {
	if (key != '0' || info->digit > 0) {
		getnum_erase_prompt(info);
		info->digit = info->digit * 10 + key - '0';
		cmdwin_print("%c", key);
	} else if (key == '0' && info->digit == 0 && !info->zero) {
		getnum_erase_prompt(info);
		cmdwin_print("0");
		info->zero = true;
	}
	return false;
}

return false;
}
#endif				/* 0 */

bool getdigit(struct KeyHandler * kh, int key)
{
	struct get_number_info *info;

	info = (struct get_number_info *) kh->data;

	if (key == CANCEL) {
		cmdwin_backspace(info->erase);
		info->digit = 0;
		return true;
	}

	if (isdigit(key)) {
		cmdwin_backspace(info->erase);
		info->digit = key - '0';
		if (info->digit != 0)
			cmdwin_print("%c", key);
		return true;
	}

	return false;
}

bool anykey(struct KeyHandler * kh, int key)
{
	return true;
}

bool scroller(struct KeyHandler * kh, int key)
{
	struct ScrollerContext *context;
	context = (struct ScrollerContext *) kh->data;

	switch (key) {
	case KEY_NORTH:
		statusScroll(ScrollUp);
		break;
	case KEY_SOUTH:
		statusScroll(ScrollDown);
		break;
	case KEY_EAST:
		statusScroll(ScrollRight);
		break;
	case KEY_WEST:
		statusScroll(ScrollLeft);
		break;
	case SDLK_PAGEUP:
		statusScroll(ScrollPageUp);
		break;
	case SDLK_PAGEDOWN:
		statusScroll(ScrollPageDown);
		break;
	case SDLK_RETURN:
	case SDLK_SPACE:
	case '\n':
		if (context != NULL) {
			context->selection =
			    statusGetSelected(context->selector);
		}
		return true;
	case SDLK_ESCAPE:
	case 'q':
		if (context)
			context->abort = true;
		return true;
	case 'm':
		if (context && context->mixing) {
			context->done = true;
			return true;
		}
		break;
	default:
		break;
	}

	return false;
}

bool movecursor(struct KeyHandler * kh, int key)
{
  // A UI mode in which the user can move the cursor 
  // with ARROW keys, select a target with 
  // (ENTER | RETURN | SPACE), or cancel with ESCAPE.
  struct cursor_movement_keyhandler * data;
  assert(kh);
  data = (struct cursor_movement_keyhandler *) kh->data;
  
  if (key == '\n' || key == SDLK_SPACE || key == SDLK_RETURN) {
    return true;  // Done (target selected)
  }
  
  if (keyIsDirection(key)) {
    int dir = keyToDirection(key);
    Cursor->move(directionToDx(dir), directionToDy(dir));
    mapUpdate(0);
    return false;  // Keep on keyhandling
  }
  
  if (key == SDLK_ESCAPE) {
    data->abort = true;
    return true;  // Done (abort)
  }
  
  return false;  // Keep on keyhandling
} // movecursor()

bool movecursor_and_do(struct KeyHandler * kh, int key)
{
  // As movecursor(), but call kh->each_point_func()
  // for each cursor move, and kh->each_target_func()
  // for each point selected with (ENTER, SPACE, RETURN).
  // 
  // Unlike movecursor(), multiple targets can be selected.
  // We expect that eventually the user will exit 
  // this UI mode with ESCAPE.
  // 
  // Also unlike movecursor(), we don't return the (last) target 
  // selected, as the ESC to exit this UI mode stomps on that info.
  struct cursor_movement_keyhandler * data;
  assert(kh);
  data = (struct cursor_movement_keyhandler *) kh->data;
  
  if (key == '\n' || key == SDLK_SPACE || key == SDLK_RETURN) {
    int x = Cursor->getX();
    int y = Cursor->getY();
    if (data->each_target_func)
      data->each_target_func(x, y);
    return false;  // Keep on keyhandling
  }
  
  if (keyIsDirection(key)) {
    int dir = keyToDirection(key);
    Cursor->move(directionToDx(dir), directionToDy(dir));
    mapUpdate(0);
    int x = Cursor->getX();
    int y = Cursor->getY();
    if (data->each_point_func)
      data->each_point_func(x, y);
    return false;  // Keep on keyhandling
  }
  
  if (key == SDLK_ESCAPE) {
    data->abort = true;
    return true;  // Done (abort)
  }
  return false;  // Keep on keyhandling
} // movecursor_and_do()

struct inv_entry *select_item(void)
{
	enum StatusMode omode;
	struct inv_entry *ie;
	class ItemType *item;
	struct KeyHandler kh;
	struct ScrollerContext sc;

	omode = statusGetMode();
	statusSetMode(Use);

	sc.selector = InventoryItem;
	sc.selection = NULL;
	kh.fx = scroller;
	kh.data = &sc;

	eventPushKeyHandler(&kh);
	cmdwin_print("<select>");
	eventHandle();
	cmdwin_backspace(strlen("<select>"));
	eventPopKeyHandler();

	statusSetMode(omode);

	ie = (struct inv_entry *) sc.selection;
	if (ie == NULL) {
		cmdwin_print("none!");
		return NULL;
	}

	item = (class ItemType *) ie->type;
	cmdwin_print(item->getName());

	return ie;
}

class Character *select_party_member(void)
{
	enum StatusMode omode;
	class Character *character;

	omode = statusGetMode();
	statusSetMode(SelectCharacter);

	struct KeyHandler kh;
	struct ScrollerContext sc;
	sc.selector = Character;
	sc.selection = NULL;
	kh.fx = scroller;
	kh.data = &sc;

	eventPushKeyHandler(&kh);
	cmdwin_print("<select>");
	eventHandle();
	cmdwin_backspace(strlen("<select>"));
	eventPopKeyHandler();

	statusRepaint();

	character = (class Character *) sc.selection;

	if (character == NULL) {
		cmdwin_print("none!");
		/* Hack alert: this saves the caller from having to remember to
		 * do this. Doing it unconditionally is undesirable because it
		 * can cause status screen flashes if the old mode requires a
		 * short status window and the next mode requires a tall one. */
	} else {
		cmdwin_print("%s", character->getName());
	}

	statusSetMode(omode);

	return character;
}

void getkey(void *data, bool(*handler) (struct KeyHandler * kh, int key))
{
	struct KeyHandler kh;
	kh.fx = handler;
	kh.data = data;

	eventPushKeyHandler(&kh);
	eventHandle();
	eventPopKeyHandler();
}

int ui_get_direction(void)
{
	int dir;
	cmdwin_print("<direction>");
	getkey(&dir, dirkey);
	cmdwin_backspace(strlen("<direction>"));
	if (dir == CANCEL) {
		cmdwin_print("none!");
	} else {
		cmdwin_print(directionToString(dir));
	}
	return dir;
}

bool cmdLook(int x, int y)
{
	int dir;

	cmdwin_clear();
	cmdwin_print("Look-");

	dir = ui_get_direction();
	if (dir == CANCEL)
		return false;

	consolePrint("You see ");
	placeDescribe(placeWrapX(x + directionToDx(dir)),
		      placeWrapY(y + directionToDy(dir)));
	return false;		// SAM: Should this be true?
}

static void myGetAux(Object * item)
{
	place_remove_object(Place, item);
	player_party->add_to_inventory(item->getObjectType(), 1);
	delete(item);
}

bool cmdGet(int x, int y, bool scoop_all)
{
	class Object *item;
	class ObjectType *type;
	int count;
	int dir;
	int n_item_types;

	cmdwin_clear();
	cmdwin_print("Get-");

	dir = ui_get_direction();
	if (dir == CANCEL)
		return false;

	consolePrint("You get ");

	x = placeWrapX(x + directionToDx(dir));
	y = placeWrapY(y + directionToDy(dir));

	item = place_get_item(Place, x, y);
	if (!item) {
		consolePrint("nothing!\n");
		return true;
	}

	n_item_types = 1;
	count = 1;
	type = item->getObjectType();

	myGetAux(item);

	if (scoop_all) {
		while ((item = place_get_item(Place, x, y)) != NULL) {

			if (item->getObjectType() != type) {
				if (n_item_types > 1)
					consolePrint(", ");
				type->describe(count);
				type = item->getObjectType();
				count = 1;
				n_item_types++;
			} else {
				count++;
			}

			myGetAux(item);
		}
	}

	if (n_item_types > 1) {
		consolePrint(" and ");
	}
	type->describe(count);
	consolePrint(".\n");

	return true;
}

bool cmdOpen(class Character * pc)
{
	int dir, x, y;
	class Mech *mech;
	class Container *container;

	cmdwin_clear();
	cmdwin_print("Open-");

	dir = ui_get_direction();
	if (dir == CANCEL)
		return false;

	if (pc) {
		x = placeWrapX(pc->getX() + directionToDx(dir));
		y = placeWrapY(pc->getY() + directionToDy(dir));
	} else {
		x = placeWrapX(player_party->getX() + directionToDx(dir));
		y = placeWrapY(player_party->getY() + directionToDy(dir));
	}

	cmdwin_print("-");

	/*** Open Mech ***/

	mech = (class Mech *) place_get_object(Place, x, y, mech_layer);
	if (mech != NULL) {
		cmdwin_print("%s-", mech->getName());
		if (mech->activate(MECH_OPEN)) {
			cmdwin_print("ok");
			// SAM: Should this printing be done entirely by mech
			// scripting?
			consolePrint("Opened ");
			mech->describe(1);
			consolePrint(".\n");
			mapSetDirty();
		} else {
			cmdwin_print("failed!");
		}
		return true;
	}

	/*** Open Container ***/

	// Get the container.
	container = (class Container *) place_get_object(Place, x, y,
							 container_layer);
	if (!container) {
		cmdwin_print("nothing!");
		return false;
	}
	cmdwin_print("%s-", container->getName());

	// Get the party member who will open the container (in combat mode
	// this is passed in as a parameter).
	if (pc != NULL) {
		cmdwin_print("%s", pc->getName());
	} else {
		pc = select_party_member();
		if (pc == NULL) {
			return false;
		}
	}

	consolePrint("%s opens ", pc->getName());
	container->describe(1);

	// Check for traps.
	if (!container->isTrapped()) {
		consolePrint(".\n");
	} else {

		class TrapType *trap = container->getTrap();
		consolePrint("...%s...", trap->getName());

		// Roll to disarm
		if (random() % 100 < pc->getDexterity()) {
			consolePrint("disarmed!\n");
		} else {
			consolePrint("oops!\n");
			int effects = trap->getEffects();
			if (effects & EFFECT_BURN) {
				pc->changeHp(-trap->getAmount());
			}
			if (effects & EFFECT_POISON && !pc->isPoisoned()) {
				pc->setPoison(true);
				pc->changeHp(-trap->getAmount());
			}
			if (effects & EFFECT_SLEEP) {
				pc->changeSleep(trap->getAmount());
			}
			// Ignoring the charm effect for now -- requires me to
			// store an alignment with the trap.

			consolePrint("%s %s!\n", pc->getName(),
				     pc->getWoundDescription());
		}

		// Give the character some experience for dealing with the trap
		pc->addExperience(XP_PER_TRAP);
	}

	// Open the container (automagically spills all the contents onto the
	// map).
	container->open();
	container->remove();
	delete container;

	mapSetDirty();
	return true;
}

bool cmdQuit(void)
{
	int yesno;

	cmdwin_clear();
	cmdwin_print("Quit-Y/N?");
	getkey(&yesno, yesnokey);

	cmdwin_backspace(4);

	if (yesno == 'y') {
		cmdwin_print("Yes!");
		consolePrint("Goodye!\n");
		Quit = true;
	} else {
		cmdwin_print("No");
	}

	return Quit;
}

void cmdFire(void)
{
	int dir;

	cmdwin_clear();
	cmdwin_print("Fire");

	if ((!player_party->vehicle ||
             !player_party->vehicle->getOrdnance())) {
		cmdwin_print("-Nothing!");
		return;
	}

	cmdwin_print(" %s-<direction>",
		     player_party->vehicle->getOrdnance()->getName());
	getkey(&dir, dirkey);
	cmdwin_backspace(strlen("<direction>"));

	if (dir == CANCEL) {
		cmdwin_print("none!");
		return;
	}

	cmdwin_print("%s", directionToString(dir));
	if (! player_party->vehicle->fire_weapon(directionToDx(dir),
                                                 directionToDy(dir))) {
		cmdwin_print("-Not a broadside!");
		return;
        }

	turnAdvance(place_adjust_turn_cost(player_party->getPlace(),
                                           TURNS_TO_FIRE_VEHICLE_WEAPON));
}

bool cmdReady(class Character * pc)
{
	bool committed = false;
	struct inv_entry *ie;
	struct KeyHandler kh;
	struct ScrollerContext sc;
	int erase;
	char *msg = 0;

	cmdwin_clear();
	cmdwin_print("Ready-");

	if (pc != NULL) {
		cmdwin_print("%s", pc->getName());
	} else {
		pc = select_party_member();
		if (pc == NULL)
			return false;
	}

	cmdwin_print("-");

	statusSelectCharacter(pc->getOrder());
	statusSetMode(Ready);
	sc.selector = InventoryItem;
	kh.fx = scroller;
	kh.data = &sc;
	eventPushKeyHandler(&kh);
	cmdwin_print("<select>");
	erase = strlen("<select>");

	for (;;) {

		sc.selection = NULL;

		eventHandle();
		cmdwin_backspace(erase);

		ie = (struct inv_entry *) sc.selection;
		if (ie == NULL) {
			cmdwin_print("Done");
			break;
		}

		committed = true;

		class ArmsType *arms = (class ArmsType *) ie->type;

		cmdwin_print("%s-", arms->getName());

		if (ie->ref && pc->unready(arms)) {
			msg = "unreadied!";
			ie->ref--;
			statusRepaint();
			consolePrint("%s unreadied %s\n", pc->getName(),
				     arms->getName());
		} else {

			switch (pc->ready(arms)) {
			case Character::Readied:
				ie->ref++;
				statusRepaint();
				msg = "readied";
				consolePrint("%s readied %s\n", pc->getName(),
					     arms->getName());
				break;
			case Character::NoAvailableSlot:
				msg = "no place to put!";
				break;
			case Character::WrongType:
				msg = "character type can't use that!";
				break;
			case Character::TooHeavy:
				msg = "too heavy!";
				break;
			default:
				assert(false);
				break;
			}
		}

		cmdwin_print(msg);
		erase = strlen(arms->getName()) + strlen(msg) + 1;
	}

	eventPopKeyHandler();
	statusSetMode(ShowParty);
	return committed;
}

int select_target(int ox, int oy, int *x, int *y, int range)
{
  Cursor->setRange(range);
  Cursor->setOrigin(ox, oy);
  Cursor->relocate(Place, *x, *y);  // Remember prev target, if any
  mapUpdate(0);
  
  struct cursor_movement_keyhandler data;
  data.each_point_func  = NULL;
  data.each_target_func = NULL;
  data.abort            = false;
  struct KeyHandler kh;
  kh.fx   = movecursor;
  kh.data = &data;
  
  eventPushKeyHandler(&kh);
  cmdwin_print("<target>");
  eventHandle();
  cmdwin_backspace(strlen("<target>"));
  eventPopKeyHandler();
  
  *x = Cursor->getX();
  *y = Cursor->getY();
  Cursor->remove();
  mapUpdate(0);
  
  struct cursor_movement_keyhandler * data_ret;
  data_ret = (struct cursor_movement_keyhandler *) kh.data;
  if (data_ret->abort) {
    cmdwin_print("none!");
    return -1;  // Aborted, no target
  }
  
  // Target has been selected, (x,y) contain where
  return 0;  
} // select_target()

int select_target_with_doing(int ox, int oy, int *x, int *y,
                             int range,
                             v_funcpointer_ii each_point_func,
                             v_funcpointer_ii each_target_func)
{
  // SAM: 
  // As select_target(), but each_point_func() 
  // will be called at each point cursored over,
  // and each_target_func() will be called at each point
  // selected as a target.
  // 
  // Eventually, the user will abort with ESC.
  // 
  // SAM: It might be nice to return the last target,
  // in case our caller wants it, but it seems that
  // the ESC abort stomps on it.
  Cursor->setRange(range);
  Cursor->setOrigin(ox, oy);
  Cursor->relocate(Place, *x, *y);  // Remember prev target, if any
  mapUpdate(0);
  
  struct cursor_movement_keyhandler data;
  data.each_point_func  = each_point_func;
  data.each_target_func = each_target_func;
  data.abort            = false;
  struct KeyHandler kh;
  kh.fx   = movecursor_and_do;
  kh.data = &data;
  
  eventPushKeyHandler(&kh);
  cmdwin_print("<target>");
  eventHandle();
  cmdwin_backspace(strlen("<target>"));
  eventPopKeyHandler();
  
  *x = Cursor->getX();
  *y = Cursor->getY();
  Cursor->remove();
  mapUpdate(0);
  
  struct cursor_movement_keyhandler * data_ret;
  data_ret = (struct cursor_movement_keyhandler *) kh.data;
  if (data_ret->abort) {
    cmdwin_print("none!");
    return -1;  // Aborted, no target
  }
  
  // Target has been selected, (x,y) contain where
  return 0;
} // select_target_with_doing()

bool cmdHandle(class Character * pc)
{
	// SAM: Adding (H)andle command...
	int x;
	int y;

	cmdwin_clear();
	cmdwin_print("Handle-");

	if (pc) {
		// A party member was specified as a parameter, so this must be
		// combat mode. Use the party member's location as the origin.
		x = pc->getX();
		y = pc->getY();
		cmdwin_print("%s", pc->getName());
	} else {
		// Must be party mode. Use the player party's location as the
		// origin.
		x = player_party->getX();
		y = player_party->getY();

		// And find out what the party member is Handling (so we can
		// print the name). If only one party member then select the
		// only one.
		if (player_party->get_num_living_members() == 1) {
			pc = player_party->get_first_living_member();
			cmdwin_print("%s", pc->getName());
		} else {
			pc = select_party_member();
			if (pc == NULL) {
				return false;
			}
		}
	}

	cmdwin_print("-");

	// *** Pick a target ***

	if (select_target(x, y, &x, &y, 1) == -1)
		return false;

	class Mech *mech;
	mech = (class Mech *) place_get_object(Place, x, y, mech_layer);
	if (mech == NULL) {
		cmdwin_print("nothing!");
	} else {
		cmdwin_print("%s", mech->getName());
		consolePrint("%s handled %s\n", pc->getName(), mech->getName());
		mech->activate(MECH_HANDLE);
		mapRecomputeLos(player_party->view);	// Often necessary when 
							// 
		// 
		// mechs
		// change surrounding terrain.
		mapSetDirty();
	}

	return true;
}

bool cmdUse(class Character * pc)
{
	struct inv_entry *ie;
	class ItemType *item;
	class Character *target;

	cmdwin_clear();
	cmdwin_print("Use-");

	if (pc != NULL) {
		cmdwin_print("%s", pc->getName());
	} else {
		pc = select_party_member();
		if (pc == NULL)
			return false;
	}

	target = pc;		// by default
	cmdwin_print("-");

	statusSelectCharacter(pc->getOrder());
	statusSetMode(Use);
	ie = select_item();
	statusSetMode(ShowParty);
	if (ie == NULL)
		return false;
	item = (class ItemType *) ie->type;

	// Get the target to use the item on
	if (item->getTarget() == TARG_FRIEND) {
		cmdwin_print("-");
		target = select_party_member();
		if (target == NULL)
			return false;
	}
	// Use it on the target
	item->use(target);
	statusRepaint();

	consolePrint("%s used ", pc->getName());
	item->describe(1);
	consolePrint(".\n");

	// Consume the item
	if (item->isConsumable())
		player_party->remove_from_inventory(ie, 1);

	return true;
}

static void myNewOrder(void)
{
	class Character *pc1, *pc2;
	int tmp;

	cmdwin_clear();
	cmdwin_print("Switch-");

	pc1 = select_party_member();
	if (pc1 == NULL)
		return;

	cmdwin_print("-with-");

	pc2 = select_party_member();
	if (pc2 == NULL) {
		return;
	}

	player_party->pc[pc1->getOrder()] = pc2;
	player_party->pc[pc2->getOrder()] = pc1;
	tmp = pc2->getOrder();
	pc2->setOrder(pc1->getOrder());
	pc1->setOrder(tmp);

	consolePrint("%s switched order with %s\n", pc1->getName(),
		     pc2->getName());

	statusRepaint();
}

static void run_combat(bool camping, class Character * guard, int hours)
{
	struct move_info minfo;
	struct combat_info cinfo;

	memset(&minfo, 0, sizeof(minfo));
	minfo.place = Place;
	minfo.x = player_party->getX();
	minfo.y = player_party->getY();
	minfo.dx = player_party->dx;
	minfo.dy = player_party->dy;

	memset(&cinfo, 0, sizeof(cinfo));
	cinfo.camping = camping;
	cinfo.guard = guard;
	cinfo.hours = hours;
	cinfo.move = &minfo;

	player_party->move_to_combat(&cinfo);
}

static void myTalk(void)
{
	struct conv *conv;
	class NpcParty *npc;
	int x, y;

	// *** Prompt user & check if valid ***

	cmdwin_clear();
	cmdwin_print("Talk-");

	if (Place->type == wilderness_place) {
		cmdwin_print("not here!");
		return;
	}

	x = player_party->getX();
	y = player_party->getY();
	if (select_target(x, y, &x, &y, 4) == -1) {
		return;
	}

	npc = (class NpcParty *) place_get_object(Place, x, y, being_layer);

	if (!npc || !npc->isType(NPCPARTY_ID)
	    || !(conv = npc->getConversation())) {
		cmdwin_print("no response!");
		return;
	}

	cmdwin_print(npc->getName());

	consolePrint("\n*** CONVERSATION ***\n");
	consolePrint("You meet a ");
	npc->describe(1);
	consolePrint(".\n");

	if (npc->act == SLEEPING) {
		consolePrint("Zzzz...\n");
		return;
	}
	// conv = npc->getConversation();
	conv->speaker = npc;

	// *** Enter conversation ***

	switch (convEnter(conv)) {

	case CONV_COMBAT:
		run_combat(false, 0, 0);
		break;

	case CONV_OK:
		break;
	}

	mapSetDirty();
}

bool cmdZtats(class Character * pc)
{
	cmdwin_clear();
	cmdwin_print("Stats-");

	if (pc != NULL) {
		cmdwin_print("%s", pc->getName());
	} else {
		pc = select_party_member();
		if (pc == NULL)
			return false;
	}

	cmdwin_print("-<ESC to exit>");

	statusSelectCharacter(pc->getOrder());
	statusSetMode(Ztats);

	struct KeyHandler kh;
	kh.fx = scroller;
	kh.data = NULL;
	eventPushKeyHandler(&kh);
	eventHandle();
	eventPopKeyHandler();

	cmdwin_backspace(strlen("<ESC to exit>"));
	cmdwin_print("ok");

	statusSetMode(ShowParty);

	return false;
}

static int select_hours(void)
{
	struct get_number_info info;

	cmdwin_print("<hours[0-9]>");

	info.digit = 0;
	info.erase = strlen("<hours[0-9]>");

	getkey(&info, &getdigit);

	if (info.digit == 0)
		cmdwin_print("none!");
	else if (info.digit == 1)
		cmdwin_print(" hour");
	else
		cmdwin_print(" hours");

	return info.digit;
}

int select_quantity(int max)
{
	struct get_number_info info;
	char prompt[64];

	if (max == -1) {
		snprintf(prompt, sizeof(prompt), "<quantity>");
	} else {
		snprintf(prompt, sizeof(prompt), "<quantity[0-%d]/RET=%d>", max,
			 max);
	}
	cmdwin_print(prompt);

	info.digit = 0;
	info.erase = strlen(prompt);
	info.state = GN_ALL;
	info.prompt = prompt;

	getkey(&info, getnum);

	if (info.state == GN_ALL) {
		if (max == -1)
			info.digit = 0;
		else
			info.digit = max;
	} else if (info.state == GN_CANCEL)
		cmdwin_print("none!");

	return info.digit;
}

static void hole_up_and_camp(void)
{
	int hours, yesno;
	class Character *guard = 0;

	cmdwin_clear();
	cmdwin_print("Camp-");

	if (Place->type != wilderness_place ||
	    place_get_portal(player_party->getPlace(), player_party->getX(),
			     player_party->getY()) ||
	    !place_is_passable(player_party->getPlace(),
			       player_party->getX(),
			       player_party->getY(),
			       player_party->get_pmask(), PFLAG_IGNOREVEHICLES))
	{
		cmdwin_print("not here!");
		return;
	}

	hours = select_hours();
	if (hours == 0)
		return;

	cmdwin_print("-set a watch <y/n>-");
	getkey(&yesno, &yesnokey);

	if (yesno == 'y') {

		cmdwin_backspace(strlen(" <y/n>-"));
		cmdwin_print("-");
		guard = select_party_member();
		if (!guard) {
			cmdwin_backspace(strlen("set a watch-none!"));
			cmdwin_print("no watch");
		}
                // else select_party_member() prints the name

	} else {
		cmdwin_backspace(strlen("set a watch <y/n>-"));
		cmdwin_print("no watch");
	}

	player_party->camping = true;
	run_combat(true, guard, hours);
	player_party->camping = false;
}

void dead(void)
{
	consolePrint("\n*** YOU HAVE DIED ***\n\n");
	consolePrint("Press any key to exit.\n");
	getkey(NULL, anykey);
}

static bool tickHandler(struct TickHandler *th)
{
	Tick++;
	wqRunToTick(&TickWorkQueue, Tick);
	return Quit;
}

static bool quitHandler(struct QuitHandler *kh)
{
	cmdQuit();
	return Quit;
}

static void myGenerateRandomEncounter(void)
{
	int x, y, dir;
	class NpcParty *npc;

	npc = place_random_encounter(Place);

	if (!npc)
		return;

	// Roll to pick a direction
	dir = random() % 4;

	// Convert to map coordinates
	switch (dir) {
	case 0:
		x = player_party->getX();
		y = player_party->getY() - MAP_TILE_H / 2;
		break;
	case 1:
		x = player_party->getX() + MAP_TILE_W / 2;
		y = player_party->getY();
		break;
	case 2:
		x = player_party->getX();
		y = player_party->getY() + MAP_TILE_H / 2;
		break;
	case 3:
		x = player_party->getX() - MAP_TILE_W / 2;
		y = player_party->getY();
		break;
	default:
		assert(0);
		break;
	}

	// Wrap
	x = place_wrap_x(Place, x);
	y = place_wrap_y(Place, y);

	// Check for passability
	if (!place_is_passable(Place, x, y, npc->getPmask(), 0)) {
		delete npc;
		return;
	}
	// Generate an NPC party
	npc->relocate(Place, x, y);
	// place_add_object(Place, npc);
}

struct get_spell_name_data {
	char spell_name[MAX_WORDS_IN_SPELL_NAME + 1];
	char *ptr;
	int n;
	int erase;
};

static inline void erase_spell_prompt(struct get_spell_name_data *ctx)
{
	if (ctx->erase > 0) {
		cmdwin_backspace(ctx->erase);
		ctx->erase = 0;
	}
}

bool get_spell_name(struct KeyHandler *kh, int key)
{
	struct get_spell_name_data *ctx;
	char *word, letter;

	ctx = (struct get_spell_name_data *) kh->data;
	if (key == '\n') {
		// Done
		erase_spell_prompt(ctx);
		*ctx->ptr = 0;
		return true;
	}

	if (key == CANCEL) {
		// Abort
		erase_spell_prompt(ctx);
		ctx->spell_name[0] = 0;
		return true;
	}

	if (key == '\b' && ctx->n) {
		// Backspace -- erase the previous word
		erase_spell_prompt(ctx);
		ctx->ptr--;
		letter = *ctx->ptr;
		word = Spell_words[letter - 'A'];
		cmdwin_backspace(strlen(word) + 1);
		*ctx->ptr = 0;
		ctx->n--;
		return false;
	}

	if (ctx->n == MAX_WORDS_IN_SPELL_NAME) {
		// Out of space -- refuse word
		return false;
	}

	if (!isalpha(key))
		// Not a letter
		return false;

	erase_spell_prompt(ctx);

	letter = toupper(key);
	word = Spell_words[letter - 'A'];
	if (!word)
		// Letter does not map to a magic word
		return false;

	// Accept the word and print it
	cmdwin_print("%s ", word);
	*ctx->ptr = letter;
	ctx->ptr++;
	ctx->n++;

	return false;
}

int select_spell(struct get_spell_name_data *context)
{
	struct KeyHandler kh;
	char *prompt = "<spell name>";

	// Get the spell name from the player
	memset(context, 0, sizeof(*context));
	context->ptr = context->spell_name;
	context->erase = strlen(prompt);

	kh.fx = get_spell_name;
	kh.data = context;

	eventPushKeyHandler(&kh);
	cmdwin_print(prompt);
	eventHandle();
	eventPopKeyHandler();

	if (strlen(context->spell_name) == 0) {
		cmdwin_print("none!");
		return -1;
	}

	return 0;
}

static class Object *target_character_for_spell(class Character * caster,
						class Spell * spell, int *tx,
						int *ty)
{
	class Object *target;
	class Object *ret = NULL;

	if (player_party->context != CONTEXT_COMBAT) {
		target = select_party_member();
		return target;
	}

	target = caster->getAttackTarget();
	*tx = target->getX();
	*ty = target->getY();

	if (select_target(caster->getX(), caster->getY(), tx, ty,
			  spell->range) == 0) {
		ret =
		    (class Object *) place_get_object(Place, *tx, *ty,
						      being_layer);
	}

	if (ret == NULL)
		cmdwin_print("none!");
	else
		cmdwin_print(ret->getName());

	return ret;
}

static class Object *target_mech_for_spell(class Character * caster,
					   class Spell * spell)
{
	int x, y;
	class Object *ret = NULL;

	if (player_party->context == CONTEXT_COMBAT) {
		x = caster->getX();
		y = caster->getY();
	} else {
		x = player_party->getX();
		y = player_party->getY();
	}

	if (select_target(x, y, &x, &y, spell->range) == 0) {
		ret =
		    (class Object *) place_get_object(Place, x, y, mech_layer);
	}

	if (ret == NULL)
		cmdwin_print("none!");
	else
		cmdwin_print(ret->getName());

	return ret;
}

static bool
target_location_for_spell(class Character * caster,
			  class Object ** ret,
			  class Spell * spell, int *x, int *y)
{
	class Character *target;

	target = caster->getAttackTarget();
	*x = target->getX();
	*y = target->getY();

	if (select_target(caster->getX(), caster->getY(), x, y,
			  spell->range) == -1)
		return false;

	// Note: cheat a bit here. If possible, grab a character from this
	// location as the target. Currently all the location-targeting spells
	// (magical fields) should also have the property of affecting an
	// occupant if one exists, or simply landing on the location if one
	// doesn't.
	*ret = (class Object *) place_get_object(Place, *x, *y, being_layer);

	return true;
}

static bool
target_spell(class Spell * spell, class Character * pc,
	     class Object ** ret, int *direction, int *x, int *y)
{
	switch (spell->target) {
	case SPELL_TARGET_NONE:
		// *ret = pc; 
		return true;
	case SPELL_TARGET_LOCATION:
		cmdwin_print("-");
		return target_location_for_spell(pc, ret, spell, x, y);
	case SPELL_TARGET_CHARACTER:
		cmdwin_print("-");
		*ret = target_character_for_spell(pc, spell, x, y);
		break;
	case SPELL_TARGET_MECH:
		cmdwin_print("-");
		*ret = target_mech_for_spell(pc, spell);
		break;
	case SPELL_TARGET_DIRECTION:
		cmdwin_print("-");
		*ret = pc;
		*direction = ui_get_direction();
		return (*direction != CANCEL);
	case SPELL_TARGET_UP:
		*direction = UP;
		return true;
	case SPELL_TARGET_DOWN:
		*direction = DOWN;
		return true;
	case SPELL_TARGET_ALL_PARTY_MEMBERS:
		return true;
	default:
		return false;
	}

	return (*ret != NULL);
}

bool cmdCastSpell(class Character * pc)
{
	struct get_spell_name_data context;
	struct inv_entry *ie = NULL;
	class Spell *spell;
	class Object *target = NULL;
	bool mixed = false;
	bool natural = false;
	int i;
	int tries;
	int max_tries;
	int tx, ty;

	cmdwin_clear();
	cmdwin_print("Cast");

	// If the pc is null then we are in non-combat mode and need to prompt
	// the user.
	if (pc == NULL) {
		cmdwin_print("-");
		pc = select_party_member();
		if (pc == NULL) {
			return false;
		}
		statusSetMode(ShowParty);
	}
	// Prompt to select a spell
	cmdwin_print("-");
	if (select_spell(&context) == -1)
		return false;

	// erase the extra ' ' left by the selection routine
	cmdwin_backspace(1);

	// Lookup the spell
	spell = Spell_lookup_by_code(context.spell_name);
	if (!spell) {
		cmdwin_print("-no effect!");
		return false;
	}
	// Check if the spell can be used in this context.
	if (!(player_party->context & spell->context)) {
		cmdwin_print("-not here!");
		return false;
	}
	// Check if the caster is of sufficient level
	if (pc->getLevel() < spell->level) {
		cmdwin_print("-need more experience!");
		return false;
	}
	// Check if the character comes by this spell naturally
	for (i = 0; i < pc->species->n_spells; i++) {
		if (pc->species->spells[i] == spell) {
			natural = true;
			break;
		}
	}

	// Otherwise check party inventory for a mixed spell.
	if (!natural) {
		ie = player_party->search_inventory(spell);
		if (ie && ie->count)
			mixed = true;
	}

	if (!natural && !mixed) {
		cmdwin_print("-none mixed!");
		return false;
	}
	// Check if the character has enough mana to cast the spell.
	if (pc->getMana() < spell->cost) {
		cmdwin_print("-need more mana!");
		return false;
	}
	// High-level casters get multiple tries per spell.
	// max_tries = 1 + pc->getLevel() / 3;
	max_tries = 1;

	for (tries = 0; tries < max_tries; tries++) {

		int direction = DIRECTION_NONE;

		// Select a target for the spell.
		if (!target_spell(spell, pc, &target, &direction, &tx, &ty))
			break;

		// Cast the spell. This automatically decrements the caster's
		// mana.
		consolePrint("%s casts %s", pc->getName(), spell->getName());
		if (direction != DIRECTION_NONE)
			consolePrint(" to the %s",
				     directionToString(direction));
		else if (target)
			consolePrint(" on %s", target->getName());
		consolePrint("...");
		switch (spell->cast(pc, target, direction, tx, ty)) {
		case Spell::ok:
			consolePrint("success!\n");
			break;
		case Spell::no_room_on_battlefield:
			consolePrint("summoned party fails to enter!\n");
			break;
		case Spell::magic_negated:
			consolePrint("a mysterious force blocks magic!\n");
			break;
		case Spell::missed_target:
			consolePrint("but misses!\n");
			break;
		case Spell::no_effect:
			consolePrint("no effect!\n");
			break;
		case Spell::teleport_failed:
			consolePrint("movement failed!\n");
			break;
		case Spell::unknown_failure:
		default:
			consolePrint("fizzles!\n");
			break;
		}

		consolePrint("\n");

		statusRepaint();

	}

	if (tries == 0)
		return false;

	// If the spell was mixed then remove it from inventory.
	if (mixed)
		player_party->remove_from_inventory(ie, 1);

	// Update the character's target memory. This will automatically resort
	// back to the character as its own target if this target is dead.
	if (spell->target == SPELL_TARGET_CHARACTER)
		pc->setAttackTarget((class Character *) target);

	return true;
}

bool cmdMixReagents(void)
{
	class Spell *spell;
	struct get_spell_name_data context;
	struct list reagents, *elem;
	int quantity, max_quantity;
	struct inv_entry *ie;
	bool mistake = false;

	list_init(&reagents);

	cmdwin_clear();
	cmdwin_print("Mix-");

	// Select a spell...
	if (select_spell(&context) == -1)
		return false;

	// Lookup the spell. If null then keep going and bomb when done.
	spell = Spell_lookup_by_code(context.spell_name);
	cmdwin_backspace(1);

	// Prompt for reagents 
	cmdwin_print("-");
	cmdwin_mark();
	cmdwin_print("<select, then 'm' to mix when done selecting>");

	// Show the reagents in the status window
	statusSetMode(MixReagents);

	struct ScrollerContext sc;
	sc.selector = Reagents;
	sc.done = false;
	sc.abort = false;
	sc.mixing = true;

	struct KeyHandler kh;
	kh.fx = scroller;
	kh.data = &sc;

	eventPushKeyHandler(&kh);

	for (;;) {
		sc.selection = NULL;
		eventHandle();

		if (sc.abort) {
			// u5 silently aborts here
			cmdwin_erase_back_to_mark();
			eventPopKeyHandler();
			cmdwin_print("none!");
			goto done;
		}

		if (sc.done)
			break;

		ie = (struct inv_entry *) sc.selection;
		assert(ie);

		if (ie->ref) {
			// unselect
			ie->ref = 0;
			list_remove(&ie->auxlist);
		} else {
			// select
			ie->ref = 1;
			list_add(&reagents, &ie->auxlist);
		}

		statusRepaint();
	}

	cmdwin_erase_back_to_mark();
	eventPopKeyHandler();

	if (list_empty(&reagents)) {
		cmdwin_print("none!");
		goto done;
	}
	// Determine the max number of mixtures the player can make.
	max_quantity = 0x7fffff;
	list_for_each(&reagents, elem) {
		ie = outcast(elem, struct inv_entry, auxlist);
		if (ie->count < max_quantity) {
			max_quantity = ie->count;
		}
	}

	// Prompt for the number of mixtures to make
	for (;;) {

		int dummy;

		quantity = select_quantity(max_quantity);

		if (quantity == 0) {
			goto done;
		}

		if (quantity == 1)
			cmdwin_print(" mixture");
		else
			cmdwin_print(" mixtures");

		if (quantity <= max_quantity)
			break;

		cmdwin_print("-not enough reagents! Hit any key to retry");
		getkey(&dummy, anykey);
		cmdwin_erase_back_to_mark();
	}

	cmdwin_print("-");

	// For each reagent required by the spell, check if it is in the list
	// of reagents given by the player. If not then remember this fact. If
	// the reagent is found then remove it from player inventory and remove
	// it from the list.
	if (spell) {
		for (int i = 0; i < spell->n_reagents; i++) {
			bool found = false;
			list_for_each(&reagents, elem) {
				ie = outcast(elem, struct inv_entry, auxlist);
				if (ie->type ==
				    (class ObjectType *) spell->reagents[i]) {
					ie->ref--;
					player_party->remove_from_inventory(ie,
									    quantity);

					list_remove(elem);	// safe only
					// because
					// this is the end
					// of the
					// list_for_each
					// loop!
					found = true;
					break;
				}
			}
			if (!found)
				mistake = true;
		}
	}
	// Now, if any reagents remain leftover then remember this fact and
	// remove the remaining reagents from inventory.
	if (!list_empty(&reagents)) {
		mistake = true;
		elem = reagents.next;
		while (elem != &reagents) {
			struct list *tmp = elem->next;
			ie = outcast(elem, struct inv_entry, auxlist);
			list_remove(elem);
			elem = tmp;
			ie->ref--;
			player_party->remove_from_inventory(ie, quantity);
		}
	}

	consolePrint("Mixing spell...");
	consoleRepaint();

	// If the spell is invalid or the reagents are incorrect then punish
	// the player.
	if (!spell || mistake) {
		statusSetMode(ShowParty);
		int damage;
		switch (random() % 3) {
		case 0:
		default:
			cmdwin_print("oops!");
			consolePrint("ACID!\n");
			damage = DAMAGE_ACID;
			player_party->for_each_member(apply_damage, &damage);
			break;
		case 1:
			cmdwin_print("ouch!");
			consolePrint("BOMB!\n");
			damage = DAMAGE_BOMB;
			player_party->for_each_member(apply_damage, &damage);
			break;
		case 2:
			cmdwin_print("yuck!");
			consolePrint("GAS!\n");
			player_party->for_each_member(apply_poison, NULL);
			break;
		}

		return true;
	}
	// All is well. Add the spell to player inventory.
	cmdwin_print("ok");
	consolePrint("%s!\n", spell->getName());
	player_party->add_to_inventory(spell, quantity);

      done:
	// In case of cancellation I need to unselect all the reagents.
	elem = reagents.next;
	while (elem != &reagents) {
		struct list *tmp = elem->next;
		ie = outcast(elem, struct inv_entry, auxlist);
		list_remove(elem);
		elem = tmp;
		ie->ref--;
	}
	statusSetMode(ShowParty);
	return true;
}

void look_at_XY(int x, int y)
{
	consolePrint("At XY=(%d,%d) you see ", x, y);
	placeDescribe(x, y);
}
void detailed_examine_XY(int x, int y)
{
	// SAM: 
	// Hmmm...how best to print more info about
	// the objects on this tile?
	consolePrint("TARGET XY=(%d,%d)\n", x, y);
}

bool cmdXamine(class Character * pc)
{
	// SAM: Working on an improved (L)ook command,
	// which works as a "Look Mode" rather than a 
	// "look at 1 tile" command...
	int x, y;

	cmdwin_clear();
	cmdwin_print("Xamine-");

	if (pc) {
		// A party member was specified as a parameter, so this must be
		// combat mode. Use the party member's location as the origin.
		x = pc->getX();
		y = pc->getY();
		// SAM: We don't care now who is examining stuff.
		// Conceivably in future, we might 
		// (different characters with different sensory abilities, 
		// or knowledge of the names of objects, or some such).  
		// But that day is a long ways off.
	} else {
		// Must be party mode. 
		// Use the player party's location as the origin.
		x = player_party->getX();
		y = player_party->getY();
	}

	if (select_target_with_doing(x, y, &x, &y, 9,
				     look_at_XY, detailed_examine_XY) == -1) {
		return false;
	}
	return true;
} // cmdXamine()

char * name_of_context (void)
{
  // SAM: Perhaps this function belongs in common.c?
  int context = player_party->context;
  switch (context) {
  case CONTEXT_WILDERNESS:
    return "Wilderness Mode";
  case CONTEXT_COMBAT:
    return "Combat Mode";
  case CONTEXT_TOWN:
    return "Town Mode";
  default: assert(0);
  }
} // name_of_context()

bool cmdAT (class Character * pc)
{
	int x, y;
    char * who = "";
    char * place_name = "";

	cmdwin_clear();

    // Should I check player_party->context
    // for the context info below, 
    // rather than the current method?
	if (pc) {
		// A party member was specified as a parameter, so this must be
		// combat mode. Use the party member's location as the origin.
        who = pc->getName();
        place_name = "a combat map";  // Hack
        // SAM: The below won't work. 
        //      'Combat' is declared static in combat.c ...
        // place_name = Combat.place.name;
		x = pc->getX();
		y = pc->getY();
	}
    else {
		// Must be party mode. 
		// Use the player party's location as the origin.
        who = "The party";
        place_name = player_party->getPlace()->name;
        x = player_party->getX();
        y = player_party->getY();
	}
    // SAM: Why is this line not safe in combat mode?
    //      Would it be The Right Thing (TM) 
    //      for it to be made safe in all contexts?
    // place_name = player_party->getPlace()->name;
    
    consolePrint("This is %s\n", name_of_context() );
    consolePrint("%s is at %s (%d,%d)\n", who, place_name, x, y);

    consolePrint("It is %s on %s, \n"
                 "%s of %s in the year %d.\n",
                 time_HHMM_as_string(), day_name(), 
                 week_name(), month_name(), Clock.year );

    // SAM: Is this really interesting though, I wonder?
    consolePrint("%d game turns have passed.\n", Turn);

    consolePrint("The wind is blowing from the %s.\n",
                 directionToString(windGetDirection()) );

    if (Place->underground) {
      consolePrint("%s is underground, and cannot see the sky.\n", who);
    } // underground
    else {
      // SAM: 
      // This message won't be true if you are under 
      // a roof in a town.  In future there should be 
      // logic querying the (future) roof-ripping code here.
      consolePrint("%s is beneath the open sky.\n", who);

      // Message(s) about the sun:
      if (sun_is_up() ) {
        consolePrint("The sun is up.%s\n",
                     is_noon() ? "  It is noon." : "");
      }
      if (sun_is_down() ) {
        consolePrint("The sun has set.%s\n",
                     is_midnight() ? "  It is midnight." : "");
      }

      // Message(s) about the moon(s):
      int num_moons_visible = 0;
      for (int i = 0; i < NUM_MOONS; i++) {
        struct moon *moon = &Moons[i];

        if (moon_is_visible(moon->arc))
          {
            num_moons_visible++;
            consolePrint("Moon %d is at arc %d in phase %d.\n",
                         i, moon->arc, moon->phase);
            // SAM:
            // In future, we shall want GhulScript for
            // the names of the heavenly bodies, 
            // and the names of their phases:
            // 
            // consolePrint("%s is %s in phase %s.\n",
            //              moon_name(i), 
            //              arc_description(moon->arc), 
            //              phase_name(moon->phase) );

          }
        // moon->arc,
        // MoonInfo.sprite[moon->phase]
      }
      if (num_moons_visible == 0)
        consolePrint("No moons can be seen now.\n");

    } // open air, under the sky

    if (player_party->vehicle) {
      consolePrint("%s is %s a %s.\n", 
                   who, "using", player_party->vehicle->getName() );
      // SAM:
      // In future, we shall want GhulScript to specify 
      // whether one is to be
      //     "riding" "driving" "piloting" "sailing"
      // a particular vehicle type.
      // The existing 'mv_desc' field (Ride, Sail)
      // is related, but we need the gerund of the verb.
    }
    else {
      // SAM: Not true for a party of Gazers or Nixies.
      // Similar GhulScript for party / character movement mode
      // descriptions and gerunds?
      consolePrint("%s is on foot.\n", who);
    }
    

    /*
      Information reported shall include:
      X the current place,X,Y
      X the in-game time in game time units (HH:MM, YYYY/MM/DD)
      X the in-game time in elapased turns (this is of less interest)
      X the current weather status (wind)
      X the current astronomy (day/night, sun, moons)
      X the UI mode of the party (Wilderness,Town,Dungeon,Combat)
      X whether the party is on foot or in a vehicle (and what type)
      . the number of party members, names, order, and basic vital stats
      . global party stats such as food and gold
      . any special effects (buffs/nerfs) currently affecting the party,
      . such as haste/quickness/slow, time stop, protection, etc.
      . Xamine type information about the tile the party is standing on.
    */

	return true;
} // cmdAT()

static bool keyHandler(struct KeyHandler *kh, int key)
{
	// This handler is always on the bottom of the key handler stack. When
	// it returns true the event loop exits, effectively ending the
	// game. So return true iff the player is dead or wants to quit.

	// Decode and run player commands. Any commands which require further
	// input from the player will push new key handlers on the stack and
	// pop them upon return. I like to think that they are "closed" in some
	// sense of the word, and don't intermingle their command input state
	// with ours.

	int saved_turn = Turn;
	int turns_used = 1;	// default used by most actions

	switch (key) {

	case KEY_NORTH:
	case KEY_EAST:
	case KEY_SOUTH:
	case KEY_WEST:
		{
			int dir = keyToDirection(key);
			player_party->move(directionToDx(dir),
					   directionToDy(dir), false);
			turns_used = 0;	// turns already advanced in
			// player_party->move
			mapSetDirty();
		}
		break;

	case 'b':
		player_party->board_vehicle();
		break;
	case 'c':
		cmdCastSpell(NULL);
		break;
	case 'e':
		player_party->enter_portal();
		break;
	case 'f':
		cmdFire();
		break;
	case 'g':
		cmdGet(player_party->getX(), player_party->getY(), true);
		mapSetDirty();
		break;
	case 'h':
		// SAM: Adding (H)andle command...
		cmdHandle(NULL);
		break;
	case 'k':
		hole_up_and_camp();
		break;
	case 'l':
		// SAM: Changing (L)ook command 
		// from "look at 1 tile" to a "Look Mode"
		cmdLook(player_party->getX(), player_party->getY());
		break;
	case 'm':
		cmdMixReagents();
		break;
	case 's':
		myNewOrder();
		break;
	case 'o':
		cmdOpen(NULL);
		break;
	case 'q':
		cmdQuit();
		break;
	case 'r':
		cmdReady(NULL);
		break;
	case 't':
		myTalk();
		break;
	case 'u':
		cmdUse(NULL);
		break;
	case 'x':
		cmdXamine(NULL);	// SAM: 1st step towards new (L)ook
					// cmd...
		break;
	case 'z':
		cmdZtats(NULL);
		break;
    case '@':
      // SAM: 'AT' command for party-centric information
      cmdAT(NULL);
      break;
	case ' ':
		consolePrint("Pass\n");
		turns_used = Place->scale;
		break;
	case '<':
		// If standing over a portal then enter it
		if (place_get_portal(player_party->getPlace(),
				     player_party->getX(),
				     player_party->getY()))
			player_party->enter_portal();
		else if (!place_is_passable(player_party->getPlace(),
					    player_party->getX(),
					    player_party->getY(),
					    player_party->get_pmask(),
					    PFLAG_IGNOREVEHICLES))
			consolePrint("Cannot zoom-in here!\n");
		else
			run_combat(false, 0, 0);
		break;
	default:
		turns_used = 0;
		break;
	}

	// Quit now before advancing turns or anything of that sort. It makes
	// life easier.
	if (Quit)
		return Quit;

	turnAdvance(turns_used);

	if (Turn != saved_turn) {

		// Sigh. Time has definitely grown into a wearisome hack. There
		// must be a better way.

		// Note: always update the clock before the turn wq. For
		// example, when entering a place all the NPC parties use the
		// wall clock time to synchronize their schedules, so it needs
		// to be set BEFORE calling them.

		clockUpdate();

		foogodAdvanceTurns();
		placeAdvanceTurns();
		player_party->advance_turns();
		skyAdvanceTurns();
		windAdvanceTurns();

		// Most commands burn through at least one turn. Let the
		// turn-based work queue catch up.
		wqRunToTick(&TurnWorkQueue, Turn);
	}
	// The player may have died as a result of executing a command or
	// running the work queue.
	if (player_party->all_dead()) {
		dead();
		return true;
	}

	if (!Quit)		// fixme: is this check necessary? 
		myGenerateRandomEncounter();

	return Quit;
}

static int play_init(struct play *play)
{

	Cursor = NULL;

	/* Load the game */
	if (loadGame(play->savefile) < 0) {
		err("Error loading game from '%s'", play->savefile);
		return -1;
	}

	if (Cursor == NULL) {
		err("No CROSSHAIR defined in savefile '%s'", play->savefile);
		return -1;
	}

	statusInit();		/* before painting the frame for the first time 
				 */
	foogodInit();		/* before painting the frame for the first time 
				 */
	screen_repaint_frame();

	statusSetMode(ShowParty);	/* note: must do this after painting
					 * the frame because the title is
					 * always painted OVER the frame. */

	mapInit(play->los_name);	// must be before placeEnter()
	mapSetPlace(Place);	        // must be before placeEnter()
	consoleInit();                  // must be before placeEnter()
	placeEnter();

	moongateSetAnimationWorkQueue(&TickWorkQueue);
	skyInit();

	windSetDirection(NORTH, 1);

	player_party->view = mapCreateView();
	if (!player_party->view)
		return -1;
	mapAddView(player_party->view);
	mapCenterView(player_party->view, player_party->getX(),
		      player_party->getY());
	player_party->recompute_los();
	mapCenterCamera(player_party->getX(), player_party->getY());
	mapUpdate(0);
	screenUpdate(0);

	foogodRepaint();

	/* Setup the astar pathfinding lib */
	if (astar_init())
		return -1;

	/* Clear the message window */
	consoleRepaint();

	/* Show the status window */
	statusRepaint();

	spriteStartAnimation(&TickWorkQueue, Tick + 1);

	play->cmdstate = CMD_IDLE;

	return 0;
}

void play_destroy(struct play *play)
{

	if (LosEngine)
		los_destroy(LosEngine);
	free(play);
}
struct play *play_create(void)
{
	struct play *play;

	CREATE(play, struct play, 0);

	play->savefile = SAVEFILE;
	play->los_name = LOS;

	Play = play;		/* hack */
	return play;
}

static void updateAfterEvent(void)
{
	// mapRepaintView(0, REPAINT_ACTIVE|REPAINT_IF_DIRTY);
	mapUpdate(REPAINT_IF_DIRTY);
}

int playRun(void)
{
	struct KeyHandler kh;
	struct QuitHandler qh;
	struct TickHandler th;

	struct play *play;

	play = play_create();

	if (play_init(play) < 0)
		return -1;

	consolePrint("Welcome to nazghul v0.1\n");

	// Setup all the event handlers.
	kh.fx = keyHandler;
	qh.fx = quitHandler;
	th.fx = tickHandler;
	eventPushKeyHandler(&kh);
	eventPushQuitHandler(&qh);
	eventPushTickHandler(&th);
	eventAddHook(updateAfterEvent);

	Quit = false;

	// Major hack warning: if the game loads up with the player in a
	// dungeon then we need to force the game into dungeon mode. The
	// easiest way to do that is to have the player party "enter" the
	// dungeon it's already in. The last two args are the direction vector
	// - just fake them to "north".
	if (place_is_dungeon(player_party->getPlace())) {
		if (!player_party->enter_dungeon(player_party->getPlace(),
						 player_party->getX(),
						 player_party->getY(), 0, 1)) {
			err("Bad starting position for party: %s [%d %d]\n",
			    player_party->getPlace()->name,
			    player_party->getX(), player_party->getY());
			return -1;
		}
	}

	// Enter the main event loop. This won't exit until the player quits or
	// dies.
	eventHandle();

	// Cleanup the event handlers.
	eventPopTickHandler();
	eventPopQuitHandler();
	eventPopKeyHandler();

	return 1;
}
