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

#include "cmd.h"
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
#include "portal.h"
#include "terrain.h"
#include "vmask.h"

#define DEBUG
#include "debug.h"

#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
// #include <sys/types.h>  // getpid()
#include <unistd.h>     // getpid()
#include <errno.h>

bool dirkey(struct KeyHandler *kh, int key, int keymod)
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

bool yesnokey(struct KeyHandler * kh, int key, int keymod)
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

bool getnum(struct KeyHandler *kh, int key, int keymod)
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

bool getdigit(struct KeyHandler * kh, int key, int keymod)
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

bool anykey(struct KeyHandler * kh, int key, int keymod)
{
	return true;
}

bool scroller(struct KeyHandler * kh, int key, int keymod)
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

bool movecursor(struct KeyHandler * kh, int key, int keymod)
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
                mapSetDirty();
                return false;  // Keep on keyhandling
        }
  
        if (key == SDLK_ESCAPE) {
                data->abort = true;
                return true;  // Done (abort)
        }
  
        return false;  // Keep on keyhandling
} // movecursor()

bool movecursor_and_do(struct KeyHandler * kh, int key, int keymod)
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
                mapSetDirty();
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

bool terraform_movecursor_and_do(struct KeyHandler * kh, int key, int keymod)
{
        // As movecursor_and_do(), but with additional keybindings
        // intended for cmdTerraform().
        struct terraform_mode_keyhandler * data;
        struct terrain_palette * pp;
        struct terrain * tt;
        assert(kh);
        data = (struct terraform_mode_keyhandler *) kh->data;
        pp   = data->palette;
  
        if (key == '\n' || key == SDLK_SPACE || key == SDLK_RETURN) {
                int x = Cursor->getX();
                int y = Cursor->getY();
                if (data->each_target_func)
                        data->each_target_func(x, y, data);
                return false;  // Keep on keyhandling
        }

        if (keyIsDirection(key)) {
                int dir = keyToDirection(key);
                Cursor->move(directionToDx(dir), directionToDy(dir));
                mapSetDirty();
                int x = Cursor->getX();
                int y = Cursor->getY();
                if (data->each_point_func)
                        data->each_point_func(x, y, data);
                return false;  // Keep on keyhandling
        }

        if (key == SDLK_PAGEUP) {
                // Page Up == Cycle back through terrain in palette
                palette_prev_terrain(pp);
                tt = palette_current_terrain(pp);
                consolePrint("[Prev]  terrain %s '%s'\n", tt->tag, tt->name);
                return false;  // Keep on keyhandling
        }
        if (key == SDLK_PAGEDOWN) {
                // Page Down == Cycle forward through terrain in palette
                palette_next_terrain(pp);
                tt = palette_current_terrain(pp);
                consolePrint("[Next]  terrain %s '%s'\n", tt->tag, tt->name);
                return false;  // Keep on keyhandling
        }
        if (key == SDLK_HOME) {
                // Home == Select first terrain in palette
                palette_first_terrain(pp);
                tt = palette_current_terrain(pp);
                consolePrint("[First] terrain %s '%s'\n", tt->tag, tt->name);
                return false;  // Keep on keyhandling
        }
        if (key == SDLK_END) {
                // End == Select last terrain in palette
                palette_last_terrain(pp);
                tt = palette_current_terrain(pp);
                consolePrint("[Last]  terrain %s '%s'\n", tt->tag, tt->name);
                return false;  // Keep on keyhandling
        }

        if (key >= SDLK_0 && key <= SDLK_9) {
                // Number key 0..9 == get/set quick terrain
                int qt = num_for_key(key);
    
                if ((keymod && KMOD_LCTRL) || (keymod && KMOD_RCTRL)) {
                        // Control-NUM == set quick terrain to current:
                        int index = palette_get_current_terrain_index(pp);
                        palette_set_quick_terrain(pp, qt, index);
                        tt = palette_current_terrain(pp);
                        consolePrint("[Quick %d] set to %s '%s'\n", qt, tt->tag, tt->name);
                        return false; // Keep on keyhandling
                }
                // Plain NUM == set current terrain from quick terrain:
                int index = palette_get_quick_terrain_index(pp, qt);
                palette_set_current_terrain(pp, index);
                tt = palette_current_terrain(pp);
                consolePrint("[Quick %d] %s '%s'\n", qt, tt->tag, tt->name);
                return false;  // Keep on keyhandling
        }

        // ...
    
        if (key == SDLK_ESCAPE) {
                data->abort = true;
                return true;  // Done (abort)
        }
        return false;  // Keep on keyhandling
} // terraform_movecursor_and_do()

int num_for_key (int key)
{
        int num;
        switch (key) {
        case SDLK_0:
        case SDLK_KP0:
                num = 0;
                break;
        case SDLK_1:
        case SDLK_KP1:
                num = 1;
                break;
        case SDLK_2:
        case SDLK_KP2:
                num = 2;
                break;
        case SDLK_3:
        case SDLK_KP3:
                num = 3;
                break;
        case SDLK_4:
        case SDLK_KP4:
                num = 4;
                break;
        case SDLK_5:
        case SDLK_KP5:
                num = 5;
                break;
        case SDLK_6:
        case SDLK_KP6:
                num = 6;
                break;
        case SDLK_7:
        case SDLK_KP7:
                num = 7;
                break;
        case SDLK_8:
        case SDLK_KP8:
                num = 8;
                break;
        case SDLK_9:
        case SDLK_KP9:
                num = 9;
                break;
        default:
                printf("num_for_key() IMPOSSIBLE - funky number %d\n", key);
                assert(0);
        } // switch(key)
        return num;
} // num_for_key()

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

void getkey(void *data, bool(*handler) (struct KeyHandler * kh, int key, int keymod))
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

bool cmdSearch(int x, int y)
{
	int dir;
        bool old_reveal;

	cmdwin_clear();
	cmdwin_print("Search-");

	dir = ui_get_direction();
	if (dir == CANCEL)
		return false;

	consolePrint("You find ");
        old_reveal = Reveal;
        Reveal = true;
	placeDescribe(placeWrapX(x + directionToDx(dir)),
		      placeWrapY(y + directionToDy(dir)),
                      PLACE_DESCRIBE_ALL);
        Reveal = old_reveal;
	return true;
}

bool cmdGet(class Object *actor, bool scoop_all)
{
	class Object *item;
	class ObjectType *type;
	int count;
	int dir;
	int n_item_types;
        int x, y;

	cmdwin_clear();
	cmdwin_print("Get-");

	dir = ui_get_direction();
	if (dir == CANCEL)
		return false;

	consolePrint("You get ");

        x = actor->getX() + directionToDx(dir);
        y = actor->getY() + directionToDy(dir);

	item = place_get_item(actor->getPlace(), x, y);
	if (!item) {
		consolePrint("nothing!\n");
		return false;
	}
       
	n_item_types = 1;
	count = 1;
	type = item->getObjectType();

        place_remove_object(actor->getPlace(), item);
        actor->addToInventory(item);

	if (scoop_all) {
		while (NULL != (item = place_get_item(actor->getPlace(), x, y))) {
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

                        place_remove_object(actor->getPlace(), item);
                        actor->addToInventory(item);
		}
	}

	if (n_item_types > 1) {
		consolePrint(" and ");
	}
	type->describe(count);
	consolePrint(".\n");
        mapSetDirty();

        actor->decActionPoints(NAZGHUL_BASE_ACTION_POINTS);

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
                consolePrint("Open ");
                mech->describe(1);
		if (mech->activate(MECH_OPEN)) {
			cmdwin_print("ok");
			// SAM: Should this printing be done entirely by mech
			// scripting?
			consolePrint(".\n");
			mapSetDirty();
		} else {
                        consolePrint("-failed!\n");
			cmdwin_print("failed!");
		}
		return true;
	}

	/*** Open Container ***/

	// Get the container.
	container = (class Container *) place_get_object(Place, x, y, container_layer);
	if (NULL == container) {
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
        pc->decActionPoints(NAZGHUL_BASE_ACTION_POINTS);

	// Check for traps.
	if (!container->isTrapped()) {
		consolePrint(".\n");
	} else {

		class TrapType *trap = container->getTrap();
		consolePrint("...%s...", trap->getName());

		// Roll to disarm
		if (random() % 999 < pc->getDexterity()) {
			consolePrint("disarmed!\n");
		} else {
			consolePrint("oops!\n");
			int effects = trap->getEffects();
			if (effects & EFFECT_BURN) {
				pc->damage(trap->getAmount());
			}
			if (effects & EFFECT_POISON && !pc->isPoisoned()) {
				pc->setPoison(true);
				pc->damage(trap->getAmount());
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

        // ---------------------------------------------------------------------
        // Delete container automatically on the combat map because if
        // containers are stacked (and they frequently are) then the top one
        // always gets selected and the player can't get at the ones
        // underneath. On the other hand, in towns I don't want to delete
        // people's furniture.
        //
        // Addendum: everything stated above still holds, but now corpse loot
        // can also get dropped on town maps outside, so I can no longer decide
        // whether or not to delete a container based on context. Furthermore,
        // I want all containers to behave the same way as much as
        // possible. This is an open issue and it may take some time and user
        // feedback to decide what best to do, so for now I'll simply always
        // remove containers after opening them.
        // ---------------------------------------------------------------------

        container->remove();
        delete container;

        consolePrint("You find ");
	placeDescribe(x, y, PLACE_DESCRIBE_OBJECTS);


	mapSetDirty();
	return true;
}

bool cmdQuit(void)
{
	int yesno;

	cmdwin_clear();
	cmdwin_print("Quit Game-Y/N?");
	getkey(&yesno, yesnokey);

	cmdwin_backspace(4);

	if (yesno == 'y') {
		cmdwin_print("Yes!");
		consolePrint("Goodbye!\n");
		Quit = true;
	} else {
		cmdwin_print("No");
	}

	return Quit;
}

void cmdAttack(void)
{
        int dir;
        struct move_info info;
        struct combat_info cinfo;

        // Initialize data structures.
        memset(&info, 0, sizeof(info));
        memset(&cinfo, 0, sizeof(cinfo));
        cinfo.move = &info;
        cinfo.defend = false;
        
        // Get the direction
	cmdwin_clear();
	cmdwin_print("Attack-<direction>");
	getkey(&dir, dirkey);
	cmdwin_backspace(strlen("<direction>"));
	if (dir == CANCEL) {
		cmdwin_print("none!");
		return;
	}
	cmdwin_print("%s", directionToString(dir));

        // Get the npc party being attacked
        info.dx = directionToDx(dir);
        info.dy =  directionToDy(dir);;
        info.x = player_party->getX() + info.dx;
        info.y = player_party->getY() + info.dy;
        info.place = player_party->getPlace();
        info.npc_party = place_get_NpcParty(info.place, info.x, info.y);
        if (info.npc_party == NULL) {
                cmdwin_print("-nobody there!");
                return;
        }
        cmdwin_print("-%s", info.npc_party->getName());

        // If the npc is not hostile then get player confirmation.
        if (!info.npc_party->isHostile(player_party->alignment)) {
                int yesno;
                cmdwin_print("-attack non-hostile-<y/n>");
                getkey(&yesno, yesnokey);
                cmdwin_backspace(strlen("<y/n>"));
                if (yesno == 'n') {
                        cmdwin_print("no");
                        return;
                }
                cmdwin_print("yes");

                // Make the npc party (and all its friends) hostile
                player_party->alignment &= ~(info.npc_party->getAlignment());
        }

        // Enter combat
        player_party->move_to_wilderness_combat(&cinfo);
}

void cmdFire(void)
{
	int dir;

	cmdwin_clear();
	cmdwin_print("Fire");

	if ((!player_party->vehicle ||
             !player_party->vehicle->getOrdnance())) {
                // SAM: 
                // In future, we may check for adjacent "cannon" 
                // mechanisms here (as in U5).
		cmdwin_print("-No cannons available!");
		return;
	}

	cmdwin_print(" %s-<direction>", player_party->vehicle->getOrdnance()->getName());
	getkey(&dir, dirkey);
	cmdwin_backspace(strlen("<direction>"));

	if (dir == CANCEL) {
		cmdwin_print("none!");
		return;
	}

	cmdwin_print("%s", directionToString(dir));
	if (! player_party->vehicle->fire_weapon(directionToDx(dir), directionToDy(dir), player_party)) {
		cmdwin_print("-Not a broadside!");
		return;
        }
}

bool cmdReady(class Character * member, int flags)
{
	bool committed = false;
	struct inv_entry *ie;
	struct KeyHandler kh;
	struct ScrollerContext sc;
	int erase;
	char *msg = 0;

	cmdwin_clear();
	cmdwin_print("Ready-");

        // Select user
        if (flags & CMD_SELECT_MEMBER) {
                member = select_party_member();
                if (member == NULL)
                        return false;       
        } else {
                assert(member);
                cmdwin_print("%s", member->getName());
        }
	statusSelectCharacter(member->getOrder());

	cmdwin_print("-");
	statusSelectCharacter(member->getOrder());

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

		if (ie->ref && member->unready(arms)) {
			msg = "unreadied!";
			ie->ref--;
			statusRepaint();
		} else {

			switch (member->ready(arms)) {
			case Character::Readied:
				ie->ref++;
				statusRepaint();
				msg = "readied";
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

        if (committed) {
                if (flags & CMD_PRINT_MEMBER) {
                        consolePrint("%s ", member->getName());
                }
                consolePrint("readies arms.\n");
                member->decActionPoints(NAZGHUL_BASE_ACTION_POINTS);
        }

	return committed;
}

int select_target(int ox, int oy, int *x, int *y, int range)
{
        Cursor->setRange(range);
        Cursor->setOrigin(ox, oy);
        Cursor->relocate(Place, *x, *y);  // Remember prev target, if any
        mapSetDirty();
  
        struct cursor_movement_keyhandler data;
        data.each_point_func  = NULL;
        data.each_target_func = NULL;
        data.abort            = false;
        struct KeyHandler kh;
        kh.fx   = movecursor;
        kh.data = &data;
  
        eventPushKeyHandler(&kh);
        cmdwin_print("<target> (ESC to cancel)");
        eventHandle();
        cmdwin_backspace(strlen("<target> (ESC to cancel)"));
        eventPopKeyHandler();
  
        *x = Cursor->getX();
        *y = Cursor->getY();
        Cursor->remove();
        mapSetDirty();
  
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
        Cursor->setViewportBounded(1);
        Cursor->setOrigin(ox, oy);
        Cursor->relocate(Place, *x, *y);  // Remember prev target, if any
        mapSetDirty();

        struct cursor_movement_keyhandler data;
        data.each_point_func  = each_point_func;
        data.each_target_func = each_target_func;
        data.abort            = false;
        struct KeyHandler kh;
        kh.fx   = movecursor_and_do;
        kh.data = &data;
  
        eventPushKeyHandler(&kh);
        cmdwin_print("<target> (ESC to exit)");
        eventHandle();
        cmdwin_backspace(strlen("<target> (ESC to exit)"));
        eventPopKeyHandler();
  
        *x = Cursor->getX();
        *y = Cursor->getY();
        Cursor->remove();
        mapSetDirty();
  
        struct cursor_movement_keyhandler * data_ret;
        data_ret = (struct cursor_movement_keyhandler *) kh.data;
        if (data_ret->abort) {
                cmdwin_print("Done.");
                return -1;  // Aborted, no target
        }
  
        // Target has been selected, (x,y) contain where
        return 0;
} // select_target_with_doing()

int terraform_cursor_func(int ox, int oy, int *x, int *y,
                          int range,
                          v_funcpointer_iiv each_point_func,
                          v_funcpointer_iiv each_target_func,
                          struct place * place)
{
        // SAM: 
        // As select_target(), select_target_with_doing(), 
        // but with additional keybindings intended for cmdTerraform().
        Cursor->setRange(range);
        Cursor->setViewportBounded(1);
        Cursor->setOrigin(ox, oy);
        Cursor->relocate(Place, *x, *y);  // Remember prev target, if any
        mapSetDirty();
  
        struct terraform_mode_keyhandler data;
        data.each_point_func  = each_point_func;
        data.each_target_func = each_target_func;
        data.abort            = false;
        data.map              = place->terrain_map;
        data.palette          = place->terrain_map->palette;

        struct KeyHandler kh;
        kh.fx   = terraform_movecursor_and_do;
        kh.data = &data;
  
        eventPushKeyHandler(&kh);
        cmdwin_print("<target> (ESC to exit)");
        eventHandle();
        cmdwin_backspace(strlen("<target> (ESC to exit)"));
        eventPopKeyHandler();
  
        *x = Cursor->getX();
        *y = Cursor->getY();
        Cursor->remove();
        mapSetDirty();
  
        struct terraform_mode_keyhandler * data_ret;
        data_ret = (struct terraform_mode_keyhandler *) kh.data;
        if (data_ret->abort) {
                cmdwin_print("Done.");
                return -1;  // Aborted, no target
        }
  
        // Target has been selected, (x,y) contain where
        return 0;
} // terraform_cursor_func()

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
                player_party->updateView(); // FIXME: what about character mode?
	}

	return true;
}

bool cmdUse(class Character * member, int flags)
{
	struct inv_entry *ie;
	class ItemType *item;
	class Character *target;
        bool print_target;

	cmdwin_clear();
	cmdwin_print("Use-");

        // Select user
        if (flags & CMD_SELECT_MEMBER) {
                member = select_party_member();
                if (member == NULL)
                        return false;       
        } else {
                assert(member);
                cmdwin_print("%s", member->getName());
        }
	statusSelectCharacter(member->getOrder());

        // select item to use
        cmdwin_print("-");
	statusSetMode(Use);
	ie = select_item();
	statusSetMode(ShowParty);
	if (ie == NULL) {
		return false;
        }
	item = (class ItemType *) ie->type;

	// Get the target to use the item on
	target = member; // default
	if (item->getTarget() == TARG_FRIEND) {
		cmdwin_print("-");
		target = select_party_member();
		if (target == NULL)
			return false;
                print_target = true;
	} else {
                print_target = false;
        }

	// Use it on the target
	item->use(target);
	statusRepaint();

        // Print console message
        if (flags & CMD_PRINT_MEMBER) {
                consolePrint("%s ", member->getName());
        }
        consolePrint("uses ");
	item->describe(1);
        if (print_target)
                consolePrint(" on %s", target->getName());
	consolePrint(".\n");

	// Consume the item
	if (item->isConsumable())
		player_party->remove_from_inventory(ie, 1);

	return true;
}

void cmdNewOrder(void)
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

static void run_combat(bool camping, class Character * guard, int hours,
                       class Object *foe)
{
	struct move_info minfo;
	struct combat_info cinfo;

        assert(!foe || foe->isType(NPCPARTY_ID));

	memset(&minfo, 0, sizeof(minfo));
	minfo.place = Place;
	minfo.x = player_party->getX();
	minfo.y = player_party->getY();
        minfo.npc_party = (class NpcParty*)foe;

	memset(&cinfo, 0, sizeof(cinfo));
	cinfo.camping = camping;
	cinfo.guard = guard;
	cinfo.hours = hours;
	cinfo.move = &minfo;

        // Is there an enemy?
        if (foe) {
                // Yes, so I assume the player party is being attacked
                // (currently this happens as a result of conversation). To
                // setup the proper orientation of the parties I need to get
                // the direction vector. The direction should be from the foe
                // to the player.
                cinfo.defend = true;
                place_get_direction_vector(minfo.place, 
                                           foe->getX(), foe->getY(),
                                           minfo.x, minfo.y, 
                                           &minfo.dx, &minfo.dy);
        } else {
                // No, so we're camping or zooming in. Party values are fine
                // here.
                minfo.dx = player_party->dx;
                minfo.dy = player_party->dy;
        } 

	player_party->move_to_wilderness_combat(&cinfo);
}

bool cmdTalk(int x, int y)
{
	struct conv *conv = NULL;
        class Object *obj;

	// *** Prompt user & check if valid ***

	cmdwin_clear();
	cmdwin_print("Talk-");

	if (Place->type == wilderness_place) {
		cmdwin_print("not here!");
                consolePrint("Can't talk here!\n");
		return false;
	}

	if (select_target(x, y, &x, &y, 4) == -1) {
		return false;
	}

	obj = place_get_object(Place, x, y, being_layer);

	if (!obj) {
                cmdwin_print("nobody there!");
                consolePrint("Try talking to a PERSON.\n");
                return true;
        }

        conv = obj->getConversation();

        if (!conv) {
		cmdwin_print("no response!");
                consolePrint("No response from ");
                obj->describe(1);
                consolePrint(".\n");
		return true;
        }

	cmdwin_print(obj->getName());

	consolePrint("\n*** CONVERSATION ***\n");
	consolePrint("You meet ");
	obj->describe(1);
	consolePrint(".\n");

	if (obj->getActivity() == SLEEPING) {
		consolePrint("Zzzz...\n");
		return true;
	}

	conv->speaker = obj;

	// *** Enter conversation ***

	switch (convEnter(conv)) {

	case CONV_COMBAT:
		break;

	case CONV_OK:
		break;
	}

	mapSetDirty();

        return true;
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

static int cmdCampInWilderness(class Object *camper)
{
	int hours, yesno;
	class Character *guard = 0;

	cmdwin_clear();
	cmdwin_print("Camp-");

	if (place_get_portal(camper->getPlace(), camper->getX(), camper->getY()) ||
	    !place_is_passable(camper->getPlace(), camper->getX(), camper->getY(), camper->getPmask(), PFLAG_IGNOREVEHICLES)) {
		cmdwin_print("not here!");
		return 0;
	}

	hours = select_hours();
	if (hours == 0)
		return 0;

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

	player_party->beginCamping(guard, hours);
        camper->endTurn();
	run_combat(true, guard, hours, NULL);

        return 0;
}

static int cmdCampInTown(class Object *camper)
{
        int hours;

        cmdwin_clear();
        cmdwin_print("Rest-");

        // Check for an object that will serve as a bed.
        if (place_get_object(camper->getPlace(), camper->getX(), camper->getY(),  bed_layer) == NULL) {
                cmdwin_print("no bed!");
                return 0;
        }

        // Rendezvous the party around the bed.
        if (! player_party->rendezvous(camper->getPlace(), camper->getX(), camper->getY())) {
                return 0;
        }

        // Prompt for the number of hours to sleep.
        hours = select_hours();
        if (hours == 0)
                return 0;

        // Put the party in "sleep" mode before returning back to the main
        // event loop.
        player_party->beginResting(hours);
        camper->endTurn();
        cmdwin_print(" resting...");
        return TURNS_PER_HOUR;
}

int cmdCamp(class Object *camper)
{
	if (place_is_wilderness(camper->getPlace()))
                return cmdCampInWilderness(camper);
        else if (place_is_town(camper->getPlace()))
                return cmdCampInTown(camper);
        else {
                cmdwin_print("not in combat mode!");
                return 0;
        }
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

bool get_spell_name(struct KeyHandler *kh, int key, int keymod)
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

static class Object *target_character_for_spell(class Character * caster, class Spell * spell, int *tx,	int *ty)
{
	class Object *target;
	class Object *ret = NULL;

	if (player_party->getContext() == CONTEXT_WILDERNESS) {
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

        x = caster->getX();
        y = caster->getY();

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

static bool target_location_for_spell(class Character * caster,
                                      class Object ** ret,
                                      class Spell * spell, int *x, int *y)
{
	class Character *target;
        int ox, oy;

        ox = caster->getX();
        oy = caster->getY();

        if (caster->isOnMap()) {
                target = caster->getAttackTarget();
                *x = target->getX();
                *y = target->getY();
        } else {
                *x = ox;
                *y = oy;
        }

	if (select_target(ox, oy, x, y, spell->range) == -1)
		return false;

	// Note: cheat a bit here. If possible, grab a character from this
	// location as the target. Currently all the location-targeting spells
	// (magical fields) should also have the property of affecting an
	// occupant if one exists, or simply landing on the location if one
	// doesn't.
	*ret = (class Object *) place_get_object(Place, *x, *y, being_layer);

	return true;
}

static bool cmd_target_spell(class Spell * spell, class Character * pc, class Object ** ret, int *direction, int *x, int *y)
{
        bool result;

	switch (spell->target) {
	case SPELL_TARGET_NONE:
                // The following was commented out. I don't know why. By
                // default, the target should always be the caster, which is
                // what SPELL_TARGET_NONE should really mean.
		*ret = pc;
		return true;
        case SPELL_TARGET_CASTER_LOCATION:
                assert(pc->isOnMap());
                *x = pc->getX();
                *y = pc->getY();
                return true;
	case SPELL_TARGET_LOCATION:
		cmdwin_print("-");
		result = target_location_for_spell(pc, ret, spell, x, y);
                cmdwin_backspace(1);
                return result;
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
        case SPELL_TARGET_PARTY_MEMBER:
                cmdwin_print("-");
                *ret = select_party_member();
                break;
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

        // ---------------------------------------------------------------------
        // Make sure the PC is not asleep, dead, etc.
        // ---------------------------------------------------------------------

        if (pc->isDead() || pc->isAsleep()) {
                cmdwin_print("-unable right now!");
                return false;
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
	if (!(player_party->getContext() & spell->context)) {
		cmdwin_print("-not here!");
		return false;
	}

	// Check if the character comes by this spell naturally
	for (i = 0; i < pc->species->n_spells; i++) {
		if (pc->species->spells[i] == spell) {
			natural = true;
			break;
		}
	}

        //Check if the caster is of sufficient level
        //
        // FIXME: what if the spell is natural? cast An Xen Exe on a snake and
	// try to cast In Nox Por to see what I mean...
        //
	if (!natural && pc->getLevel() < spell->level) {
		cmdwin_print("-need more experience!");
		return false;
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
		if (!cmd_target_spell(spell, pc, &target, &direction, &tx, &ty))
			break;

                // Console message.
		consolePrint("%s casts %s", pc->getName(), spell->getName());
                consolePrint("...");


		// Cast the spell. This automatically decrements the caster's
		// mana.

                cmdwin_print("-");
		switch (spell->cast(pc, target, direction, tx, ty)) {
		case Spell::ok:
			cmdwin_print("success!");
			consolePrint("success!\n");
			break;
		case Spell::no_room_on_battlefield:
			cmdwin_print("no room on battlefield!");
			consolePrint("no room on battlefield!\n");
			break;
		case Spell::magic_negated:
			cmdwin_print("magic negated!");
			consolePrint("magic negated!\n");
			break;
		case Spell::missed_target:
			cmdwin_print("miss!");
                        consolePrint("missed!\n");
			break;
		case Spell::no_effect:
			cmdwin_print("no effect!");
                        consolePrint("no effect!\n");
			break;
		case Spell::teleport_failed:
			cmdwin_print("teleport failed!");
                        consolePrint("teleport failed!\n");
			break;
		case Spell::unknown_failure:
		default:
			cmdwin_print("fizzles!");
                        consolePrint("fizzles!\n");
			break;
		}

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
		switch (random() % 3) {
		case 0:
		default:
			cmdwin_print("oops!");
			consolePrint("ACID!\n");
			player_party->damage(DAMAGE_ACID);
			break;
		case 1:
			cmdwin_print("ouch!");
			consolePrint("BOMB!\n");
			player_party->damage(DAMAGE_BOMB);
			break;
		case 2:
			cmdwin_print("yuck!");
			consolePrint("GAS!\n");
			player_party->poison();
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
        if ( mapTileIsVisible(x, y) ) {
                consolePrint("At XY=(%d,%d) you see ", x, y);
                placeDescribe(x, y, PLACE_DESCRIBE_ALL);
                return;
        } else if (ShowAllTerrain) {
                consolePrint("At XY=(%d,%d) you see (via xray) ", x, y);
                placeDescribe(x, y, PLACE_DESCRIBE_TERRAIN);
                return;
        }
        consolePrint("At XY=(%d,%d) you see nothing (out of LOS)\n", x, y);
}

void detailed_examine_XY(int x, int y)
{
	// SAM: 
	// Hmmm...how best to print more info about
	// the objects on this tile?
        if ( mapTileIsVisible(x, y) ) {
                consolePrint("DETAIL XY=(%d,%d) TODO - print detailed view\n", x, y);
                // For each object/terrain on the tile, print
                // the name (and perhaps show the sprite in a Status Window mode),
                // and also show:
                //     o whether this object blocks LOS (alpha)
                //     o whether this object blocks movement (pmask)
                //     o whether this object causes some effect when stepped upon
                //       (hazardous terrain effects, pressure plate triggers)
                //     o information specific to the object type, such as:
                //       o Triggers: current state, and perhaps what it is connected to?
                //       o NpcParties: alignment/hostility, movement mode (pmask), ...
                //       o Vehicles: movement mode, armament, current HP
                //       o Portable items: weapon/armor stats, (U)se effects, etc...
                // Hmmm...what else?
                return;
        }
        consolePrint("DETAIL XY=(%d,%d) out of LOS\n", x, y);
}

void DM_XRAY_look_at_XY(int x, int y, void * data)
{
        // Like look_at_XY() but unconditionally reports what is there.
        // For use by cmdTerraform and similar.
        // 
        // NOTE: data needs to be unused unless cmdTerraform() 
        //       itself makes such a one.
        if (!mapTileIsVisible(x, y) ) {
                consolePrint("(Out of LOS) ", x, y);
                consolePrint("At XY=(%d,%d) you see ", x, y);
                placeDescribe(x, y, PLACE_DESCRIBE_ALL);
                return;
        }
        consolePrint("At XY=(%d,%d) you see ", x, y);
        placeDescribe(x, y, PLACE_DESCRIBE_ALL);
}

void terraform_XY(int x, int y, void * data)
{
        struct terraform_mode_keyhandler * kh = 
                (struct terraform_mode_keyhandler *) data;
        struct terrain_map     * map = kh->map;
        struct terrain_palette * pp  = kh->palette;
        struct terrain         * tt  = palette_current_terrain(pp);

        if (!mapTileIsVisible(x, y)) {
                consolePrint("TERRAFORM warning - XY=(%d,%d) out of LOS\n", x, y);
        }
        terrain_map_fill(map, x, y, 1, 1, tt);
        vmask_invalidate(Place, x, y, 1, 1); // FIXME: need the place
        mapSetDirty();
        player_party->updateView();
        consolePrint("TERRAFORM put %s '%s' at XY=(%d,%d)\n", 
                     tt->tag, tt->name, x, y);
} // terraform_XY()

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

        look_at_XY(x,y);  // First look at the current tile
	if (select_target_with_doing(x, y, &x, &y, 99,
				     look_at_XY, detailed_examine_XY) == -1) {
		return false;
	}
	return true;
} // cmdXamine()

char * name_of_context (void)
{
        // SAM: Perhaps this function belongs in common.c?
        switch (player_party->getContext()) {
        case CONTEXT_WILDERNESS:
                return "Party Context";
                break;
        default:
                return "Character Context";
                break;
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
                place_name =  Place->name;
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
    
        consolePrint("\n");
        consolePrint("This is %s\n", name_of_context() );
        consolePrint("%s is in %s (%d,%d)\n", who, place_name, x, y);

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
                        consolePrint("The sun is up at arc %d.%s\n", Sun.arc,
                                     is_noon() ? "  It is noon." : "");
                }
                if (sun_is_down() ) {
                        consolePrint("The sun has set at arc %d.%s\n", Sun.arc,
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
        consolePrint("\n");
    

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

bool cmdTerraform(class Character * pc)
{
	int x, y;
        struct place           * place;
        struct terrain_map     * map;
        struct terrain_palette * palette;
        struct terrain         * terrain;

	cmdwin_clear();
	cmdwin_print("Terraform-");

	if (pc) {
		// Combat Mode
                // Use the party member's location as the origin.
                place = Place;
		x = pc->getX();
		y = pc->getY();
	} else {
		// Party Mode
		// Use the player party's location as the origin.
                place = player_party->getPlace();
		x     = player_party->getX();
		y     = player_party->getY();
	}

        map     = place->terrain_map;
        palette = map->palette;
        terrain = palette_current_terrain(palette);

        consolePrint("\n");
        // SAM: 
        // It would probably be better to set the upper-right 
        // "status window" to a new mode.
        // Then I could show the sprite for the current terrain, and so forth.
        // That is TODO later; I have not written a new status mode before.
        // First thing is to get the map editor working.
        consolePrint("Terraform on map %s, palette %s\n"
                     "Current terrain is %s '%s'\n",
                     map->tag, palette->tag, 
                     terrain->tag, terrain->name);

        DM_XRAY_look_at_XY(x,y, NULL);  // First look at the current tile
	if (terraform_cursor_func(x, y, &x, &y, 99,
                                  DM_XRAY_look_at_XY, terraform_XY,
                                  place) == -1) {
                return false;
        }
	return true;
} // cmdTerraform()

#define BOGUS_FILENAME_LENGTH 255  // Hack, do something proper for this...
bool cmdSaveTerrainMap(class Character * pc)
{
	int x, y;
        struct place           * place;
        struct terrain_map     * map;
        struct terrain_palette * palette;
        char map_filename    [BOGUS_FILENAME_LENGTH+1];
        char palette_filename[BOGUS_FILENAME_LENGTH+1];
        FILE * map_fp     = NULL;
        FILE * palette_fp = NULL;

	cmdwin_clear();

	if (pc) {
		// Combat Mode
                // Use the party member's location as the origin.
                place = Place;
		x = pc->getX();
		y = pc->getY();
	} else {
		// Party Mode
		// Use the player party's location as the origin.
                place = player_party->getPlace();
		x     = player_party->getX();
		y     = player_party->getY();
	}

        map     = place->terrain_map;
        palette = map->palette;

        // Save the palette for the current map:
        sprintf(palette_filename, "/tmp/nazghul.pal.%s.%d.ghul", 
                palette->tag, getpid() );
        palette_fp = fopen(palette_filename, "w");
        if (!palette_fp) {
                printf("Filed to open palette save file '%s' for writing "
                       "because '%s'.\n",
                       palette_filename, strerror(errno) );
        }
        palette_print(palette_fp, INITIAL_INDENTATION, palette);
        fclose(palette_fp);
        consolePrint("Saved palette as '%s'.\n", palette_filename);
        printf("Saved palette as '%s'.\n", palette_filename);

        // And save the current map:
        sprintf(map_filename, "/tmp/nazghul.map.%s.%d.ghul", 
                map->tag, getpid() );
        map_fp = fopen(map_filename, "w");
        if (!map_fp) {
                printf("Filed to open map save file '%s' for writing "
                       "because '%s'.\n",
                       map_filename, strerror(errno) );
        }
        terrain_map_print(map_fp, INITIAL_INDENTATION, map);
        fclose(map_fp);
        consolePrint("Saved map as '%s'.\n", map_filename);
        printf("Saved map as '%s'.\n", map_filename);

	return true;
} // cmdSaveTerrainMap()

void cmdZoomIn(void)
{
        // 
        // SAM: Curently no "Enter" message is printed.  It turns out to be
        // moderately complicated to do so properly, as a distinct message for
        // each enter_combat() case might be desired...
        // 
        // For now, I print a placeholder message for each case here:
        
        Portal * pp = 
                place_get_portal(player_party->getPlace(),
                                 player_party->getX(),
                                 player_party->getY() );
        if (pp) {

                // If standing over a portal then enter it:
                consolePrint("Enter-%s\n", pp->getName() );
                player_party->enter_portal();
        } else if (!place_is_passable(player_party->getPlace(),
                                      player_party->getX(),
                                      player_party->getY(),
                                      player_party->getPmask(),
                                      PFLAG_IGNOREVEHICLES)) {

                // Currently zooming in to impassable terrain is not doable;
                // since the party might not be placeable on the default map,
                // which would be a solid grid of that impassable terrain.
                // Also, if somehow placed, the party could not escape unless
                // on an edge.  There would be no harm in it otherwise,
                // however.
                struct terrain * tt = 
                        place_get_terrain(player_party->getPlace(),
                                          player_party->getX(),
                                          player_party->getY() );
                consolePrint("Enter-Cannot zoom-in to %s!\n", tt->name);
        } else {
                // If standing on ordinary terrain, zoom in:
                struct terrain * tt = 
                        place_get_terrain(player_party->getPlace(),
                                          player_party->getX(),
                                          player_party->getY() );
                consolePrint("Enter-%s\n", tt->name);
                run_combat(false, 0, 0, NULL);
        }
}
