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
#include "wq.h"
#include "foogod.h"
#include "combat.h"
#include "cursor.h"
#include "Arms.h"
#include "event.h"
#include "wind.h"
#include "Container.h"
#include "dup_constants.h"
#include "cmdwin.h"
#include "vehicle.h"
#include "Portal.h"
#include "terrain.h"
#include "vmask.h"
#include "session.h"
#include "sched.h"
#include "conv.h"

#define DEBUG
#include "debug.h"

#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
// #include <sys/types.h>  // getpid()
#include <unistd.h>     // getpid()
#include <errno.h>

#define QUICKSAVE_FNAME "save.scm"

int dirkey(struct KeyHandler *kh, int key, int keymod)
{
	int *dir = (int *) kh->data;

	if (key >= KEY_SOUTHWEST && key <= KEY_NORTHEAST) {
		*dir = keyToDirection(key);
		return 1;
	}

	if (key == SDLK_ESCAPE) {
		*dir = key;
		return 1;
	}

	return 0;
}

int yesnokey(struct KeyHandler * kh, int key, int keymod)
{
	int *yesno = (int *) kh->data;

	switch (key) {
	case 'y':
	case 'Y':
		*yesno = 'y';
		return 1;
	case 'n':
	case 'N':
	case CANCEL:
		*yesno = 'n';
		return 1;
	default:
		return 0;
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

int getnum(struct KeyHandler *kh, int key, int keymod)
{
	struct get_number_info *info;

	info = (struct get_number_info *) kh->data;

	switch (info->state) {
	case GN_ALL:
		if (key == CANCEL) {
			getnum_erase_prompt(info);
			info->digit = 0;
			info->state = GN_CANCEL;
			return 1;
		}
		if (key == '\n') {
			getnum_erase_prompt(info);
			return 1;
		}
		if (key == '0') {
			getnum_erase_prompt(info);
			cmdwin_print("0");
			info->digit = 0;
			info->state = GN_ZERO;
			return 0;
		}
		if (isdigit(key)) {
			getnum_erase_prompt(info);
			info->digit = info->digit * 10 + key - '0';
			cmdwin_print("%c", key);
			info->state = GN_SOME;
			return 0;
		}
		break;
	case GN_ZERO:
		if (key == CANCEL) {
			info->digit = 0;
			info->state = GN_CANCEL;
			return 1;
		}
		if (key == '\n') {
			return 1;
		}
		if (key == '\b') {
			cmdwin_backspace(1);
			if (info->prompt)
				cmdwin_print(info->prompt);
			info->state = GN_ALL;
			return 0;
		}
		if (key == '0')
			return 0;
		if (isdigit(key)) {
			cmdwin_backspace(1);
			info->digit = info->digit * 10 + key - '0';
			cmdwin_print("%c", key);
			info->state = GN_SOME;
			return 0;
		}
		break;
	case GN_SOME:
		if (key == CANCEL) {
			info->digit = 0;
			info->state = GN_CANCEL;
			return 1;
		}
		if (key == '\n') {
			return 1;
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
			return 0;
		}
		if (isdigit(key)) {
			info->digit = info->digit * 10 + key - '0';
			cmdwin_print("%c", key);
			return 0;
		}
		break;
	}

	return 0;
}

int getdigit(struct KeyHandler * kh, int key, int keymod)
{
        struct get_number_info *info;

        info = (struct get_number_info *) kh->data;
        
        if (key == CANCEL) {
                cmdwin_backspace(info->erase);
                info->digit = 0;
                return 1;
        }

        if (isdigit(key)) {
                cmdwin_backspace(info->erase);
                info->digit = key - '0';
                if (info->digit != 0)
                        cmdwin_print("%c", key);
                return 1;
        }
        
        return 0;
}

int anykey(struct KeyHandler * kh, int key, int keymod)
{
	return 1;
}

int scroller(struct KeyHandler * kh, int key, int keymod)
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
		return 1;
	case SDLK_ESCAPE:
	case 'q':
		if (context)
			context->abort = 1;
		return 1;
	case 'm':
		if (context && context->mixing) {
			context->done = 1;
			return 1;
		}
		break;
	default:
		break;
	}

	return 0;
}

int movecursor(struct KeyHandler * kh, int key, int keymod)
{
        // A UI mode in which the user can move the cursor 
        // with ARROW keys, select a target with 
        // (ENTER | RETURN | SPACE), or cancel with ESCAPE.
        struct cursor_movement_keyhandler * data;
        assert(kh);
        data = (struct cursor_movement_keyhandler *) kh->data;
  
        if (key == '\n' || key == SDLK_SPACE || key == SDLK_RETURN) {
                return 1;  // Done (target selected)
        }
  
        if (keyIsDirection(key)) {
                int dir = keyToDirection(key);
                Session->crosshair->move(directionToDx(dir), directionToDy(dir));
                mapSetDirty();
                return 0;  // Keep on keyhandling
        }
  
        if (key == SDLK_ESCAPE) {
                data->abort = 1;
                return 1;  // Done (abort)
        }
  
        return 0;  // Keep on keyhandling
} // movecursor()

int movecursor_and_do(struct KeyHandler * kh, int key, int keymod)
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
                int x = Session->crosshair->getX();
                int y = Session->crosshair->getY();
                if (data->each_target_func)
                        data->each_target_func(Session->crosshair->getPlace(),
                                               x, y);
                return 0;  // Keep on keyhandling
        }
  
        if (keyIsDirection(key)) {
                int dir = keyToDirection(key);
                Session->crosshair->move(directionToDx(dir), 
                                         directionToDy(dir));
                mapSetDirty();
                int x = Session->crosshair->getX();
                int y = Session->crosshair->getY();
                if (data->each_point_func)
                        data->each_point_func(Session->crosshair->getPlace(),
                                              x, y);
                return 0;  // Keep on keyhandling
        }
  
        if (key == SDLK_ESCAPE) {
                data->abort = 1;
                return 1;  // Done (abort)
        }
        return 0;  // Keep on keyhandling
} // movecursor_and_do()

int terraform_movecursor_and_do(struct KeyHandler * kh, int key, int keymod)
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
                int x = Session->crosshair->getX();
                int y = Session->crosshair->getY();
                if (data->each_target_func)
                        data->each_target_func(Session->crosshair->getPlace(),
                                               x, y, data);
                return 0;  // Keep on keyhandling
        }

        if (keyIsDirection(key)) {
                int dir = keyToDirection(key);
                Session->crosshair->move(directionToDx(dir), 
                                         directionToDy(dir));
                mapSetDirty();
                int x = Session->crosshair->getX();
                int y = Session->crosshair->getY();
                if (data->each_point_func)
                        data->each_point_func(Session->crosshair->getPlace(),
                                              x, y, data);
                return 0;  // Keep on keyhandling
        }

        if (key == SDLK_PAGEUP) {
                // Page Up == Cycle back through terrain in palette
                palette_prev_terrain(pp);
                tt = palette_current_terrain(pp);
                consolePrint("[Prev]  terrain %s '%s'\n", tt->tag, tt->name);
                return 0;  // Keep on keyhandling
        }
        if (key == SDLK_PAGEDOWN) {
                // Page Down == Cycle forward through terrain in palette
                palette_next_terrain(pp);
                tt = palette_current_terrain(pp);
                consolePrint("[Next]  terrain %s '%s'\n", tt->tag, tt->name);
                return 0;  // Keep on keyhandling
        }
        if (key == SDLK_HOME) {
                // Home == Select first terrain in palette
                palette_first_terrain(pp);
                tt = palette_current_terrain(pp);
                consolePrint("[First] terrain %s '%s'\n", tt->tag, tt->name);
                return 0;  // Keep on keyhandling
        }
        if (key == SDLK_END) {
                // End == Select last terrain in palette
                palette_last_terrain(pp);
                tt = palette_current_terrain(pp);
                consolePrint("[Last]  terrain %s '%s'\n", tt->tag, tt->name);
                return 0;  // Keep on keyhandling
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
                        return 0; // Keep on keyhandling
                }
                // Plain NUM == set current terrain from quick terrain:
                int index = palette_get_quick_terrain_index(pp, qt);
                palette_set_current_terrain(pp, index);
                tt = palette_current_terrain(pp);
                consolePrint("[Quick %d] %s '%s'\n", qt, tt->tag, tt->name);
                return 0;  // Keep on keyhandling
        }

        // ...
    
        if (key == SDLK_ESCAPE) {
                data->abort = 1;
                return 1;  // Done (abort)
        }
        return 0;  // Keep on keyhandling
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

	cmdwin_print(ie->type->getName());

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

void getkey(void *data, int(*handler) (struct KeyHandler * kh, int key, int keymod))
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

bool cmdSearch(struct place *place, int x, int y)
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
	place_describe(place, x + directionToDx(dir),
                       y + directionToDy(dir),
                       PLACE_DESCRIBE_ALL);
        Reveal = old_reveal;
	return true;
}

void cmdGetObject(Object *actor, Object *subject)
{
        closure_t *handler;

        if (! subject->getObjectType()->canGet()) {
                consolePrint("Can't get ");
                subject->describe();
                consolePrint("\n");
                return;                
        }
                
        subject->getObjectType()->get(subject, actor);

}

bool cmdGet(class Object *actor, bool scoop_all)
{
	class Object *item;
	int dir;
        int x, y;
        closure_t *handler;

	cmdwin_clear();
	cmdwin_print("Get-");

	dir = ui_get_direction();
	if (dir == CANCEL)
		return false;

        x = actor->getX() + directionToDx(dir);
        y = actor->getY() + directionToDy(dir);

	item = place_get_item(actor->getPlace(), x, y);
	if (!item) {
		consolePrint("Nothing there!\n");
		return false;
	}
       
        cmdGetObject(actor, item);

	if (scoop_all) {
		while (NULL != (item = place_get_item(actor->getPlace(), x, y))) {
                        cmdGetObject(actor, item);
		}
	}

        mapSetDirty();
        actor->decActionPoints(NAZGHUL_BASE_ACTION_POINTS);

	return true;
}

bool cmdOpen(class Character * pc)
{
	int dir, x, y;
	class Object *mech;
	class Container *container;

	cmdwin_clear();
	cmdwin_print("Open-");

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

	dir = ui_get_direction();
	if (dir == CANCEL)
		return false;

	if (pc) {
		x = place_wrap_x(pc->getPlace(), 
                                 pc->getX() + directionToDx(dir));
		y = place_wrap_y(pc->getPlace(), 
                                 pc->getY() + directionToDy(dir));
	} else {
		x = place_wrap_x(player_party->getPlace(),
                                 player_party->getX() + directionToDx(dir));
		y = place_wrap_y(player_party->getPlace(),
                                 player_party->getY() + directionToDy(dir));
	}

	cmdwin_print("-");

	/*** Open Mech ***/

        mech = place_get_object(pc->getPlace(), x, y, mech_layer);
        if (mech && mech->getObjectType()->canOpen()) {
                mech->getObjectType()->open(mech, pc);
                mapSetDirty();
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

	consolePrint("%s opens ", pc->getName());
	container->describe();
        pc->decActionPoints(NAZGHUL_BASE_ACTION_POINTS);

	// Check for traps.
	if (!container->isTrapped()) {
		consolePrint(".\n");
	} else {

		closure_t *trap = container->getTrap();

		// Roll to disarm
		if (random() % 999 < pc->getDexterity()) {
			consolePrint("...disarmed a trap!\n");
		} else {
			consolePrint("...triggered a trap!\n");
                        closure_exec(trap, "pp", pc, container);
			consolePrint("%s %s!\n", pc->getName(),
				     pc->getWoundDescription());
		}

		// Give the character some experience for dealing with the trap
		pc->addExperience(XP_PER_TRAP);
	}

	// Open the container (automagically spills all the contents onto the
	// map).
	container->open();

        // --------------------------------------------------------------------
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
        // --------------------------------------------------------------------

        container->remove();
        delete container;

        consolePrint("You find ");
	place_describe(pc->getPlace(), x, y, PLACE_DESCRIBE_OBJECTS);


	mapSetDirty();
	return true;
}

bool cmdQuit(void)
{
	int yesno;

	cmdwin_clear();
	cmdwin_print("Quit & Save Game-Y/N?");
	getkey(&yesno, yesnokey);

	cmdwin_backspace(4);

	if (yesno == 'y') {
		cmdwin_print("Yes!");
		consolePrint("Goodbye!\n");
                session_save(QUICKSAVE_FNAME);
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
        info.place = player_party->getPlace();
        info.x = place_wrap_x(info.place, player_party->getX() + info.dx);
        info.y = place_wrap_y(info.place, player_party->getY() + info.dy);
        info.npc_party = place_get_Party(info.place, info.x, info.y);
        if (info.npc_party == NULL) {
                cmdwin_print("-nobody there!");
                return;
        } 
        info.px = player_party->getX();
        info.py = player_party->getY();

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
        combat_enter(&cinfo);
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

        if (member->isCharmed()) {
                consolePrint("Charmed characters can't ready arms!\n");
                return false;
        }

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
        Session->crosshair->setRange(range);
        Session->crosshair->setOrigin(ox, oy);
        Session->crosshair->relocate(Place, *x, *y);  // Remember prev target, if any
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
  
        *x = Session->crosshair->getX();
        *y = Session->crosshair->getY();
        Session->crosshair->remove();
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
        Session->crosshair->setRange(range);
        Session->crosshair->setViewportBounded(1);
        Session->crosshair->setOrigin(ox, oy);
        Session->crosshair->relocate(Place, *x, *y);  // Remember prev target, if any
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
  
        *x = Session->crosshair->getX();
        *y = Session->crosshair->getY();
        Session->crosshair->remove();
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
        Session->crosshair->setRange(range);
        Session->crosshair->setViewportBounded(1);
        Session->crosshair->setOrigin(ox, oy);
        Session->crosshair->relocate(Place, *x, *y);  // Remember prev target, if any
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
  
        *x = Session->crosshair->getX();
        *y = Session->crosshair->getY();
        Session->crosshair->remove();
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

        // Try to find a mech
	class Object *mech;
	mech = place_get_object(Place, x, y, mech_layer);
	if (! mech || ! mech->getObjectType()->canHandle()) {
                cmdwin_print("nothing!");
                return false;
        }

        // Handle it
        mech->getObjectType()->handle(mech, pc);
        mapSetDirty();

        // I think the following was added to update LOS in cases where the
        // mech changed state and changed LOS. Not sure what happens in
        // character mode.
        //player_party->updateView();

	return true;
}

bool cmdUse(class Character * member, int flags)
{
	struct inv_entry *ie;
	class ObjectType *item;
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

	item = ie->type;
        assert(item->isUsable());

        consolePrint("%s uses ", member->getName());
        item->describe(1);
        consolePrint("\n");

	item->use(member);
	statusRepaint();


        /* Hack: assume all usable items are consumable. Can't fix this until
         * items are stored in inventory as objects, not types. That or add a
         * consumable flag to all object types. Or pass the container to the
         * use() method. */
        //member->takeOut(item, 1);

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

        player_party->switchOrder(pc1, pc2);

	consolePrint("%s switched order with %s\n", pc1->getName(),
		     pc2->getName());

	statusRepaint();
}

static void run_combat(bool camping, class Character * guard, int hours,
                       class Object *foe)
{
	struct move_info minfo;
	struct combat_info cinfo;

        assert(!foe || foe->isType(PARTY_ID));

	memset(&minfo, 0, sizeof(minfo));
	minfo.place = Place;
	minfo.x = player_party->getX();
	minfo.y = player_party->getY();
        minfo.px = minfo.x;
        minfo.py = minfo.y;
        minfo.npc_party = (class Party*)foe;

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
                minfo.dx = player_party->getDx();
                minfo.dy = player_party->getDy();
        } 

	combat_enter(&cinfo);
}

bool cmdTalk(Object *member)
{
	struct conv *conv = NULL;
        class Object *obj;
        int x = member->getX();
        int y = member->getY();

	// *** Prompt user & check if valid ***

	cmdwin_clear();
	cmdwin_print("Talk-");

	if (Place->wilderness) {
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

        //conv = obj->getConversation();
        //if (!conv) {

        if (! obj->canTalk()) {
		cmdwin_print("no response!");
                consolePrint("No response from ");
                obj->describe();
                consolePrint(".\n");
		return true;
        }

	cmdwin_print(obj->getName());

	consolePrint("\n\n*** CONVERSATION ***\n\n");
	consolePrint("You meet ");
	obj->describe();
	consolePrint(".\n");

	if (obj->getActivity() == SLEEPING) {
		consolePrint("Zzzz...\n");
		return true;
	}

        conv_enter(obj, member);


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

	if (place_get_portal(camper->getPlace(), camper->getX(), 
                             camper->getY()) ||
	    !place_is_passable(camper->getPlace(), camper->getX(), 
                               camper->getY(), camper, PFLAG_IGNOREVEHICLES)) {
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

int get_spell_name(struct KeyHandler *kh, int key, int keymod)
{
	struct get_spell_name_data *ctx;
	char *word, letter;

	ctx = (struct get_spell_name_data *) kh->data;
	if (key == '\n') {
		// Done
		erase_spell_prompt(ctx);
		*ctx->ptr = 0;
		return 1;
	}

	if (key == CANCEL) {
		// Abort
		erase_spell_prompt(ctx);
		ctx->spell_name[0] = 0;
		return 1;
	}

	if (key == '\b' && ctx->n) {
		// Backspace -- erase the previous word
		erase_spell_prompt(ctx);
		ctx->ptr--;
		letter = *ctx->ptr;
		word = magic_lookup_word(&Session->magic, letter);
		cmdwin_backspace(strlen(word) + 1);
		*ctx->ptr = 0;
		ctx->n--;
		return 0;
	}

	if (ctx->n == MAX_WORDS_IN_SPELL_NAME) {
		// Out of space -- refuse word
		return 0;
	}

	if (!isalpha(key))
		// Not a letter
		return 0;

	erase_spell_prompt(ctx);

	letter = toupper(key);
        word = magic_lookup_word(&Session->magic, letter);
	if (!word)
		// Letter does not map to a magic word
		return 0;

	// Accept the word and print it
	cmdwin_print("%s ", word);
	*ctx->ptr = letter;
	ctx->ptr++;
	ctx->n++;

	return 0;
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

bool cmdYuse(class Character *pc)
{
        /* Yuse a skill or special ability. */
        return false;
}

bool cmdCastSpell(class Character * pc)
{
	struct get_spell_name_data context;
	struct inv_entry *ie = NULL;
	struct spell *spell;
	class Object *target = NULL;
	bool mixed = false;
	bool natural = false;
	int i;
	int tries;
	int max_tries;
	int tx, ty;

        if (MagicNegated) {
                consolePrint("Magic negated!\n");
                return false;
        }

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

        // --------------------------------------------------------------------
        // Make sure the PC is not asleep, dead, etc.
        // --------------------------------------------------------------------

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
	spell = magic_lookup_spell(&Session->magic, context.spell_name);
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
		if (! strcmp(pc->species->spells[i], spell->code)) {
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
		ie = player_party->inventory->search(spell->type);
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
                spell->type->cast(pc);
	}

	if (tries == 0)
		return false;

	// If the spell was mixed then remove it from inventory.
	if (mixed)
		player_party->takeOut(ie->type, 1);

#if 0
	// Update the character's target memory. This will automatically resort
	// back to the character as its own target if this target is dead.
	if (spell->target == SPELL_TARGET_CHARACTER)
		pc->setAttackTarget((class Character *) target);
#endif

        /* Some spells have status in the foogod window, so repaint it now. */
        foogodRepaint();

	return true;

}

bool cmdMixReagents(void)
{
	struct spell *spell;
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
	spell = magic_lookup_spell(&Session->magic, context.spell_name);
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
                if (! ie) {
                        /* This happens when the player has no reagents
                         * whatsoever. */
			cmdwin_erase_back_to_mark();
			eventPopKeyHandler();
			cmdwin_print("none!");
			goto done;
                }

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
					player_party->takeOut(ie->type, quantity);
                                        // The following line is safe only
					// because this is the end of the
					// list_for_each loop!
					list_remove(elem);
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
			player_party->takeOut(ie->type, quantity);
		}
	}

	consolePrint("Mixing spell...");
	consoleRepaint();

	// If the spell is invalid or the reagents are incorrect then punish
	// the player.
	if (!spell || mistake) {
		statusSetMode(ShowParty);
		switch (random() % 2) {
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
		}

		return true;
	}
	// All is well. Add the spell to player inventory.
	cmdwin_print("ok");
	consolePrint("%s!\n", spell->type->getName());
	player_party->add(spell->type, quantity);

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

void look_at_XY(struct place *place, int x, int y)
{
        if ( mapTileIsVisible(x, y) ) {
                consolePrint("At XY=(%d,%d) you see ", x, y);
                place_describe(place, x, y, PLACE_DESCRIBE_ALL);
                return;
        } else if (ShowAllTerrain) {
                consolePrint("At XY=(%d,%d) you see (via xray) ", x, y);
                place_describe(place, x, y, PLACE_DESCRIBE_TERRAIN);
                return;
        }
        consolePrint("At XY=(%d,%d) you see nothing (out of LOS)\n", x, y);
}

void detailed_examine_XY(struct place *place, int x, int y)
{
	// SAM: 
	// Hmmm...how best to print more info about
	// the objects on this tile?
        if ( mapTileIsVisible(x, y) ) {
                consolePrint("DETAIL XY=(%d,%d) TODO - print detailed view\n", x, y);
                // For each object/terrain on the tile, print the name (and
                // perhaps show the sprite in a Status Window mode), and also
                // show:
                //
                //     o whether this object blocks LOS (alpha)
                //     o whether this object blocks movement (pmask)
                //     o whether this object causes some effect when stepped
                //       upon (hazardous terrain effects, pressure plate
                //       triggers)
                //     o information specific to the object type, such as:
                //     o Triggers: current state, and perhaps what it is 
                //       connected to?
                //     o NpcParties: alignment/hostility, movement mode 
                //       (pmask), ...
                //     o Vehicles: movement mode, armament, current HP
                //     o Portable items: weapon/armor stats, (U)se effects, 
                //       etc...
                // Hmmm...what else?
                return;
        }
        consolePrint("DETAIL XY=(%d,%d) out of LOS\n", x, y);
}

void DM_XRAY_look_at_XY(struct place *place, int x, int y, void * data)
{
        // Like look_at_XY() but unconditionally reports what is there.
        // For use by cmdTerraform and similar.
        // 
        // NOTE: data needs to be unused unless cmdTerraform() 
        //       itself makes such a one.
        if (!mapTileIsVisible(x, y) ) {
                consolePrint("(Out of LOS) ", x, y);
                consolePrint("At XY=(%d,%d) you see ", x, y);
                place_describe(place, x, y, PLACE_DESCRIBE_ALL);
                return;
        }
        consolePrint("At XY=(%d,%d) you see ", x, y);
        place_describe(place, x, y, PLACE_DESCRIBE_ALL);
}

void terraform_XY(struct place *place, int x, int y, void * data)
{
        struct terraform_mode_keyhandler * kh = 
                (struct terraform_mode_keyhandler *) data;
        struct terrain_map     * map = kh->map;
        struct terrain_palette * pp  = kh->palette;
        struct terrain         * tt  = palette_current_terrain(pp);

        if (!mapTileIsVisible(x, y)) {
                consolePrint("TERRAFORM warning - XY=(%d,%d) out of LOS\n", 
                             x, y);
        }
        terrain_map_fill(map, x, y, 1, 1, tt);
        vmask_invalidate(place, x, y, 1, 1); // FIXME: need the place
        mapSetDirty();
        //player_party->updateView();
        mapUpdate(0);
        consolePrint("TERRAFORM put %s '%s' at XY=(%d,%d)\n", 
                     tt->tag, tt->name, x, y);
} // terraform_XY()

bool cmdXamine(class Object * pc)
{
	// SAM: Working on an improved (L)ook command,
	// which works as a "Look Mode" rather than a 
	// "look at 1 tile" command...
	int x, y;

	cmdwin_clear();
	cmdwin_print("Xamine-");

        x = pc->getX();
        y = pc->getY();

        look_at_XY(pc->getPlace(), x,y);  // First look at the current tile
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
                     week_name(), month_name(), Session->clock.year );

        // SAM: Is this really interesting though, I wonder?
        consolePrint("%d game turns have passed.\n", Turn);

        consolePrint("The wind is blowing from the %s.\n",
                     directionToString(windGetDirection()) );

        if (Place->underground) {
                consolePrint("%s is underground, and cannot see the sky.\n", 
                             who);
        } // underground
        else {
                struct list *elem;
                
                // SAM: 
                // This message won't be true if you are under 
                // a roof in a town.  In future there should be 
                // logic querying the (future) roof-ripping code here.
                consolePrint("%s is beneath the open sky.\n", who);

                // The kernel no longer has any special knowledge about which
                // astral body is the sun, so we have to deal with all astral
                // bodies generically now. I mean, a game may have two or even
                // more suns. The time runs independently and isn't cued off
                // the sun.
                if (is_noon())
                        consolePrint("It is noon. ");
                else if (is_midnight())
                        consolePrint("It is midnight. ");

                // Report on each astral body generically.
                list_for_each(&Session->sky.bodies, elem) {
                        struct astral_body *body;
                        body = outcast(elem, struct astral_body, list);
                        if (astral_body_is_visible(body->arc)) {
                                consolePrint("%s is up at arc %d", body->name, 
                                             body->arc);
                                if (body->n_phases > 1) {
                                        char *phase_name = body->phases[body->phase].name;                                
                                        if (phase_name)
                                                consolePrint(" in its %s phase", 
                                                             phase_name);
                                        else
                                                consolePrint(" in phase %d", 
                                                             body->phase);
                                }
                                consolePrint(". ");
                        }
                }
                consolePrint("\n");

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

        DM_XRAY_look_at_XY(place,   // pc->getPlace(),  // SAM: caused SIGSEGV here.  Hmmm...I thought this was fixed.
                           x,y, NULL);  // First look at the current tile
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
        struct place *subplace = 0;
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
        } else if ((subplace = place_get_subplace(player_party->getPlace(),
                                                  player_party->getX(),
                                                  player_party->getY()))) {

                // Standing over a subplace. Try to enter with no direction,
                // this will prompt the player to provide a direction.
                consolePrint("Enter-%s\n", subplace->name);
                player_party->try_to_enter_subplace_from_edge(subplace, 0, 0);

        } else if (!place_is_passable(player_party->getPlace(),
                                      player_party->getX(),
                                      player_party->getY(),
                                      player_party,
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

void cmdQuickSave(void)
{
        consolePrint("Saving to %s...", QUICKSAVE_FNAME);
        consoleRepaint();
        session_save(QUICKSAVE_FNAME);
        consolePrint("ok!\n");
        consoleRepaint();
}

void cmdReload(void)
{
        consolePrint("Loading from %s...", QUICKSAVE_FNAME);
        consoleRepaint();
        session_load(QUICKSAVE_FNAME);
        Session->reloaded = 1;
        consolePrint("ok!\n");
}

/****** New UI ******/

#define MARKUP 4


int ui_get_yes_no(char *name)
{
	int yesno;
	cmdwin_clear();
	cmdwin_print("Reply-<Y/N>");
	getkey(&yesno, yesnokey);
	cmdwin_backspace(strlen("<Y/N>"));
	if (yesno == 'y') {
		cmdwin_print("yes");
		consolePrint("%s: Yes\n", name);
                return 1;
	} else {
		cmdwin_print("no");
		consolePrint("%s: No\n", name);
                return 0;
	}
}

static void buy(struct merchant *merch)
{
	struct KeyHandler kh;
	struct ScrollerContext sc;
	struct trade_info *trade;
	int quantity, cost, max_q;

	statusSetTradeInfo(merch->n_trades, merch->trades);
	statusSetMode(Trade);

	sc.selector = TradeItem;
	kh.fx = scroller;
	kh.data = &sc;

	for (;;) {

		// *** selection ***

		sc.selection = NULL;

		cmdwin_clear();
		cmdwin_print("Buy-<select/ESC>");
		eventPushKeyHandler(&kh);
		eventHandle();
		eventPopKeyHandler();
		cmdwin_backspace(strlen("<select/ESC>"));

		trade = (struct trade_info *) sc.selection;

		if (!trade) {
			cmdwin_print("none!");
			break;
		}

		cmdwin_print("%s-", trade->name);

		if (player_party->gold < trade->cost) {
			int dummy;
			cmdwin_print("not enough gold! <hit any key>");
			getkey(&dummy, anykey);
			continue;
		}
		// *** quantity ***

		max_q = player_party->gold / trade->cost;

		cmdwin_mark();
		quantity = select_quantity(max_q);
		cmdwin_erase_back_to_mark();

		if (quantity == 0) {
			cmdwin_print("none!");
			continue;
		}

		quantity = min(quantity, max_q);
		cmdwin_print("%d-", quantity);

		cost = quantity * trade->cost;

		// *** trade ***

		player_party->gold -= cost;
		player_party->add((class ObjectType *) trade->data,
                                  quantity);
		cmdwin_print("ok");
		consolePrint("You buy %d %s%s for %d gold\n", quantity,
			     trade->name, quantity > 1 ? "s" : "", cost);
		foogodRepaint();
	}

	statusSetMode(ShowParty);
}

static bool conv_filter_trade(struct inv_entry *ie, void *cookie)
{
        struct trade_info *trade = (struct trade_info*)cookie;
        return (ie->type == trade->data && ie->count > ie->ref);
}

static int fill_sell_list(struct merchant *merch, struct trade_info *trades)
{
	struct inv_entry *ie = NULL;
        struct filter filter;
	int i, j = 0;

        filter.fx = conv_filter_trade;

        for (i = 0; i < merch->n_trades; i++) {

                if (merch->trades[i].cost / MARKUP == 0)
                        continue;
                
                filter.cookie = &merch->trades[i];
                ie = player_party->inventory->next(ie, &filter);
                if (!ie)
                        continue;

                trades[j] = merch->trades[i];
                trades[j].cost /= MARKUP;
                trades[j].quantity = ie->count - ie->ref;
                trades[j].show_quantity = 1;
                j++;
        }

	return j;
}

static void sell(struct merchant *merch)
{
	// A bit trickier than the "Buy" scenario. A merchant will only buy
	// items that it is willing to turn around and sell at a profit. When
	// it comes time to select an item to sell the user should only see the
	// list of items in player inventory which the merchant is willing to
	// buy. So here we need to build that list and feed it to the status
	// viewer.

	int n_trades = 0;
	struct trade_info *trades;
	struct KeyHandler kh;
	struct ScrollerContext sc;
	struct trade_info *trade;

	// Allocate the trade list.
	trades = new struct trade_info[merch->n_trades];
	if (!trades) {
		consolePrint("%s: I don't need anything.\n", merch->name);
		return;
	}
	// Fill out the list
	n_trades = fill_sell_list(merch, trades);
	statusSetTradeInfo(n_trades, trades);
	statusSetMode(Trade);

	sc.selector = TradeItem;
	kh.fx = scroller;
	kh.data = &sc;

	for (;;) {

		struct inv_entry *ie;
		int quantity, max_q;

		sc.selection = NULL;

		cmdwin_clear();
		cmdwin_print("Sell-<select or ESC>");
		eventPushKeyHandler(&kh);
		eventHandle();
		eventPopKeyHandler();
		cmdwin_backspace(strlen("<select or ESC>"));

		trade = (struct trade_info *) sc.selection;

		if (!trade) {
			cmdwin_print("none!");
			break;
		}

		cmdwin_print("%s-", trade->name);

		ie = player_party->inventory->search((class ObjectType *) trade->data);
		assert(ie);
		assert(ie->ref < ie->count);

		// quantity

		max_q = ie->count - ie->ref;

		cmdwin_mark();
		quantity = select_quantity(max_q);
		cmdwin_erase_back_to_mark();

		if (quantity == 0) {
			cmdwin_print("none!");
			continue;
		}

		quantity = min(quantity, max_q);
		cmdwin_print("%d-", quantity);

		// make the trade
		player_party->takeOut(ie->type, quantity);
		player_party->gold += quantity * trade->cost;
		foogodRepaint();

		cmdwin_print("ok");
		consolePrint("You sell %d %s%s for %d gold\n", quantity,
			     trade->name, quantity > 1 ? "s" : "",
			     quantity * trade->cost);

		// refresh the sell list
		n_trades = fill_sell_list(merch, trades);
		statusSetTradeInfo(n_trades, trades);
		statusUpdateTradeInfo(n_trades, trades);
	}

	statusSetMode(ShowParty);

	delete trades;
}

static int get_buy_or_sell_key(struct KeyHandler *kh, int key, int keymod)
{
	int *val = (int *) kh->data;

	switch (key) {
	case 'b':
	case 'B':
		*val = 'b';
		return 1;
	case 's':
	case 'S':
		*val = 's';
		return 1;
	case CANCEL:
		*val = 'x';
		return 1;
	default:
		return 0;
	}
}

void ui_trade(struct merchant *merch)
{
	int key;

	for (;;) {
		cmdwin_clear();
		cmdwin_print("Buy or sell-<B/S/ESC>");
		getkey(&key, get_buy_or_sell_key);

		switch (key) {
		case 'b':
			buy(merch);
			break;
		case 's':
			sell(merch);
			break;
		default:
			cmdwin_backspace(strlen("<B/S>"));
			cmdwin_print("none!");
			return;
		}
	}
}
