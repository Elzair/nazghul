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
#include "../config.h" /* for USE_SKILLS */
#include "conv.h"
#include "place.h"
#include "constants.h"
#include "file.h"
#include "foogod.h"
#include "images.h"
#include "sprite.h"
#include "los.h"
#include "astar.h"
#include "common.h"
#include "screen.h"
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
#include "terrain.h"
#include "vmask.h"
#include "session.h"
#include "sched.h"
#include "conv.h"
#include "log.h"
#include "factions.h"
#include "result.h"
#include "dice.h"
#include "menus.h"
#include "kern_intvar.h"
#include "skill.h"
#include "skill_set.h"
#include "skill_set_entry.h"
#include "templ.h"
#include "occ.h"
#include "nazghul.h"  // for DeveloperMode
#include "ztats.h"
#include "terrain_editor.h"

#define DEBUG
#include "debug.h"

#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
#include <unistd.h>     // getpid()
#include <errno.h>

#define ESCAPE_CHARACTER 110

/* Disabling the '>' command when standing over a subplace. This is generally
 * not useful and can be abused for some towns. */
#ifndef ENABLE_TOWN_ZOOM_IN
#define ENABLE_TOWN_ZOOM_IN 0
#endif

/* SAM: Using this typedef below */
typedef void (*v_fncptr_iiv_t) (struct place *, int x, int y, void * v);
typedef int (*i_fncptr_iiv_t) (struct place *, int x, int y, void * v);


/**
 * Struct used by the movecursor function and it's mouse-handling counterparts
 * for commands which prompt the player to select a target from the map. 
 */
struct movecursor_data {
        v_fncptr_iiv_t each_tile_func;   /* called when cursor moves         */
        i_fncptr_iiv_t each_target_func; /* called on 'enter' or leftclick   */
        struct list *loc_list;           /* quick target list                */
        struct list *cur_loc;            /* current target from list         */
        int jump;                        /* distance to jump cursor          */
        void *data;                      /* caller data passed to callbacks  */
        char abort : 1;                  /* command was aborted              */
};

/* fwd decls */

#ifdef USE_SKILLS
static class Character *cmd_front_end(class Character *pc, const char *cmdstr);
#endif
static int cmd_eval_and_log_result(int result);
static int select_target_rlcb(struct place *place,
                              int ox, int oy, int *x, int *y,
                              int range,
                              struct list *suggest,
                              v_fncptr_iiv_t each_tile_func,
                              i_fncptr_iiv_t each_target_func);

/* functions */

static class Character * cmdAnyPartyMemberEngagedInTask(void)
{
    int num_pcs = player_party->getSize();
    for (int i = 0; i < num_pcs; i++) {
        class Character *pc = player_party->getMemberAtIndex(i);
        if (pc->engagedInTask()) {
            return pc;
        }
    }
    return NULL;
}

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

        /* Special case: let '.' mean KEY_HERE for the numeric keypad
         * challenged. */
        if (key == '.') {
                *dir = keyToDirection(KEY_HERE);
                return 1;
        }

	return 0;
}

int cardinaldirkey(struct KeyHandler *kh, int key, int keymod)
{
	int *dir = (int *) kh->data;

	switch (key) {
	case KEY_NORTH:
	case KEY_SOUTH:
	case KEY_EAST:
	case KEY_WEST:
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

/* Max number getnum() will accept */
const int MAX_GETNUM = 999999999;

struct get_number_info {
	int digit;
	int state;
	char *prompt;
};

struct get_char_info {
        const char *string;
        char c;
	int state;
	char *prompt;
};

struct get_spell_name_data {
	char spell_name[MAX_WORDS_IN_SPELL_NAME + 1];
        const char *prompt;
	char *ptr;
	int n;
        int state;
};

int getnum(struct KeyHandler *kh, int key, int keymod)
{
	struct get_number_info *info;

	info = (struct get_number_info *) kh->data;

	switch (info->state) {
	case GN_ALL:
		if (key == CANCEL) {
                        cmdwin_pop();
			info->digit = 0;
			info->state = GN_CANCEL;
			return 1;
		}
		if (key == '\n') {
                        cmdwin_pop();
			return 1;
		}
		if (key == '0') {
                        cmdwin_pop();
			cmdwin_push("0");
			info->digit = 0;
			info->state = GN_ZERO;
			return 0;
		}
		if (isdigit(key)) {
                        cmdwin_pop();
			info->digit = info->digit * 10 + key - '0';
			cmdwin_push("%c", key);
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
			cmdwin_pop();
			if (info->prompt)
				cmdwin_spush(info->prompt);
			info->state = GN_ALL;
			return 0;
		}
		if (key == '0')
			return 0;
		if (isdigit(key)) {
			cmdwin_pop();
			info->digit = info->digit * 10 + key - '0';
			cmdwin_push("%c", key);
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
			cmdwin_pop();
			if (info->digit == 0) {
				info->state = GN_ALL;
				if (info->prompt)
					cmdwin_spush(info->prompt);
			}
			return 0;
		}
		if (isdigit(key)) {
                        int keyval = key - '0';
                        if ((MAX_GETNUM - keyval) >= info->digit) {
                                info->digit = info->digit * 10 + keyval;
                                cmdwin_push("%c", key);
                        }
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
                cmdwin_pop();
                info->digit = 0;
                return 1;
        }

        if (isdigit(key)) {
                cmdwin_pop();
                info->digit = key - '0';
                if (info->digit != 0)
                        cmdwin_push("%c", key);
                return 1;
        }
        
        return 0;
}

static int cmd_getchar(struct KeyHandler * kh, int key, int keymod)
{
        struct get_char_info *info;

        info = (struct get_char_info *) kh->data;
        
        if (key == CANCEL) {
                cmdwin_pop();
                info->c = 0;
                return 1;
        }

        if (strchr(info->string, key)) {
                cmdwin_pop();
                info->c = key;
                cmdwin_push("%c", key);
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
        case KEY_HERE:
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

bool mouse_button_cursor(struct MouseButtonHandler *mh, SDL_MouseButtonEvent *event)
{
        struct movecursor_data * data
                = (struct movecursor_data *) mh->data;
        int mx = event->x;
        int my = event->y;

        /* Off-map? */
        if (mapScreenToPlaceCoords(&mx, &my)) {
                return false;
        }

        /* Did the crosshair move? */
        if (Session->crosshair->getX() != mx
            || Session->crosshair->getY() != my) {

                /* turn on range shading after the first move */
                Session->crosshair->shadeRange(true);

                /* Move the crosshair */
                Session->crosshair->move(mx - Session->crosshair->getX(),
                                         my - Session->crosshair->getY());
                mapSetDirty();

                /* Need to run our visitor function on each tile? */
                if (data->each_tile_func) {
                        data->each_tile_func(Session->crosshair->getPlace(),
                                             Session->crosshair->getX(),
                                             Session->crosshair->getY(),
                                             data->data);
                }

        }

        /* target selected? */
        if (event->button == SDL_BUTTON_LEFT) {
                if (data->each_target_func) {
                        return data->each_target_func(Session->crosshair->getPlace(),
                                                      Session->crosshair->getX(),
                                                      Session->crosshair->getY(),
                                                      data->data);
                }
                return 1; /* target selected */
        }


        return false;
}

bool mouse_motion_cursor(struct MouseMotionHandler *mh, SDL_MouseMotionEvent *event)
{
        struct movecursor_data * data
                = (struct movecursor_data *) mh->data;
        int mx = event->x;
        int my = event->y;

        /* Off-map? */
        if (mapScreenToPlaceCoords(&mx, &my)) {
                return false;
        }
        
        /* Did the crosshair NOT move? */
        if (Session->crosshair->getX() == mx
            && Session->crosshair->getY() == my) {
                return false;
        }

        /* turn on range shading after the first move */
        Session->crosshair->shadeRange(true);

        /* Move the crosshair */
        Session->crosshair->move(mx - Session->crosshair->getX(),
                                 my - Session->crosshair->getY());
        mapSetDirty();

        /* Need to run our visitor function on each tile? */
        if (data->each_tile_func) {
                data->each_tile_func(Session->crosshair->getPlace(),
                                     Session->crosshair->getX(),
                                     Session->crosshair->getY(),
                                     data->data);
        }

        /* Mouse dragging? */
        if (event->state & SDL_BUTTON(1)
            && data->each_target_func) {
                return data->each_target_func(Session->crosshair->getPlace(),
                                              Session->crosshair->getX(),
                                              Session->crosshair->getY(),
                                              data->data);
        }


        return false;
}

/**
 * movecursor - move the crosshair around, possibly running a function on each
 * tile entered by the crosshair or on each tile selected
 */
int movecursor(struct KeyHandler * kh, int key, int keymod)
{
        int moved = 0;
        struct movecursor_data * data
                = (struct movecursor_data *) kh->data;
  
        /* target selected? */
        switch (key) {
	case SDLK_RETURN:
	case SDLK_SPACE:
        case KEY_HERE:
	case '\n':
                if (data->each_target_func) {
                        return data->each_target_func(Session->crosshair->getPlace(),
                                                      Session->crosshair->getX(),
                                                      Session->crosshair->getY(),
                                                      data->data);
                }
                return 1; /* target selected */
        default:
                break;
        }
  
        /* crosshairs moved? */
        if (keyIsDirection(key)) {
                int dir = keyToDirection(key);
                int dx = directionToDx(dir);
                int dy = directionToDy(dir);
                
                /* Brain-dead but simple way to clamp the jump distance to
                 * range: iteratively back-off until it's ok. */
                while (OutOfRange == Session->crosshair->move(dx * data->jump, 
                                                              dy * data->jump)
                        && data->jump > 1) {
                        data->jump--;
                }
                moved = 1;
        } else if (isdigit(key)) {
                data->jump = key - '0';
                if (! data->jump)
                        data->jump = 1; /* disallow zero */
        } else {
  
                struct list *old_loc = data->cur_loc;

                switch (key) {
                        
                case SDLK_ESCAPE:
                        /* Abort */
                        data->abort = 1;
                        return 1;   /* done */
                        
                case '+':
                case '=':
                case 'n':
                        /* Next target */
                        if (data->loc_list
                            && ! list_empty(data->loc_list)) {
                                data->cur_loc = data->cur_loc->next;
                                if (data->cur_loc == data->loc_list) {
                                        /* wrap around */
                                        data->cur_loc = 
                                                data->cur_loc->next;
                                }
                        }
                        break;
                        
                case '-':
                case 'p':
                        /* Previous target */
                        if (data->loc_list
                            && ! list_empty(data->loc_list)) {
                                data->cur_loc = data->cur_loc->prev;
                                if (data->cur_loc == data->loc_list) {
                                        /* wrap around */
                                        data->cur_loc = 
                                                data->cur_loc->prev;
                                }
                        }
                        break;
                default:
                        break;
                }

                /* Target changed? */
                if (old_loc != data->cur_loc) {
                        struct location_list *loc = 
                                (struct location_list*)data->cur_loc;
                        Session->crosshair->move(loc->x - 
                                                 Session->crosshair->getX(),
                                                 loc->y - 
                                                 Session->crosshair->getY());
                        moved = 1;
                }
        }

        /* Cursor was moved? */
        if (moved) {
                data->jump = 1;
                mapSetDirty();
                if (data->each_tile_func) {
                        data->each_tile_func(Session->crosshair->getPlace(),
                                             Session->crosshair->getX(),
                                             Session->crosshair->getY(),
                                             data->data);
                }

                /* turn on range shading after the first move */
                Session->crosshair->shadeRange(true);
        }

        return 0;   /* not done */
}

struct inv_entry *ui_select_item(void)
{
	struct inv_entry *ie;
	struct KeyHandler kh;
	struct ScrollerContext sc;

        foogodSetHintText(SCROLLER_HINT);
        foogodSetMode(FOOGOD_HINT);        

	sc.selector = InventoryItem;
	sc.selection = NULL;
	kh.fx = scroller;
	kh.data = &sc;

	eventPushKeyHandler(&kh);
	cmdwin_push("<select>");
	eventHandle();
	cmdwin_pop();
	eventPopKeyHandler();

        foogodSetMode(FOOGOD_DEFAULT);

	ie = (struct inv_entry *) sc.selection;
	if (ie == NULL) {
		cmdwin_push("none!");
		return NULL;
	}

	cmdwin_spush(ie->type->getName());

	return ie;
}

class Character *select_party_member(void)
{
	enum StatusMode omode;
	class Character *character;

        if (1 == player_party->getSize()) {
                character = player_party->getMemberByOrder(0);
                /* fixme: move to cmd_front_end? */
		cmdwin_spush("%s", character->getName());
                return character;
        }

        foogodSetHintText(SCROLLER_HINT);
        foogodSetMode(FOOGOD_HINT);        
	omode = statusGetMode();
	statusSetMode(SelectCharacter);

	struct KeyHandler kh;
	struct ScrollerContext sc;
	sc.selector = Character;
	sc.selection = NULL;
	kh.fx = scroller;
	kh.data = &sc;

	eventPushKeyHandler(&kh);
	cmdwin_push("<select>");
	eventHandle();
	cmdwin_pop();
	eventPopKeyHandler();

	statusRepaint();

	character = (class Character *) sc.selection;

	if (character == NULL) {
		cmdwin_push("none!"); /* fixme: move to cmd_front_end? */
		/* Hack alert: this saves the caller from having to remember to
		 * do this. Doing it unconditionally is undesirable because it
		 * can cause status screen flashes if the old mode requires a
		 * short status window and the next mode requires a tall
		 * one. */
	} else {
                /* fixme: move to cmd_front_end? */
		cmdwin_spush("%s", character->getName());
	}

	statusSetMode(omode);
        foogodSetMode(FOOGOD_DEFAULT);

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
	cmdwin_push("<direction>");
	getkey(&dir, dirkey);
	cmdwin_pop();
	if (dir == CANCEL) {
		cmdwin_push("none!");
	} else {
		cmdwin_spush(directionToString(dir));
	}
	return dir;
}

static void search_visitor(class Object *obj, void *arg)
{
        class ObjectType *type = obj->getObjectType();
        /* Caution: searching can destroy the object if it triggers traps; the
         * arg should be protected by the caller. */
        if (type && type->canSearch()) {
                type->search(obj, (class Object*)arg);
        }
}

bool cmdSearch(class Character *pc)
{
	int dir;
        bool old_reveal;
        int x, y, x2,  y2;
        struct place *place = 0;

	cmdwin_clear();
	cmdwin_spush("Search");

        /* FIXME: this is duplicated in cmdHandle(), these command functions
         * all need to be cleaned up to ensure consistency. */
        if (! pc) {
                if (player_party->get_num_living_members() == 1) {
			pc = player_party->get_first_living_member();
			cmdwin_spush("%s", pc->getName());
		} else {
			pc = select_party_member();
			if (pc == NULL) {
				return false;
			}
		}
        }

        assert(pc);
        place = pc->getPlace();
        x = pc->getX();
        y = pc->getY();

	dir = ui_get_direction();
	if (dir == CANCEL)
		return false;

        x2 = x + directionToDx(dir);
        y2 = y + directionToDy(dir);

        /* Caution: searching can destroy the pc if it triggers traps. The
         * following is iterative, so protect the pc from destruction until it
         * completes. */
        obj_inc_ref(pc);
        place_for_each_object_at(place, x2, y2, search_visitor, pc);
        obj_dec_ref(pc);

	log_begin("You find ");
        old_reveal = Reveal;
        Reveal = true;
	place_describe(place, x2, y2, PLACE_DESCRIBE_ALL);
        log_end(".");
        Reveal = old_reveal;
        pc->decActionPoints(kern_intvar_get("AP_COST:search"));  // SAM: We may want a '-1' value here, to signify "all remaining AP"...
	
	return true;
}

static int cmdGetFilter(class Object *obj)
{
        return (int)(obj->getObjectType() && 
                     obj->getObjectType()->canGet());
}

bool cmdGet(class Object *actor)
{
	class Object *item;
	int dir;
	int x, y;
	
	cmdwin_clear();
	cmdwin_spush("Get");
	
	dir = ui_get_direction();
	
	if (dir == CANCEL)
		return false;
	
	x = actor->getX() + directionToDx(dir);
	y = actor->getY() + directionToDy(dir);
	
	item = place_get_filtered_object(actor->getPlace(), x, y, 
	cmdGetFilter);
	if (!item) {
		log_msg("Get - nothing there!");
		return false;
	}
	
	log_begin_group();
	
	while (NULL != (item = place_get_filtered_object(
				actor->getPlace(), 
				x, y, cmdGetFilter)))
	{
		item->getObjectType()->get(item, actor);
		//do not allow too much AP debt if in combat
		if ((combat_get_state() == COMBAT_STATE_FIGHTING)
					&& ((2 * actor->getActionPoints()) + actor->getSpeed() < 0))
		{
			break;   
		}
	}
	
	log_end_group();
	
	mapSetDirty();
	actor->runHook(OBJ_HOOK_GET_DONE, 0);
	actor->decActionPoints(kern_intvar_get("AP_COST:get_item"));  // SAM: Better to have a number of AP by item type...
	
	
	return true;
}

static void cmd_describe_inv_entry(struct inv_entry *ie, void *unused)
{
        log_begin("...");
        ie->type->describeType(ie->count);
        log_end(NULL);
}

bool cmdOpen(class Character * pc)
{
	int dir, x, y;
	class Object *mech;
	class Container *container;

	cmdwin_clear();
	cmdwin_spush("Open");

	// Get the party member who will open the container (in combat mode
	// this is passed in as a parameter).
	if (pc) {
		cmdwin_spush(pc->getName());
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

        /* Check for a mechanism */
         mech = place_get_object(pc->getPlace(), x, y, mech_layer);

         /* Check for a container */
         container = (class Container *) place_get_object(Place, x, y, 
                                                          container_layer);

         /* ignore invisible objects unless Reveal is in effect */
         if (! Reveal) {
                 if (mech && ! mech->isVisible())
                         mech = 0;
                 if (container && ! container->isVisible())
                         container = 0;
         }

         /* If both are present the user must select one */
         if (mech && mech->getObjectType()->canOpen() && container) {

                 enum StatusMode omode;
                 struct stat_list_entry statlist[2];
                 struct KeyHandler kh;
                 struct ScrollerContext data;

                 cmdwin_push("<select>");

                 statlist[0].sprite = mech->getSprite();
                 snprintf(statlist[0].line1, sizeof(statlist[0].line1), "%s",
                          mech->getName());
                 statlist[0].line2[0] = 0;
                 statlist[0].data = mech;

                 statlist[1].sprite = container->getSprite();
                 snprintf(statlist[1].line1, sizeof(statlist[1].line2), "%s",
                          container->getName());
                 statlist[1].line2[0] = 0;
                 statlist[1].data   = container;

                 foogodSetHintText(SCROLLER_HINT);
                 foogodSetMode(FOOGOD_HINT);        
                 omode = statusGetMode();
                 statusSetGenericList("Choose Target", 2, statlist);
                 statusSetMode(GenericList);
                 
                 data.selection = NULL;
                 data.selector  = Generic;
                 kh.fx   = scroller;
                 kh.data = &data;
                 eventPushKeyHandler(&kh);
                 eventHandle();
                 eventPopKeyHandler();
                 
                 statusSetMode(omode);
                 foogodSetMode(FOOGOD_DEFAULT);
                 
                 /* Disqualify the object NOT selected */
                 if (data.selection == mech)
                         container = NULL;
                 else
                         mech = NULL;

                 cmdwin_pop();
         }

         /* Open a mechanism */
         if (mech && mech->getObjectType()->canOpen()) {
                 cmdwin_push("%s!", mech->getName());
                 mech->getObjectType()->open(mech, pc);
                 mapSetDirty();
                 pc->runHook(OBJ_HOOK_OPEN_DONE, "p", mech);
                 pc->decActionPoints(kern_intvar_get("AP_COST:open_mechanism"));
                 return true;
         }

         /* Nothing to open */
         if (NULL == container) {
                 cmdwin_push("abort!");
                 log_msg("Open - nothing there!");
                 return false;
         }

	/* Open Container */

        log_begin_group();

        pc->runHook(OBJ_HOOK_OPEN_DONE, "p", container);
        pc->decActionPoints(kern_intvar_get("AP_COST:open_container"));
        cmdwin_push("%s!", container->getName());

        // Describe the contents of the container.
        log_msg("You find:");
        container->forEach(cmd_describe_inv_entry, NULL);

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

        log_end_group();

	mapSetDirty();
	return true;
}

static bool cmd_nop_qh(struct QuitHandler *kh)
{
        return false;
}


bool cmdQuit(void)
{
	int yesno;
 	struct QuitHandler qh;

        /* Bugfix: if the player tries to close the window while we're in one
         * of our getkey() calls, we'll enter this function recursively,
         * messing up the prompts. So push a nop quit handler to prevent
         * that. Kind of a hack: why should this function "know" it is called
         * by the default quit handler? */
	qh.fx = cmd_nop_qh;
	eventPushQuitHandler(&qh);
        
	cmdwin_clear();
	cmdwin_spush("Quit");
        cmdwin_spush("<y/n>");
	getkey(&yesno, yesnokey);
	cmdwin_pop();

        /* Cancel quit? */
	if (yesno == 'n') {
                cmdwin_spush("abort!");
                Quit = false;
                goto pop_qh;
        }

        cmdwin_spush("save");
        cmdwin_spush("<y/n>");
        getkey(&yesno, yesnokey);
        cmdwin_pop();

        /* Don't save? */
        if (yesno == 'n') {
                cmdwin_spush("not saving!");
                Quit = true;
                goto pop_qh;
        }

        if (cmdSave()) {
            cmdwin_spush("saved!");
            log_msg("Goodbye!\n");
            Quit = true;
        } else {
            Quit = false;
        }

 pop_qh:
        eventPopQuitHandler();

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
	cmdwin_spush("Attack");
        cmdwin_spush("<direction>");
	getkey(&dir, cardinaldirkey);
	cmdwin_pop();
	if (dir == CANCEL) {
		cmdwin_spush("none!");
		return;
	}
	cmdwin_spush("%s", directionToString(dir));

        // Get the npc party being attacked
        info.dx = directionToDx(dir);
        info.dy =  directionToDy(dir);;
        info.place = player_party->getPlace();
        info.x = place_wrap_x(info.place, player_party->getX() + info.dx);
        info.y = place_wrap_y(info.place, player_party->getY() + info.dy);
        info.npc_party = place_get_Party(info.place, info.x, info.y);
				
        if (info.npc_party == NULL) {
                cmdwin_spush("nobody there!");
                log_msg("Attack - nobody there!");
                return;
        } 
        info.px = player_party->getX();
        info.py = player_party->getY();

        cmdwin_spush("%s", info.npc_party->getName());

        // If the npc is not hostile then get player confirmation.
        if (! are_hostile(info.npc_party, player_party)) {
                int yesno;
                cmdwin_spush("attack non-hostile");
                cmdwin_spush("<y/n>");
                getkey(&yesno, yesnokey);
                cmdwin_pop();
                if (yesno == 'n') {
                        cmdwin_spush("no");
                        return;
                }
                cmdwin_spush("yes");

                make_hostile(info.npc_party, player_party);
        }

        // Log the attack.
        log_begin("You attack ");
        info.npc_party->describe();
        log_end(".");

        // Enter combat
        combat_enter(&cinfo);
}

void cmdDeveloperEval(struct session *session)
{
        unsigned int len = 1024;
        char *buf = (char*)calloc(len, sizeof(char));
        if (!buf) {
                log_msg("Eval: not enough memory!");
                return;
        }

        cmdwin_clear();
        cmdwin_push("Eval:");

        if (!ui_getline_filtered(buf, len, NULL)) {
                log_msg("Eval: abort");
                cmdwin_push("abort!");
                goto cleanup;
        }

        log_msg("Eval: %s", buf);
        session_eval(session, buf);

 cleanup:
        free(buf);
}

void cmdFire(void)
{
	int dir;

	cmdwin_clear();
	cmdwin_spush("Fire");

        class Vehicle *vehicle = player_party->getVehicle();
	if ((!vehicle ||
             !vehicle->getOrdnance())) {
                // SAM: 
                // In future, we may check for adjacent "cannon" 
                // mechanisms here (as in U5).
		cmdwin_spush("No cannons available!");
                log_msg("Fire - no cannons!");
		return;
	}

	cmdwin_spush("%s", vehicle->getOrdnance()->getName());
        cmdwin_spush("<direction>");
	getkey(&dir, dirkey);
	cmdwin_pop();

	if (dir == CANCEL) {
		cmdwin_spush("none!");
		return;
	}

	cmdwin_spush("%s", directionToString(dir));
	if (! vehicle->fire_weapon(directionToDx(dir), 
                                                 directionToDy(dir), 
                                                 player_party)) {
		cmdwin_spush("Not a broadside!");
                log_msg("Fire - not a broadside!");
		return;
        }
}

bool cmdReady(class Character * member)
{
	bool committed = false;
	struct inv_entry *ie;
	struct KeyHandler kh;
	struct ScrollerContext sc;
	const char *msg = 0;

	cmdwin_clear();
	cmdwin_spush("Ready");

        // Select user
        if (member) {
		cmdwin_spush("%s", member->getName());                
        } else {
                member = select_party_member();
                if (member == NULL)
                        return false;       

                if (member->isCharmed()) {
                        cmdwin_push("Charmed!");
                        log_msg("Ready - charmed!");
                        return false;
                }

        }

        log_begin_group();
        log_msg("%s readies arms:", member->getName());

	statusSelectCharacter(member->getOrder());

	player_party->sortReadiedItems(member);
        foogodSetHintText(SCROLLER_HINT);
        foogodSetMode(FOOGOD_HINT);        
	statusSetMode(Ready);
	sc.selector = InventoryItem;
	kh.fx = scroller;
	kh.data = &sc;
	eventPushKeyHandler(&kh);

        cmdwin_spush("<select/ESC>");

	for (;;) {

		sc.selection = NULL;

		eventHandle();
                cmdwin_pop();

		ie = (struct inv_entry *) sc.selection;
		if (ie == NULL) {
			cmdwin_spush("done!");
			break;
		}

		committed = true;

		class ArmsType *arms = (class ArmsType *) ie->type;

                log_begin("%s - ", arms->getName());

		if (ie->ref && member->unready(arms)) {
			msg = "unreadied!";
			member->decActionPoints(arms->getRequiredActionPoints());
			statusRepaint();
		} else {

			switch (member->ready(arms)) {
			case Character::Readied:
				statusRepaint();
				msg = "readied!";
				member->decActionPoints(arms->getRequiredActionPoints());
           /* Move the readied item to the front of the
            * list for easy access next time, and to
            * percolate frequently-used items up to the
            * top. */
           //player_party->inventory->moveToFront(ie);
           /* After re-ordering the list, reset the status
            * viewer to synch it back up with the new
            * list. */
           //statusSetMode(Ready);
				break;
			case Character::NoAvailableSlot:
				msg = "all full!";
				break;
			case Character::WrongType:
				msg = "can't use!";
				break;
			case Character::TooHeavy:
				msg = "too heavy!";
				break;
			default:
				assert(false);
				break;
			}
		}

		cmdwin_spush("%s %s", arms->getName(), msg);
                log_end(msg);
      
     	//do not allow too much AP debt if in combat
		if ((combat_get_state() == COMBAT_STATE_FIGHTING)
					&& ((2 * member->getActionPoints()) + member->getSpeed() < 0))
		{
			break;   
		}
      
	}

	eventPopKeyHandler();
	statusSetMode(ShowParty);
        foogodSetMode(FOOGOD_DEFAULT);

        if (committed) {
                player_party->sortReadiedItems(member);
                member->runHook(OBJ_HOOK_READY_DONE, 0);
        }

        log_end_group();

	return committed;
}

static void cmd_init_movecursor_data(struct movecursor_data *data, 
                                     struct list *suggest)
{
        struct list *entry = 0;

        memset(data, 0, sizeof(*data));
        data->jump = 1;

        if (! suggest)
                return;

        /* Copy the head of the list. */
        data->loc_list = suggest;
        data->cur_loc = data->loc_list;

        /* Look for the crosshair in the suggest list. */
        list_for_each(suggest, entry) {
                struct location_list *loc = 
                        (struct location_list*)entry;
                if (loc->x == Session->crosshair->getX()
                    && loc->y == Session->crosshair->getY()) {
                        data->cur_loc = &loc->list;
                        break;
                }
        }
}

int select_target(int ox, int oy, int *x, int *y, int range, 
                  struct list *suggest)
{
        return select_target_rlcb(Place, ox, oy, x, y, range, suggest, 0, 0);
}

void ui_select_target_req_init(ui_select_target_req_t *req)
{
        memset(req, 0, sizeof(*req));
        list_init(&req->suggest);
}

static int select_target_rlcb(struct place *place, 
                              int ox, int oy, 
                              int *x, int *y,
                              int range,
                              struct list *suggest,
                              v_fncptr_iiv_t each_tile_func,
                              i_fncptr_iiv_t each_target_func)
{
        ui_select_target_req_t req;
        int ret;

        /* convert args to a targeting request */
        ui_select_target_req_init(&req);
        req.place = place;
        req.x1 = ox;
        req.y1 = oy;
        req.x2 = *x;
        req.y2 = *y;
        req.move = each_tile_func;
        req.select = each_target_func;
        req.tiles = templ_new_from_range(range);
        templ_set_origin(req.tiles, ox, oy);
        if (suggest) {
                list_move(&req.suggest, suggest);
        }
        
        /* call generic target selection */
        ret = ui_select_target_generic(&req);

        /* convert back */
        *x = req.x2;
        *y = req.y2;
        if (suggest) {
                list_move(suggest, &req.suggest);
        }
        templ_unref(req.tiles);

        return ret;
}

int ui_select_target_generic(ui_select_target_req_t *req)
{
        struct movecursor_data data;
        struct KeyHandler kh;
        struct MouseButtonHandler mbh;
        struct MouseMotionHandler mmh;

        Session->crosshair->setZone(req->tiles);
        Session->crosshair->setViewportBounded(1);
        Session->crosshair->setOrigin(req->x1, req->y1);
        Session->crosshair->relocate(req->place, req->x2, req->y2);
        Session->show_boxes=1;
        mapSetDirty();

        cmd_init_movecursor_data(&data, &req->suggest);
        data.each_tile_func   = req->move;
        data.each_target_func = req->select;
        data.data = req->data;

        kh.fx   = movecursor;
        kh.data = &data;
  
        mbh.fx = mouse_button_cursor;
        mbh.data = &data;

        mmh.fx = mouse_motion_cursor;
        mmh.data = &data;

        eventPushMouseButtonHandler(&mbh);
        eventPushKeyHandler(&kh);
        cmdwin_spush("<target> (ESC to exit)");
        eventHandle();
        cmdwin_pop();
        eventPopKeyHandler();
        eventPopMouseButtonHandler();
  
        Session->show_boxes=0;
        req->x2 = Session->crosshair->getX();
        req->y2 = Session->crosshair->getY();
        Session->crosshair->remove();
        Session->crosshair->setZone(0);
        mapSetDirty();
  
        if (data.abort) {
                cmdwin_spush("Done.");
                return -1;
        }
  
        return 0;
}

int select_target_with_doing(int ox, int oy, int *x, int *y,
                             int range,
                             v_fncptr_iiv_t each_tile_func,
                             i_fncptr_iiv_t each_target_func)
{
        return select_target_rlcb(Place, ox, oy, x, y, range, 0, 
                                  each_tile_func, 
                                  each_target_func);
}

bool cmdHandle(class Character * pc)
{
	// SAM: Adding (H)andle command...
	int x;
	int y;

	cmdwin_clear();
	cmdwin_spush("Handle");

	if (pc) {
		// A party member was specified as a parameter, so this must be
		// combat mode. Use the party member's location as the origin.
		x = pc->getX();
		y = pc->getY();
		cmdwin_spush("%s", pc->getName());
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
			cmdwin_spush("%s", pc->getName());
		} else {
			pc = select_party_member();
			if (pc == NULL) {
				return false;
			}
		}
	}

	// *** Pick a target ***

	if (select_target(x, y, &x, &y, 1, 0) == -1)
		return false;

        // Try to find a mech
	class Object *mech;
	mech = place_get_object(Place, x, y, mech_layer);
	if (! mech 
            || ! mech->getObjectType()->canHandle()
            || (! mech->isVisible()            
                && ! Reveal)) {
                cmdwin_spush("nothing!");
                log_msg("Handle - nothing there to handle!");
                return false;
        }

        // Handle it (sometimes mechs are intentionally nameless so that they
        // remain hidden from x)amine and s)earch commands)
        const char *mechName=mech->getName();
        if (!mechName) {
                mechName = "a hidden mechanism";
        }
        cmdwin_spush("%s", mechName);
        log_msg("%s handles %s", pc->getName(), mechName);
        mech->getObjectType()->handle(mech, pc);
        pc->runHook(OBJ_HOOK_HANDLE_DONE, "p", mech);
        pc->decActionPoints(kern_intvar_get("AP_COST:handle_mechanism"));
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
        int result;

	cmdwin_clear();
	cmdwin_spush("Use");

        // Select user
        if (flags & CMD_SELECT_MEMBER) {
                member = select_party_member();
                if (member == NULL)
                        return false;       
        } else {
                assert(member);
                cmdwin_spush("%s", member->getName());
        }
	statusSelectCharacter(member->getOrder());

        // select item to use
	statusSetMode(Use);
	ie = ui_select_item();
	statusSetMode(ShowParty);
	if (ie == NULL) {
		return false;
        }

        /* warning: assume usable item came from player inventory; move it to
         * the front of the list so that frequently-used items percolate to the
         * top for easy selection by the player. Oh, and do this *before* using
         * it, since using it may delete the ie if it is a consumable item and
         * the last one in inventory. */
        player_party->inventory->moveToFront(ie);

	item = ie->type;
        assert(item->isUsable());

        // Use the item
        log_begin("%s: %s - ", member->getName(), item->getName());
	result = item->use(member);
        cmd_eval_and_log_result(result);
        log_end(0);

        member->runHook(OBJ_HOOK_USE_DONE, "p", item);

        // Item's appear to decrement AP in the script...
        //member->decActionPoints(kern_intvar_get("AP_COST:use_item"));
	statusRepaint();

	return true;
}

/* Helper function called by cmdNewOrder: */
static void cmd_switch_party_leader(class Character *old_leader,
                                    class Character *new_leader)
{
        new_leader->setLeader(true);
        old_leader->setLeader(false);
        old_leader->endTurn();
        old_leader->setControlMode(CONTROL_MODE_FOLLOW);
        player_party->setLeader(new_leader);
}

void cmdNewOrder(void)
{
	class Character *pc1, *pc2;

        switch (player_party->getSize()) {
        case 0:
                assert(0);
                break;
        case 1:
                log_msg("New Order - only one party member!");
                return;
        case 2:
                pc1 = player_party->getMemberByOrder(0);
                pc2 = player_party->getMemberByOrder(1);
                goto swap;
        }

	cmdwin_clear();
	cmdwin_spush("Switch");

        // Set the mode now - before calling select_party_member - so that the
        // screen will not flash back to a short status window between the two
        // calls to select_party_member.
        statusSetMode(SelectCharacter);

	pc1 = select_party_member();
	if (pc1 == NULL) {
                statusSetMode(ShowParty);
		return;
        }

	cmdwin_spush("with");

	pc2 = select_party_member();
	if (pc2 == NULL) {
                statusSetMode(ShowParty);
		return;
	}

        statusSetMode(ShowParty);
 swap:
        player_party->switchOrder(pc1, pc2);

	log_msg("New Order: %s switched with %s\n", pc1->getName(),
		     pc2->getName());

        // If one of the switched members was the party leader then make the
        // other one the new leader (unless the other one is dead or otherwise
        // incapable of being a party leader).
        if (pc1->isLeader() && pc2->canBeLeader()) {
                cmd_switch_party_leader(pc1, pc2);
        } else if (pc2->isLeader() && pc1->canBeLeader()) {
                cmd_switch_party_leader(pc2, pc1);
        }

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

struct talk_info {
        class Object *origin;
        class Object *nearest;
        int min_distance;
        int range;
        struct list suggest;
};

/* cmd_talk_info_visitor - called on each object in the current place. Used by
 * cmdTalk() to find the nearest conversant and build a list of other available
 * conversants. */
static void cmd_talk_info_visitor(class Object *obj, void *data)
{
        struct location_list *entry = 0;
        int dist = 0;
        struct talk_info *info = 
                (struct talk_info*)data;

        if (! obj->getConversation())
                return;

        if (obj->isPlayerControlled())
                return;

        if (TimeStop)
                return;
        
        if (! obj->isVisible() && ! Reveal)
                return;

        /* Filter out objects not in los of the subject */
        if (! place_in_los(info->origin->getPlace(),
                           info->origin->getX(),
                           info->origin->getY(),
                           obj->getPlace(),
                           obj->getX(),
                           obj->getY()))
                return;
        

        /* Filter out objects not in range of conversation. */
        dist = place_flying_distance(info->origin->getPlace(),
                                     info->origin->getX(),
                                     info->origin->getY(),
                                     obj->getX(),
                                     obj->getY());

        if (dist > info->range)
                return;

        /* Add this conversant to the suggestion list. */
        entry = (struct location_list*)malloc(sizeof(*entry));
        assert(entry);
        entry->x = obj->getX();
        entry->y = obj->getY();
        list_add_tail(&info->suggest, &entry->list);

        /* Remember the nearest conversant. */
        if (! info->nearest
            || dist < info->min_distance) {
                info->nearest = obj;
                info->min_distance = dist;
        }
}

/* cmd_get_talk_info - find the nearest conversant and build the
 * 'suggest' list. */
static class Object *cmd_get_talk_info(struct talk_info *info,
                                             class Object *member, 
                                             int range)
{
        memset(info, 0, sizeof(info));
        info->origin = member;
        info->nearest = NULL;
        info->min_distance = 0;
        info->range = range;
        list_init(&info->suggest);

        place_for_each_object(member->getPlace(), 
                              cmd_talk_info_visitor,
                              info);

        return info->nearest;
}

/* cmd_cleanup_talk_info - free the 'suggest' list. */
static void cmd_cleanup_talk_info(struct talk_info *info)
{
        struct list *entry;
        for (entry = info->suggest.next; entry != &info->suggest; ) {
                struct location_list *loc = (struct location_list*)entry;
                entry = entry->next;
                list_remove(&loc->list);
                free(loc);
        }
}

void cmdTalk(Object *member)
{
	struct conv *conv = NULL;
        class Object *obj, *conversant = NULL;
        int x, y;
        struct talk_info info;
        const int max_distance = 5;

	// *** Prompt user & check if valid ***

	cmdwin_clear();
	cmdwin_spush("Talk");

        if (! member) {
                member = select_party_member();
                if (! member)
                        return;
        }

        // start cursor on nearest object with a conversation
        conversant = cmd_get_talk_info(&info, member, max_distance);
        if (! conversant) {
                conversant = member;
        }

        x = conversant->getX();
        y = conversant->getY();

	if (select_target(member->getX(), member->getY(), 
                          &x, &y, max_distance, &info.suggest) == -1) {
                goto cleanup;
	}

	obj = place_get_object(Place, x, y, being_layer);

	if (!obj) {
                cmdwin_spush("nobody there!");
                log_msg("Try talking to a PERSON.");
                goto cleanup;
        }

        // This next bit was added to support talking to parties, where the
        // speaker is not the party itself.
        obj = obj->getSpeaker();
        if (! obj) {
                cmdwin_spush("cancel");
                goto cleanup;
        }

        if (TimeStop && !obj->isPlayerPartyMember()) {
                cmdwin_spush("time stopped!");
                log_msg("This person seems frozen in time.");
                goto cleanup;
        }

        conv = obj->getConversation();
        if (!conv) {
		cmdwin_spush("no response!");
                log_begin("No response from ");
                obj->describe();
                log_end(".");
                goto cleanup;
        }

	cmdwin_spush(obj->getName());

	if (((obj->getLayer() == being_layer) 
             && ((class Character*)obj)->isAsleep())) {
		log_msg("Zzzz...\n");
                goto cleanup;
	}

        conv_enter(obj, member, conv);
	mapSetDirty();

 cleanup:
        
        cmd_cleanup_talk_info(&info);

        return;
}

bool cmdZtats(class Character * pc)
{
	statusRunApplet(ztats_get_applet()); /* runs until user ESC */
	statusSetMode(ShowParty); /* restore default status mode */
	return false;
}

static int select_hours(int allow_sunrise)
{
	struct get_char_info info;

        if (allow_sunrise) {
                cmdwin_spush("<hours[0-9]/[s]unrise>");
                info.string = "0123456789sS";
        } else {
                cmdwin_spush("<hours[0-9]>");
                info.string = "0123456789";
        }

	info.c = '0';

	getkey(&info, &cmd_getchar);

	if (! info.c || info.c == '0') {
                cmdwin_pop();
		cmdwin_spush("none!");
                return 0;
        }
        else if (allow_sunrise
                 && (info.c == 's' ||
                     info.c == 'S')) {
                int hour;
                int sunrise;

                cmdwin_pop();
                cmdwin_push("until sunrise");
                hour = clock_time_of_day() / 60;
                sunrise = SUNRISE_HOUR + 1;
                if (hour < sunrise)
                        return sunrise - hour;
                return HOURS_PER_DAY - hour + sunrise;
        }
	else if (info.c == '1') {
		cmdwin_push(" hour");
                return 1;
        }
	else {
		cmdwin_push(" hours");
                return info.c - '0';
        }
}

int ui_get_quantity(int max)
{
	struct get_number_info info;
        char prompt[64];

        /* Push the prompt but remember it for use within getnum() */
	if (max == -1) {
                snprintf(prompt, sizeof(prompt), "<quantity>");
	} else {
                snprintf(prompt, sizeof(prompt), 
                         "<quantity[0-%d]/RET=%d>", max, max);
	}

	info.digit = 0;
	info.state = GN_ALL;
	info.prompt = prompt;

        cmdwin_spush(info.prompt);
	getkey(&info, getnum);

	if (info.state == GN_ALL) {
		if (max == -1)
			info.digit = 0;
		else
			info.digit = max;
	} else if (info.state == GN_CANCEL)
		cmdwin_spush("none!");

	return info.digit;
}

int cmd_camp_in_wilderness(class Party *camper)
{
	int hours, yesno;
	class Character *guard = 0;

	cmdwin_clear();
	cmdwin_spush("Camp");

	if (!place_is_passable(camper->getPlace(), camper->getX(), 
                               camper->getY(), camper, PFLAG_IGNOREVEHICLES)) {
		cmdwin_spush("not here!");
                log_msg("Camp - not here!");
		return 0;
	}

        if (place_get_subplace(camper->getPlace(), 
                               camper->getX(), 
                               camper->getY())) {
		cmdwin_spush("not here!");
                log_msg("Camp - not here!");
                return 0;
        }

	hours = select_hours(1);
	if (hours == 0)
		return 0;

        cmdwin_spush(""); /* for the '-' */
	cmdwin_spush("set a watch");
        cmdwin_spush("<y/n>");
	getkey(&yesno, &yesnokey);

	if (yesno == 'y') {

		cmdwin_pop();
		guard = select_party_member();
		if (!guard) {
                        cmdwin_pop();
			cmdwin_push("no watch");
		}
                else if (guard->isDead()) {
                        log_msg("You prop up the corpse and wave off "
                                "the flies...");
                }
                // else select_party_member() prints the name

	} else {
		cmdwin_pop();
		cmdwin_spush("no watch");
	}

	player_party->beginCamping(guard, hours);
        camper->endTurn();
	run_combat(true, guard, hours, NULL);

        return 0;
}

void cmdLoiter(class Being *subject)
{
    int hours = 0;

        cmdwin_clear();
        cmdwin_spush("Loiter");

        /* Check if enemies are around. */
        if (place_contains_hostiles(subject->getPlace(), subject)) {
                cmdwin_spush("foes nearby!");
                log_msg("Loiter - foes nearby!");
                return;
        }
        
        /* Check if any party members are engaged in a task. */
        class Character *pc;
        if ((pc = cmdAnyPartyMemberEngagedInTask())) {
            log_msg("Loiter - %s engaged in task!", pc->getName());
            cmdwin_spush("busy with tasks!");
            return;
        }

        /* Prompt for the number of hours. */
        hours = select_hours(0);
        if (!hours) {
                return;
        }

        /* Tell the party to start loitering. */
        cmdwin_spush("loitering...");
        player_party->beginLoitering(hours);

        /* End the turn. */
        subject->endTurn();
        
}

int cmd_camp_in_town(class Character *camper)
{
        int hours;

        cmdwin_clear();
        cmdwin_spush("Rest");

        // Party must be in follow mode.
        if (player_party->getPartyControlMode() != PARTY_CONTROL_FOLLOW) {
                cmdwin_spush("must be in follow mode!");
                log_begin_group();
                log_msg("Camp - party not in follow mode!");
                log_msg("(Hint: hit 'f' to enter follow mode)");
                log_end_group();
                return 0;
        }
                
        // Check for an object that will serve as a bed.
        if (place_get_object(camper->getPlace(), camper->getX(), 
                             camper->getY(),  bed_layer) == NULL) {
                cmdwin_spush("no bed!");
                log_msg("Camp - no bed here!");
                return 0;
        }

        // Rendezvous the party around the bed.
        if (! player_party->rendezvous(camper->getPlace(), camper->getX(), 
                                       camper->getY())) {
                log_msg("Camp - party can't rendezvous!");
                return 0;
        }

        // Prompt for the number of hours to sleep.
        hours = select_hours(! camper->getPlace()->underground);
        if (hours == 0)
                return 0;

        // Put the party in "sleep" mode before returning back to the main
        // event loop.
        cmdwin_spush("resting...");
        player_party->beginResting(hours);
        camper->endTurn();

        return TURNS_PER_HOUR;
}

int get_spell_name(struct KeyHandler *kh, int key, int keymod)
{
	struct get_spell_name_data *ctx;
	char *word, letter;

	ctx = (struct get_spell_name_data *) kh->data;

        switch (ctx->state) {

        case GN_ZERO: /* No spell words are entered yet and the prompt is
                       * sitting there. */

                /* Done? */
                if (key == '\n') {
                        cmdwin_pop();
                        return 1;
                }

                /* Abort? */
                if (key == CANCEL) {
                        cmdwin_pop();
                        ctx->spell_name[0] = 0;
                        return 1;
                }

                /* A letter? */
                if (isalpha(key)) {
                        
                        /* Lookup the word that goes with the letter. */
                        letter = toupper(key);
                        word = magic_lookup_word(&Session->magic, letter);

                        /* Valid word? */
                        if (word) {

                                /* Clear prompt, show the word and advance. */
                                cmdwin_pop();
                                cmdwin_push(word);
                                *ctx->ptr = letter;
                                ctx->ptr++;
                                ctx->n++;
                                ctx->state = GN_SOME;
                        }
                }
                return 0;

        case GN_SOME: /* One or more words are already entered. */

                /* Done? Ensure null-termination. */
                if (key == '\n') {
                        /* Segment-push a null string to force a '-' following
                         * the spell name. */
                        cmdwin_spush(0);
                        *ctx->ptr = 0;
                        return 1;
                }

                /* Abort? Terminate string at beginning. */
                if (key == CANCEL) {
                        ctx->spell_name[0] = 0;
                        return 1;
                }

                /* Backspace? */
                if (key == '\b' && ctx->n) {
                        cmdwin_pop();
                        ctx->ptr--;
                        *ctx->ptr = 0;
                        ctx->n--;

                        /* Back to empty? Re-prompt. */
                        if (!ctx->n) {
                                cmdwin_spush(ctx->prompt);
                                ctx->state = GN_ZERO;
                        }
                        return 0;
                }

                /* Out of space? */
                if (ctx->n == MAX_WORDS_IN_SPELL_NAME) {
                        return 0;
                }

                /* Not a letter? */
                if (!isalpha(key))
                        return 0;

                /* Lookup the word that goes with the letter. */
                letter = toupper(key);
                word = magic_lookup_word(&Session->magic, letter);
                if (!word) {
                        return 0;
                }
                
                /* Accept the word and print it. After the first word separate words
                 * with a space. */
                cmdwin_push(" %s", word);
                *ctx->ptr = letter;
                ctx->ptr++;
                ctx->n++;
                return 0;

        default: /* Invalid state */
                assert(0);
                return 0;
        }

}

int select_spell(struct get_spell_name_data *context)
{
	struct KeyHandler kh;

	memset(context, 0, sizeof(*context));
	context->ptr = context->spell_name;
        context->prompt = "<spell name>";
        context->state = GN_ZERO;

	kh.fx = get_spell_name;
	kh.data = context;

        cmdwin_spush(context->prompt);
	eventPushKeyHandler(&kh);
	eventHandle();
	eventPopKeyHandler();

	if (strlen(context->spell_name) == 0) {
		cmdwin_spush("none!");
		return -1;
	}

	return 0;
}

/**
 * Log console messages describing the results of a closure call and return
 * whether or not it succeeded.
 *
 * @param result is what closure_exec() returned. It should be one of the
 * standard result codes.
 *
 * @returns non-zero iff the closure call was successful. "Succesful" means it
 * actually did something; whereas "unsuccesful" means the request was
 * effectively aborted in the script, so the caller should skip post-processing
 * like mp or ap reduction, etc.
 */
static int cmd_eval_and_log_result(int result)
{
        static struct {
                const char *string;
                int success;
        } tbl[] = {
                { "^c+gok^c-!",               1 },
                { "^c+Gno target^c-!",        0 },
                { "^c+yno effect^c-!",        1 },
                { "^c+yno hostiles here^c-!", 1 },
                { "^c+Glacks skill^c-!",      0 },
                { "^c+rfailed^c-!",           1 },
                { "^c+Gnot here^c-!",         0 },
                { "^c+rcritical fail^c-!!!",  1 },
                { "^c+ynot now^c-!",          0 },
        };

        if (result < 0 || result >= array_sz(tbl)) {
                warn("result code '%d' unknown\n", result);
                return 1;
        }

        log_continue(tbl[result].string);
        return tbl[result].success;
}

bool cmdCastSpell(class Character * pc)
{
	struct get_spell_name_data context;
	struct inv_entry *ie = NULL;
	struct spell *spell;
	bool mixed = false;
	bool natural = false;
	int i, cast = 0, result = 0;
        char spell_name[MAX_SPELL_NAME_LENGTH];

        if (MagicNegated) {
                log_msg("Cast - magic negated!\n");
                return false;
        }

	cmdwin_clear();
	cmdwin_spush("Cast");

	/* If the pc is null then we are in non-combat mode and need to promp
         * the user. */
	if (pc == NULL) {
		pc = select_party_member();
		if (pc == NULL) {
			return false;
		}
		statusSetMode(ShowParty);
	}

        /* Make sure the PC is not asleep, dead, etc. */
        if (pc->isDead()) {
                cmdwin_spush("unable right now!");
                log_msg("Cast - %s is too dead!", pc->getName());
                return false;
        }

        if (pc->isAsleep()) {
                cmdwin_spush("unable right now!");
                log_msg("Cast - %s is asleep!", pc->getName());
                return false;
        }

	/* Prompt to select a spell */
	if (select_spell(&context) == -1)
		return false;

        /* The code for the spell is stored in the context, but not the full
         * name. I want the full name for log msgs. */
        magic_spell_code_to_name(&Session->magic, spell_name, 
                                 MAX_SPELL_NAME_LENGTH, 
                                 context.spell_name);

        log_begin("%s: %s - ", pc->getName(), spell_name);

	/* Lookup the spell in the list of valid spells. */
	spell = magic_lookup_spell(&Session->magic, context.spell_name);
	if (!spell) {
                /* Bugfix for SF1564255: don't let player guess at spells. */
		cmdwin_spush("none mixed!");
                log_end("none mixed!");
		return false;
	}

	/* Check if the spell can be used in this context. */
	if (!(player_party->getContext() & spell->context)) {
		cmdwin_spush("not here!");
                log_end("not here!");
		return false;
	}

	/* Check if the character comes by this spell naturally. */
	for (i = 0; i < pc->species->n_spells; i++) {
		if (! strcmp(pc->species->spells[i], spell->code)) {
			natural = true;
			break;
		}
	}

        /* Check if the caster is of sufficient level. */
        /*
         * FIXME: what if the spell is natural? cast An Xen Exe on a snake and
         * try to cast In Nox Por to see what I mean...
         */
	if (!natural && pc->getLevel() < spell->level) {
		cmdwin_spush("need more experience!");
                log_end("must be level %d!", spell->level);
		return false;
	}

	/* Check party inventory for a mixed spell. */
	if (!natural) {
		ie = player_party->inventory->search(spell->type);
		if (ie && ie->count)
			mixed = true;
	}

	if (!natural && !mixed) {
		cmdwin_spush("none mixed!");
                log_end("none mixed!");
		return false;
	}

	/* Check if the character has enough mana to cast the spell. */
	if (pc->getMana() < spell->cost) {
		cmdwin_spush("need more mana!");
                log_end("need more mana!");
		return false;
	}

        /* Cast the spell. */
        result = spell->type->cast(pc);
        cast = cmd_eval_and_log_result(result);

        if (! cast) {
                log_end(NULL);
                return false;
        }

        /* Decrement the caster's mana. */
        pc->runHook(OBJ_HOOK_CAST_DONE, 0);
        pc->addMana(0 - spell->cost);
        pc->decActionPoints(spell->action_points);
        pc->addExperience(spell->cost);

	/* If the spell was mixed then remove it from inventory. */
	if (mixed) {
                int count = ie->count - 1;
		player_party->takeOut(ie->type, 1);
                log_msg("%d %s remaining", count, spell_name);
        }

        /* Some spells have status in the foogod window, so repaint it now. */
        foogodRepaint();

        log_end(NULL);

	return true;

}

bool cmdMixReagents(class Character *character)
{
	struct spell *spell;
	struct get_spell_name_data context;
	struct list reagents, *elem;
	int quantity, max_quantity;
	struct inv_entry *ie, *ie_spell = 0;
	bool mistake = false;
        char spell_name[MAX_SPELL_NAME_LENGTH];

	list_init(&reagents);

	cmdwin_clear();
	cmdwin_spush("Mix");

	// Select a spell...
	if (select_spell(&context) == -1)
		return false;

        // The code for the spell is stored in the context, but not the full
        // name. I want the full name for log msgs.
        magic_spell_code_to_name(&Session->magic, spell_name, 
                                 MAX_SPELL_NAME_LENGTH, 
                                 context.spell_name);

	// Lookup the spell. If null then keep going and bomb when done.
	spell = magic_lookup_spell(&Session->magic, context.spell_name);

        // Show the player how many he already has mixed...
        ie_spell = 0;
        if (spell) {
                ie_spell = player_party->inventory->search(spell->type);
        }
        if (ie_spell && ie_spell->count) {
                cmdwin_spush("%d mixed", ie_spell->count);
        } else {
                cmdwin_spush("0 mixed");
        }

	// Prompt for reagents 
	cmdwin_spush("<select, then M)ix>");

        foogodSetHintText("\005\006=scroll ENT=add/remove ESC=abort M=done");
        foogodSetMode(FOOGOD_HINT);

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
                        cmdwin_pop();
			eventPopKeyHandler();
			cmdwin_spush("none!");
			goto done;
		}

		if (sc.done)
			break;

		ie = (struct inv_entry *) sc.selection;
                if (! ie) {
                        /* This happens when the player has no reagents
                         * whatsoever. */
			cmdwin_pop();
			eventPopKeyHandler();
			cmdwin_spush("none!");
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

	cmdwin_pop();
	eventPopKeyHandler();

	if (list_empty(&reagents)) {
		cmdwin_spush("none!");
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

                cmdwin_push_mark();
		quantity = ui_get_quantity(max_quantity);

		if (quantity == 0) {
			goto done;
		}

		if (quantity <= max_quantity)
			break;

                cmdwin_spush(0); /* for the '-' after the quantity */
		cmdwin_spush("not enough reagents!");
		getkey(&dummy, anykey);
		cmdwin_pop_to_mark();
	}

        cmdwin_push("-");
	log_begin("Mix: %s - ", spell_name);

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
                                        // The following line is safe only
					// because this is the end of the
					// list_for_each loop!
					list_remove(elem);
					ie->ref--;
					player_party->takeOut(ie->type, 
                                                              quantity);
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

        statusSetMode(ShowParty);
        foogodSetMode(FOOGOD_DEFAULT);

        // committed to action now, so decrement AP
        if (character) {
                character->runHook(OBJ_HOOK_MIX_DONE, 0);
                
                if (!spell)
                {
		    // Failed attempt (mixing a spell which does not exist)
		    int num  = kern_intvar_get("AP_COST:mix_reagents_nospell_num");
		    int dice = kern_intvar_get("AP_COST:mix_reagents_nospell_dice");
		    int plus = kern_intvar_get("AP_COST:mix_reagents_nospell_plus");
		    int AP_wasted = dice_roll_numeric(num, dice, plus);
		    character->decActionPoints(AP_wasted);
		    // was 3d50+20 -- dice_roll_numeric(3, 50, 20)
                }
                else if (mistake)
                {
		    int level = spell->level;
		    int num  = kern_intvar_get("AP_COST:mix_reagents_badmix_num");
		    int dice = kern_intvar_get("AP_COST:mix_reagents_badmix_dice");
		    int plus = kern_intvar_get("AP_COST:mix_reagents_badmix_plus");
		    int AP_wasted = dice_roll_numeric(level * num, dice, plus);
		    character->decActionPoints(AP_wasted);
		    // was 1d(2*AP)+100 -- dice_roll_numeric(1 ,(2 * spell->action_points) , 100)
            	 }
            	else
            	{
		    // mixing should be SLOW
		    int base      = kern_intvar_get("AP_COST:mix_reagents_base");
		    int per_mix   = kern_intvar_get("AP_COST:mix_reagents_per_mix");
		    int per_level = kern_intvar_get("AP_COST:mix_reagents_per_level");
		    int AP_spent  = base + (quantity * per_mix) + (spell->level * per_level);
		    character->decActionPoints(AP_spent);
		    // was 100 + 2 * spell->action_points
            	}
        }

	// If the spell is invalid or the reagents are incorrect then punish
	// the player.
	if (!spell) {
                cmdwin_spush("oops!");
                player_party->damage(DAMAGE_ACID);
                log_end("ACID!");
                goto done;

        } else if (mistake) {
                cmdwin_spush("ouch!");
                player_party->damage(DAMAGE_BOMB);
                log_end("BOMB!");
                goto done;
	}

	// All is well. Add the spell to player inventory.
        cmdwin_spush("ok");
	player_party->add(spell->type, quantity);
        log_end("ok!");

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
        foogodSetMode(FOOGOD_DEFAULT);
	return true;
}

void look_at_XY(struct place *place, int x, int y, void *unused)
{
        if (DeveloperMode) {
                log_begin("At XY=(%d,%d): ", x, y);
        } else {
                log_begin("");
        }

        if ( mapTileIsVisible(x, y) ) {
                if (mapTileLightLevel(x,y) < MIN_XAMINE_LIGHT_LEVEL) {
                        log_continue("Too dark!");
                } else {
                        log_continue("You see ");
                        place_describe(place, x, y, PLACE_DESCRIBE_ALL);
                }
        } else if (ShowAllTerrain || XrayVision) {
                log_continue("You see (via xray) ");
                place_describe(place, x, y, PLACE_DESCRIBE_TERRAIN);
        } else {
                log_continue("You can't see!");
        }

        log_end(NULL);
}

int detailed_examine_XY(struct place *place, int x, int y, void *unused)
{
	if (DeveloperMode) {
			log_begin("At XY=(%d,%d): ", x, y);
	} else {
			log_begin("");
	}

	if ( mapTileIsVisible(x, y) ) {
			if (mapTileLightLevel(x,y) < MIN_XAMINE_LIGHT_LEVEL) {
					log_continue("You can't see!");
			} else {
					log_continue("You see:\n");
					place_examine(place, x, y);
			}
	} else if (ShowAllTerrain || XrayVision) {
			log_continue("You see (via xray):\n");
			place_examine(place, x, y);
	} else {
			log_continue("You can't see!");
	}

	#if 0
	// SAM: 
	// Hmmm...how best to print more info about
	// the objects on this tile?
        if ( mapTileIsVisible(x, y) ) {
                log_msg("DETAIL XY=(%d,%d) TODO - print detailed view\n", x, y);
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
                //     o NpcParties: faction, movement mode 
                //       (pmask), ...
                //     o Vehicles: movement mode, armament, current HP
                //     o Portable items: weapon/armor stats, (U)se effects, 
                //       etc...
                // Hmmm...what else?
                return;
        }
        log_msg("DETAIL XY=(%d,%d) out of LOS\n", x, y);
	#endif
	
	log_end(NULL);

        return 0; /* keep on targeting */
}

bool cmdXamine(class Object * pc)
{
	// SAM: Working on an improved (L)ook command,
	// which works as a "Look Mode" rather than a 
	// "look at 1 tile" command...
	int x, y;
        bool ret = true;

	cmdwin_clear();
	cmdwin_spush("Xamine");

        x = pc->getX();
        y = pc->getY();

        log_begin_group();

        if (pc)
                log_msg("%s examines around...", pc->getName());
        else
                log_msg("You examine around...");

        look_at_XY(pc->getPlace(), x, y, 0);  // First look at the current tile
	if (select_target_with_doing(x, y, &x, &y, pc->getVisionRadius(),
				     look_at_XY, detailed_examine_XY) == -1) {
		ret = false;
	}

        log_end_group();

	return ret;
} // cmdXamine()

const char * name_of_context (void)
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
        const char * who = "";
        const char * place_name = "";

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
    
        log_begin_group();
        log_msg("This is %s.", name_of_context() );
        log_msg("%s is in %s.", who, place_name);
		if (Place->underground) {
			log_msg("It is %s, %s of %s in the year %d.",
                day_name(), week_name(), month_name(), Session->clock.year );
        }
		else
		{
			log_msg("It is %s on %s, "
                "%s of %s in the year %d.",
                vague_time_as_string(), day_name(), 
                week_name(), month_name(), Session->clock.year );
		}
        // SAM: Is this really interesting though, I wonder?
        log_msg("%d game turns have passed.", Turn);

        log_msg("The wind is blowing from the %s.",
                directionToString(windGetDirection()) );

        if (Place->underground) {
                log_msg("%s is underground, and cannot see the sky.", 
                        who);
        } // underground
        else {
                struct list *elem;
                
                // SAM: 
                // This message won't be true if you are under 
                // a roof in a town.  In future there should be 
                // logic querying the (future) roof-ripping code here.
                log_msg("%s is beneath the open sky.", who);

                // The kernel no longer has any special knowledge about which
                // astral body is the sun, so we have to deal with all astral
                // bodies generically now. I mean, a game may have two or even
                // more suns. The time runs independently and isn't cued off
                // the sun.
                if (is_noon())
                        log_msg("It is noon.");
                else if (is_midnight())
                        log_msg("It is midnight.");

                // Report on each astral body generically.
                list_for_each(&Session->sky.bodies, elem) {
                        struct astral_body *body;
                        body = outcast(elem, struct astral_body, list);
                        if (astral_body_is_visible(body->arc)) {
                                log_begin("%s is up at arc %d", body->name, 
                                        body->arc);
                                if (body->n_phases > 1) {
                                        char *phase_name = 
                                                body->phases[body->phase].
                                                name;
                                        if (phase_name)
                                                log_continue(" in its %s "
                                                             "phase", 
                                                             phase_name);
                                        else
                                                log_continue(" in phase %d", 
                                                             body->phase);
                                }
                                log_end(".");
                        }
                }

        } // open air, under the sky

        if (player_party->getVehicle()) {
                log_msg("%s is %s a %s.", 
                        who, "using", player_party->getVehicle()->getName() );
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
                log_msg("%s is on foot.", who);
        }

        log_end_group();

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

/**
 * cmd_terraform - edit terrain interactively
 */
bool cmd_terraform(struct place *place, int x, int y)
{
    terrain_editor_run(place, x, y);
    return true;
}

bool cmd_save_current_place (struct place * place)
{
    FILE *   file;
    const char *   file_path;
    save_t * save;
    int ret;

    //log_msg("cmd_save_current_place");
    //printf("cmd_save_current_place()\n");

    file_path = "_test_save_place";

    file = file_open_in_save_dir(file_path, "w");
    if (file == NULL) {
	log_msg("Save place to file '%s' failed.", file_path);
	printf("Error on fopen() for file '%s': '%s'\n", file_path, strerror(errno));
	return 0;
    }
    Session->session_id++;  // Must increment to cause saving.

    save = save_new(file);
    save->indent     = 0;
    save->session_id = Session->session_id;

    //terrain_map_print(file, 0, place->terrain_map);
    terrain_map_save(save, place->terrain_map);
    log_msg("Saved map to file:\n'%s'", file_path);
    printf( "Saved map to file '%s'\n", file_path);

    save_del(save);

    ret = fclose(file);
    if (ret != 0) {
	// SAM: Not sure what we can do about it, 
	//      and this seems kind of low-level to log_msg() about...
	printf("Error on fclose() for file '%s': '%s'\n", file_path, strerror(errno));
	// It seems that the save method should return non-void, 
	// so that we know success/failure in this and other cases...
	return 0;
    }
    return 1;
}

void cmdZoomIn(void)
{
        struct place *subplace = 0;
        // 
        // SAM: Curently no "Enter" message is printed.  It turns out to be
        // moderately complicated to do so properly, as a distinct message for
        // each enter_combat() case might be desired...
        // 
        // For now, I print a placeholder message for each case here:
        
        if ((subplace = place_get_subplace(player_party->getPlace(),
                                                  player_party->getX(),
                                                  player_party->getY()))) {
                if (ENABLE_TOWN_ZOOM_IN) {
                        // Standing over a subplace. Try to enter with no
                        // direction, this will prompt the player to provide a
                        // direction.
                        log_msg("Enter-%s", subplace->name);
                        player_party->try_to_enter_subplace_from_edge(subplace,
                                                                      0, 0);
                } else {
                        log_msg("Enter-Use a side entrance!");
                }

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
                log_msg("Enter-Cannot zoom-in to %s!", tt->name);
        } else {
                // If standing on ordinary terrain, zoom in:
                struct terrain * tt = 
                        place_get_terrain(player_party->getPlace(),
                                          player_party->getX(),
                                          player_party->getY() );
                log_msg("Enter-%s", tt->name);
                run_combat(false, 0, 0, NULL);
        }
}

bool cmdSave(void)
{
    bool ret = true;
    class Character *pc;
    if ((pc = cmdAnyPartyMemberEngagedInTask())) {
        log_msg("Denied - %s engaged in task!", pc->getName());
        cmdwin_spush("busy with tasks!");
        return false;
    }

    char *fname = save_game_menu();
    if (!fname) {
        cmdwin_spush("abort!");
        return false;
    }
    
    log_begin("Saving to %s...", fname);
    if (session_save(fname)) {
        log_end("^c+rfailed!^c-");
        cmdwin_spush("failed!");
        ret = false;
    } else {
        cmdwin_spush("ok!");
        log_end("^c+gok!^c-");
    }
    session_save(fname);
    free(fname);
    return ret;
}

void cmdReload(void)
{
        Reload = 1;        
}

/****** New UI ******/

#define MARKUP 4


int ui_get_yes_no(const char *name)
{
	int yesno;
	cmdwin_clear();
	cmdwin_spush("Reply");
        cmdwin_spush("<y/n>");
	getkey(&yesno, yesnokey);
	cmdwin_pop();
	if (yesno == 'y') {
		cmdwin_spush("yes");
		log_msg("^c+%c%s:^c- Yes", CONV_PC_COLOR, name);
                return 1;
	} else {
		cmdwin_spush("no");
		log_msg("^c+%c%s:^c- No", CONV_PC_COLOR, name);
                return 0;
	}
}

typedef struct ui_getline_data {
        char *ptr;
        char *buf;
        int room;
        int (*filter)(int key);
} getline_t;

static int ui_getline_handler(struct KeyHandler *kh, int key, int keymod)
{
        getline_t *data = (getline_t*)kh->data;

	if (key == CANCEL) {
		while (data->ptr > data->buf) {
			data->ptr--;
			*data->ptr = 0;
			cmdwin_pop();
			data->room++;
		}
		return 1;
	}

	if (key == '\n') {
		return 1;
	}

	if (key == '\b') {
		if (data->ptr != data->buf) {
			data->ptr--;
			*data->ptr = 0;
			data->room++;
			cmdwin_pop();
		}
		return 0;
	}

        if (data->filter
            && data->filter(key)) {
                return 0;
        }

	if (isprintable(key) 
            && data->room) {
		cmdwin_push("%c", key);
		*data->ptr++ = key;
		data->room--;
	}

	return 0;
}

int ui_getline_filtered(char *buf, int len, int (*filter)(int key))
{
        struct KeyHandler kh;
        getline_t data;

        data.buf  = buf;
        data.ptr  = buf;
        data.room = len - 1;
        data.filter = filter;

        memset(buf, 0, len);

        kh.fx   = ui_getline_handler;
        kh.data = &data;

        eventPushKeyHandler(&kh);
        eventHandle();
        eventPopKeyHandler();

        return len - (data.room + 1);        
}

int ui_getline_plain(char *buf, int len)
{
        return ui_getline_filtered(buf, len, 0);
}

int ui_getline(char *buf, int len)
{
        cmdwin_clear();
        cmdwin_push("Say: ");
        return ui_getline_plain(buf, len);
}

int ui_buy(struct merchant *merch)
{
	struct KeyHandler kh;
	struct ScrollerContext sc;
	struct trade_info *trade;
	int quantity, cost, max_q, bought = 0;

	statusSetTradeInfo(merch->n_trades, merch->trades);
	statusSetMode(Trade);

	sc.selector = TradeItem;
	kh.fx = scroller;
	kh.data = &sc;

	for (;;) {

		// *** selection ***

		sc.selection = NULL;

		cmdwin_clear();
		cmdwin_spush("Buy");
                cmdwin_spush("<select/ESC>");
		eventPushKeyHandler(&kh);
		eventHandle();
		eventPopKeyHandler();
		cmdwin_pop();

		trade = (struct trade_info *) sc.selection;

		if (!trade) {
			cmdwin_spush("none!");
			break;
		}

                /* Print the sales pitch to the console, if one exists */
                if (trade->sales_pitch) {
                        log_msg("^c+%c%s:^c- %s", CONV_NPC_COLOR, merch->name,
                                trade->sales_pitch);
                }

		cmdwin_spush("%s", trade->name);

		if (player_party->gold < trade->cost) {
			int dummy;
			cmdwin_spush("not enough gold! <hit any key>");
			getkey(&dummy, anykey);
			continue;
		}
		// *** quantity ***

                cmdwin_push_mark();
		max_q = player_party->gold / trade->cost;
		quantity = ui_get_quantity(max_q);
                cmdwin_pop_to_mark();

		if (quantity == 0) {
			cmdwin_spush("none!");
			continue;
		}

		quantity = min(quantity, max_q);
		cmdwin_spush("%d", quantity);

		cost = quantity * trade->cost;

		// *** trade ***

                class ObjectType *type = (class ObjectType*)trade->data;
		cmdwin_spush("ok");
		log_msg("You buy %d %s%s for %d gold\n", quantity,
			     trade->name, quantity > 1 ? "s" : "", cost);

		player_party->gold -= cost;
                if (type->canBuy()) {
                        type->buy(player_party->get_leader(), quantity);
                } else {
                        player_party->add(type, quantity);
                }
                trade->quantity = player_party->inventory->numAvail(type);
                statusRepaint();
		foogodRepaint();
                bought++;
	}

	statusSetMode(ShowParty);
        return bought;
}

static bool conv_filter_trade(struct inv_entry *ie, void *fdata)
{
        struct trade_info *trade = (struct trade_info*)fdata;
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
                
                filter.fdata = &merch->trades[i];
                ie = player_party->inventory->first(&filter);
                if (!ie)
                        continue;

                /* Why don't aren't we setting show_sprite here, too? */
                trades[j] = merch->trades[i];
                trades[j].cost /= MARKUP;
                trades[j].quantity = ie->count - ie->ref;
                trades[j].show_quantity = 1;
                j++;
        }

	return j;
}

int ui_sell(struct merchant *merch)
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
        int sold = 0;

	// Allocate the trade list.
	trades = new struct trade_info[merch->n_trades];
	if (!trades) {
		log_msg("^c+%c%s:^c- I don't need anything.\n", 
                        CONV_NPC_COLOR, merch->name);                
		return 0;
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
		cmdwin_spush("Sell");
                cmdwin_spush("<select or ESC>");
		eventPushKeyHandler(&kh);
		eventHandle();
		eventPopKeyHandler();
		cmdwin_pop();

		trade = (struct trade_info *) sc.selection;

		if (!trade) {
			cmdwin_spush("none!");
			break;
		}

		cmdwin_spush("%s", trade->name);

		ie = player_party->inventory->search((class ObjectType *) 
                                                     trade->data);
		assert(ie);
		assert(ie->ref < ie->count);

		// quantity

		max_q = ie->count - ie->ref;

		cmdwin_push_mark();
		quantity = ui_get_quantity(max_q);
                cmdwin_pop_to_mark();

		if (quantity == 0) {
			cmdwin_spush("none!");
			continue;
		}

		quantity = min(quantity, max_q);
		cmdwin_spush("%d", quantity);

		// make the trade
		player_party->takeOut(ie->type, quantity);
		player_party->gold += quantity * trade->cost;
		foogodRepaint();

		cmdwin_spush("ok");
		log_msg("You sell %d %s%s for %d gold\n", quantity,
			     trade->name, quantity > 1 ? "s" : "",
			     quantity * trade->cost);

		// refresh the sell list
		n_trades = fill_sell_list(merch, trades);
		statusSetTradeInfo(n_trades, trades);
		statusUpdateTradeInfo(n_trades, trades);
                sold++;
	}

	statusSetMode(ShowParty);

	delete trades;
        return sold;
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

int ui_trade(struct merchant *merch)
{
	int key, traded = 0;

	for (;;) {
		cmdwin_clear();
		cmdwin_spush("Buy or sell");
                cmdwin_spush("<B/S/ESC>");
		getkey(&key, get_buy_or_sell_key);

		switch (key) {
		case 'b':
			traded += ui_buy(merch);
			break;
		case 's':
			traded += ui_sell(merch);
			break;
		default:
			cmdwin_pop();
			cmdwin_spush("none!");
			return traded;
		}
	}
}

static const char *cmd_help_text =
"Use the arrow keys to indicate direction.\n"
"Use the ESC key to cancel commands.\n"
"Use the first letter to start a command.\n"
"\n"
"A)ttack something\n"
"B)oard a ship or other vehicle\n"
"C)ast a spell\n"
"E)nter a town or dungeon\n"
"F)ire a ship's cannon or other ordnance\n"
"G)et something on the ground\n"
"H)andle a lever or mechanism\n"
"K)amp in a bed or the wilderness\n"
"L)oiter for a while\n"
"N)ew-Order (rearrange party order)\n"
"O)pen a chest, door or other closed object\n"
"Q)uit and save the game\n"
"R)eady weapons or armor\n"
"S)earch for hidden stuff\n"
"T)alk to somebody\n"
"U)se an item in inventory\n"
"Z)tats (show party status)\n"
"X)amine around\n"
"@)AT (info about place & time)\n"
"<space> (pass a turn)\n"
"CTRL-Q)uit without saving\n"
"CTRL-S)ave without quitting\n"
"CTRL-R)eload the last saved game\n"
"\n"
"When talking to people, enter a keyword.\n"
"Most people reply to NAME, JOB, TRADE and\n"
"JOIN. Their replies will give you hints\n"
"about more keywords.\n"
;

void cmdHelp(void)
{
        struct KeyHandler kh;

        foogodSetHintText(PAGER_HINT);
        foogodSetMode(FOOGOD_HINT);
        statusSetPageText("Commands", cmd_help_text);
        statusSetMode(Page);

        kh.fx = scroller;
        kh.data = NULL;
	eventPushKeyHandler(&kh);
	eventHandle();
	eventPopKeyHandler();

        statusSetMode(ShowParty);
        foogodSetMode(FOOGOD_DEFAULT);
}

void ui_name_vehicle(class Vehicle *vehicle)
{
        int yesno;
        char buf[64];

        log_begin("Do you want to name your ");
        vehicle->describe();
        log_end("?");

        cmdwin_spush("Name");
        cmdwin_spush("<y/n>");
        getkey(&yesno, yesnokey);
        cmdwin_pop();
        cmdwin_pop();

        if (yesno == 'n') {
                cmdwin_spush("no!");
                log_msg("It's likely to be stolen!");
                return;
        }

        if (!ui_getline(buf, sizeof(buf))) {
                log_msg("It's likely to be stolen!");
                return;
        }

        vehicle->setName(buf);

        log_begin("You christen ");
        vehicle->describe();
        log_end(".");
}

void cmdSettings(void)
{
        StatusMode omode = statusGetMode();
        options_menu();
        statusSetMode(omode);
}

void cmdDrop(class Character *actor)
{
        enum StatusMode omode;
        struct inv_entry *ie;
        class Object *obj;
        int maxq, quantity, dir, x, y;

        assert(actor);

        cmdwin_clear();
        cmdwin_spush("Drop");

        omode = statusGetMode();
        statusBrowseContainer(actor->getInventoryContainer(), "Drop");
        ie = ui_select_item();
        statusSetMode(omode);

        if (!ie) {
                return;
        }

        maxq = ie->count - ie->ref;
        assert(maxq);

        /* Don't drop quest items in temporary places! */
        if (place_is_wilderness_combat(actor->getPlace())
            && ie->type->isQuestItem()) {
                log_msg("%s seems important, it might get lost here!", 
                        ie->type->getName());
                return;
        }

        /* prompt for a count (unless there is only one) */
        if (ie->count == 1) {
                quantity = 1;
        } else {
                quantity = ui_get_quantity(maxq);
                if (!quantity) {
                        return;
                }
        }

        /* prompt for location */
        dir = ui_get_direction();
	if (dir == CANCEL) {
		return;
        }
        x = actor->getX() + directionToDx(dir);
        y = actor->getY() + directionToDy(dir);

        /* put it on the map */
        obj = ie->type->createInstance();
        assert(obj);
        obj->setCount(quantity);
        if (place_get_movement_cost(actor->getPlace(), x, y, 
                            obj, 0) < PTABLE_NO_DROP)
        {
	        obj->relocate(actor->getPlace(), x, y,
   	                   REL_NOSTEP, /* FIXME: really? */
      	                NULL);
	        actor->takeOut(ie->type, quantity);
	        actor->runHook(OBJ_HOOK_DROP_DONE, "pd", ie->type, quantity);
   	  }
   	  else if (place_get_movement_cost(actor->getPlace(), actor->getX(), actor->getY(), 
                            obj, 0) < PTABLE_IMPASSABLE)
        {
   	  	        obj->relocate(actor->getPlace(), actor->getX(), actor->getY(),
   	                   REL_NOSTEP, /* FIXME: really? */
      	                NULL);
	        actor->takeOut(ie->type, quantity);
	        actor->runHook(OBJ_HOOK_DROP_DONE, "pd", ie->type, quantity);
	        log_msg("%s wouldnt fit!", ie->type->getName());
		 }
		 else
		 {
			 log_msg("Couldnt drop %s!", ie->type->getName());
		 }
        /* remove from party inventory */
        actor->decActionPoints(kern_intvar_get("AP_COST:drop_item"));

        statusRepaint();
        mapUpdate(REPAINT_IF_DIRTY);
        return;
}

#ifdef USE_SKILLS

static const void *cmd_select_generic()
{
	struct KeyHandler kh;
	struct ScrollerContext sc;

        foogodSetHintText(SCROLLER_HINT);
        foogodSetMode(FOOGOD_HINT);        

	sc.selector = SelectSuperGeneric;
	sc.selection = NULL;
	kh.fx = scroller;
	kh.data = &sc;

	eventPushKeyHandler(&kh);
	cmdwin_push("<select>");
	eventHandle();
	cmdwin_pop();
	eventPopKeyHandler();

        foogodSetMode(FOOGOD_DEFAULT);
        return sc.selection;
}


/* Do common front-end processing. Migrate all commands to start using this. */
static class Character *cmd_front_end(class Character *pc, const char *cmdstr)
{
        cmdwin_clear();
        cmdwin_spush(cmdstr);

        /* prompt user? */
        if (!pc) {

                /* only one choice? */
                if (player_party->get_num_living_members() == 1) {
                        pc = player_party->get_first_living_member();
                        cmdwin_spush(pc->getName());
                } else {
                        pc = select_party_member();
                }

        } else {
                cmdwin_spush(pc->getName());
        }

        /* user abort? */
        if (!pc) {
                return 0;
        }

        /* dead actor? */
        if (pc->isDead()) {
                log_msg("%s - %s is too dead!", cmdstr, pc->getName());
                cmdwin_push("can't!");
                return 0;
        }

        /* sleeping actor? */
        if (pc->isAsleep()) {
                log_msg("%s - %s rolls over and snores!", cmdstr, 
                        pc->getName());
                cmdwin_push("can't!");
                return 0;
        }

        /* tell status who the actor is (sometimes it matters) */
        statusSelectCharacter(pc->getOrder());

        return pc;
}

static void cmd_add_skill_set(struct node *head, class Character *pc, 
                              struct skill_set *skset)
{
        struct list *elem;
        int pclvl = pc->getLevel();

        /* for each skill in the skill set */
        list_for_each(&skset->skills, elem) {

                struct skill_set_entry *ssent;
                struct node *node;
                ssent = list_entry(elem, struct skill_set_entry, list);

                /* is it a passive skill? */
                if (ssent->skill->passive) {
                        continue;
                }

                /* is the character is of sufficient level? */
                if (pclvl < ssent->level) {
                        continue;
                }

                /* add it to the list */
                node = node_new(ssent);
                node_add_tail(head, node);
        }
}

static void cmd_build_skill_list(struct node *head, class Character *pc)
{
        node_init(head);

        /* add species skills */
        if (pc->species
            && pc->species->skills) {
                cmd_add_skill_set(head, pc, pc->species->skills);
        }

        /* add occupation skills */
        if (pc->occ
            && pc->occ->skills) {
                cmd_add_skill_set(head, pc, pc->occ->skills);
        }

        /* add bonus skills? */
}

static int cmd_paint_skill(struct stat_super_generic_data *self, 
                            struct node *node, 
                            SDL_Rect *rect)
{
        struct skill_set_entry *ssent = (struct skill_set_entry *)node->ptr;
        struct skill *skill = ssent->skill;
        const char *requires = "Requires:";
        struct node *tnode;
        struct list *elem;
        SDL_Rect orect;
        int complete = 0;

        /* remember original rect */
        orect = *rect;

        /* name */
        if (rect->h < ASCII_H) {
                return -1;
        }
        screenPrint(rect, 0, "^c+m%s^c-", skill->name);

        /* level, ap, mp */
        screenPrint(rect, SP_RIGHTJUSTIFIED, 
                    "^c+GLvl:^c+y%d^c- MP:^c+b%d^c- AP:^c+r%d^c-^c-",
                    ssent->level, 
                    skill->mp, 
                    skill->ap);
        rect->y += ASCII_H;
        rect->h -= ASCII_H;

        /* check for required items */
        if (! node_list_empty(&skill->tools)
            || ! list_empty(&skill->materials)) {

                if (rect->h < ASCII_H) {
                        complete = -1;
                } else {

                        /* print "requires:" */
                        screenPrint(rect, 0, " ^c+G%s^c-", requires);
                        
                        /* temporarily change x to print to right of "requires" */
                        rect->x += (strlen(requires) + 1) * ASCII_W;
                        
                        /* list tools */
                        node_for_each(&skill->tools, tnode) {

                                if (rect->h < ASCII_H) {
                                        complete = -1;
                                        break;
                                }

                                class ObjectType *tool = (class ObjectType*)tnode->ptr;
                                struct inv_entry *ie=player_party->inventory->
                                        search(tool);
                                char tool_clr=(ie&&ie->count)?'g':'r';
                                screenPrint(rect, 0, "^c+%c%s^c-", tool_clr, 
                                            tool->getName());

                                rect->y += ASCII_H;
                                rect->h -= ASCII_H;
                        }

                        /* list materials */
                        list_for_each(&skill->materials, elem) {

                                if (rect->h < ASCII_H) {
                                        complete = -1;
                                        break;
                                }

                                struct skill_material *mat =
                                        list_entry(elem, struct skill_material, list);
                                class ObjectType *objtype = 
                                        (class ObjectType*)mat->objtype;
                                struct inv_entry *ie=player_party->inventory->
                                        search(objtype);
                                char mat_clr=ie?'g':'r';
                                char q_clr=(ie&&(ie->count>=mat->quantity))?'g':'r';
                                screenPrint(rect, 0, "^c+%c%s^c+%c (%d/%d)^c-^c-", 
                                            mat_clr,
                                            objtype->getName(), 
                                            q_clr,
                                            mat->quantity,
                                            ie?ie->count:0);

                                rect->y += ASCII_H;
                                rect->h -= ASCII_H;
                        }

                        /* restore rect x */
                        rect->x = orect.x;
                }
        }
        
        /* figure out how much area we used */
        orect.h = rect->y - orect.y;

        /* if this is not the currently selected item then shade it */
        if (self->selected != node) {
                screenShade(&orect, 128);
        }

        return complete;
}

static void cmd_skill_list_unref(struct stat_super_generic_data *self)
{
        struct node *node;

        /* Decrement the refcount. */
        assert(self->refcount > 0);
        self->refcount--;
        if (self->refcount > 0) {
                return;
        }

        /* Cleanup if no more refs. */
        node = node_next(&self->list);
        while (node != &self->list) {
                struct node *tmp = node;
                node = node_next(node);
                node_unref(tmp);
        }
}

static struct skill_set_entry *cmd_select_skill(class Character *pc)
{
        struct skill_set_entry *ssent;
        struct node *selected;
        struct stat_super_generic_data data;

        /* setup the status browser data */
        memset(&data, 0, sizeof(data));
        cmd_build_skill_list(&data.list, pc);
        data.title = "Yuse";
        data.paint = cmd_paint_skill;
        data.unref = cmd_skill_list_unref;

        /* put the status browser in selection mode */
        statusSetSuperGenericData(&data);
        statusPushMode(SuperGeneric);

        /* wait for user selection */
        selected = (struct node*)cmd_select_generic();

        /* extract result */
        if (selected) {
                ssent = (struct skill_set_entry *)selected->ptr;
                cmdwin_push(ssent->skill->name);
        } else {
                ssent = 0;
                cmdwin_push("none");
        }

        /* restore browser status mode */
        statusPopMode();

        assert(! data.refcount);

        return ssent;
}

void cmdYuse(class Character *actor)
{
        struct skill_set_entry *ssent;
        struct skill *skill;
        int cant = 0, result = 0, yused = 0;

        /* select/verify the actor */
        if (!(actor = cmd_front_end(actor, "Yuse"))) {
                return;
        }

        /* select the skill to yuse */
        if (!(ssent = cmd_select_skill(actor))) {
                return;
        }
        skill = ssent->skill;
        log_begin("%s: %s - ", actor->getName(), skill->name);

        /* check wilderness */
        if (! skill->wilderness_ok
            && place_is_wilderness(actor->getPlace())) {
                cant = 1;
                log_msg("Not in the wilderness!");
        }

        /* check level */
        if (actor->getLevel() < ssent->level) {
                cant = 1;
                log_msg("Must be level %d!", ssent->level);
        }

        /* check mana */
        if (actor->getMana() < skill->mp) {
                cant = 1;
                log_msg("Not enough mana!");
        }

        /* check tools */
        if (!node_list_empty(&skill->tools)) {
                struct node *tnode;
                node_for_each(&skill->tools, tnode) {
                        class ObjectType *tool = (class ObjectType*)tnode->ptr;
                        struct inv_entry *ie=player_party->inventory->
                                search(tool);
                        if (!ie || !ie->count) {
                                log_msg("Need %s!", tool->getName());
                                cant = 1;
                        }
                }

        }

        /* check material */
        if (! list_empty(&skill->materials)) {
                struct list *elem;
                struct skill_material *mat;
                class ObjectType *objtype;
                struct inv_entry *ie;
                list_for_each(&skill->materials, elem) {
                        mat = list_entry(elem, struct skill_material, list);
                        objtype = (class ObjectType*)mat->objtype;
                        ie = player_party->inventory->search(objtype);
                        if (!ie || ie->count < mat->quantity) {
                                cant = 1;
                                log_msg("Need %d %s!", mat->quantity, 
                                        objtype->getName());
                        }
                }
        }

        /* check special */
        if (skill->can_yuse
            && ! closure_exec(skill->can_yuse, "p", actor)) {
                cant = 1;
        }

        /* cant? */
        if (cant) {
                cmdwin_push("failed!");
                log_end("^c+rFailed!^c-");
                return;
        }

        /* yuse the skill */
        result = closure_exec(skill->yuse, "p", actor);
        yused = cmd_eval_and_log_result(result);
        
        /* change ap/mp/xp and consume materials */
        if (yused) {
                actor->runHook(OBJ_HOOK_YUSE_DONE, 0);
                actor->addMana(0 - skill->mp);
                actor->decActionPoints(skill->ap);
                actor->addExperience(ssent->level);

                if (! list_empty(&skill->materials)) {
                        struct list *elem;
                        struct skill_material *mat;
                        list_for_each(&skill->materials, elem) {
                                mat = list_entry(elem, struct skill_material, 
                                                 list);
                                player_party->takeOut((class ObjectType*)
                                                      mat->objtype, 
                                                      mat->quantity);
                        }
                }
        }       

        log_end(0);
}
#endif /* USE_SKILLS */

bool cmdSetSoloMode(int party_member_index)
{
    class Character *solo_member = player_party->getMemberAtIndex(party_member_index);
    if (solo_member
        && ! solo_member->isIncapacitated() 
        && solo_member->isOnMap()
        && solo_member->isPlayerControlled()
        ) {

        if (solo_member->engagedInTask()) {            
            log_msg("%s is engaged in %s, abort?", solo_member->getName(), solo_member->getTaskName());
            if (! ui_get_yes_no(solo_member->getName())) {
                return false;
            }
        }

        player_party->enableSoloMode(solo_member);
        return true;
    }
    return false;
}

bool cmdToggleFollowMode(void)
{
    log_begin("Follow mode ");
    if (player_party->getPartyControlMode() == PARTY_CONTROL_FOLLOW) {
        log_end("OFF");
        player_party->enableRoundRobinMode();
        return false;
    } else {
        log_end("ON");
        player_party->enableFollowMode();
        return true;
    }   
}

