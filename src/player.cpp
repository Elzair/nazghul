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
#include "player.h"
#include "object.h"
#include "status.h"
#include "console.h"
#include "place.h"
#include "NpcParty.h"
#include "portal.h"
#include "screen.h"
#include "sound.h"
#include "terrain.h"
#include "map.h"
#include "moongate.h"
#include "wq.h"
#include "foogod.h"
#include "combat.h"
#include "wind.h"
#include "Item.h"
#include "Mech.h"
#include "Mech.h"
#include "dup_constants.h"
#include "cmdwin.h"
#include "vehicle.h"
#include "sprite.h"
#include "play.h"		// for ui_get_direction()
#include "Field.h"

#include <unistd.h>
#include <math.h>

class player_party *player_party;

#define DIRLOC(dir,place,coord) { \
    if ((dir) < 0) \
        (coord) = place_w((place)) - 1; \
    else if (! (dir)) \
        (coord) = place_w((place)) / 2; \
    else \
        (coord) = 0; \
}

extern void play_update_place_viewer(void);	/* hack */

static bool player_apply_generic_effects(class Character * c, void *val)
{
        int effects = *(int*)val;

        if (effects & TERRAIN_BURN) {
                c->changeHp(-DAMAGE_FIRE);
                consolePrint("%s burning-%s!\n", c->getName(),
                             c->getWoundDescription());
                consoleRepaint();
        }
        if (effects & TERRAIN_POISON && !c->isPoisoned()) {
                c->setPoison(true);
                c->changeHp(-DAMAGE_POISON);
                consolePrint("%s poisoned-%s!\n", c->getName(),
                             c->getWoundDescription());
                consoleRepaint();
        }
        if (effects & EFFECT_SLEEP && !c->isAsleep()) {
                c->changeSleep(true);
                consolePrint("%s sleeping!\n", c->getName());
                consoleRepaint();
        }

        return false;
}

bool apply_damage(class Character * pm, void *amount)
{
	if (!pm->getHp())
		return false;

	pm->changeHp(-*((int *) amount));
	return false;
}

#if 0
static bool apply_fire_damage(class Character * pm, void *unused)
{
	int amount = DAMAGE_FIRE;
	apply_damage(pm, &amount);
	return false;
}
#endif

bool apply_poison(class Character * pm, void *unused)
{
	pm->setPoison(true);
	return false;
}

static bool apply_existing(class Character * pm, void *data)
{
        class player_party *party = (class player_party*)data;

	if (pm->isPoisoned()) {
		int amount = DAMAGE_POISON;
		apply_damage(pm, &amount);
	}
        if (pm->isAsleep()) {
                if (pm->getRestCredits())
                        pm->rest(1);

                // If not intentionally sleeping then roll to wakeup
                if (! party->resting()) {
                        consolePrint("%s sleeping...", pm->getName());
                        if ((random() % 100) < PROB_AWAKEN) {
                                pm->awaken();
                                consolePrint("awakes!");
                        }
                        consolePrint("\n");
                }
        }
	return false;
}

static bool pc_get_first_living(class Character * pm, void *data)
{
        class Character **pc = (class Character**)data;
	if (!pm->isDead()) {
                *pc = pm;
		return true;
	}
	return false;
}

static bool pc_check_if_alive(class Character * pm, void *data)
{
	int *count = (int *) data;
	if (!pm->isDead()) {
		(*count)++;
		return false;
	}
	return false;
}

static bool pc_check_if_not_immobilized(class Character * pm, void *data)
{
	int *count = (int *) data;
	if (!pm->isAsleep()) {
		(*count)++;
		return false;
	}
	return false;
}

static bool pc_eat_food(class Character * pm, void *data)
{
	if (player_party->food) {
		player_party->food--;
		return false;
	}

	consolePrint("Starving!");
	consoleNewline();
	consoleRepaint();
	int damage = DAMAGE_STARVATION;
	apply_damage(pm, &damage);
	return false;
}

static bool give_pc_rest_credit(class Character * pm, void *data)
{
	// Characters must be awake to earn rest credits:
	if (!pm->isAsleep())
		pm->addRestCredits(1);
	return false;
}

static void eat_food(struct wq_job *job, struct list *wq)
{
	player_party->for_each_member(pc_eat_food, 0);
	foogodRepaint();
	wqReschedule(wq, job);
}

static void give_rest_credit(struct wq_job *job, struct list *wq)
{
	player_party->for_each_member(give_pc_rest_credit, 0);
	wqReschedule(wq, job);
}

#if USE_OLD_MOONGATE_ENTRY
enum move_result player_party::enter_moongate(struct move_info *info)
{
	class Moongate *moongate;
	struct move_info dinfo;

	// Make sure we have a destination gate. Default to the source gate if
	// not.
	moongate = moongateGetDestinationGate();
	if (!moongate) {
		if (!info || !info->moongate)
			return move_no_moongate_destination;
		moongate = info->moongate;
	}
	// Check if the destination gate is blocked.
	memset(&dinfo, 0, sizeof(dinfo));
	dinfo.place = moongate->getPlace();
	dinfo.x = moongate->getX();
	dinfo.y = moongate->getY();
	if (check_move_to(&dinfo) != move_ok)
		return move_moongate_blocked;

	// Animate the player entering the source gate (if one was specified -
	// a gate spell won't specify one).
	if (info && info->moongate) {

		// Briefly move the player party over the open gate
		relocate(info->place, info->x, info->y);
		mapUpdate(0);
		usleep(MS_PER_TICK * 1000);

		// Take the player party off the scene.
		remove();

		// Now show the source gate closing.
		info->moongate->animateClosing();
		soundPlay(info->moongate->getEnterSound());
	} else {
		// Remove the player in case he is visible when the map is
		// centered on the destination gate.
		remove();
	}

	mapFlash(250);

	// Show the destination gate opening
	mapSetPlace(moongate->getPlace());
	mapCenterCamera(moongate->getX(), moongate->getY());
	mapCenterView(view, moongate->getX(), moongate->getY());
	mapRecomputeLos(view);
	mapUpdate(0);
	moongate->animateOpening();

	// Finish moving the player through the gate
	relocate(moongate->getPlace(), moongate->getX(), moongate->getY());

	return move_enter_moongate;
}

#else				/* ! USE_OLD_MOONGATE_ENTRY */

void player_party::enter_moongate(class Moongate * moongate)
{
	// Remove the player in case he is visible when the map is centered on
	// the destination gate (otherwise the player will see the party while
	// the destination gate opens).
	remove();

	// "Flash" the map to make the experience magical.
	mapFlash(250);

	// Show the destination gate opening
	mapSetPlace(moongate->getPlace());
	mapCenterCamera(moongate->getX(), moongate->getY());
	mapCenterView(view, moongate->getX(), moongate->getY());
	mapRecomputeLos(view);
	mapUpdate(0);
	moongate->animateOpening();

	// Move the party to the opened gate.
	relocate(moongate->getPlace(), moongate->getX(), moongate->getY());
}

#endif				/* ! USE_OLD_MOONGATE_ENTRY */

void player_party::relocate(struct place *place, int x, int y)
{
	class Mech *mech;
	int turns;

	// Play any associated movement sound.
	//soundPlay(get_movement_sound());

	// The one time we do not have a place is immediately after combat when
	// we are transitioning from combat mode to party mode. In this case we
	// don't count movement cost anyway, so I guess this isn't TOO bad of a
	// hack.
	if (getPlace()) {
		turns = get_speed() * place_get_movement_cost(place, x, y);
		if (turns > 4 * getPlace()->scale)
			cmdwin_print("-very slow!");
		else if (turns > 2 * getPlace()->scale)
			cmdwin_print("-slow!");
		turnAdvance(turns);
	}

	Object::relocate(place, x, y);

	if (place != Place) {
		placeExit();
		mapSetPlace(place);
		Place = place;
		placeEnter();
		consolePrint("Entering %s.\n", Place->name);
		context = (Place->type == wilderness_place ?
			   CONTEXT_WILDERNESS : CONTEXT_TOWN);
	}

	/* Check for a mech */
	mech = (class Mech *) place_get_object(place, x, y, mech_layer);
	if (mech)
		mech->activate(MECH_STEP);

	mapCenterView(view, x, y);
	recompute_los();
	mapCenterCamera(x, y);
        mapUpdate(0); // force a repaint
	//mapSetDirty();
}

enum move_result player_party::check_move_to(struct move_info *info)
{
	class Portal *new_portal;
	class NpcParty *npc_party;

	// null place?
	if (!info->place)
		return move_null_place;

	info->x = place_wrap_x(info->place, info->x);
	info->y = place_wrap_y(info->place, info->y);

	// off-map?
	if (place_off_map(info->place, info->x, info->y)) {
		return move_off_map;
	}
	// occupied (this handles occupied vehicles, too)
	if ((npc_party = place_get_NpcParty(info->place, info->x, info->y))) {
		if (npc_party->isHostile(alignment)) {
			if (info->place->type == wilderness_place ||
			    info->place->type == town_place) {
				info->npc_party = npc_party;
			}
			return move_enter_combat;
		}
		return move_occupied;
	}
	// another vehicle?
	if (vehicle && place_get_vehicle(info->place, info->x, info->y))
		return move_occupied;

        // I give automatic portals higher priority than passability. This is
        // mainly to allow ships to have access to towns which are placed on
        // land but which may have a port. Passability should be determined by
        // checking the town map to see if it allows entrance, not by checking
        // the terrain the tile is on.
        //
        // Note that this - as written now - will allow the party to enter
        // portals with impassable destinations. That's fine, see
        // doc/GAME_RULES Design Discussion #3: sane portal linkages are the
        // responsibility of game developers.
	if (!info->portal &&
	    (new_portal = place_get_portal(info->place, info->x, info->y))) {
                if (new_portal->isAutomatic()) {
                        info->portal = new_portal;
                        return move_enter_auto_portal;
                }
                return move_ok;
	}

	// The same rules as above apply to moongates.
	info->moongate = place_get_moongate(info->place, info->x, info->y);
	if (info->moongate && info->moongate->isOpen())
		return move_enter_moongate;

	// passable? I moved this check to be before a test for portals. A
	// portal is accessible iff the player can move across the terrain it
	// is placed on. The "ladder in the lake to the ladder on land" test
	// case just plain looks counterintuitive without it.
	if (!place_is_passable(info->place, info->x, info->y, get_pmask(), 0))
		return move_impassable;

	return move_ok;
}

bool player_party::turn_vehicle(void)
{
	int cost = 0;

	// Three possible outcomes:
	// 
	// 1. We do NOT turn any vehicle, therefore we want to continue
	// processing the move() command which calls us and we do NOT want to
	// consume any turns (e.g., no vehicle, or horse turning north/south).
	// 
	// 2. We DO turn a vehicle, but it does not cost us anything (e.g.,
	// horse turning east/west).
	// 
	// 3. We do turn a vehicle and it cost us a turn (e.g., ship).

	if (!vehicle || !vehicle->turn(dx, dy, &cost) || !vehicle->mustTurn())
		return false;

	cmdwin_print("Turn %s %s", vehicle->getName(), get_dir_str(dx, dy));
	cost *= getPlace()->scale;
	printf("*** player used %d turns\n", cost);
	turnAdvance(cost);

	return true;
}

bool player_party::try_to_enter_moongate(class Moongate * src_gate)
{
	class Moongate *dest_gate;

	// Get the current destination moongate.
	dest_gate = moongateGetDestinationGate();
	if (!dest_gate) {

		if (src_gate) {
			// Currently there is no destination gate so default to
			// the source gate.
			dest_gate = src_gate;
		} else {
			// No src gate means we probably got in here via a
			// spell or other effect.
			return false;
		}

	} else {

		// Check if the destination is blocked, impassable, etc.
		struct move_info info;
		memset(&info, 0, sizeof(info));
		info.place = dest_gate->getPlace();
		info.x = dest_gate->getX();
		info.y = dest_gate->getY();

#if CHECK_MOONGATE_DESTINATION
                // See Design Discussion #3 in GAME_RULES. Sane portal linkages
                // (including moongates) are the responsibility of the game
                // developer, not the engine.
                //
                // One exception _might_ be to check for an npc party on the
                // other side. But the engine can handle stacking npc parties,
                // so it might be simplest just to allow it.
		switch (check_move_to(&info)) {

		case move_ok:	// great
		case move_enter_auto_portal:	// ok, but second portal not
			// invoked
		case move_enter_moongate:	// can happen if dst_gate ==
			// src_gate
			break;

		case move_occupied:	// friendly npc party sitting on
			// destination
		case move_enter_combat:	// hostile npc party on
			// destination; disallow
			cmdwin_print("-destination blocked!");
			return false;

		case move_impassable:	// could allow if I were feeling mean
			cmdwin_print("-destination impassable!");
			return false;

		default:	// no other cases expected
			assert(false);
		}
#endif
	}

	cmdwin_print("-ok");

	// Briefly move the player party over the open source gate if one is
	// specified (for spells it won't be)
        if (src_gate) {
                relocate(src_gate->getPlace(), src_gate->getX(), 
                         src_gate->getY());
                mapUpdate(0);
                usleep(MS_PER_TICK * 1000);
        }

	// Take the player party off the scene.
	remove();

	// Now show the source gate closing.
        if (src_gate)
                src_gate->animateClosing();

	soundPlay(dest_gate->getEnterSound(), SOUND_MAX_VOLUME);

	// Pass through to the destination gate.
	enter_moongate(dest_gate);
	return true;

}

void player_party::move_to_wilderness_combat(struct combat_info *cinfo)
{
	// For wilderness combat the player party always exits back to the
	// place it was in prior to entering combat.

	struct place *saved_place = getPlace();
	int saved_x = getX();
	int saved_y = getY();

	combat_enter(cinfo);
	relocate(saved_place, saved_x, saved_y);
}

void player_party::move_to_combat(struct combat_info *cinfo)
{
      repeat:
	// For towns and wilderness places combat occurs in a temporary combat
	// map.
	if (cinfo->move->place->type == wilderness_place ||
	    cinfo->move->place->type == town_place) {
		move_to_wilderness_combat(cinfo);
		return;
	}
	// Before entering combat move the party to the tile where it
	// will occur. This is important! If the player quits during
	// combat the global Place must be the same as the place the
	// party is in. (fixme: global search-and-replace Place with
	// player_party->getPlace()?)
	// 
	// Update: not sure this is true anymore. And it causes problems
	// because the combat location may be impassable, and upon exit the
	// party is supposed to revert to its original tile, not the combat
	// tile.
	// 
	// relocate(cinfo->move->place, cinfo->move->x, cinfo->move->y);

	// Begin combat. This function will not return until combat is
	// complete. The party may flee or exit to a different place
	// during combat, which makes this tricky. Combat will modify
	// the info and set the new place and coordinates before exit,
	// so we need to run through the loop again.
	if (!combat_enter(cinfo))
		return;

	if (Quit)
		return;

	// Combat will save the party's exit location in the info struct prior
	// to returning.
	switch (check_move_to(cinfo->move)) {

	case move_enter_combat:
		// The party has left one combat only to enter another. This
		// will happen a lot in dungeons where most rooms contain
		// hostiles.
		goto repeat;

	case move_ok:		// Great
	case move_enter_auto_portal:	// Ok, ignore the portal
	case move_enter_moongate:	// Ok, ignore the moongate
		// I don't think I'll apply a movement cost in this case, just
		// move the party.
		relocate(cinfo->move->place, cinfo->move->x, cinfo->move->y);
		return;

	case move_player_quit:	// Ok, skip any more processing
		return;

	default:
		// Combat is supposed to make sure this doesn't happen.
		assert(false);
		break;
	}

	return;
}

bool player_party::try_to_enter_town_from_edge(class Portal * portal, int dx,
					       int dy)
{
	struct move_info info;
	int i, max_i, dx2, dy2, dir;
	bool blocked = false;

	memset(&info, 0, sizeof(info));

	info.place = portal->getToPlace();

	// Towns cannot be wilderness types.
	assert(info.place->type != wilderness_place);

	cmdwin_print("enter %s", info.place->name);

	if (dx == 0 && dy == 0) {
		cmdwin_print("-");
		dir = ui_get_direction();
	} else {
		dir = vector_to_dir(dx, dy);
	}

	// Default to the center of the edge.
	switch (dir) {
	case CANCEL:
		return false;
	case NORTHWEST:
	case NORTHEAST:
	case SOUTHWEST:
	case SOUTHEAST:
	case HERE:
		cmdwin_print("-direction not allowed!");
		return false;
	case SOUTH:
		info.x = place_w(info.place) / 2;
		info.y = 0;
		max_i = place_w(info.place);
		dy2 = 0;
		dx2 = 1;
		break;
	case NORTH:
		info.x = place_w(info.place) / 2;
		info.y = place_h(info.place) - 1;
		max_i = place_w(info.place);
		dy2 = 0;
		dx2 = 1;
		break;
	case WEST:
		info.x = info.x = place_w(info.place) - 1;
		info.y = place_h(info.place) / 2;
		max_i = place_h(info.place);
		dy2 = 1;
		dx2 = 0;
		break;
	case EAST:
		info.x = 0;
		info.y = place_h(info.place) / 2;
		max_i = place_h(info.place);
		dy2 = 1;
		dx2 = 0;
		break;
	default:
		assert(false);
	}

	info.dx = directionToDx(dir);
	info.dy = directionToDy(dir);

	cmdwin_print("-%s", portal->getName());

	for (i = 0; i < max_i; i++) {

		printf("Checking [%d %d]\n", info.x, info.y);

	      retry:

		switch (check_move_to(&info)) {
		case move_ok:
		case move_enter_auto_portal:
		case move_enter_moongate:
			cmdwin_print("-ok");
			relocate(info.place, info.x, info.y);
			return true;

		case move_occupied:
			blocked = true;
			break;

		case move_impassable:
			break;

		case move_enter_combat:
			// This is the case when the player enters the town and
			// immediately enters combat because a hostile npc is
			// standing on the entrance. After combat is over I
			// want the player to be in the town on that edge. To
			// make that happen we retry the same location after
			// combat returns. (Note: if the player dies in combat
			// then it's okay - the last thing the player will see
			// is the party standing in town on the spot where it
			// perished, which is actually not a bad effect).
			cmdwin_print("-combat!");
			struct combat_info cinfo;
			memset(&cinfo, 0, sizeof(cinfo));
			cinfo.move = &info;
			move_to_combat(&cinfo);
			goto retry;

		case move_player_quit:
		case move_off_map:
		case move_null_place:
		default:
			assert(false);
			return false;
		}

		info.x += i * dx2;
		info.y += i * dy2;

		dx2 *= -1;
		dy2 *= -1;
	}

	if (blocked)
		cmdwin_print("-entrance blocked!");
	else
		cmdwin_print("-entrance impassable!");
	return false;
}

bool player_party::enter_dungeon(struct place *dungeon, int dungeon_x, 
                                 int dungeon_y, int dx, int dy)
{
        struct combat_info cinfo;
        struct move_info move;
        bool first_time = true;

        // On initial entry we still don't know if we can enter combat or not,
        // so the following might fail.

        memset(&move, 0, sizeof(move));
        move.place = dungeon;
        move.x = dungeon_x;
        move.y = dungeon_y;
        move.dx = dx;
        move.dy = dy;

        memset(&cinfo, 0, sizeof(cinfo));
        cinfo.move = &move;

 retry:
        consolePrint("\n*** D U N G E O N ***\n\n");
        consolePrint("You enter %s...\n\n", move.place->name);
        if (!combat_enter(&cinfo)) {
                assert(first_time);
                return false;
        }
        
        // There are three ways to exit a dungeon: quit, die or teleport. In
        // the case of teleporting the party might end up in another
        // dungeon. Or the same dungeon, if the portal just leads to elsewhere
        // in the same one - combat doesn't deal with that special case so do
        // it here.

        if (Quit)
                return true;

        if (all_dead())
                return true;

        if (place_is_dungeon(move.place)) {
                first_time = false;
                goto retry;
        }
        
        // Otherwise the party is back in party mode on a town or wilderness
        // map, just like entering a portal.

	mapSetPlace(move.place);
	mapCenterCamera(move.x, move.y);
	mapCenterView(view, move.x, move.y);
	mapRecomputeLos(view);
	mapUpdate(0);
	usleep(MS_PER_TICK * 2000);

        relocate(move.place, move.x, move.y);

        return true;
}

bool player_party::try_to_enter_portal(class Portal * portal, int dx, int dy)
{

	cmdwin_print("-%s-ok", portal->getName());

        // If the destination is a dungeon then enter the dungeon loop.
        if (place_is_dungeon(portal->getToPlace()))
                return enter_dungeon(portal->getToPlace(), portal->getToX(),
                                     portal->getToY(), dx, dy);

	// Show the destination so the player knows what he's getting himself
	// into...
	mapSetPlace(portal->getToPlace());
	mapCenterCamera(portal->getToX(), portal->getToY());
	mapCenterView(view, portal->getToX(), portal->getToY());
	mapRecomputeLos(view);
	mapUpdate(0);
	usleep(MS_PER_TICK * 2000);

        relocate(portal->getToPlace(), portal->getToX(), portal->getToY());

        return true;
}

bool player_party::try_to_move_off_map(struct move_info * info)
{
         
        // Yes, I'm making the following unconditional with no further checks.
        // The parent tile is _always_ and _unconditionally_ passable.
        // Consider everything that could make that tile impassable:
        //   --Terrain on parent map... don't care
        //   --Autoportals leading elsewhere... ignore them
        //   --Npc's... they aren't allowed on town tiles in the wilderness,
        //     and if one sneaks through ignore it
        //   --Fields... unlikely, and don't care
        //   --Vehicles (when player is already in one)... ignore them (and
        //     don't allow player to abandon a vehicle over a town, otherwise
        //     we can leak vehicles since consecutive abandonments will 
        //     clobber previous ones)
        // See notes on the ship problem in discussion #1 of doc/GAME_RULES

        if (! info->place->location.place) {
		cmdwin_print("-no place to go!");
                return false;
        }

        cmdwin_print("-ok");
        relocate(info->place->location.place, 
                 info->place->location.x, 
                 info->place->location.y);

        return true;
}

bool player_party::move(int newdx, int newdy, bool teleport)
{
	struct move_info info;
	struct combat_info cinfo;

	mapSetDirty();
	cmdwin_clear();

	// Cache the requested direction. Can't remember what we use this for
	// or if it's still necessary.
	dx = newdx;
	dy = newdy;

	// Change vehicle facing. This might consume a turn in which case
	// that's all we'll do.
	if (!teleport && turn_vehicle())
		return true;

	cmdwin_print("%s %s", teleport ? "teleport" :
		     get_movement_description(),
		     directionToString(vector_to_dir(dx, dy)));

	// Check the consequences of moving to the target tile.
	memset(&info, 0, sizeof(info));
	info.place = getPlace();
	info.x = x + dx;
	info.y = y + dy;
	info.dx = dx;
	info.dy = dy;
	switch (check_move_to(&info)) {

	case move_ok:
		// No complications. Update the turn counter based on player
		// speed and terrain difficulties then move the player.
		cmdwin_print("-ok");
		relocate(info.place, info.x, info.y);
		return true;

	case move_off_map:
		cmdwin_print("-exit place");
		return try_to_move_off_map(&info);
		return false;

	case move_occupied:
		// Occupied by a friendly npc party. If they were unfriendly
		// we'd be going into combat.
		cmdwin_print("-occupied!");
		return false;

	case move_impassable:
		// Impassable terrain.
		cmdwin_print("-impassable!");
		return false;

	case move_enter_moongate:
		// An open moongate is on that tile.
		cmdwin_print("-enter moongate");
		if (!try_to_enter_moongate(info.moongate))
			return false;
		return true;

	case move_enter_combat:
		// Handle combat (possible multiple combats) until we're all
		// done with them.
		cmdwin_print("-attack!");
		memset(&cinfo, 0, sizeof(cinfo));
		cinfo.move = &info;
		move_to_combat(&cinfo);
		return true;

	case move_enter_auto_portal:
		// A teleporter is on that tile.
		if (info.portal->edge_entrance) {
			return try_to_enter_town_from_edge(info.portal,
							   info.dx, info.dy);
		}
		return try_to_enter_portal(info.portal, info.dx, info.dy);

	default:
		// no other results expected
		assert(false);
		return false;
	}
}

static bool union_pmask(class Character * pm, void *data)
{
        unsigned char *pmask = (unsigned char*)data;
        *pmask &= pm->getPmask();
        return false;
}

unsigned char player_party::get_pmask(void)
{
        unsigned char pmask = 0xff;

	if (vehicle)
		return vehicle->getPmask();

        for_each_member(union_pmask, &pmask);
	return pmask;
}

struct sprite *player_party::getSprite(void)
{
	if (vehicle)
		return vehicle->getSprite();

	return sprite;
}

int player_party::get_speed(void)
{
	if (vehicle) {
		return vehicle->getSpeed();
	}
	return speed;
}

char *player_party::get_movement_description(void)
{
	if (!vehicle)
		return mv_desc;
	return vehicle->getMvDesc();
}

char *player_party::get_movement_sound(void)
{
	if (vehicle) {
		return vehicle->getMvSound();
	}
	return mv_sound;
}

static struct inv_entry *myCreateIE(void)
{
	struct inv_entry *ie;
	CREATE(ie, struct inv_entry, 0);
	list_init(&ie->list);
	return ie;
}

static struct inv_entry *myLookupIE(struct list *head,
				    class ObjectType * object)
{
	struct list *list;
	struct inv_entry *ie;

	list_for_each(head, list) {
		ie = outcast(list, struct inv_entry, list);
		if (object == ie->type)
			return ie;
	}
	return 0;
}

void player_party::add_to_inventory(class ObjectType * type, int quantity)
{
	struct inv_entry *ie;

	if (!quantity)
		return;

	if (type->getType() == ITEM_TYPE_ID) {
		// Put food straight into the food counter
		class ItemType *item = (class ItemType *) type;
		if (item->isFood()) {
			food += quantity * item->getAmount();
			foogodRepaint();
			return;
		}
	}

	ie = myLookupIE(&inventory, type);
	if (!ie) {
		ie = myCreateIE();
		if (!ie) {
			err("Allocation failed");
			return;
		}
		list_add(&inventory, &ie->list);

		switch (type->getType()) {
		case ARMS_TYPE_ID:
			nArms++;
			break;
		case REAGENT_TYPE_ID:
			nReagents++;
			break;
		case ITEM_TYPE_ID:
			nItems++;
			break;
		case AMMO_TYPE_ID:
			nAmmo++;
			break;
		case SPELL_TYPE_ID:
			nSpells++;
			break;
		default:
			break;
		}
	}

	ie->count += quantity;
	ie->type = type;
}

void player_party::remove_from_inventory(struct inv_entry *ie, int q)
{
	/* Note: for arms we should check the ref count and take corrective
	 * action if not zero */
	q = min(q, ie->count);
	ie->count -= q;
	if (!ie->count) {
		list_remove(&ie->list);
		assert(!ie->ref);
		free(ie);
	}
}

void player_party::enter_portal(void)
{
	class Portal *portal;

	cmdwin_clear();
	cmdwin_print("Enter-");

	portal = place_get_portal(Place, x, y);
	if (!portal) {
		cmdwin_print("nothing!");
		return;
	}

	if (portal->edge_entrance)
		try_to_enter_town_from_edge(portal, 0, 0);
	else
		try_to_enter_portal(portal, 0, 0);

#ifdef OLD_ENTER_PORTAL
	cmdwin_print("%s", portal->getName());

	struct move_info mv_info;
	memset(&mv_info, 0, sizeof(mv_info));
	mv_info.place = portal->getToPlace();
	mv_info.x = portal->getToX();
	mv_info.y = portal->getToY();
	mv_info.turns = 1;
	mv_info.dx = dx;
	mv_info.dy = dy;

	switch (check_move_to(&mv_info)) {
	case move_ok:
		cmdwin_print("-ok");
		printf("*** player used %d turns\n", mv_info.turns);
		turnAdvance(mv_info.turns);
		break;
	case move_null_place:
		cmdwin_print("-no place to go!");
		break;
	case move_occupied:
		cmdwin_print("-occupied!");
		break;
	case move_impassable:
		cmdwin_print("-impassable!");
		break;
	case move_enter_moongate:
		cmdwin_print("-enter moongate");
		printf("*** player used %d turns\n", mv_info.turns);
		turnAdvance(mv_info.turns);
		break;
	case move_moongate_blocked:
		cmdwin_print("-enter moongate failed!");
		break;
	case move_player_quit:
		break;
	default:
		// no other results expected
		assert(false);
	}
#endif				/* OLD_ENTER_PORTAL */
}

void player_party::for_each_member(bool(*fx) (class Character *, void *data),
				   void *data)
{
	int i;
	for (i = 0; i < n_pc; i++) {
		if (fx(pc[i], data))
			return;
	}
}

static bool player_wakeup_member(class Character *member, void *data)
{
        member->awaken();
        return false;
}

void player_party::advance_turns(void)
{
	/* Apply terrain and field affects */
	struct terrain *terrain;
        class Field *field;
        int effects = 0;

	terrain = placeGetTerrain(x, y);
        effects |= terrain->effects;

        // Get any field effects. Note that only the topmost field at this
        // tile (if any) is used.
        field = (class Field *) place_get_object(getPlace(), getX(),
                                                 getY(), field_layer);
        if (field != NULL)
                effects |= field->getObjectType()->getEffects();

	if (effects) {
                
                for_each_member(player_apply_generic_effects, &effects);

		//statusRepaint();
		//consoleNewline();
		//consoleRepaint();
	}

	/* Apply existing effects */
	for_each_member(apply_existing, this);

        if (resting()) {
                hours_to_rest--;
                assert(hours_to_rest >= 0);
                if (!resting()) {
                        for_each_member(player_wakeup_member, this);
                        cmdwin_print("rested!");
                        statusRepaint();
                }
        }
}

bool player_party::all_dead(void)
{
	int count = 0;
	for_each_member(pc_check_if_alive, &count);
	return (count == 0);
}

bool player_party::immobilized(void)
{
	int count = 0;
	for_each_member(pc_check_if_not_immobilized, &count);
	return (count == 0);        
}

struct VradInfo {
	int vrad;
	int light;
};

bool myGetBestVisionRadius(class Character * c, void *data)
{
	struct VradInfo *info = (struct VradInfo *) data;
	info->vrad = max(info->vrad, c->getVisionRadius());
	info->light = max(info->light, c->getLight());
	return false;
}

void player_party::recompute_los(void)
{
	struct VradInfo info;

	// Scan all party members to find the best light source and the best
	// eyes. They needn't be from the same member since I assume they are
	// traveling close enough to share their light sources with each other.
	info.vrad = 0;
	info.light = 0;
	for_each_member(myGetBestVisionRadius, &info);

	// hack -- should replace this with a routine for getting the ambient
	// light from the local environment (place?)
	if (!Place->underground)
		info.light += sky_get_ambient_light();

	// Set the vision radius to the minimum of the best available light
	// radius or the best available vision radius of all party members
	light = max(info.light, MIN_PLAYER_LIGHT);
	mapSetRadius(view, min(info.vrad, MAX_VISION_RADIUS));
	mapRecomputeLos(view);
	mapSetDirty();
}

struct inv_entry *player_party::search_inventory(class ObjectType * type)
{
	return myLookupIE(&inventory, type);
}

player_party::player_party()
{
	dx = 0;
	dy = -1;
	sprite = 0;
	speed = 0;
	//pmask = 0; obsolete
	vehicle = 0;
	turns = 0;
	mv_desc = 0;
	mv_sound = 0;
	n_pc = 0;
	memset(pc, 0, sizeof(pc));
	leader = 0;
	list_init(&inventory);
	nArms = 0;
	nReagents = 0;
	nSpells = 0;
	nItems = 0;
	nAmmo = 0;
	light = 1;
	food = 0;
	view = 0;
	onMap = true;
	alignment = 0;
	gold = 0;
	context = 0;
	container_link.key = being_layer;
	formation = 0;
	campsite_map = 0;
	campsite_formation = 0;
	camping = false;
        hours_to_rest = 0;
}

player_party::~player_party()
{
}

int player_init(void)
{
	if (!(player_party = new class player_party())) {
		err("Failed to allocate player_party");
		return -1;
	}

	/* Eat 3 meals a day */
	wqCreateJob(&TurnWorkQueue, Turn + TURNS_PER_FOOD, TURNS_PER_FOOD,
		    0, eat_food);

	/* Sleep up to 9 hours a day */
	wqCreateJob(&TurnWorkQueue, Turn + TURNS_PER_REST_CREDIT,
		    TURNS_PER_REST_CREDIT, 0, give_rest_credit);

	return 0;
}

void player_party::board_vehicle(void)
{
	cmdwin_clear();

	// already in a vehicle so exit
	if (vehicle) {
		vehicle->occupant = 0;
		vehicle->relocate(getPlace(), getX(), getY());
		cmdwin_print("Exit-%s", vehicle->getName());
		vehicle = 0;
		mapSetDirty();
		printf("*** player used 1 turns\n");
		turnAdvance(1);
		return;
	}

	vehicle = place_get_vehicle(Place, x, y);
	if (!vehicle) {
		cmdwin_print("Board-Nothing!");
		return;
	}

	vehicle->occupant = this;
	vehicle->remove();
	// place_remove_vehicle(Place, vehicle);
	cmdwin_print("Board-%s", vehicle->getName());
	mapSetDirty();
	printf("*** player used 1 turns\n");
	turnAdvance(1);
}

static bool check_if_leader(class Character * pc, void *data)
{
        // Going to add a check for sleep here to handle the case where the
        // party is in follow mode and the leader is put asleep for some
        // reason. Otherwise the engine cranks out turns until the leader wakes
        // up.
	if (!pc->isDead() && pc->isOnMap() && !pc->isAsleep()) {
		player_party->leader = pc;
		return true;
	}
	return false;
}

class Character *player_party::get_leader(void)
{
#ifdef CACHE_LEADER // false
        // Going to add a check for sleep here to handle the case where the
        // party is in follow mode and the leader is put asleep for some
        // reason. Otherwise the engine cranks out turns until the leader wakes
        // up.
	if (!leader || leader->isDead() || !leader->isOnMap() 
            || leader->isAsleep()) {
		leader = NULL;
		for_each_member(check_if_leader, 0);
	}
#else
        // Force a reevaluation every time. For example, say Thorald is the
        // leader. Then he goes to sleep. Now Kama should be the leader. But as
        // soon as Thorald wakes up I want leadership to revert back to him.
        leader = NULL;
        for_each_member(check_if_leader, 0);
#endif
	return leader;
}

bool player_party::add_to_party(class Character * c)
{
	int i = 0;

	// Note: this is called so early in startup that I can't touch the
	// paint routines in here. Callers will have to update the map and
	// status if necessary.

	assert(!c->isPlayerControlled());

        // Check if passability is compatible with rest of party
        if (n_pc && ! (c->getPmask() & get_pmask())) {
                return false;
        }

	for (i = 0; i < MAX_N_PC && pc[i]; i++) ;

	if (i == MAX_N_PC)
		return false;

	pc[i] = c;
	n_pc++;
	c->setOrder(i);
	c->setPlayerControlled(true);
	c->setAlignment(c->getAlignment() | alignment);

        // gmcnutt: added this as a hack to support quickly determining if a
        // character belongs to the player party.
        c->party = (NpcParty*)this;

	// Loop over all readied weapons and add them to player inventory. Also
	// must set the refcount once they are in inventory.

	for (class ArmsType * weapon = c->enumerateArms();
	     weapon != NULL; weapon = c->getNextArms()) {

		struct inv_entry *ie;

		add_to_inventory(weapon, 1);
		ie = search_inventory(weapon);
		assert(ie);
		ie->ref++;

	}

	// Note: leave the readied arms as-is and character inventory as-is for
	// now.

	return true;
}

int player_party::get_room_in_party(void)
{
	int i, n = 0;

	// um... this assumes that if I remove a member I shift everybody down
	// to fill the gap. Which I probably will, come to think of it.

	for (i = 0; i < MAX_N_PC; i++) {
		if (!pc[i])
			n++;
	}
	return n;
}

void player_party::paint(int sx, int sy)
{
	if (vehicle)
		vehicle->paint(sx, sy);
	else
		spritePaint(getSprite(), 0, sx, sy);
}

char *player_party::getName()
{
	return "player party";
}

bool player_party::isVisible()
{
	return true;
}

void player_party::describe(int count)
{
	consolePrint("the %s", getName());
}

static bool kill_member(class Character * member, void *data)
{
	member->kill();
	return false;
}

void player_party::hit_by_ordnance(class OrdnanceType * ordnance)
{
	int amount;

	consolePrint("Hit by %s!\n", ordnance->getName());

	// First apply damage to the vehicle. If the vehicle is destroyed then
	// destroy the party, too.
	if (vehicle) {
		vehicle->damage(ordnance->get_damage());
		mapFlash(50);
		foogodRepaint();
		if (vehicle->isDestroyed()) {
			delete vehicle;
			vehicle = NULL;

			// kill everyone so that all_dead() returns true,
			// signalling end-of-game.
			for_each_member(kill_member, NULL);
		}
		return;
	}
	// Apply damage to all party members. If they all die then the party is
	// destroyed, too.
	amount = ordnance->get_damage();
	for_each_member(apply_damage, &amount);
}

struct formation *player_party::get_formation()
{
	if (vehicle && vehicle->get_formation())
		return vehicle->get_formation();
	if (camping && campsite_formation)
		return campsite_formation;
	return formation;
}

int player_party::get_num_living_members(void)
{
	int count = 0;
	for_each_member(pc_check_if_alive, &count);
        return count;
}

class Character *player_party::get_first_living_member(void)
{
        class Character *pc = NULL;
        for_each_member(pc_get_first_living, &pc);
        return pc;
}

static bool member_begin_resting(class Character *pc, void *data)
{
        pc->changeSleep(true);
        return false;
}

void player_party::begin_resting(int hours)
{
        assert(hours > 0);
        for_each_member(member_begin_resting, NULL);
        statusRepaint();
        hours_to_rest = hours;
}

bool player_party::resting()
{
        return (hours_to_rest > 0);
}
