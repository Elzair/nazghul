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
#include "cmd.h"             // for ui_get_direction()
#include "Field.h"
#include "event.h"
#include "formation.h"

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

#define FOR_EACH_MEMBER(i, m) for (i = 0, (m) = pc[i]; i < n_pc; (m) = pc[++i])

extern void play_update_place_viewer(void);	/* hack */


static bool apply_damage(class Character * pm, void *amount)
{
        pm->damage(*((int *)amount));
	return false;
}
static bool kill_member(class Character * member, void *data)
{
	member->kill();
	return false;
}


void player_party::damage(int amount) {

	/* First apply damage to the vehicle. If the vehicle is destroyed then
           destroy the party, too. */
	if (vehicle) {
		vehicle->damage(amount);
		mapFlash(0);
		foogodRepaint();
		if (vehicle->isDestroyed()) {
			delete vehicle;
			vehicle = NULL;
			for_each_member(kill_member, NULL);
		}
		return;
	}

	/* Apply damage to all party members. If they all die then the party is
           destroyed, too. */
        for_each_member(apply_damage, &amount);
}

static bool apply_poison(class Character * pm, void *amount)
{
        pm->poison();
	return false;
}

void player_party::poison() {
        for_each_member(apply_poison, NULL);
}

static bool apply_existing(class Character * pm, void *data)
{
        pm->applyExistingEffects();
	return false;
}

void player_party::applyExistingEffects()
{
        for_each_member(apply_existing, this);
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

void player_party::enter_moongate(class Moongate * moongate)
{
        // ---------------------------------------------------------------------
	// Remove the player in case he is visible when the map is centered on
	// the destination gate (otherwise the player will see the party while
	// the destination gate opens).
        //
        // Addendum: another more important reason for doing this is that it
        // makes sure that only the moongate's view is functional during the
        // interim when the party is "traveling" between places.
        // ---------------------------------------------------------------------
	remove();
        removeMembers();

	// "Flash" the map to make the experience magical.
	mapFlash(250);

	// Show the destination gate opening
	moongate->animateOpening();

	// Move the party to the opened gate.
	relocate(moongate->getPlace(), moongate->getX(), moongate->getY());
}

void player_party::changePlaceHook()
{
        mapSetPlace(place);
        Place = place;
        place_enter(place);
        consolePrint("Entering %s.\n", Place->name);
        
        // ---------------------------------------------------------------------
        // If the party is relocating to a non-wilderness place then I have to
        // break it out.
        // ---------------------------------------------------------------------
        
        if (! place_is_wilderness(place)) {
                distributeMembers(place, x, y, dx, dy);
                return;
        }
}

void player_party::relocate(struct place *place, int x, int y)
{
	class Mech *mech;

	// Play any associated movement sound.
	//soundPlay(get_movement_sound());

        //attachCamera(true);
	Object::relocate(place, x, y);

	/* Check for a mech */
	mech = (class Mech *) place_get_object(place, x, y, mech_layer);
	if (mech)
		mech->activate(MECH_STEP);

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
	if (!place_is_passable(info->place, info->x, info->y, getPmask(), 0))
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

	}

	cmdwin_print("-ok");

	// Briefly move the player party over the open source gate if one is
	// specified (for spells it won't be)
        if (src_gate) {
                relocate(src_gate->getPlace(), src_gate->getX(), 
                         src_gate->getY());
                mapSetDirty();
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

        combat_enter(cinfo);

#if 0
	struct place *saved_place = getPlace();
	int saved_x = getX();
	int saved_y = getY();

	combat_enter(cinfo);
	relocate(saved_place, saved_x, saved_y);
#endif
}

void player_party::distributeMembers(struct place *new_place, int new_x, int new_y, int new_dx, int new_dy)
{
        // ---------------------------------------------------------------------
        // Remove the player party from the current place and remove its view
        // from the map viewer's consideration (instead we'll use the character
        // views, which are added as each character is placed on the map
        // below).
        // ---------------------------------------------------------------------

        remove();
#if 0
        mapRmView(view);
#endif
        // ---------------------------------------------------------------------
        // Switch the current place to the portal destination place.
        // ---------------------------------------------------------------------

        Place = new_place;

        // ---------------------------------------------------------------------
        // Set the new destination place to be the subject of the map
        // viewer. Center the camera on the portal destination.
        // ---------------------------------------------------------------------

        mapSetPlace(new_place);
        mapCenterCamera(new_x, new_y);
        mapSetDirty();

        // ---------------------------------------------------------------------
        // Distribute all the party members centered on the portal destination
        // point. I assume this always succeeds, which means I'm relying on map
        // editors to be nice and make nice, clear areas around their portal
        // entrances.
        //
        // For now, I'll reuse the existing combat placement algorithm. Compare
        // the code below with position_player_party() in combat.c:
        // ---------------------------------------------------------------------

#if 0
        // ---------------------------------------------------------------------
        // The combat alg requires me to fill out a "position info" structure
        // based on the player party destination.
        // ---------------------------------------------------------------------
        
        combat_fill_position_info(&pinfo, new_place, new_x, new_y, new_dx, new_dy, false);

        // ---------------------------------------------------------------------
        // Set the party formation to a sane default.
        // ---------------------------------------------------------------------

        if (NULL == pinfo.formation)
                pinfo.formation = formation_get_default();

        // ---------------------------------------------------------------------
        // Party members must be placed such that they can pathfind back to the
        // party. This minimizes the chance of a party member getting stranded
        // (which in turn will strand the whole party in that place).
        // ---------------------------------------------------------------------

        pinfo.find_party = true;

        // ---------------------------------------------------------------------
        // Use the combat algorithm to place each member. Currently this will
        // never fail, in the degenerate case all party members will end up
        // "stranded" on top of the destination tile.
        // ---------------------------------------------------------------------

        for_each_member(combat_place_character, &pinfo);
#else
        int i;
        class Character *member;

        FOR_EACH_MEMBER(i, member) {
                if (!member->isDead())
                        member->putOnMap(new_place, new_x, new_y, 2 * n_pc);
	}
#endif

        // ---------------------------------------------------------------------
        // Set the party mode to "follow" by default, but if hostiles are in
        // this place then set to "character" mode.
        // ---------------------------------------------------------------------

        if (place_contains_hostiles(new_place, getAlignment())) {
                enableRoundRobinMode();
                combat_set_state(COMBAT_STATE_FIGHTING);
        } else {
                enableFollowMode();
                combat_set_state(COMBAT_STATE_LOOTING);
        }        

        endTurn();

}

bool player_party::try_to_enter_town_from_edge(class Portal * portal, int dx, int dy)
{
        int new_x;
        int new_y;
        int dir;


        // ---------------------------------------------------------------------
        // There should be no way to get here unless we are already in party
        // mode, which means we are in the wilderness. And the destination
        // should never be another wilderness.
        // ---------------------------------------------------------------------

        assert(place_is_wilderness(getPlace()));
        assert(!place_is_wilderness(portal->getToPlace()));

	cmdwin_print("enter %s", portal->getToPlace()->name);

        // ---------------------------------------------------------------------
        // Entering a town REQUIRES a direction so I know which edge of the town
        // the party should enter from. If we don't have one then prompt the
        // user to get one.
        // ---------------------------------------------------------------------

	if (dx == 0 && dy == 0) {
		cmdwin_print("-");
		dir = ui_get_direction();
	} else {
		dir = vector_to_dir(dx, dy);
	}

        // ---------------------------------------------------------------------
        // Calculate the entry coordinates based on the direction of entry.
        // ---------------------------------------------------------------------

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
		new_x = place_w(portal->getToPlace()) / 2;
		new_y = 0;
		break;
	case NORTH:
		new_x = place_w(portal->getToPlace()) / 2;
		new_y = place_h(portal->getToPlace()) - 1;
		break;
	case WEST:
		new_x = new_x = place_w(portal->getToPlace()) - 1;
		new_y = place_h(portal->getToPlace()) / 2;
		break;
	case EAST:
		new_x = 0;
		new_y = place_h(portal->getToPlace()) / 2;
		break;
	default:
		assert(false);
	}

        dx = directionToDx(dir);
        dy = directionToDy(dir);

        relocate(portal->getToPlace(), new_x, new_y);

        return true;
}


void player_party::try_to_enter_portal(class Portal * portal)
{
        // ---------------------------------------------------------------------
        // There should be no way to get here unless we are already in party
        // mode, which means we are in the wilderness.
        // ---------------------------------------------------------------------

        assert(place_is_wilderness(getPlace()));

        if (place_is_wilderness(portal->getToPlace())) {

                // -------------------------------------------------------------
                // This is the simple case: wilderness-to-wilderness
                // portalling. No mode switch is required, and relocate()
                // handles the place-to-place handoff.
                // -------------------------------------------------------------
                
                cmdwin_print("-%s-ok", portal->getName());
                relocate(portal->getToPlace(), portal->getToX(), portal->getToY());
                return;
        }

        // ---------------------------------------------------------------------
        // This is the complicated case: we have to handle a mode switch as
        // well as the place-to-place handoff. But relocate() will handle it
        // automatically now.
        // ---------------------------------------------------------------------

        relocate(portal->getToPlace(), portal->getToX(), portal->getToY());

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
        {
                int mv_cost = 0;
                char *progress = "";

		// No complications. Update the turn counter based on player
		// speed and terrain difficulties then move the player.
		cmdwin_print("-ok");
		relocate(info.place, info.x, info.y);
                mv_cost = place_get_movement_cost(info.place, info.x, info.y);
                if (vehicle)
                        mv_cost *= vehicle->getMovementCostMultiplier();
                decActionPoints(mv_cost);
                if (mv_cost < getSpeed()) {
                        progress = "";
                } else if (mv_cost < (getSpeed() * 2)) {
                        progress = "-slow";
                } else {
                        progress = "-very slow";
                }
                consolePrint("Move %s%s [%d AP]\n", directionToString(vector_to_dir(dx, dy)),
                             progress, mv_cost);
		return true;
        }

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
		return try_to_enter_moongate(info.moongate);

	case move_enter_combat:
		// Handle combat (possible multiple combats) until we're all
		// done with them.
		cmdwin_print("-attack!");
		memset(&cinfo, 0, sizeof(cinfo));
		cinfo.move = &info;

                // -------------------------------------------------------------
                // Enter combat on the current square, not the square we're
                // trying to move onto.
                // -------------------------------------------------------------

                info.x = getX();
                info.y = getY();

		move_to_wilderness_combat(&cinfo);

                endTurn();
		return true;

	case move_enter_auto_portal:
		// A teleporter is on that tile.
		if (info.portal->edge_entrance) {
			return try_to_enter_town_from_edge(info.portal,
							   info.dx, info.dy);
		}
		try_to_enter_portal(info.portal);
                return true;

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

int player_party::getPmask(void)
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

static bool member_find_slowest(class Character *character, void *data)
{
        *((int*)data) = min(*((int*)data), character->getSpeed());
        return false;
}

int player_party::getSpeed(void)
{
        int speed = 0x7fffffff;
	if (vehicle) {
		return vehicle->getSpeed();
	}
        for_each_member(&member_find_slowest, &speed);
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
		try_to_enter_portal(portal);

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

int G_latency_start = 0;

static bool party_mode_key_handler(struct KeyHandler *kh, int key, int keymod)
{
        cmdwin_clear();
        cmdwin_repaint();
        
        G_latency_start = SDL_GetTicks();

        if (keymod == KMOD_LCTRL || keymod == KMOD_RCTRL) {

                // SAM: This seemed like a less ugly way of setting off a group
                // of keybindings for "DM Mode" use or the like.  If we find
                // something more aesthetic wrt/ switch() syntax, we will
                // surely prefer it...
                // 
                // Control-key bindings for "DM Mode" commands like terrain
                // editing.  In future, these may be enabled/disabled at
                // compile time, or via a GhulScript keyword in the mapfile.
                switch (key) {
      
                case 't':
                        cmdTerraform(NULL);
                        break;

                case 's':
                        cmdSaveTerrainMap(NULL);
                        break;

                case 'z':
                        mapTogglePeering();
                        break;

                default:
                        break;
                } // switch(key)
        } // keymod

        else {
                // !keymod
                switch (key) {

                case KEY_NORTH:
                case KEY_EAST:
                case KEY_SOUTH:
                case KEY_WEST:
                {
                        int dir = keyToDirection(key);
                        player_party->move(directionToDx(dir),
                                           directionToDy(dir), false);
                        mapSetDirty();
                }
                break;

                case 'a':
                        cmdAttack();
                        break;
                case 'b':
                        player_party->board_vehicle();
                        break;
                case 'c':
                        cmdCastSpell(NULL);
                        break;
                case 'e':
                        // SAM:
                        // Perhaps this command should be merged with '>' ?
                        player_party->enter_portal();
                        break;
                case 'f':
                        cmdFire();
                        break;
                case 'g':
                        cmdGet(player_party, true);
                        break;
                case 'h':
                        // SAM: Adding (H)andle command...
                        cmdHandle(NULL);
                        break;
                case 'k':
                        cmdCamp(player_party);
                        player_party->endTurn();
                        break;
                case 'm':
                        cmdMixReagents();
                        break;
                case 'n':
                        cmdNewOrder();
                        break;
                case 'o':
                        cmdOpen(NULL);
                        break;
                case 'q':
                        cmdQuit();
                        break;
                case 'r':
                        cmdReady(NULL, CMD_SELECT_MEMBER|CMD_PRINT_MEMBER);
                        break;
                case 's':
                        cmdSearch(player_party->getX(), player_party->getY());
                        break;
                case 't':
                        cmdTalk(player_party->getX(), player_party->getY());
                        break;
                case 'u':
                        cmdUse(NULL, CMD_SELECT_MEMBER|CMD_PRINT_MEMBER);
                        break;
                case 'x':
                        cmdXamine(NULL);
                        break;
                case 'z':
                        cmdZtats(NULL);
                        break;
                case '@':
                        // SAM: 'AT' command for party-centric information
                        cmdAT(NULL);
                        break;
                case ' ':
                        player_party->endTurn();
                        consolePrint("Pass\n");
                        break;
                case '>':
                        // This key was chosen to be a cognate for '>' in
                        // NetHack and other roguelike games.
                        cmdZoomIn();
                        player_party->endTurn();
                        break;
                default:
                        break;
                } // switch(key)
        } // !keymod

        /* Return true when done processing commands. */
        return player_party->isTurnEnded();
}

static bool player_member_rest_one_hour(class Character * pm, void *data)
{
        if (pm->isAsleep() && pm->getRestCredits())
                pm->rest(1);
        return false;
}

void player_party::exec(struct exec_context *context)
{
        struct KeyHandler kh;

        if (allDead())
                return;

        if (isResting()) {

                /* After each hour of rest heal/restore party members */
                if (clock_alarm_is_expired(&rest_alarm)) {
                        for_each_member(player_member_rest_one_hour, NULL);
                        clock_alarm_set(&rest_alarm, 60);
                }

                if (clock_alarm_is_expired(&wakeup_alarm)) {
                        endResting();
                        cmdwin_print("rested!");
                }

                return;
        }

        startTurn();

        if (action_points > 0) {        
                kh.fx = &party_mode_key_handler;
                eventPushKeyHandler(&kh);
                eventHandle();
                eventPopKeyHandler();
        }

        endTurn();
}

bool player_party::allDead(void)
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

#if 0
void player_party::updateView()
{
        mapCenterView(getView(), x, y);
        mapRecomputeLos(getView());
        mapCenterCamera(x, y);
        mapSetDirty(); // force a repaint
}
#endif

int player_party::getLight()
{
        int i;
        class Character *member;

        light = 0;

        FOR_EACH_MEMBER(i,member) {
                light += member->getLight();
        }

        return light;
}

int player_party::getVisionRadius()
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


        // left off here

	return min(info.vrad, MAX_VISION_RADIUS);
}

struct inv_entry *player_party::search_inventory(class ObjectType * type)
{
	return myLookupIE(&inventory, type);
}

player_party::player_party()
{
	dx                 = 0;
	dy                 = -1;
	sprite             = 0;
	speed              = 0;
	vehicle            = 0;
	turns              = 0;
	mv_desc            = NULL;
	mv_sound           = NULL;
	n_pc               = 0;
	leader             = NULL;
	nArms              = 0;
	nReagents          = 0;
	nSpells            = 0;
	nItems             = 0;
	nAmmo              = 0;
	light              = 1;
	food               = 0;
	view               = 0;
	onMap              = true;
	alignment          = 0;
	gold               = 0;
	formation          = 0;
	campsite_map       = 0;
	campsite_formation = 0;
	camping            = false;
        resting            = false;
        turn_count         = 0;
        leader             = NULL;
        solo_member        = NULL;
        control_mode       = PARTY_CONTROL_ROUND_ROBIN;

	memset(pc, 0, sizeof(pc));
	list_init(&inventory);
        clearCombatExitDestination();
	container_link.key = being_layer;
        view = mapCreateView();
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
	turnAdvance(1);
}

static bool check_if_leader(class Character * pc, void *data)
{
        // ---------------------------------------------------------------------
        // Going to add a check for sleep here to handle the case where the
        // party is in follow mode and the leader is put asleep for some
        // reason. Otherwise the engine cranks out turns until the leader wakes
        // up.
        //
        // Note: um... what about charmed?
        // ---------------------------------------------------------------------

	if (!pc->isDead() && pc->isOnMap() && !pc->isAsleep()) {
		player_party->setLeader(pc);
		return true;
	}
	return false;
}

class Character *player_party::get_leader(void)
{
        if (leader == NULL) {
                chooseNewLeader();
        }
        return leader;
}

void player_party::removeMember(class Character *c)
{
        assert(c->party == (class NpcParty*)this);

        // ---------------------------------------------------------------------
        // Remove all its readied arms from party inventory.
        // ---------------------------------------------------------------------

	for (class ArmsType * weapon = c->enumerateArms(); weapon != NULL; weapon = c->getNextArms()) {

		struct inv_entry *ie;

		ie = search_inventory(weapon);
		assert(ie);
		ie->ref--;
		remove_from_inventory(ie, 1);
	}

        // ---------------------------------------------------------------------
        // Unhook it from the party.
        // ---------------------------------------------------------------------

        pc[c->getOrder()] = NULL;
        c->party = NULL;

        // ---------------------------------------------------------------------
        // Relinquish control.
        // ---------------------------------------------------------------------

        c->setPlayerControlled(false);
        c->setControlMode(CONTROL_MODE_AUTO);
}

bool player_party::addMember(class Character * c)
{
	int i = 0;

	// Note: this is called so early in startup that I can't touch the
	// paint routines in here. Callers will have to update the map and
	// status if necessary.

	assert(!c->isPlayerControlled());

        // Check if passability is compatible with rest of party
        if (n_pc && ! (c->getPmask() & getPmask())) {
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

        if (NULL == c->getView())
                c->setView(mapCreateView());

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

        // ---------------------------------------------------------------------
        // Reevaluate leadership with the new member added to the group.
        // ---------------------------------------------------------------------

        switch (getPartyControlMode()) {
        case PARTY_CONTROL_FOLLOW:
                if (c != get_leader()) {
                        c->setControlMode(CONTROL_MODE_FOLLOW);
                }
                break;
                
        case PARTY_CONTROL_SOLO:
                c->setControlMode(CONTROL_MODE_IDLE);
                break;
                
        case PARTY_CONTROL_ROUND_ROBIN:
                c->setControlMode(CONTROL_MODE_PLAYER);
                break;
                
        default:
                assert(false);
                break;
        }

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

static bool member_begin_resting(class Character *member, void *data)
{
        member->beginResting(*((int*)data));
        return false;
}

void player_party::beginResting(int hours)
{
        assert(hours > 0);

        for_each_member(member_begin_resting, &hours);
        statusRepaint();
        clock_alarm_set(&wakeup_alarm, hours * 60);
        clock_alarm_set(&rest_alarm, 60);
        resting   = true;
        mapBlackout(1);
        mapSetDirty();
}

bool player_party::isResting()
{
        return (resting || camping);
}

void player_party::throw_out_of_bed()
{
        assert(isResting());
        endResting();
        cmdwin_print("thrown out!");
}

int player_party::getTurnCount()
{
        return turn_count;
}

void player_party::decActionPoints(int points)
{        
        Object::decActionPoints(points);
        turn_count++;
        foogodRepaint();
}

class Character *player_party::getMemberAtIndex(int index)
{
        if (index < 0 || index >= MAX_N_PC)
                return NULL;
        return pc[index];
}

static bool member_remove(class Character *member, void *data)
{
        member->remove();
        return false;
}

void player_party::removeMembers()
{
        for_each_member(member_remove, NULL);
}

void player_party::setCombatExitDestination(struct location *location)
{
        combat_exit_destination.place = location->place;
        combat_exit_destination.x     = location->x;
        combat_exit_destination.y     = location->y;
}

void player_party::getCombatExitDestination(struct location *location)
{
        location->place = combat_exit_destination.place;
        location->x     = combat_exit_destination.x;
        location->y     = combat_exit_destination.y;
}

void player_party::clearCombatExitDestination()
{
        combat_exit_destination.place = NULL;
        combat_exit_destination.x     = -1;
        combat_exit_destination.y     = -1;
}

static bool member_uncharm(class Character *member, void *data)
{
        member->unCharm();
        return false;
}

void player_party::unCharmMembers()
{
        for_each_member(member_uncharm, NULL);
}

int player_party::getAlignment()
{
        return alignment;
}

static bool member_clear_alignment(class Character *member, void *data)
{
        member->clearAlignment(*((int*)data));
        return false;
}

void player_party::clearAlignment(int alignment)
{
        this->alignment &= ~alignment;
        for_each_member(member_clear_alignment, &alignment);
}

bool player_party::addToInventory(class Object *object)
{
        // ---------------------------------------------------------------------
        // This is the overloaded generic Object method. For now it is just a
        // wrapper for the pre-existing, historical add_to_inventory() method.
        //
        // Probably about the time I convert the player inventory to use
        // objects instead of types I'll remove add_to_inventory() and roll the
        // implementation into here.
        // ---------------------------------------------------------------------

        add_to_inventory(object->getObjectType(), 1);
        delete object;
        return true;
}

bool player_party::isCamping()
{
        return camping;
}

void player_party::beginCamping(class Character *guard, int hours)
{
        int i;
        class Character *member;

        camping    = true;
        camp_guard = guard;        

        if (NULL != camp_guard)
                camp_guard->beginGuarding(hours);

        FOR_EACH_MEMBER(i, member) {
                if (member != camp_guard)
                        member->beginCamping(hours);
	}


}

void player_party::endCamping()
{
        int i;
        class Character *member;

        if (! isCamping())
                return;

        camping    = false;

        if (NULL != camp_guard) {
                camp_guard->endGuarding();
                camp_guard = NULL;
        }

        FOR_EACH_MEMBER(i, member) {
                if (member != camp_guard)
                        member->endCamping();
        }

}

void player_party::ambushWhileCamping()
{
        int i;
        class Character *member;

        camping = false;

        FOR_EACH_MEMBER(i, member) {

                // -------------------------------------------------------------
                // If there's a guard then he/she/it will wake everybody else
                // up.
                // -------------------------------------------------------------
                
                if (camp_guard != NULL) {
                        member->awaken();
                }

                // -------------------------------------------------------------
                // Since there is no guard each member will roll to wake up.
                // -------------------------------------------------------------

                else {
                        member->ambushWhileCamping();
                }
        }
}

void player_party::endResting()
{
        int i;
        class Character *member;

        resting   = false;

        FOR_EACH_MEMBER(i, member) {
                member->endResting();
        }

        mapBlackout(0);
        mapSetDirty();
}

// -----------------------------------------------------------------------------
//
//      Party control mode functions
//
// -----------------------------------------------------------------------------

static bool member_set_control_mode(class Character *member, void *data)
{
        member->setControlMode(*((enum control_mode*)data));
        return false;
}

enum party_control player_party::getPartyControlMode()
{
        return control_mode;
}

void player_party::disableCurrentMode()
{
        if (solo_member) {
                solo_member->setSolo(false);
                solo_member = NULL;
        }

        if (leader) {
                leader->setLeader(false);
                leader = NULL;
        }
}

void player_party::enableFollowMode()
{
        enum control_mode mode = CONTROL_MODE_FOLLOW;

        disableCurrentMode();
        for_each_member(member_set_control_mode, &mode);
        chooseNewLeader();
        if (NULL == leader)
                enableRoundRobinMode();
        else
                control_mode = PARTY_CONTROL_FOLLOW;

}

void player_party::enableRoundRobinMode()
{
        enum control_mode mode = CONTROL_MODE_PLAYER;

        disableCurrentMode();
        for_each_member(member_set_control_mode, &mode);
        control_mode = PARTY_CONTROL_ROUND_ROBIN;
}

void player_party::enableSoloMode(class Character *solo)
{
        enum control_mode mode = CONTROL_MODE_IDLE;

        assert(solo->party == (class NpcParty*)this);

        disableCurrentMode();
        for_each_member(member_set_control_mode, &mode);
        solo->setSolo(true);
        solo_member = solo;
        control_mode = PARTY_CONTROL_SOLO;
}

void player_party::chooseNewLeader()
{
        if (NULL != leader) {
                leader->setLeader(false);
                leader = NULL;
        }
        for_each_member(check_if_leader, 0);
        if (NULL != leader)
                leader->setLeader(true);
}

void player_party::setLeader(class Character *character)
{
        leader = character;
}

bool player_party::rendezvous(struct place *place, int rx, int ry)
{
        int i;
        bool abort = false;
        bool done;
        int max_path_len;
        class Character *member;

        assert(NULL != leader);

        // ---------------------------------------------------------------------
        // If any member cannot find a path at least this short than rendezvous
        // fails.
        // ---------------------------------------------------------------------

        max_path_len = 10;

        // ---------------------------------------------------------------------
        // Center the camera on the rendezvous point.
        // ---------------------------------------------------------------------

        mapCenterCamera(rx, ry);
        mapUpdate(0);
        consolePrint("Rendezvous...");
        consoleRepaint();

        // ---------------------------------------------------------------------
        // Have each party member find and store a path to the rendezvous
        // point.
        // ---------------------------------------------------------------------

        for (i = 0; i < n_pc; i++) {

                struct astar_search_info as_info;

                member = pc[i];

                if (NULL == member || member->isDead() || !member->isOnMap() || member == leader ||
                    (member->getX() == rx && member->getY() == ry))
                        continue;

                memset(&as_info, 0, sizeof (as_info));
                as_info.x0    = member->getX();
                as_info.y0    = member->getY();
                as_info.x1    = rx;
                as_info.y1    = ry;
                as_info.flags = PFLAG_IGNOREBEINGS;
                member->path = place_find_path(place, &as_info, member->getPmask(), NULL);

                if (!member->path) {
                        consolePrint("%s cannot make the rendezvous!\n", member->getName());
                        abort = true;
                }
                else if (max_path_len > 0 && member->path->len > max_path_len) {
                        consolePrint("%s is too far away!\n", member->getName());
                        abort = true;
                }
        }

        // ---------------------------------------------------------------------
        // If anyone could not find a path then abort.
        // ---------------------------------------------------------------------

        if (abort) {
                for (i = 0; i < n_pc; i++) {

                        member = pc[i];

                        if (member->path) {
                                astar_path_destroy(member->path);
                                member->path = 0;
                        }
                }
                return false;
        }

        // ---------------------------------------------------------------------
        // Otherwise everyone has a path, so have them each take turns
        // following their own path to the rendezvous point.
        // ---------------------------------------------------------------------

        done = false;
        while (!done) {
                done = true;
                consolePrint(".");
                for (i = 0; i < n_pc; i++) {

                        struct astar_node *tmp;

                        member = pc[i];

                        // already arrived in an earlier iteration
                        if (!member->path)
                                continue;

                        // should always be at least two nodes
                        assert(member->path->next);

                        // arrived
                        if (member->path->next->x == rx && member->path->next->y == ry) {
                                astar_node_destroy(member->path);
                                member->path = 0;
                                //member->remove();   // try this... seems ok
                                continue;
                        }

                        done = false;

                        // move one step
                        member->move(member->path->next->x - member->getX(), member->path->next->y - member->getY());

                        // clean up used path node
                        tmp = member->path;
                        member->path = member->path->next;
                        astar_node_destroy(tmp);

                        mapUpdate(0);
                }
        }

        consolePrint("Ok!\n");

        return true;

}

int player_party::getContext(void)
{
        return (isOnMap() ? CONTEXT_WILDERNESS : (CONTEXT_TOWN | CONTEXT_COMBAT));
}

void player_party::addView()
{
        attachCamera(true);
        mapSetPlace(getPlace());
        Object::addView();
}
