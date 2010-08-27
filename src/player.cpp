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
#include "Party.h"
#include "screen.h"
#include "sound.h"
#include "terrain.h"
#include "map.h"
#include "foogod.h"
#include "combat.h"
#include "wind.h"
#include "dup_constants.h"
#include "cmdwin.h"
#include "vehicle.h"
#include "sprite.h"
#include "cmd.h"             // for ui_get_direction()
#include "Field.h"
#include "event.h"
#include "formation.h"
#include "ctrl.h"
#include "session.h"
#include "log.h"
#include "factions.h"
#include "gob.h"

#include <unistd.h>
#include <math.h>

#define CAMPING_TIME_ACCELERATION (TURNS_PER_HOUR)


#define DIRLOC(dir,place,coord) { \
    if ((dir) < 0) \
        (coord) = place_w((place)) - 1; \
    else if (! (dir)) \
        (coord) = place_w((place)) / 2; \
    else \
        (coord) = 0; \
}

extern void play_update_place_viewer(void);	/* hack */


static bool apply_damage(class Character * pm, void *amount)
{
        pm->damage(*((int *)amount));
	return false;
}

void PlayerParty::damage(int amount)
{
	/* First apply damage to the vehicle. If the vehicle is destroyed then
           destroy the party, too. */
	if (vehicle && vehicle->isVulnerable()) {

                // Subtle: save a pointer to the vehicle. If vehicle->damage()
                // actually destroys the vehicle, then in a roundabout way by
                // the time that call returns we will have disembarked (meaning
                // our usual vehicle pointrer will be NULL). But we still bear
                // responsibility hear for destroying the vehicle. Messy, but
                // there you go. It works and I don't care to fool with it.
                class Vehicle *vehptr = vehicle;

                obj_inc_ref(vehicle);
		vehicle->damage(amount);
		mapFlash(0);
		foogodRepaint();

                /* If the vehicle is destroyed then we are dead. */
		if (vehptr->isDestroyed()) {
			vehicle = NULL;   
		}

                obj_dec_ref(vehptr);

                return;
	}

	/* Apply damage to all party members. If they all die then the party is
           destroyed, too. */
        forEachMember(apply_damage, &amount);
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
        if (pm->isDead()) {
                return false;
        }

	if (player_party->food) {
		player_party->food--;
		return false;
	}

	log_msg("Starving!");

        /* Partial fix for SF BUG [ 1526910 ] "starvation is lame". Instead of
         * a fixed amount of damage, make damage proportional to existing
         * health. The player can starve for a while before dying, but the loss
         * of HP is a serious threat now even at high levels. */
	int damage = max(pm->getHp()/2, 1);
	apply_damage(pm, &damage);
	return false;
}

void PlayerParty::changePlaceHook()
{
        mapSetPlace(place);
        Place = place;
        place_enter(place);
        log_banner("Entering %s", Place->name);
        
        // --------------------------------------------------------------------
        // If the party is relocating to a non-wilderness place then I have to
        // break it out.
        // --------------------------------------------------------------------
        
        if (! place_is_wilderness(place)) {
                distributeMembers(place, x, y, dx, dy);
                return;
        } else {
            foogod_set_title("Wilderness Travel");
            foogodRepaint();
        }
}

enum move_result PlayerParty::check_move_to(struct move_info *info)
{
	class Party *npc_party;

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
	if ((npc_party = place_get_Party(info->place, info->x, info->y))) {
		if (are_hostile(this, npc_party)) {
			if (info->place->wilderness) {
				info->npc_party = npc_party;
			}
			return move_enter_combat;
		}
		return move_occupied;
	}
	// another vehicle?
	if (vehicle && place_get_vehicle(info->place, info->x, info->y))
		return move_occupied;

        // pass subp vehi
        // *    t    t    exit vehicle
        // t    f    *    ok
        // f    f    *    impassable

        info->subplace = place_get_subplace(info->place, info->x, info->y);
        if (info->subplace) {
                if (vehicle)
                        return move_not_in_vehicle;
                return move_enter_subplace;
        }

	// passable? I moved this check to be before a test for portals. A
	// portal is accessible iff the player can move across the terrain it
	// is placed on. The "ladder in the lake to the ladder on land" test
	// case just plain looks counterintuitive without it.
	if (!place_move_is_passable(info->place, info->x - info->dx, info->y-info->dy, info->x, info->y, this, 
                               PFLAG_MOVEATTEMPT))
		return move_impassable;

	return move_ok;
}

bool PlayerParty::turn_vehicle(void)
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

	return true;
}


/* Note it is not safe to modify the member list within the block of the below
 * 'iterator'  */
#define FOR_EACH_MEMBER(e,c) \
        for ((e) = node_next(&members), (c) = (class Character *)(e)->ptr; \
           (e) != &members; \
           (e) = node_next(e), (c) = (class Character *)(e)->ptr)

#define FOR_EACH_REVERSE_MEMBER(e,c) \
        for ((e) = node_prev(&members), (c) = (class Character *)(e)->ptr; \
           (e) != &members; \
           (e) = node_prev(e), (c) = (class Character *)(e)->ptr)


void PlayerParty::distributeMembers(struct place *new_place, int new_x, 
                                     int new_y, int new_dx, int new_dy)
{
        // --------------------------------------------------------------------
        // Remove the player party from the current place and remove its view
        // from the map viewer's consideration (instead we'll use the character
        // views, which are added as each character is placed on the map
        // below).
        // --------------------------------------------------------------------

        remove();

        // --------------------------------------------------------------------
        // Switch the current place to the portal destination place.
        // --------------------------------------------------------------------

        Place = new_place;

        // --------------------------------------------------------------------
        // Set the new destination place to be the subject of the map
        // viewer. Center the camera on the portal destination.
        // --------------------------------------------------------------------

        mapSetPlace(new_place);
        mapCenterCamera(new_x, new_y);
        mapSetDirty();

        // --------------------------------------------------------------------
        // Distribute all the party members centered on the portal destination
        // point. I assume this always succeeds, which means I'm relying on map
        // editors to be nice and make nice, clear areas around their portal
        // entrances.
        //
        // For now, I'll reuse the existing combat placement algorithm. Compare
        // the code below with position_player_party() in combat.c:
        // --------------------------------------------------------------------

        struct node *entry;
        class Character *member;

        FOR_EACH_REVERSE_MEMBER(entry, member) {
                if (!member->isDead()) {

                        int flags = 0;

                        if (member->putOnMap(new_place, new_x, new_y, 
                                             2 * size, 0))
                                continue;

                        // Try again, ignoring other objects.
                        flags |= PFLAG_IGNOREMECHS;
                        flags |= PFLAG_IGNOREBEINGS;
                        if (member->putOnMap(new_place, new_x, new_y, 
                                             2 * size, flags))
                                continue;

                        // Try again, ignoring hazards.
                        flags |= PFLAG_IGNOREHAZARDS;
                        flags |= PFLAG_IGNOREFIELDS;
                        if (member->putOnMap(new_place, new_x, new_y, 
                                             2 * size, flags))
                                continue;

                        // Try again, ignoring terrain.
                        flags |= PFLAG_IGNORETERRAIN;
                        if (member->putOnMap(new_place, new_x, new_y, 
                                             2 * size, flags))
                                continue;

                        // Ok, now that should have worked!
                        assert(0);
                }
	}

// I've messed with this a bit. I think it's confusing (even to me!) to have
// the party mode switched automatically. If it felt natural during play it
// would be ok, but it's quite the opposite.
#undef CONFIG_ENABLE_ROUND_ROBIN_ON_ENTRANCE_TO_HOSTILE_PLACE
#ifdef CONFIG_ENABLE_ROUND_ROBIN_ON_ENTRANCE_TO_HOSTILE_PLACE
        // --------------------------------------------------------------------
        // Set the party mode to "follow" by default, but if hostiles are in
        // this place then set to "character" mode.
        // --------------------------------------------------------------------

        if (place_contains_hostiles(new_place, this)) {
                enableRoundRobinMode();
                combat_set_state(COMBAT_STATE_FIGHTING);                
        } else {
                enableFollowMode();
                combat_set_state(COMBAT_STATE_LOOTING);
        }        
#else
        // --------------------------------------------------------------------
        // Set the party mode to "follow" by default. If hostiles are present
        // the user can opt to engage them manually. If hostiles are present
        // but not visible it's confusing/annoying when the camera does not
        // follow the party leader. This is especially true when there is only
        // one party member. 
        // --------------------------------------------------------------------
        enableFollowMode();
        if (place_contains_hostiles(new_place, this)) {
                combat_set_state(COMBAT_STATE_FIGHTING);                
        }
#endif
        endTurn();

}

enum MoveResult PlayerParty::try_to_enter_subplace_from_edge(
        struct place *subplace, int dx, int dy)
{
        int new_x;
        int new_y;
        int dir;


        // --------------------------------------------------------------------
        // There should be no way to get here unless we are already in party
        // mode, which means we are in the wilderness. And the destination
        // should never be another wilderness.
        // --------------------------------------------------------------------

        assert(place_is_wilderness(getPlace()));
        assert(!place_is_wilderness(subplace));

        // --------------------------------------------------------------------
        // Entering a subplace REQUIRES a direction so I know which edge of the
        // subplace the party should enter from. If we don't have one then
        // prompt the user to get one.
        // --------------------------------------------------------------------

	if (dx == 0 && dy == 0) {
		cmdwin_spush("Enter");
		dir = ui_get_direction();
	} else {
		dir = vector_to_8facing(dx, dy);
	}

        // --------------------------------------------------------------------
        // Get the entry coordinates from the subplace.
        // --------------------------------------------------------------------

	switch (dir) {
	case CANCEL:
		return UserCanceled;
	case HERE:
		return NoDestination;
	default:
                if (place_get_edge_entrance(subplace, dir, &new_x, &new_y))
                        return NoDestination;
                break;
	}

        dx = directionToDx(dir);
        dy = directionToDy(dir);

        relocate(subplace, new_x, new_y);

        return MovedOk;
}

enum MoveResult PlayerParty::try_to_move_off_map(struct move_info * info)
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
                return OffMap;
        }

        relocate(info->place->location.place, 
                 info->place->location.x, 
                 info->place->location.y);

        return MovedOk;
}

MoveResult PlayerParty::move(int newdx, int newdy)
{
	struct move_info info;
	struct combat_info cinfo;
        bool teleport = (abs(newdx) > 1 || abs(newdy) > 1);

	mapSetDirty();
	cmdwin_clear();

	// Cache the requested direction. Can't remember what we use this for
	// or if it's still necessary.
	dx = newdx;
	dy = newdy;

	// Change vehicle facing. This might consume a turn in which case
	// that's all we'll do.
	if (!teleport && turn_vehicle())
		return ChangedFacing;

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

                // No complications. Update the turn counter based on player
                // speed and terrain difficulties then move the player.
                mv_cost = place_get_diagonal_movement_cost(info.place, x, y, info.x, info.y, this,0);
					relocate(info.place, info.x, info.y);          
                if (vehicle)
                        mv_cost *= vehicle->getMovementCostMultiplier();
                decActionPoints(mv_cost);

                extern int DeveloperMode;
                if (DeveloperMode) {
                    log_msg("mv_cost=%d\n", mv_cost);
                }

                // none of this is used atm
                //
                // // reload mv_cost because diagonal movement shouldnt affect 'very slow' etc
                //mv_cost = place_get_movement_cost(info.place, info.x, info.y, this,0);
                //if (vehicle)
                //        mv_cost *= vehicle->getMovementCostMultiplier();                
                //if (mv_cost < getSpeed()) {
                //        progress = "";
                //} else if (mv_cost < (getSpeed() * 2)) {
                //        progress = "-slow";
                //} else {
                //        progress = "-very slow";
                //}

               //consolePrint("Move %s%s [%d AP]\n", 
               //               directionToString(vector_to_8facing(dx, dy)),
               //               progress, mv_cost);

		return MovedOk;
        }

	case move_off_map:
		return try_to_move_off_map(&info);
		return OffMap;

	case move_occupied:
		// Occupied by a friendly npc party. If they were unfriendly
		// we'd be going into combat.
		return WasOccupied;

	case move_impassable:
		// Impassable terrain.
		return WasImpassable;

        case move_not_in_vehicle:
                return NotInVehicle;

	case move_enter_combat:
		// Handle combat (possible multiple combats) until we're all
		// done with them.
		memset(&cinfo, 0, sizeof(cinfo));
		cinfo.move = &info;

                // ------------------------------------------------------------
                // Enter combat on the current square, not the square we're
                // trying to move onto.
                // ------------------------------------------------------------

                info.x = getX();
                info.y = getY();
                info.px = getX();
                info.py = getY();

		combat_enter(&cinfo);

                endTurn();
		return MovedOk;

        case move_enter_subplace:

#if OLD_PRE_ENTRY_INVOCATION
                // Going to try and move this call to place_enter()... as I
                // recall I put this here to allow an on-entry hook to prompt
                // the player to see if he really wants to enter.

                // run the place's pre-entry hook, if applicable
                if (info.subplace->pre_entry_hook &&
                    ! closure_exec(info.subplace->pre_entry_hook, "pp", 
                                   info.subplace, this))
                        return UserCanceled;
#endif
                return try_to_enter_subplace_from_edge(info.subplace, info.dx, 
                                                       info.dy);

	default:
		// no other results expected
		assert(false);
		return NotApplicable;
	}
}

struct sprite *PlayerParty::getSprite(void)
{
	if (vehicle)
		return vehicle->getSprite();

	return sprite;
}

char *PlayerParty::get_movement_description(void)
{
	if (!vehicle)
		return mv_desc;
	return vehicle->getMvDesc();
}

sound_t *PlayerParty::get_movement_sound(void)
{
	if (vehicle) {
		return vehicle->get_movement_sound();
	}
	return mv_sound;
}

bool PlayerParty::add(class ObjectType * type, int quantity)
{
	if (!quantity)
		return true;

        log_begin("You get ");
        type->describeType(quantity);
        log_end(NULL);

        return inventory->add(type, quantity);
}

bool PlayerParty::takeOut(ObjectType *type, int q)
{
        // Some types can be in more than one category, so remove from all.
        log_begin("You lose ");
        type->describeType(q);
        log_end(NULL);
        return inventory->takeOut(type, q);
}

static bool player_member_rest_one_hour(class Character * pm, void *data)
{
        if (pm->isAsleep())
                pm->rest(1);
        return false;
}

void PlayerParty::exec()
{

        if (allDead())
                return;

        // NOTE: by running startTurn() after this we are skipping the party
        // members' start-of-party-turn hooks when resting. That's probably not
        // desirable.

        if (isResting()) {

                /* After each hour of rest heal/restore party members */
                if (clock_alarm_is_expired(&rest_alarm)) {
                        forEachMember(player_member_rest_one_hour, NULL);
                        clock_alarm_set(&rest_alarm, 60);
                }

                if (clock_alarm_is_expired(&wakeup_alarm)) {
                        endResting();
                }

                return;
        }

        startTurn();      
        
        absorbMemberAPDebt();
        
        /* Loiter */
        if (isLoitering())
        {
				if (clock_alarm_is_expired(&wakeup_alarm))
				{
					endLoitering();
				}
				endTurn();
        }  

        if (action_points > 0 && ! isDestroyed()) {        

                /* In party mode increment the turn count whenever the player
                 * moves. I'm assuming he'll move at most once per turn. */
                session_inc_turn_count();
                foogodRepaint();
                
                ctrl(this);

                if (Session->reloaded)
                        /* Hack: this object has been destroyed with the old
                         * session. Leave now! */
                        return;
        }

        endTurn();
}

bool PlayerParty::allDead(void)
{
	int count = 0;
	forEachMember(pc_check_if_alive, &count);
	return (count == 0);
}

bool PlayerParty::immobilized(void)
{
	int count = 0;
	forEachMember(pc_check_if_not_immobilized, &count);
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

int PlayerParty::getLight()
{
        struct node *entry;
        class Character *member;

        light = 0;

        FOR_EACH_MEMBER(entry, member) {
                light += member->getLight();
        }

        return light;
}

int PlayerParty::getVisionRadius()
{
	struct VradInfo info;

	// Scan all party members to find the best light source and the best
	// eyes. They needn't be from the same member since I assume they are
	// traveling close enough to share their light sources with each other.
	info.vrad = 0;
	info.light = 0;
	forEachMember(myGetBestVisionRadius, &info);

	// hack -- should replace this with a routine for getting the ambient
	// light from the local environment (place?)
        assert(getPlace());
	if (!getPlace()->underground)
		info.light += sky_get_ambient_light(&Session->sky);

	// Set the vision radius to the minimum of the best available light
	// radius or the best available vision radius of all party members
	light = max(info.light, MIN_PLAYER_LIGHT);


        // left off here

	return min(info.vrad, MAX_VISION_RADIUS);
}

PlayerParty::PlayerParty()
{
	dx                 = 0;
	dy                 = -1;
	sprite             = 0;
	speed              = 0;
	vehicle            = 0;
	turns              = 0;
	mv_desc            = NULL;
	mv_sound           = NULL_SOUND;
	leader             = NULL;
	light              = 1;
	food               = 0;
	view               = 0;
	onMap              = true;
	gold               = 0;
	formation          = 0;
	campsite_map       = 0;
	campsite_formation = 0;
	camping            = false;
        resting            = false;
        loitering          = false;
        leader             = NULL;
        solo_member        = NULL;
        control_mode       = PARTY_CONTROL_ROUND_ROBIN;
        ctrl               = ctrl_party_ui;
        inventory          = NULL;

        setTurnsToNextMeal(TURNS_PER_FOOD);
        setBaseFaction(PLAYER_PARTY_FACTION);
        view = mapCreateView();
}

PlayerParty::PlayerParty(char *_tag,
                           struct sprite *sprite,
                           char *movement_desc, sound_t *movement_sound,
                           int _food, int _gold,
                           struct formation *_formation, 
                           struct terrain_map *_camping_map,
                           struct formation *_camping_formation)
{
        /* Do standard initialization first. */
	dx                 = 0;
	dy                 = -1;
	speed              = 0;
	vehicle            = 0;
	turns              = 0;
	leader             = NULL;
	light              = 1;
	food               = _food;
	view               = 0;
	gold               = _gold;
	formation          = _formation;
	campsite_map       = _camping_map;
	campsite_formation = _camping_formation;
	camping            = false;
        resting            = false;
        loitering          = false;
        leader             = NULL;
        solo_member        = NULL;
        control_mode       = PARTY_CONTROL_ROUND_ROBIN;
        ctrl               = ctrl_party_ui;
        tag                = 0;
        inventory          = NULL;

        // --------------------------------------------------------------------
        // Location (if any) will be set after construction by the loading
        // code. Initially the party is off-map and on the orphan list.
        // --------------------------------------------------------------------

        setOnMap(false);

        setTurnsToNextMeal(TURNS_PER_FOOD);
        setBaseFaction(PLAYER_PARTY_FACTION);
        view = mapCreateView();

        /* Now use the arguments */

        if (_tag) {
                tag = strdup(_tag);
                assert(tag);
        }
        
        this->sprite = sprite;
        if (movement_desc) {
                this->mv_desc = strdup(movement_desc);
                assert(this->mv_desc);
        } else {
                mv_desc = 0;
        }

        this->mv_sound = movement_sound;
}

PlayerParty::~PlayerParty()
{
        if (mv_desc)
                free(mv_desc);

        obj_dec_ref_safe(inventory);

        // Bugfix: when quitting while on board a vehicle, force the vehicle to
        // give up its reference to the player (otherwise this causes an
        // assert in session_del()).
        if (vehicle) {
                vehicle->setOccupant(0);
                vehicle->relocate(getPlace(), getX(), getY());
                obj_dec_ref(vehicle);
		vehicle = NULL;
        }

        /* fixme: need to somehow cancel those wq jobs setup by player_init? */
}

void PlayerParty::board_vehicle(void)
{
	cmdwin_clear();

	// already in a vehicle so exit
	if (vehicle) {
                log_begin("You exit ");
                vehicle->describe();
                log_end(".");
		vehicle->setOccupant(0);
		vehicle->relocate(getPlace(), getX(), getY());
                obj_dec_ref(vehicle);
		vehicle = NULL;
		mapSetDirty();

                /* Erase vehicle hull stats. */
                foogodRepaint();
		return;
	}

	vehicle = place_get_vehicle(Place, x, y);
	if (!vehicle) {
		return;
	}

        obj_inc_ref(vehicle);

        log_begin("You board ");
        vehicle->describe();
        log_end(".");

	vehicle->setOccupant(this);
	vehicle->remove();
	mapSetDirty();

        /* Show vehicle hull stats. */
        foogodRepaint();

        /* Prompt the player to name the vehicle so it isn't
         * garbage-collected. */
        if (! vehicle->isNamed()) {
                ui_name_vehicle(vehicle);
        }
}

class Character *PlayerParty::get_leader(void)
{
        if (leader == NULL) {
                chooseNewLeader();
        }
        return leader;
}

void PlayerParty::removeMember(class Character *c)
{
        struct node *entry;
        class Character *next_member;
        int index;

        assert(c->party == (class Party*)this);

        // Remove all its readied arms from party inventory.
		int armsIndex = 0;
	for (class ArmsType * weapon = c->enumerateArms(&armsIndex); weapon != NULL; 
             weapon = c->getNextArms(&armsIndex)) {

		struct inv_entry *ie;

		ie = inventory->search(weapon);
		assert(ie);
		ie->ref--;
		takeOut(weapon, 1);
	}

        // Re-order the indices of the remaining party members
        for (entry = node_next(c->plnode), index = c->getOrder(); 
             entry != &members; 
             entry = node_next(entry), index++) {
                next_member = (class Character *)entry->ptr;
                next_member->setOrder(index);
        }

        // Relinquish control.
        c->setPlayerControlled(false);
        c->setControlMode(CONTROL_MODE_AUTO);

        // remove its view
        c->rmView();
        
        // Unhook it from the party
        Party::removeMember(c);
}

bool PlayerParty::addMember(class Character * c)
{
	// Note: this is called so early in startup that I can't touch the
	// paint routines in here. Callers will have to update the map and
	// status if necessary.

	assert(!c->isPlayerControlled());

        // Hook it to the party
        Party::addMember(c);

        // Make sure it's not going to try and run a schedule
        c->setSchedule(NULL);

        // Set special player-controlled flag
	c->setPlayerControlled(true);

        // gmcnutt: added this as a hack to support quickly determining if a
        // character belongs to the player party.
        c->party = (Party*)this;

        // Can't think of any reason why a char should be on the orphan list
        assert(! c->handle);

        if (NULL == c->getView())
                c->setView(mapCreateView());

	// Loop over all readied weapons and add them to player inventory. Also
	// must set the refcount once they are in inventory.

	int armsIndex = 0;
	for (class ArmsType * weapon = c->enumerateArms(&armsIndex);
	     weapon != NULL; weapon = c->getNextArms(&armsIndex)) {

		struct inv_entry *ie;

		add(weapon, 1);
		ie = inventory->search(weapon);
		assert(ie);
		ie->ref++;

	}

	// Note: leave the readied arms as-is and character inventory as-is for
	// now.

        // --------------------------------------------------------------------
        // Reevaluate leadership with the new member added to the group.
        // --------------------------------------------------------------------
        if (! c->engagedInTask()) {
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
        }

        // Call statusSetMode() to force it to re-evaluate the necessary screen
        // size for showing the party.
        statusSetMode(ShowParty);

        // Force a repaint on the status window.
        statusRepaint();

	return true;

}

void PlayerParty::paint(int sx, int sy)
{
	if (vehicle)
		vehicle->paint(sx, sy);
	else
		Object::paint(sx, sy);
}

const char *PlayerParty::getName()
{
	return "player party";
}

bool PlayerParty::isVisible()
{
	return true;
}

void PlayerParty::describe()
{
	log_continue("the %s", getName());
}

struct formation *PlayerParty::get_formation()
{
	if (vehicle && vehicle->get_formation())
		return vehicle->get_formation();
	if (camping && campsite_formation)
		return campsite_formation;
	return formation;
}

int PlayerParty::get_num_living_members(void)
{
	int count = 0;
	forEachMember(pc_check_if_alive, &count);
        return count;
}

class Character *PlayerParty::get_first_living_member(void)
{
        class Character *pc = NULL;
        forEachMember(pc_get_first_living, &pc);
        return pc;
}

void PlayerParty::beginLoitering(int hours)
{
        struct node *entry = 0;
        class Character *member = 0;

        assert(hours > 0);

        log_begin_group();
        log_msg("Loitering...");
        FOR_EACH_MEMBER(entry, member) {
                member->beginLoitering(hours);
        }
        log_end_group();

        clock_alarm_set(&wakeup_alarm, hours * 60);
        loitering = true;

        /* Bugfix for [ 1629974 ] "loitering in wilderness uses too much
         * food". We want the acceleration to be one hour per turn at the
         * current map scale. CAMPING_TIME_ACCELERATION is appropriate for a
         * town-scale wilderness combat map (where camping typically occurs),
         * but party loitering is done at wilderness scale. */
        session_set_time_accel((float)CAMPING_TIME_ACCELERATION
                               / (float)place_get_scale(getPlace()));
}

bool PlayerParty::isLoitering()
{
        return loitering;
}

void PlayerParty::endLoitering()
{
        struct node *entry;
        class Character *member;

        loitering  = false;
        session_set_time_accel(1);
        log_begin_group();
        FOR_EACH_MEMBER(entry, member) {
                member->endLoitering();
        }
        enableFollowMode();
        log_end_group();
}


void PlayerParty::forceAbortLoitering()
{
	endLoitering();
	
	struct node *entry;
	class Character *member;

	log_begin_group();
	FOR_EACH_MEMBER(entry, member) {
		member->resetActionPoints();
	}
	log_end_group();
}


static bool member_begin_resting(class Character *member, void *data)
{
        member->beginResting(*((int*)data));
        return false;
}

void PlayerParty::beginResting(int hours)
{
        assert(hours > 0);

        log_begin_group();
        log_msg("Begin resting...");
        forEachMember(member_begin_resting, &hours);
        log_end_group();

        statusRepaint();

        clock_alarm_set(&wakeup_alarm, hours * 60);
        clock_alarm_set(&rest_alarm, 60);
        resting   = true;

        mapBlackout(1);
        mapSetDirty();

        // Accelerate time while resting.
        session_set_time_accel(CAMPING_TIME_ACCELERATION);
}

bool PlayerParty::isResting()
{
        return (resting || camping);
}

void PlayerParty::throw_out_of_bed()
{
        assert(isResting());
        endResting();
}

class Character *PlayerParty::getMemberAtIndex(int index)
{
        struct node *entry;
        class Character *member;

        FOR_EACH_MEMBER(entry, member) {
                if (!index)
                        return member;
                index--;
        }

        return NULL;
}

static bool member_uncharm(class Character *member, void *data)
{
        member->unCharm();
        return false;
}

void PlayerParty::unCharmMembers()
{
        forEachMember(member_uncharm, NULL);
}

bool PlayerParty::addToInventory(class Object *object)
{
        // ---------------------------------------------------------------------
        // This is the overloaded generic Object method. For now it is just a
        // wrapper for the pre-existing, historical add_to_inventory() method.
        //
        // Probably about the time I convert the player inventory to use
        // objects instead of types I'll remove add_to_inventory() and roll the
        // implementation into here.
        // ---------------------------------------------------------------------

        add(object->getObjectType(), object->getCount());
        obj_dec_ref(object);
        return true;
}

bool PlayerParty::isCamping()
{
        return camping;
}

void PlayerParty::beginCamping(class Character *guard, int hours)
{
        struct node *entry;
        class Character *member;

        camping    = true;
        camp_guard = guard;        

        if (camp_guard
            && ! camp_guard->isDead()
            && ! camp_guard->isAsleep())
                camp_guard->beginGuarding(hours);

        FOR_EACH_MEMBER(entry, member) {
                if (member != camp_guard)
                        member->beginCamping(hours);
	}

        // Accelerate time while camping.
        session_set_time_accel(CAMPING_TIME_ACCELERATION);
}

void PlayerParty::endCamping()
{
        struct node *entry;
        class Character *member;

        if (! isCamping())
                return;

        camping    = false;

        // Un-accelerate time when done camping.
        session_set_time_accel(1);

        if (NULL != camp_guard) {
                camp_guard->endGuarding();
                camp_guard = NULL;
        }

        FOR_EACH_MEMBER(entry, member) {
                if (member != camp_guard)
                        member->endCamping();
        }

}

void PlayerParty::ambushWhileCamping()
{
        struct node *entry;
        class Character *member;

        camping = false;

        // Un-accelerate time when done camping.
        session_set_time_accel(1);

        FOR_EACH_MEMBER(entry, member) {

                // ------------------------------------------------------------
                // If there's a guard then he/she/it will wake everybody else
                // up.
                // ------------------------------------------------------------
                
                if (camp_guard != NULL) {
                        member->endResting();
                }

                // ------------------------------------------------------------
                // Since there is no guard each member will roll to wake up.
                // ------------------------------------------------------------

                else {
                        member->ambushWhileCamping();
                }
        }

        if (camp_guard)
                camp_guard->endGuarding();
}

void PlayerParty::endResting()
{
        struct node *entry;
        class Character *member;

        resting   = false;

        // Un-accelerate time when done resting.
        session_set_time_accel(1);

        log_begin_group();
        FOR_EACH_MEMBER(entry, member) {
                member->endResting();
        }
        enableFollowMode();
        log_end_group();

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
    if (member->isPlayerControlled()
        && ! member->engagedInTask()
        ) {
        member->setControlMode(*((enum control_mode*)data));
    }
    return false;
}

enum party_control PlayerParty::getPartyControlMode()
{
        return control_mode;
}

void PlayerParty::disableCurrentMode()
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

void PlayerParty::enableFollowMode()
{
        enum control_mode mode = CONTROL_MODE_FOLLOW;

        disableCurrentMode();
        forEachMember(member_set_control_mode, &mode);
        chooseNewLeader();
        if (NULL == leader) {
            enableRoundRobinMode();
        } else {
            control_mode = PARTY_CONTROL_FOLLOW;
            foogod_set_title("Follow: %s", leader->getName());
            foogodRepaint();
        }

}

void PlayerParty::enableRoundRobinMode()
{
        enum control_mode mode = CONTROL_MODE_PLAYER;

        disableCurrentMode();
        forEachMember(member_set_control_mode, &mode);
        control_mode = PARTY_CONTROL_ROUND_ROBIN;
        foogod_set_title("Round Robin: <pending>");
        foogodRepaint();
}

void PlayerParty::enableSoloMode(class Character *solo)
{
        enum control_mode mode = CONTROL_MODE_IDLE;

        assert(solo->party == (class Party*)this);

        disableCurrentMode();
        forEachMember(member_set_control_mode, &mode);
        solo->setSolo(true);
        solo_member = solo;
        control_mode = PARTY_CONTROL_SOLO;
        foogod_set_title("Solo: %s", solo->getName());
        foogodRepaint();
}

void PlayerParty::chooseNewLeader()
{
        if (NULL != leader) {
                leader->setLeader(false);
                leader = NULL;
        }

        struct node *entry;
        class Character *member;

        FOR_EACH_MEMBER(entry, member) {
                if (member->canBeLeader()) {
                        setLeader(member);
                        break;
                }
        }

        if (NULL != leader) {
                leader->setLeader(true);
                if (PARTY_CONTROL_FOLLOW == control_mode) {
                    foogod_set_title("Follow: %s", leader->getName());
                    foogodRepaint();
                }
        }
}

void PlayerParty::setLeader(class Character *character)
{
        leader = character;
}

bool PlayerParty::rendezvous(struct place *place, int rx, int ry)
{
        bool abort = false;
        bool done;
        int max_path_len;
        struct node *entry;
        class Character *member;

        assert(NULL != leader);

        // --------------------------------------------------------------------
        // If any member cannot find a path at least this short than rendezvous
        // fails
        // --------------------------------------------------------------------

        max_path_len = 10;

        // --------------------------------------------------------------------
        // Center the camera on the rendezvous point.
        // --------------------------------------------------------------------

        mapCenterCamera(rx, ry);
        mapUpdate(0);

        // --------------------------------------------------------------------
        // Have each party member find and store a path to the rendezvous
        // point.
        // --------------------------------------------------------------------

        FOR_EACH_MEMBER(entry, member) {

                struct astar_search_info as_info;

                // ------------------------------------------------------------
                // If the member already has a cached path then clean it up.
                // ------------------------------------------------------------
                if (member->cachedPath) {
                        astar_path_destroy(member->cachedPath);
                        member->cachedPathPlace = NULL;
                        member->cachedPath = NULL;
                }

                if (NULL == member 
                    || member->isDead() 
                    || !member->isOnMap() 
                    || member == leader 
                    || (member->getX() == rx 
                        && member->getY() == ry))
                        continue;

                /* The following works for sleeping, but in general is not a
                 * good solution. What about paralyzation, or other effects
                 * unknown to the kernel? What will happen is these effects
                 * will be ignored in the code below. Although technically not
                 * correct behavior, it shouldn't crash or make the game
                 * unplayable. */
                if (member->isAsleep()
                    || member->engagedInTask()
                    || ! member->isPlayerControlled()
                    ) {
                        abort = true;
                        continue;
                }

                memset(&as_info, 0, sizeof (as_info));
                as_info.x0    = member->getX();
                as_info.y0    = member->getY();
                as_info.x1    = rx;
                as_info.y1    = ry;
                as_info.flags = PFLAG_IGNOREBEINGS;
                member->cachedPath = place_find_path(place, &as_info, 
                                               member);

                if (!member->cachedPath) {
                        log_msg("%s cannot make the rendezvous!", 
                                     member->getName());
                        abort = true;
                }
                else if (max_path_len > 0 && 
                         member->cachedPath->len > max_path_len) {
                        log_msg("%s is too far away!", 
                                     member->getName());
                        abort = true;
                }
        }

        // --------------------------------------------------------------------
        // If anyone could not find a path then abort.
        // --------------------------------------------------------------------

        if (abort) {
                FOR_EACH_MEMBER(entry, member) {
                        if (member->cachedPath) {
                                astar_path_destroy(member->cachedPath);
                                member->cachedPath = 0;
                        }
                }
                return false;
        }

        // --------------------------------------------------------------------
        // Otherwise everyone has a path, so have them each take turns
        // following their own path to the rendezvous point.
        // --------------------------------------------------------------------

        done = false;
        while (!done) {
                done = true;
                //consolePrint(".");
                FOR_EACH_MEMBER(entry, member) {

                        struct astar_node *tmp;

                        // already arrived in an earlier iteration
                        if (!member->cachedPath)
                                continue;

                        // should always be at least two nodes
                        assert(member->cachedPath->next);

                        // arrived
                        if (member->cachedPath->next->x == rx && 
                            member->cachedPath->next->y == ry) {
                                astar_node_destroy(member->cachedPath);
                                member->cachedPath = 0;
                                //member->remove();   // try this... seems ok
                                continue;
                        }

                        done = false;

                        // move one step
                        member->move(
                                member->cachedPath->next->x - member->getX(), 
                                member->cachedPath->next->y - member->getY());

                        // clean up used path node
                        tmp = member->cachedPath;
                        member->cachedPath = member->cachedPath->next;
                        astar_node_destroy(tmp);

                        // Bugfix: after rendezvous party members sometimes
                        // have a heavy action point debt, penalizing them in
                        // the new place. Zero out the debt during rendezvous
                        // to prevent that.
                        member->resetActionPoints();

                        mapUpdate(0);
                }
        }

        //consolePrint("Ok!\n");

        return true;

}

int PlayerParty::getContext(void)
{
        return (isOnMap() ? CONTEXT_WILDERNESS : CONTEXT_TOWN);
}

void PlayerParty::addView()
{
        attachCamera(true);
        mapSetPlace(getPlace());
        Object::addView();
}

struct place *PlayerParty::getPlaceFromMembers(void)
{
        struct node *elem;
        class Character *member;

        member = get_leader();

        if (member)
                return member->getPlace();

        FOR_EACH_MEMBER(elem, member) {
                if (member->isOnMap()) {
                        return member->getPlace();
                }
        }

        return 0;
}

void PlayerParty::startSession(void)
{
        struct node *elem;
        class Character *member;


        if (isOnMap())
                return;

        // --------------------------------------------------------------------
        // Not on the map, so we must be in a small-scale place. At least one
        // of the members needs to be on the map or I can't figure out where
        // the starting place is. Once I know, I can setup the map viewer and
        // add all the member views.
        // --------------------------------------------------------------------
       
        Place = getPlaceFromMembers();

        if (! Place)
                return;

        assert(Place);
        mapSetPlace(Place);
        mapSetDirty();
        setPlace(Place);
        
        FOR_EACH_MEMBER(elem, member) {
                if (member->isOnMap()) {
                        member->addView();
                }
        }

        // --------------------------------------------------------------------
        // Set the party mode to "follow" by default, but if hostiles are in
        // this place then set to "character" mode.
        // --------------------------------------------------------------------

        if (place_contains_hostiles(getPlace(), this)) {
                enableRoundRobinMode();
                combat_set_state(COMBAT_STATE_FIGHTING);
        } else {
                enableFollowMode();
                combat_set_state(COMBAT_STATE_DONE);
        }        

}

void PlayerParty::setOnMap(bool val)
{
        if (val == isOnMap())
                return;

        Object::setOnMap(val);

        // --------------------------------------------------------------------
        // If the party is going from off-map to on, then remove it from the
        // orphan list. If the opposite then add it.
        // --------------------------------------------------------------------

        if (val) {
                assert(handle);
                session_rm_obj(Session, this);
                handle = 0;
        } else {
                assert(! handle);
                session_add_obj(Session, this, player_dtor, player_save, NULL);
        }
}

void player_dtor(void *val)
{
        class PlayerParty *party = (class PlayerParty*)val;
        obj_dec_ref(party);
}

void player_save(save_t *save, void *val)
{
        ((class PlayerParty*)val)->save(save);
}

void PlayerParty::save(save_t *save)
{
        save->enter(save, "(let ((kplayer ");

        save->enter(save, "(kern-mk-player\n");
        if (tag)
                save->write(save, "'%s\n", tag);
        else
                save->write(save, "nil\n");
        save->write(save, "%s\n", sprite_get_tag(this->sprite));
        save->write(save, "\"%s\"\n", this->mv_desc);
        if (NULL_SOUND != mv_sound)
                save->write(save, "%s\n", sound_get_tag(this->mv_sound));
        else
                save->write(save, "nil\n");
        save->write(save, "%d %d\n", food, gold);
        save->write(save, "%d ;; turns to next meal\n", 
                    turns_to_next_meal);
        save->write(save, "%s\n", 
                this->formation ? this->formation->tag : "nil");
        save->write(save, "%s\n", 
                this->campsite_map ? this->campsite_map->tag : "nil");
        save->write(save, "%s\n", 
                this->campsite_formation ? 
                this->campsite_formation->tag : "nil");

        if (vehicle)
                vehicle->save(save);
        else
                save->write(save, "nil ; player's vehicle\n");

        inventory->save(save);

        if (list_empty(nodelst(&this->members))) {
                save->write(save, "nil\n");
        } else {
                struct node *elem;
                class Character *ch;

                save->enter(save, "(list\n");
                FOR_EACH_MEMBER(elem,ch) {
                        char_save(save, ch);
                }
                save->exit(save, ")  ; list of party members\n");
        }

        save->exit(save, ")))\n"); // end ((kplayer (kern-mk-player ...)))

        if (getGob()) {
                save->enter(save, "(bind kplayer ");
                gob_save(getGob(), save);
                save->exit(save, ")\n");
        }

        save->exit(save, ")\n"); // end (let ...) block
}

bool PlayerParty::addFood(int amount)
{
        if (amount == 0)
                return true;

        if (amount > 0)
                log_msg("You get %d food.", amount);
        else
                log_msg("You lose %d food.", -amount);

        food += amount;
        if (food < 0)
                food = 0;
        foogodRepaint();

        return true;
}

bool PlayerParty::addGold(int amount)
{
        if (amount == 0)
                return true;

        if (amount > 0)
                log_msg("You get %d gold.", amount);
        else
                log_msg("You lose %d gold.", -amount);

        gold += amount;
        if (gold < 0)
                gold = 0;
        foogodRepaint();

        return true;
}

bool PlayerParty::isPlayerControlled()
{
        return true;
}

void PlayerParty::advanceTurns(int turns)
{
        // Check if its time to eat
        turns_to_next_meal -= turns;
        if (turns_to_next_meal <= 0) {
                forEachMember(pc_eat_food, 0);
                foogodRepaint();
                turns_to_next_meal += TURNS_PER_FOOD;
        }
}

void PlayerParty::setTurnsToNextMeal(int turns)
{
        turns_to_next_meal = turns;
}

bool PlayerParty::hasInInventory(class ObjectType *type)
{
        return inventory->search(type) != NULL;
}

void PlayerParty::unrefInventoryObject(ObjectType *type)
{
        struct inv_entry *ie = inventory->search(type);
        assert(ie);
        ie->ref--;
}

void PlayerParty::refInventoryObject(ObjectType *type)
{
        struct inv_entry *ie = inventory->search(type);
        assert(ie);
        ie->ref++;
}

void PlayerParty::setInventoryContainer(Container *val)
{
        assert(!inventory);
        assert(val);

        inventory = val;
        obj_inc_ref(inventory);
}

void PlayerParty::addExperience(int val)
{
        struct node *entry;
        class Character *member;

        FOR_EACH_MEMBER(entry, member) {
                if (!member->isDead()) {
                        member->addExperience(val);
                }
        }
}

bool PlayerParty::canSeeLocation(struct place *place, int x, int y)
{
        // fixme -- ref naz.scm (any-player-party-member-visible?)
        return false;
}

/* shifts currently wielded items to the top of the equipment list */
void PlayerParty::sortReadiedItems(class Character * member)
{
	struct inv_entry *ie;
	int armsIndex = 0;
	for (class ArmsType * weapon = member->enumerateArms(&armsIndex); weapon != NULL; 
             weapon = member->getNextArms(&armsIndex)) {

		ie = inventory->search(weapon);
		assert(ie);
		inventory->moveToFront(ie);
	}
}

class Container *PlayerParty::getInventory()
{
        // yes, I know it's public, but this is polymorphic with the Party base class
        return inventory;
}
