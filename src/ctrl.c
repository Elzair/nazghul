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

#include "ctrl.h"
#include "dice.h"
#include "event.h"
#include "cmd.h"
#include "cmdwin.h"
#include "map.h"
#include "place.h"
#include "player.h"
#include "Party.h"
#include "character.h"
#include "combat.h"
#include "terrain.h"
#include "sched.h"
#include "session.h"
#include "log.h"
#include "factions.h"

int G_latency_start = 0;
int G_turnaround_start = 0;
int G_turnaround_stop  = 0;


static int ctrl_party_key_handler(struct KeyHandler *kh, int key, int keymod)
{
        class player_party *party = (class player_party*)kh->data;

        cmdwin_flush();
        
        G_latency_start = SDL_GetTicks();

        switch (key) {

        case KEY_NORTH:
        case KEY_EAST:
        case KEY_SOUTH:
        case KEY_WEST:
        {
                int dir = keyToDirection(key);
                party->move(directionToDx(dir), directionToDy(dir));
                mapSetDirty();
        }
        break;

        case 'a':
                cmdAttack();
                break;
        case 'b':
                party->board_vehicle();
                break;
        case 'c':
                cmdCastSpell(NULL);
                break;
        case 'f':
                cmdFire();
                break;
        case 'g':
                cmdGet(party, true);
                break;
        case 'h':
                // SAM: Adding (H)andle command...
                cmdHandle(NULL);
                break;
        case 'k':
                cmdCampInWilderness(party);
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
                cmdReady(NULL);
                break;
        case 's':
                cmdSearch(party->getPlace(),
                          party->getX(), party->getY());
                break;
        case 't':
                cmdTalk(NULL);
                break;
        case 'u':
                cmdUse(NULL, CMD_SELECT_MEMBER|CMD_PRINT_MEMBER);
                break;
        case 'x':
                cmdXamine(party);
                break;
        case 'z':
                cmdZtats(NULL);
                break;
        case '@':
                // SAM: 'AT' command for party-centric information
                cmdAT(NULL);
                break;
        case ' ':
                party->endTurn();
                log_msg("Pass");
                break;
        case '>':
                // This key was chosen to be a cognate for '>' in
                // NetHack and other roguelike games.
                cmdZoomIn();
                party->endTurn();
                break;

        case KEY_CTRL_Q:
                cmdQuickSave();
                break;
                        
        case KEY_CTRL_R:
                cmdReload();
                return true;
                break;
                        
        case KEY_CTRL_S:
                cmdSaveTerrainMap(NULL);
                break;
                        
        case KEY_CTRL_T:
                cmdTerraform(NULL);
                break;
                        
        case KEY_CTRL_Z:
                mapTogglePeering();
                break;
        default:
                break;
        } // switch(key)

        /* Prep cmdwin for next prompt */
        cmdwin_clear();

        /* Return true when done processing commands. */
        return party->isTurnEnded();
}

void ctrl_wander(class Object *object)
{
        // *** left off here ***

	int dx = 0, dy = 0;

	/* Roll for direction */
	dx = rand() % 3 - 1;
	if (!dx)
		dy = rand() % 3 - 1;

	if (dx || dy) {

                int newx, newy;
                newx = object->getX() + dx;
                newy = object->getY() + dy;

                if (! object->canWanderTo(newx, newy))
                        return;

                // ------------------------------------------------------------
                // Do a quick check here if this would take the character off
                // the map. If so, then don't do it. Can't have NPC's wandering
                // off out of town...
                // ------------------------------------------------------------

                if (place_off_map(object->getPlace(), object->getX() + dx, 
                                  object->getY() + dy) ||
                    place_is_hazardous(object->getPlace(), object->getX() + dx,
                                       object->getY() + dy))
                        return;

		object->move(dx, dy);
	}
}

static void ctrl_work(class Party *party)
{
	int d;

        /* Check if this party is friendly to the player or if the player is
         * not around */
	if (! are_hostile(party, player_party) ||
            Place != party->getPlace()) {
		// This party is friendly to the player, so just wander for now
		// (later I'll add schedules).
		ctrl_wander(party);
		return;
	}

        /* Check if the player is _on this spot_. Yes, this can happen under
         * current game rules. If a player enters a portal and an npc is on the
         * destination then... */
	if (party->getX() == player_party->getX() && party->getY() == player_party->getY()) {
                
                struct move_info info;
                struct combat_info cinfo;

                memset(&info, 0, sizeof(info));
                info.place = party->getPlace();
                info.x = party->getX();
                info.y = party->getY();
                info.dx = party->getDx();
                info.dy = party->getDy();
                info.px = player_party->getX();
                info.py = player_party->getY();
                info.npc_party = party;
                
                if (!info.dx && !info.dy)
                        info.dx = 1;
                else if (info.dx && info.dy)
                        info.dy = 0;

                memset(&cinfo, 0, sizeof(cinfo));
                cinfo.defend = true;
                cinfo.move = &info;
                
                combat_enter(&cinfo);
                party->endTurn();
		return;
	}

        

	/* Check if the player is in visible range */
	d = place_walking_distance(party->getPlace(), party->getX(), party->getY(),
				   player_party->getX(), player_party->getY());

	if (d > party->getVisionRadius()) {
		ctrl_wander(party);
		return;
	}

	if (d > 1 && party->attack_with_ordnance(d)) {
		return;
	}

	if (!party->gotoSpot(player_party->getX(), player_party->getY())) {
		ctrl_wander(party);
		return;
	}
}

void ctrl_do_attack(class Character *character, class ArmsType *weapon, 
                    class Character *target)
{
        int hit;
        int def;
        int damage;
        int armor;
        bool miss;

        log_begin("%s: %s - %s ", character->getName()
                  , weapon->getName()
                  , target->getName()
                );

        miss = ! weapon->fire(target, character->getX(), character->getY());
        character->decActionPoints(weapon->getRequiredActionPoints());
        character->useAmmo(weapon);

        if (miss) {
                log_end("missed!");
                return;
        }

        // Roll to hit.
        hit = dice_roll("1d20") + dice_roll(weapon->getToHitDice());
        def = target->getDefend();

        dbg("%s to-hit: %d\n", character->getName(), hit);
        dbg("%s to-def: %d\n", target->getName(), def);

        if (hit < def) {
                log_end("barely scratched!");
                return;
        }

        // roll for damage
        damage = dice_roll(weapon->getDamageDice());
        armor = target->getArmor();
        damage -= armor;
        damage = max(damage, 0);
        target->damage(damage);

        log_end("%s!", target->getWoundDescription());
}


static class Character *
ctrl_get_interfering_hostile(class Character *character)
{
        static int dx_to_neighbor[] = { 0, -1, 0, 1 };
        static int dy_to_neighbor[] = { -1, 0, 1, 0 };
        class Character *near;
        int i;

        for (i = 0; i < 4; i++) {
                near = (class Character*)place_get_object(
                        character->getPlace(), 
                        character->getX() + dx_to_neighbor[i], 
                        character->getY() + dy_to_neighbor[i], 
                        being_layer);
                
                if (near &&
                    are_hostile(near, character) &&
                    !near->isIncapacitated()) {
                        return near;
                }
        }

        return NULL;
}

static void ctrl_attack_ui(class Character *character)
{
        int x;
        int y;
        class ArmsType *weapon;
        class Character *target;
        struct terrain *terrain;
        class Object *mech;

        // If in follow mode, when the leader attacks automatically switch to
        // turn-based mode.
        if (player_party->getPartyControlMode() == PARTY_CONTROL_FOLLOW &&
            player_party->getSize() > 1) {
                log_msg("Switching from Follow to Round Robin Mode.\n");
                player_party->enableRoundRobinMode();
        }

        // Loop over all readied weapons
        for (weapon = character->enumerateWeapons(); weapon != NULL; 
             weapon = character->getNextWeapon()) {
                
                // prompt the user
                cmdwin_clear();
                cmdwin_print("Attack-");

                if (weapon->isMissileWeapon()) {
                        // SAM: It would be nice to get ammo name, too...
                        cmdwin_print("%s (range %d, %d ammo)-", 
                                     weapon->getName(), weapon->getRange(), 
                                     character->hasAmmo(weapon));
                }
                else if (weapon->isThrownWeapon()) {
                        // SAM: It would be nice to get ammo name, too...
                        cmdwin_print("%s (range %d, %d left)-",
                                     weapon->getName(), weapon->getRange(), 
                                     character->hasAmmo(weapon));
                }
                else {
                        cmdwin_print("%s (reach %d)-", 
                                     weapon->getName(), weapon->getRange() );
                }


                // Check ammo
                if (!character->hasAmmo(weapon)) {
                        cmdwin_print("no ammo!\n");
                        log_msg("%s: %s - no ammo!\n",
                                     character->getName(),
                                     weapon->getName());
                        continue;
                }

                /* Get the target. It's important to do this every time the
                 * loop because the last iteration may have killed the previous
                 * target, or it may be out of range of the weapon. The
                 * getAttackTarget routine will reevaluate the current
                 * target. */
                target = character->getAttackTarget();


                /* Check the four adjacent tiles for hostiles who will
                 * interfere with a missile weapon */
                if (weapon->isMissileWeapon()) {
                        class Character *near;
                        near = ctrl_get_interfering_hostile(character);
                        if (near) {
                                cmdwin_print("blocked!");
                                log_msg("%s: %s - blocked by %s!\n",
                                             character->getName(),
                                             weapon->getName(),
                                             near->getName());
                                continue;
                        }
                }

                // select the target location
                x = target->getX();
                y = target->getY();


        prompt_for_target:
                // SAM:
                // select_target() might be a more elegant place to put
                // logic to prevent (or require confirm of) attacking self, 
                // party members, etc.
                if (select_target(character->getX(), character->getY(), &x, &y,
                                  weapon->getRange()) == -1) {
                        cmdwin_print("abort!");
                        continue;
                }

                // Find the new target under the cursor
                target = (class Character *) 
                        place_get_object(character->getPlace(), x, y, 
                                         being_layer);
                character->setAttackTarget(target);
                if (target == NULL) {

                        /* Attack the terrain */
                        terrain = place_get_terrain(character->getPlace(),
                                                    x, y);

                        log_begin("%s: %s - ", character->getName()
                                  , weapon->getName()
                                );

                        character->attackTerrain(x, y);
                        cmdwin_print("%s", terrain->name);

                        /* Check for a mech */
                        mech = place_get_object(character->getPlace(), x, y, 
                                                mech_layer);
                        if (mech) {
                                log_end("%s hit!", mech->getName());
                                mech->attack(character);
                        } else {
                                log_end("%s hit!", terrain->name);
                        }
                }
                else if (target == character) {

                        // ----------------------------------------------------
                        // Don't allow attacking self. This results in a nested
                        // enumerateArms() call when we call getDefend() on
                        // ourself, which messes up the loop we're in right
                        // now. If we really want to support suicide then we'll
                        // need to rework the enumeration code.
                        // ----------------------------------------------------

                        goto prompt_for_target;
                }               // confirm attack self
                else {
                        // confirmed_attack_ally:

                        // in combat all npc parties and the player party
                        // should be removed, so only characters reside at the
                        // being layer
                        assert(target->isType(CHARACTER_ID));

                        cmdwin_print("%s", target->getName());

                        // Strike the target
                        ctrl_do_attack(character, weapon, target);

                        // If we hit a party member then show their new hit
                        // points in the status window
                        if (target->isPlayerControlled())
                                statusRepaint();
                }

                // Warn the user if out of ammo
                if (NULL == character->getCurrentWeapon() ||
                    false == character->hasAmmo(character->getCurrentWeapon()))
                        log_msg("%s : %s now out of ammo)\n", 
                                     character->getName(), weapon->getName());

                // Once the player uses a weapon he can't cancel out of the
                // attack and continue his round with a different command.
                character->addExperience(XP_PER_ATTACK);
        }
}

static void ctrl_move_character(class Character *character, int dir)
{
        char *result = NULL;
        char *dirstr = directionToString(dir);
        
        switch (character->move(directionToDx(dir),
                                directionToDy(dir))) {
        case MovedOk:
                break;
        case OffMap:
                result = "no place to go!";
                break;
        case ExitedMap:
                result = "exit!";
                character->endTurn();
                break;
        case EngagedEnemy:
                cmdwin_print("enter combat!");
                break;
        case WasOccupied:
                result = "occupied!";
                break;
        case WasImpassable:
                result = "impassable!";
                break;
        case SlowProgress:
                result = "slow progress!";
                break;
        case SwitchedOccupants:
                result = "switch!";
                break;
        case NotFollowMode:
                result = "must be in follow mode!";
                break;
        case CantRendezvous:
                result = "party can't rendezvous!";
                break;
        case CouldNotSwitchOccupants:
                result = "can't switch places!";
                break;
        }

        /* If something interesting happened... */
        if (result) {

                /* Log unusual results to the console */
                log_msg("%s: %s - %s", character->getName(), dirstr,
                             result);
        }

        mapCenterView(character->getView(), character->getX(),
                      character->getY());
}

static int ctrl_character_key_handler(struct KeyHandler *kh, int key, 
                                       int keymod)
{
        extern int G_latency_start;
        int dir;
        class Character *character = (class Character *) kh->data;
        static int unshift[] = { KEY_NORTH, KEY_SOUTH, KEY_EAST, KEY_WEST };
        class Object *portal;
        class Character *solo_member;

        G_latency_start = SDL_GetTicks();


        /* First process commands which should not be affected by the keystroke
         * hooks. */
        switch (key) {

        case KEY_CTRL_Q:
                cmdQuickSave();
                break;

        case KEY_CTRL_R:
                cmdReload();
                return true;
                break;
      
        case KEY_CTRL_S:
                cmdSaveTerrainMap(character);
                break;

        case KEY_CTRL_T:
                cmdTerraform(character);
                break;

        case KEY_CTRL_Z:
                mapTogglePeering();
                break;

        default:
                break;
        }

        // Don't run the keystroke hook until we get here. Keystroke
        // effects should not affect the special ctrl charactes
        // (otherwise something like being stuck in a web can prevent a
        // user from reloading a game).
        character->runHook(OBJ_HOOK_KEYSTROKE);
        if (character->isTurnEnded())
                return true;


        switch (key) {

        case KEY_NORTH:
        case KEY_EAST:
        case KEY_SOUTH:
        case KEY_WEST:

                dir = keyToDirection(key);
                ctrl_move_character(character, dir);
                break;


        case KEY_SHIFT_NORTH:
        case KEY_SHIFT_EAST:
        case KEY_SHIFT_SOUTH:
        case KEY_SHIFT_WEST:

                // ----------------------------------------------------
                // Pan the camera.
                // ----------------------------------------------------
                        
                key = unshift[(key - KEY_SHIFT_NORTH)];
                dir = keyToDirection(key);
                mapMoveCamera(directionToDx(dir), directionToDy(dir));
                mapSetDirty();
                break;


        case 'a':
                ctrl_attack_ui(character);
                break;

        case 'c':
                cmdCastSpell(character);
                break;


        case 'e':

                // ----------------------------------------------------
                // Enter a portal. For this to work a portal must exist
                // here, the party must be in follow mode, and all the
                // party members must be able to rendezvous at this
                // character's position.
                // ----------------------------------------------------

                portal = place_get_object(character->getPlace(), 
                                          character->getX(), 
                                          character->getY(), 
                                          mech_layer);
                if (!portal || !portal->canEnter()) {
                        break;
                }
                        
                log_begin_group();
                portal->enter(character);
                log_end_group();

                break;


        case 'f':

                // ----------------------------------------------------
                // Toggle Follow mode on or off. When turning follow
                // mode off, set all party members to player
                // control. When turning it on, set all party member to
                // follow mode but set the leader to player control.
                // ----------------------------------------------------
                        
                log_begin("Follow mode ");
                if (player_party->getPartyControlMode() == 
                    PARTY_CONTROL_FOLLOW) {
                        log_end("OFF");
                        player_party->enableRoundRobinMode();
                } else {
                        log_end("ON");
                        player_party->enableFollowMode();
                }
                if (! character->isLeader())
                        character->endTurn();
                break;

        case 'g':
                cmdGet(character, 
                       !place_contains_hostiles(character->getPlace(), 
                                                character));
                break;
        case 'h':
                cmdHandle(character);
                break;
        case 'k':
                cmdCampInTown(character);
                break;
        case 'm':
                cmdMixReagents();
                break;
        case 'n':
                cmdNewOrder();
                break;
        case 'o':
                cmdOpen(character);
                break;
        case 'q':
                cmdQuit();
                break;
        case 'r':
                cmdReady(character);
                break;
        case 's':
                cmdSearch(character->getPlace(),
                          character->getX(),
                          character->getY());
                break;
        case 't':
                cmdTalk(character);
                break;
        case 'u':
                cmdUse(character, 0);
                break;
        case 'x':
                cmdXamine(character);
                break;
        case 'y':
                cmdYuse(character);
                break;
        case 'z':
                cmdZtats(character);
                break;
        case '@':
                cmdAT(character);
                break;
        case ' ':
                log_msg("Pass");
                character->endTurn();
                break;

        case SDLK_1:
        case SDLK_2:
        case SDLK_3:
        case SDLK_4:
        case SDLK_5:
        case SDLK_6:
        case SDLK_7:
        case SDLK_8:
        case SDLK_9:                        

                // ----------------------------------------------------
                // Put a character in solo mode.
                // ----------------------------------------------------
                        
                solo_member = 
                        player_party->getMemberAtIndex(key - SDLK_1);
                if (solo_member != NULL             &&
                    !solo_member->isIncapacitated() &&
                    solo_member->isOnMap()) {
                        player_party->enableSoloMode(solo_member);
                        character->endTurn();
                }
                break;

        case SDLK_0:
                // ----------------------------------------------------
                // Exit solo mode.
                // ----------------------------------------------------
                player_party->enableRoundRobinMode();
                character->endTurn();
                break;


        case '<':
                // ----------------------------------------------------
                // Quick exit from wilderness combat. The current place
                // must be the special wildernss combat place and it
                // must be empty of hostile characters or this fails.
                // ----------------------------------------------------

                if (!place_is_wilderness_combat(character->getPlace()))
                {
                        log_msg("Must use an exit!");
                        break;
                }

                if (place_contains_hostiles(character->getPlace(), 
                                            character))
                {
                        log_msg("Not while foes remain!");
                        break;
                }

                // ----------------------------------------------------
                // This next call is to make sure the "Victory" and
                // "Defeated" messages are printed properly. I don't
                // *think* it has any other interesting side-effects in
                // this case.
                // ----------------------------------------------------

                combat_analyze_results_of_last_turn();

                // ----------------------------------------------------
                // Remove all party members.
                // ----------------------------------------------------

                player_party->removeMembers();

                character->endTurn();

                break;

        default:
                break;
        }

        cmdwin_clear();

        if (!character->isTurnEnded()) {
                cmdwin_print("%s:", character->getName());
        }

        return character->isTurnEnded();
}

static void ctrl_pathfind_between_objects(class Object *source, 
                                          class Object *target)
{
        struct astar_node *path;
        struct astar_search_info as_info;

        memset(&as_info, 0, sizeof (as_info));
        as_info.x0 = source->getX();
        as_info.y0 = source->getY();
        as_info.x1 = target->getX();
        as_info.y1 = target->getY();
        as_info.flags = PFLAG_IGNORECOMPANIONS;
        path = place_find_path(source->getPlace(), &as_info, source);
        
        if (!path) {
                return;
        }
        
        if (path->next) {
                source->move(path->next->x - source->getX(), 
                             path->next->y - source->getY());
                
        }

        astar_path_destroy(path);                
}



static bool ctrl_attack_target(class Character *character, 
                               class Character *target)
{
        int distance;
        bool attacked = false;
        int hit;
        int def;
        int damage;
        int armor;
        bool miss;


        distance = place_flying_distance(character->getPlace(), 
                                         character->getX(), character->getY(), 
                                         target->getX(), target->getY());

        for (class ArmsType * weapon = character->enumerateWeapons(); 
             weapon != NULL; weapon = character->getNextWeapon()) {

                if (distance > weapon->getRange()) {
                        continue;
                }

                if (!character->hasAmmo(weapon)) {
                        continue;
                }

                if (distance <= 1 && weapon->isMissileWeapon()) {
                        // Handle missile weapon interference
                        continue;
                }

                ctrl_do_attack(character, weapon, target);
                attacked = true;
                statusRepaint();

                if (target->isDead())
                        break;

                if (character->getActionPoints() <= 0)
                        break;
        }

        if (character->needToRearm())
                character->armThyself();

        return attacked;
}

/* Data structure used by the ctrl_is_valid_target visitor function below */
struct ctrl_select_target_data {
        class Character *target;    /* best target found so far */
        class Character *attacker;  /* attacking character */
        int distance;               /* distance to 'target' */
};

/*****************************************************************************
 * ctrl_is_valid_target - check if 'obj' is a valid target for 'attacker'
 *****************************************************************************/
static int ctrl_is_valid_target(class Character *attacker, class Object *obj)
{
        /* Skip NULL */
        if (! obj)
                return 0;

        /* Skip non-characters */
        if (! obj->isType(CHARACTER_ID))
                return 0;

        /* Skip the attacker */
        if (obj == attacker)
                return 0;

        /* Skip dead beings */
        if (obj->isDead())
                return 0;

        /* Skip non-hostiles */
        if (! are_hostile(attacker, (Being*)obj))
                return 0;

        /* Skip non-visible objects */
        if (! attacker->canSee(obj))
                return 0;

        /* Skip off-map beings (is this even possible?) */
        if (! obj->isOnMap())
                return 0;

        return 1;
}

/*****************************************************************************
 * ctrl_select_target_visitor - find and remember the closest valid target for
 * an attacker
 *****************************************************************************/
static void ctrl_select_target_visitor(struct node *node, void *parm)
{
        class Object *obj;
        struct ctrl_select_target_data *data;
        int distance;

        /* Extract the typed variables from the generic parms */
        obj = (class Object*)node->ptr;
        data = (struct ctrl_select_target_data *)parm;

        /* Check if this object makes a valid target */
        if (! ctrl_is_valid_target(data->attacker, obj))
                return;

        /* Compute the distance from attacker to obj */
        distance = place_flying_distance(data->attacker->getPlace(), 
                                         data->attacker->getX(), 
                                         data->attacker->getY(), 
                                         obj->getX(), 
                                         obj->getY());

        /* Memoize the closest target and its distance */
        if (distance < data->distance) {
                data->distance = distance;
                data->target = (class Character*)obj;
        }
}

/*****************************************************************************
 * ctrl_select_target - use a heuristic to pick a target for an attacker
 *****************************************************************************/
static class Character * ctrl_select_target(class Character *character)
{
        struct ctrl_select_target_data data;

        /* Initialize the search data. */
        data.attacker = character;
        data.target = NULL;
        data.distance = place_max_distance(character->getPlace()) + 1;

        /* Search all objects in the current place for targets. */
        node_foldr(place_get_all_objects(character->getPlace()),
                   ctrl_select_target_visitor,
                   &data);

        /* Check if one was found */
        if (data.target) {
                character->setAttackTarget((class Character*)data.target);
                return data.target;
        }

        /* Try the old one */
        if (ctrl_is_valid_target(character,
                                 character->getAttackTarget()))
                return character->getAttackTarget();

        /* No valid targets */
        character->setAttackTarget(NULL);
        return NULL;
}


static void ctrl_idle(class Character *character)
{
        class Character *target;

        if (character->ai) {
                int time = SDL_GetTicks();
                closure_exec(character->ai, "p", character);
/*                 printf("%s ai: %d ms\n", character->getName(), */
/*                        SDL_GetTicks() - time); */
                return;
        }

        // -------------------------------------------------------------------
        // If they see an enemy they'll engage. Otherwise they just wander
        // uselessly within the rectangular area imposed by their schedule (or
        // freely if they have no schedule).
        // -------------------------------------------------------------------

        target = ctrl_select_target(character);
        if (!target) {
                ctrl_wander(character);
                return;
        }
        
        // -------------------------------------------------------------------
        // A bit confusing here next. If the NPC can't see a target I still let
        // them pathfind. Why? Because the target might be "remembered" - maybe
        // they were visible last turn and they just stepped out of LOS.
        // -------------------------------------------------------------------
        
        if (!character->canSee(target)) {
                ctrl_pathfind_between_objects(character, target);
                return;
        }
        
        // -------------------------------------------------------------------
        // Then try force.
        // -------------------------------------------------------------------

        if (!ctrl_attack_target(character, target))
                ctrl_pathfind_between_objects(character, target);
}

void ctrl_character_ai(class Character *character)
{
        // -------------------------------------------------------------------
        // Fleeing overrides the current activity (maybe it should be an
        // activity?)
        // -------------------------------------------------------------------

        if (character->isFleeing()) {
                character->flee();
                return;
        }
        
        // -------------------------------------------------------------------
        // What we do in auto mode depends on the character's "activity", which
        // is set by the schedule. Character's with no schedule are always
        // "idle", which means they wander aimlessly and act agressively toward
        // perceived enemies. Think of them as juvenile delinquents.
        // -------------------------------------------------------------------

        switch (character->getActivity()) {
        case COMMUTING:
                character->commute();
                break;
        case EATING:
                break;
        default:
                ctrl_idle(character);
                break;
        }
}

void ctrl_character_ui(class Character *character)
{
        struct KeyHandler kh;

        /* Setup cmdwin prompt for first entry to the control loop */
        cmdwin_clear();
        cmdwin_print("%s:", character->getName());

        /* Push the key handler and enter the event loop until character is
         * done with turn */
        kh.fx = &ctrl_character_key_handler;
        kh.data = character;
        eventPushKeyHandler(&kh);
        G_turnaround_stop = SDL_GetTicks();
/*         printf("*** turnaround: %d ms\n\n",  */
/*                G_turnaround_stop - G_turnaround_start); */
        eventHandle();
        G_turnaround_start = SDL_GetTicks();
        eventPopKeyHandler();
        mapUpdate(REPAINT_IF_DIRTY);
}


void ctrl_party_ai(class Party *party)
{
        switch (party->getActivity()) {
        case SLEEPING:
        case EATING:
                break;
        default:
        case WORKING:
                ctrl_work(party);
                break;
        }

}

void ctrl_party_ui(class player_party *party)
{
        struct KeyHandler kh;

        /* ready the cmdwin prompt */
        cmdwin_clear();

        kh.fx = &ctrl_party_key_handler;
        kh.data = party;
        eventPushKeyHandler(&kh);
        G_turnaround_stop = SDL_GetTicks();
/*         printf("turnaround: %d ms\n\n",  */
/*                G_turnaround_stop - G_turnaround_start); */
        eventHandle();
        G_turnaround_start = SDL_GetTicks();
        eventPopKeyHandler();
        mapUpdate(REPAINT_IF_DIRTY);
}
