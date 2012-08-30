/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2012 Gordon McNutt
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 *
 * Gordon McNutt
 * gmcnutt@users.sourceforge.net
 */

#include "ai.h"
#include "combat.h"
#include "ctrl.h"
#include "factions.h"
#include "kern_intvar.h"
#include "player.h"
#include "place.h"

static int ctrl_too_close_to_target(class Character *character, 
                                    class Character *target)
{
        int distance;

        distance = place_flying_distance(character->getPlace(), 
                                         character->getX(), character->getY(), 
                                         target->getX(), target->getY());

        if (distance > 1)
                return 0;

		int armsIndex=0;
        for (class ArmsType * weapon = character->enumerateWeapons(&armsIndex); 
             weapon != NULL; weapon = character->getNextWeapon(&armsIndex)) {

                /* if npc has at least one melee weapon then not too close */
                if (character->hasAmmo(weapon) &&
                    ! weapon->isMissileWeapon()) {
                        return 0;
                }
        }

        return 1;
}

static int ctrl_try_move_toward(class Character *character, int dx, int dy)
{
        int x = character->getX() + dx;
        int y = character->getY() + dy;
        
        if (place_is_passable(character->getPlace(), x, y, character, 0)) {
                switch (character->move(dx, dy)) {
                case MovedOk:
                case ExitedMap:
                case SwitchedOccupants:
                        return 1;
                default:
                        return 0;
                }
        }

        return 0;
}

static int ctrl_move_away_from_target(class Character *character, 
                                      class Character *target)
{
        /* first try moving directly away from the target */
        int dx = character->getX() - target->getX();
        int dy = character->getY() - target->getY();

        if (character->isStationary())
                return 0;

        /* normalize vector */
        dx = dx > 0 ? 1 : (dx < 0 ? -1 : 0);
        dy = dy > 0 ? 1 : (dy < 0 ? -1 : 0);

        /* disallow diagonal moves */
        if (dx == 0 || dy == 0) {
                if (ctrl_try_move_toward(character, dx, dy))
                        return 1;
        }

        /* try another vector */
        if (dx != 0) {
                if (ctrl_try_move_toward(character, 0, 1))
                        return 1;
                if (ctrl_try_move_toward(character, 0, -1))
                        return 1;
        }

        if (dy != 0) {
                if (ctrl_try_move_toward(character, 1, 0))
                        return 1;
                if (ctrl_try_move_toward(character, -1, 0))
                        return 1;
        }

        return 0;
}

/**
 * Called for AI only (not UI).
 */
static bool ctrl_attack_target(class Character *character, 
                               class Character *target)
{
        int distance;
        bool attacked = false;

        distance = place_flying_distance(character->getPlace(), 
                                         character->getX(), character->getY(), 
                                         target->getX(), target->getY());

	int armsIndex = 0;
	int this_is_nth_attack = 0;
	int slowest_attack_AP = 0;
	int total_AP = 0;
	int this_wpn_AP;

        for (class ArmsType * weapon = character->enumerateWeapons(&armsIndex);
             weapon != NULL; weapon = character->getNextWeapon(&armsIndex)) {

		// log_msg("DEBUG: wpn = %s (AP=%d), remaining AP=%d\n",
		//         weapon->getName(), weapon->getRequiredActionPoints(), character->getActionPoints() );
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

		if (weapon->obstructed(character->getPlace(), character->getX(), character->getY(), 
				       target->getX(), target->getY())) {
			continue;
		}

		this_is_nth_attack++;
                ctrl_do_attack(character, weapon, target, character->getToHitPenalty());
                
                // sum up AP requirement
                this_wpn_AP = weapon->getRequiredActionPoints();
                if (this_wpn_AP > slowest_attack_AP)
                {
	              	slowest_attack_AP = this_wpn_AP;
                }
                total_AP += this_wpn_AP;
                attacked = 1;

		if (this_is_nth_attack == 1) {
			// 1st weapon attack (usual case)
		}
		else if (this_is_nth_attack == 2) {
			// 2st weapon attack (dual weapon, 2nd weapon)
			int mult = kern_intvar_get("AP_MULT12:second_wpn_attack");
			this_wpn_AP = (int) (this_wpn_AP * mult) / 12;
		}
		else if (this_is_nth_attack >= 3) {
			// 3rd+ weapon attack (unusual case for multi-limbed beings...)
			int mult = kern_intvar_get("AP_MULT12:third_plus_wpn_attack");
			this_wpn_AP = (int) (this_wpn_AP * mult) / 12;
		}
		character->decActionPoints(this_wpn_AP);
		//log_msg("DEBUG: after attack, used %d, remaining AP=%d\n",
                //        this_wpn_AP, character->getActionPoints() );
                statusRepaint();

                if (target->isDead())
                        break;

		// If the AP use is not over the multi-weapon extra allowance, continue:
		int threshold = kern_intvar_get("AP_THRESHOLD:multi_attack_overage");
		if (character->getActionPoints() + threshold < 0) {
			//log_msg("DEBUG: AP = %d, threshold = %d -- breaking multi-attack\n",
			//    character->getActionPoints(), threshold );
			break;
		}
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
                                 character->getAttackTarget(NULL)))
                return character->getAttackTarget(NULL);

        /* No valid targets */
        character->setAttackTarget(NULL);
        return NULL;
}

void ctrl_wander(class Object *object)
{
        // *** left off here ***

	int dx = 0, dy = 0;

        if (object->isStationary())
                return;

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


static void ctrl_idle(class Character *character)
{
        class Character *target;

        if (character->getAI()) {

                /* closure returns true if it handled the turn, otherwise fall
                 * through to standard AI */
                if (closure_exec(character->getAI(), "p", character))
                        return;

                if (character->getActionPoints() <= 0)
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
        
        // note isOnMap checks so that if something has removed the character
        // from the map (or killed it) as a result of its move attempt then
        // you should quit having it wander around
        
	if (!character->canSee(target))	{
		if (! character->pathfindTo(target->getPlace(),
					    target->getX(),
					    target->getY())) {
			if (character->isOnMap())
			{
				ctrl_wander(character);
			}
			return;
		}
	}
			       
        if (ctrl_too_close_to_target(character, target)) {
                if (! ctrl_move_away_from_target(character, target))
                        /*if (! ctrl_switch_to_melee_weapon(character))*/
                        ctrl_wander(character);
                return;
        }

        // -------------------------------------------------------------------
        // Then try force.
        // -------------------------------------------------------------------

	if (!ctrl_attack_target(character, target))
	{
		if ((! character->pathfindTo(target->getPlace(),
					     target->getX(),
					     target->getY()))
		    && character->isOnMap())
		{
			ctrl_wander(character);
		}
	}
}

void ctrl_character_ai(class Character *character)
{
        if (character->isFleeing()
            && character->flee()) {
                return;
        }
        ctrl_idle(character);
}

void ctrl_party_ai(class Party *party)
{
	int d;

	dbg("%s:%s@%s[%d %d]\n", __FUNCTION__, party->getName(), party->getPlace()->name, party->getX(), party->getY());

        /* Check if this party is friendly to the player or if the player is
         * not around */
	if (! are_hostile(party, player_party)
#if ! CONFIG_CONCURRENT_WILDERNESS
            || Place != party->getPlace()
#endif
		) {
		// This party is friendly to the player, so just wander for now
		// (later I'll add schedules).
		ctrl_wander(party);
		return;
	}

	dbg("%s:player@%s[%d %d]\n", __FUNCTION__, player_party->getPlace()->name, player_party->getX(), player_party->getY());
	dbg("%s:Place=%s\n", __FUNCTION__, Place->name);

	struct place *player_place = player_party->getPlace();
	int target_x = 0;
	int target_y = 0;

	/* Check if the player party is in the same wilderness as this
	 * party. This will be true even if the player party is in a temporary
	 * wilderness combat map. */
	if (player_place == party->getPlace()) {
		target_x = player_party->getX();
		target_y = player_party->getY();
		dbg("%s:player in this place@[%d %d]\n", __FUNCTION__, target_x, target_y);
	} 

	/* Check if the player is in a town in this wilderness. */
	else if (place_get_parent(player_place) == party->getPlace()) {
		target_x = place_get_x(player_place);
		target_y = place_get_y(player_place);
		dbg("%s:player in sub-place@[%d %d]\n", __FUNCTION__, target_x, target_y);
	}

	/* Player is not accessible. */
	else {
		dbg("%s:player not around\n", __FUNCTION__);
		ctrl_wander(party);
		return;
	}

        /* Check if the player is _on this spot_. Yes, this can happen under
         * current game rules. If a player enters a portal and an npc is on the
         * destination then... */
	if (party->getX() == target_x && 
            party->getY() == target_y) {

		/* Now check if the player is already in a combat map or a
		 * town. If so, then join in, otherwise start a new combat
		 * map. */
		if (player_place != party->getPlace()) {
			combat_add_party_on_edge(party, party->getDx(), party->getDy(), player_place);
			return;
		}

                struct move_info info;
                struct combat_info cinfo;

                memset(&info, 0, sizeof(info));
                info.place = party->getPlace();
                info.x = party->getX();
                info.y = party->getY();
                info.dx = party->getDx();
                info.dy = party->getDy();
                info.px = target_x;
                info.py = target_y;
                info.npc_party = party;
                
                if (!info.dx && !info.dy)
                        info.dx = 1;
                else if (info.dx && info.dy)
                        info.dy = 0;

                memset(&cinfo, 0, sizeof(cinfo));
                cinfo.defend = 1;
                cinfo.move = &info;
                
                combat_enter(&cinfo);
                party->endTurn();
		return;
	}

	/* get distance to player */
	d = place_walking_distance(party->getPlace(), party->getX(), 
                                   party->getY(),
				   target_x, target_y);
	dbg("%s:%s:distance=%d\n", __FUNCTION__, party->getName(), d);

        /* if adjacent attack */
        if (1==d) {
                int dx=0, dy=0;
                place_get_direction_vector(party->getPlace(),
                                           party->getX(), party->getY(),
                                           target_x, target_y,
                                           &dx, &dy);
                if (party->attackPlayer(dx, dy)) {
                        return;
                }
        }

	if (d > party->getVisionRadius()) {
		ctrl_wander(party);
		return;
	}

	if (d > 1 
	    && (Place == party->getPlace()) /* don't attack towns or temp combat maps */
	    && party->attack_with_ordnance(d)) {
		return;
	}

	if (!party->gotoSpot(target_x, target_y)) {
		dbg("%s:%s:can't gotoSpot\n", __FUNCTION__, party->getName());
		ctrl_wander(party);
		return;
	}
}
