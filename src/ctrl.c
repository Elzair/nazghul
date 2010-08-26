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

#include "../config.h" /* for USE_SKILLS */
#include "ctrl.h"
#include "dice.h"
#include "event.h"
#include "foogod.h"
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
#include "kern_intvar.h"
#include "nazghul.h"  // for DeveloperMode

#ifndef CONFIG_DIAGONAL_MOVEMENT
#define CONFIG_DIAGONAL_MOVEMENT 1
#endif

int G_latency_start = 0;
int G_turnaround_start = 0;
int G_turnaround  = 0;

static int ctrl_party_key_handler(struct KeyHandler *kh, int key, int keymod)
{
        int dir;

        class PlayerParty *party = (class PlayerParty*)kh->data;

        Session->subject = player_party;   
        
        G_latency_start = SDL_GetTicks();

        /* Commands which are only enabled in developer mode */
        if (DeveloperMode) {
	    switch (key) {
                        
                case KEY_CTRL_T:
		    cmd_terraform(party->getPlace(), party->getX(), party->getY());
		    break;

		case KEY_CTRL_O:
		    cmd_save_current_place(party->getPlace());
		    break;

                case KEY_CTRL_Z:
		    mapTogglePeering();
		    break;
	    }
        }

        switch (key) {
                
#if CONFIG_DIAGONAL_MOVEMENT
        case KEY_NORTHWEST:
        case KEY_NORTHEAST:
        case KEY_SOUTHWEST:
        case KEY_SOUTHEAST:
#endif   /* CONFIG_DIAGONAL_MOVEMENT */
        case KEY_NORTH:
        case KEY_WEST:
        case KEY_EAST:
        case KEY_SOUTH:
        {
                dir = keyToDirection(key);
                party->move(directionToDx(dir), directionToDy(dir));
                mapSetDirty();
                dir = keyToDirection(key);
        }
        break;

#if CONFIG_DIAGONAL_MOVEMENT
        case KEY_SHIFT_NORTHWEST:
        case KEY_SHIFT_NORTHEAST:
        case KEY_SHIFT_SOUTHWEST:
        case KEY_SHIFT_SOUTHEAST:
#endif   /* CONFIG_DIAGONAL_MOVEMENT */
        case KEY_SHIFT_NORTH:
        case KEY_SHIFT_EAST:
        case KEY_SHIFT_SOUTH:
        case KEY_SHIFT_WEST:

                // ----------------------------------------------------
                // Pan the camera.
                // ----------------------------------------------------
                        
                key &= ~KEY_SHIFT; /* clear shift bit */
                dir = keyToDirection(key);
                mapMoveCamera(directionToDx(dir), directionToDy(dir));
                mapSetDirty();
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
                cmdGet(party);
                break;
        case 'h':
                // SAM: Adding (H)andle command...
                cmdHandle(NULL);
                break;
        case 'k':
                cmd_camp_in_wilderness(party);
                break;
        case 'l':
                cmdLoiter(party);
                break;
        case 'm':
                cmdMixReagents(NULL);
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
                cmdSearch(0);
                break;
        case 't':
                cmdTalk(NULL);
                break;
        case 'u':
                cmdUse(NULL, CMD_SELECT_MEMBER|CMD_PRINT_MEMBER);
                break;
#ifdef USE_SKILLS
        case 'y':
                cmdYuse(NULL);
                break;
#endif
        case 'x':
                cmdXamine(party);
                break;
        case 'z':
                cmdZtats(party->getMemberAtIndex(0));
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
        case '?':
                cmdHelp();
                break;

        case KEY_CTRL_S:
                cmdSave();
                break;
                        
        case KEY_CTRL_R:
                cmdReload();
                Session->subject = NULL;
                return true;

        case SDLK_F10:
                cmdSettings();
                break;
                        
        default:
                break;
        } // switch(key)

        /* Prep cmdwin for next prompt */
        cmdwin_clear();

        Session->subject = NULL;
        
        party->absorbMemberAPDebt();

        /* Return true when done processing commands. */
		if(!party->isTurnEnded())
				cmdwin_push("Party [%d ap]:",party->getActionPoints());
                
        return party->isTurnEnded();

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

void ctrl_party_ai(class Party *party)
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
	if (party->getX() == player_party->getX() && 
            party->getY() == player_party->getY()) {
                
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

	/* get distance to player */
	d = place_walking_distance(party->getPlace(), party->getX(), 
                                   party->getY(),
				   player_party->getX(), player_party->getY());

        /* if adjacent attack */
        if (1==d) {
                int dx=0, dy=0;
                place_get_direction_vector(party->getPlace(),
                                           party->getX(), party->getY(),
                                           player_party->getX(), player_party->getY(),
                                           &dx, &dy);
                if (party->attackPlayer(dx, dy)) {
                        return;
                }
        }

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

static int ctrl_calc_to_hit(class Character *character, 
                            class ArmsType *weapon, 
                            int penalty)
{
        int base        = dice_roll("1d20");
        int weaponBonus = dice_roll(weapon->getToHitDice());
        int attackBonus = character->getAttackBonus(weapon);
        int val         = base + weaponBonus + attackBonus + penalty;

        log_continue("to-hit: %d=%d+%d+%d", val, base, weaponBonus, 
                     attackBonus);
        if (penalty >= 0) {
                log_continue("+");
        }
        log_continue("%d\n", penalty);

        return val;
}

static int ctrl_calc_to_defend(class Character *target)
{
        int base  = target->getDefend();
        int bonus = target->getAvoidBonus();
        int val   = base + bonus;

        log_continue("to-def: %d=%d+%d\n", val, base, bonus);
        
        return val;
}

static int ctrl_calc_damage(class Character *character, 
                            class ArmsType *weapon, 
                            char critical)
{
        int weaponDamage   = dice_roll(weapon->getDamageDice());
        int characterBonus = character->getDamageBonus(weapon);
        int memberBonus    = 0;
        int criticalBonus  = 0;

        if (character->isPlayerControlled()) {
                memberBonus = dice_roll("1d4");
        }

        if (critical) {
                criticalBonus = character->getDamageBonus(weapon);
        }

        int val = weaponDamage + characterBonus + criticalBonus + memberBonus;

        log_continue("damage: %d=%d+%d", val, weaponDamage, characterBonus);
        if (memberBonus) {
                log_continue("+%d", memberBonus);
        }
        if (criticalBonus) {
                log_continue("+%d", criticalBonus);
        }
        log_continue("\n");

        return val;
}

static int ctrl_calc_armor(class Character *target, int critical)
{
        int armor = 0;

        if (! critical) {
                armor = target->getArmor();
        }

        log_continue(" armor: %d\n", armor);
        
        return armor;
}

static void ctrl_attack_done(class Character *character, class ArmsType *weapon, 
                             class Character *target)
{
        character->runHook(OBJ_HOOK_ATTACK_DONE, "pp", weapon, target);
        character->useAmmo(weapon);
}

void ctrl_do_attack(class Character *character, class ArmsType *weapon, 
                    class Character *target, int to_hit_penalty)
{
        int hit;
        int def;
        int damage;
        int armor;
        int critical = 0;
		int misx;
		int misy;
        bool miss;

        /* Reduce the diplomacy rating between the attacker's and target's
         * factions */
        harm_relations(character, target);

        log_begin("^c%c%s^cw attacks ^c%c%s^cw with %s: "
                  , (are_hostile(character, player_party)?'r':'g')
                  , character->getName()
                  , (are_hostile(target, player_party)?'r':'g')
                  , target->getName()
                  , weapon->getName()
                );

        if (weapon->canOnAttack())
        {
	      	weapon->onAttack(NULL,character);  
     		}
                
        miss = ! weapon->fire(target, character->getX(), character->getY(), &misx, &misy);
        ctrl_attack_done(character, weapon, target);

        if (miss)
        {
				log_end("obstructed!");
				weapon->fireHitLoc(character, NULL, character->getPlace(),misx,misy,-1);
				return;
        }

        /* Roll to hit. */
        log_continue("\n");
        hit = ctrl_calc_to_hit(character, weapon, to_hit_penalty);
        def = ctrl_calc_to_defend(target);
        if (hit < def)
        {
				log_end("evaded!");
				weapon->fireHitLoc(character, NULL, character->getPlace(),misx,misy,-1);
				return;
        }

        /* roll for critical hit */
        if (20 <= (dice_roll("1d20") 
                   + logBase2(character->getBaseAttackBonus(weapon)))) {
                critical = 1;
                log_continue("^c+yCritical hit!^c-\n");
        }

        /* roll for damage */
        damage = ctrl_calc_damage(character, weapon, critical);
        armor = ctrl_calc_armor(target, critical);
        damage -= armor;
        damage = max(damage, 0);
        
        if (damage <= 0)
        {
				log_end("blocked!");
				weapon->fireHitLoc(character, target, character->getPlace(),misx,misy,0);
				return;
        }

        // the damage() method may destroy the target, so bump the refcount
        // since we still need the target through the end of this function
        obj_inc_ref(target);
        target->damage(damage);

			weapon->fireHitLoc(character, target, character->getPlace(),misx,misy,damage);

        log_end("%s!", target->getWoundDescription());

        /* If the target was killed then add xp to the attacker */
        if (target->isDead()) {
                character->addExperience(target->getExperienceValue());
        }

        obj_dec_ref(target);
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

struct nearest_hostile_info {
        class Character *origin;
        class Character *nearest;
        int min_distance;
        int range;
        struct list suggest; /* for ctrl_attack_ui */
};

static void ctrl_suggest_visitor(class Object *obj, void *data)
{
        struct nearest_hostile_info *info = (struct nearest_hostile_info*)data;
        class Character *npc = 0;
        int dist = 0;
        struct location_list *entry = 0;

        if (being_layer!=obj->getLayer())
                return;

        npc = (class Character*)obj;

        if (! are_hostile(npc, info->origin))
                return;

        if (! npc->isVisible() && ! Reveal)
                return;
        
        if (! place_in_los(info->origin->getPlace(),
                           info->origin->getX(),
                           info->origin->getY(),
                           info->origin->getPlace(),
                           obj->getX(),
                           obj->getY()))
                return;

        if (info->range < (dist = place_flying_distance(info->origin->getPlace(),
                                                        info->origin->getX(),
                                                        info->origin->getY(),
                                                        obj->getX(),
                                                        obj->getY())))
                return;

        /* Add it to the list */
        entry = (struct location_list*)malloc(sizeof(*entry));
        assert(entry);
        entry->x = obj->getX();
        entry->y = obj->getY();
        list_add_tail(&info->suggest, &entry->list);

        /* Keep track of the nearest as we go. */
        if (! info->nearest
            || (dist < info->min_distance)) {
                info->min_distance = dist;
                info->nearest = npc;
        }

        printf("Added %s at [%d %d]\n", obj->getName(), obj->getX(), obj->getY());        
}

static void ctrl_del_suggest_list(struct list *head)
{
        struct list *entry = head->next;
        while (entry != head) {
                struct location_list *tmp = 
                        (struct location_list*)entry;
                entry = entry->next;
                list_remove(&tmp->list);
                free(tmp);
        }
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
        int armsIndex=0;
	int this_is_nth_attack = 1;
        int this_wpn_AP;
        for (weapon = character->enumerateWeapons(&armsIndex); weapon != NULL; 
             weapon = character->getNextWeapon(&armsIndex)) {
	    struct nearest_hostile_info info;
                				
                // prompt the user
                cmdwin_clear();
                cmdwin_spush("Attack");

		// Determine AP for this (potential) attack,
		// as a discount may be applied for dual weapon attacks and such,
		// and we need the discounted figure to display in the UI:
                this_wpn_AP = weapon->getRequiredActionPoints();
		if (this_is_nth_attack ==  1) {
		    // 1st weapon attack (usual case), no AP cost adjustments
		}
		else if (this_is_nth_attack == 2) {
		    // 2nd weapon attack (dual weapon, 2nd weapon)
		    int mult = kern_intvar_get("AP_MULT12:second_wpn_attack");
		    this_wpn_AP = (int) (this_wpn_AP * mult) / 12;
		}
		else if (this_is_nth_attack >= 3) {
		    // 3rd+ weapon attack (unusual case for multi-limbed beings...)
		    int mult = kern_intvar_get("AP_MULT12:third_plus_wpn_attack");
		    this_wpn_AP = (int) (this_wpn_AP * mult) / 12;
		}

		//log_msg("DEBUG: wpn = %s (AP=%d-->%d), remaining AP=%d\n", 
		//	weapon->getName(), weapon->getRequiredActionPoints(), this_wpn_AP,
		//	character->getActionPoints() );

                if (weapon->isMissileWeapon()) {
                        // SAM: It would be nice to get ammo name, too...
                        cmdwin_spush("%s (%d AP, range %d, %d ammo)", 
                                     weapon->getName(), 
				     this_wpn_AP,
				     weapon->getRange(), 
                                     character->hasAmmo(weapon));
                }
                else if (weapon->isThrownWeapon()) {
                        // SAM: It would be nice to get ammo name, too...
                        cmdwin_spush("%s (%d AP, range %d, %d left)",
                                     weapon->getName(), 
				     this_wpn_AP,
				     weapon->getRange(), 
                                     character->hasAmmo(weapon));
                }
                else {
                        cmdwin_spush("%s (%d AP, reach %d)", 
                                     weapon->getName(), 
				     this_wpn_AP,
				     weapon->getRange() );
                }


                // Check ammo
                if (!character->hasAmmo(weapon)) {
                        cmdwin_spush("no ammo!");
                        log_msg("%s: %s - no ammo!\n",
                                     character->getName(),
                                     weapon->getName());
                        continue;
                }

                /* Check the four adjacent tiles for hostiles who will
                 * interfere with a missile weapon */
                if (weapon->isMissileWeapon()) {
                        class Character *near;
                        near = ctrl_get_interfering_hostile(character);
                        if (near) {
                                cmdwin_spush("blocked!");
                                log_msg("%s: %s - blocked by %s!\n",
                                             character->getName(),
                                             weapon->getName(),
                                             near->getName());
                                continue;
                        }
                }

                /* Build the list of suggested targets. */
                memset(&info, 0, sizeof(info));
                info.origin = character;
                info.range = weapon->getRange();
                list_init(&info.suggest);
                place_for_each_object(character->getPlace(),
                                      ctrl_suggest_visitor,
                                      &info);

                /* Get the default target. It's important to do this every time
                 * the loop because the last iteration may have killed the
                 * previous target, or it may be out of range of the
                 * weapon. The getAttackTarget routine will reevaluate the
                 * current target. */
                target = character->getAttackTarget(weapon);

                /* If the target is the character that means there is no
                 * default target now. Select the nearest one is the new
                 * default. */
                if (target==character) {
                        if (! list_empty(&info.suggest)) {
                                assert(info.nearest);
                                target = info.nearest;
                        }
                }

                assert(target);

                // select the target location
                x = target->getX();
                y = target->getY();

                // SAM:
                // select_target() might be a more elegant place to put
                // logic to prevent (or require confirm of) attacking self, 
                // party members, etc.
                if (-1 == select_target(character->getX(), 
                                        character->getY(), 
                                        &x, &y,
                                        weapon->getRange(),
                                        &info.suggest)) {
                        cmdwin_spush("abort!");
                        continue;
                }

                /* Cleanup the suggestion list */
                ctrl_del_suggest_list(&info.suggest);

                // Find the new target under the cursor
                target = (class Character *) 
                        place_get_object(character->getPlace(), x, y, 
                                         being_layer);
                character->setAttackTarget(target);
                if (target == NULL) {
						
                        /* Attack the terrain */
                        terrain = place_get_terrain(character->getPlace(),
                                                    x, y);

                        cmdwin_spush(" %s", terrain->name);                  
                                                    
                        log_begin("%s: %s - ", character->getName()
                                  , weapon->getName()
                                );
				
			int misx = x;
			int misy = y;
			
			this_is_nth_attack++;				
			
			        if (weapon->canOnAttack())
			        {
				      	weapon->onAttack(NULL,character);  
			     		}
     			
                        bool miss = ! weapon->fire(character->getPlace(), 
                                     character->getX(), 
                                     character->getY(), 
                                     &misx, 
                                     &misy);

                        if (miss)
                        {
	                        log_end("obstructed!");
                        }
                                     
                        weapon->fireHitLoc(character, NULL, character->getPlace(),misx,misy,-1);
                                     
                        ctrl_attack_done(character, weapon, NULL);

								if (!miss)
								{                     
	                        /* Check for a mech */
	                        mech = place_get_object(character->getPlace(), x, y, 
	                                                mech_layer);
	                        if (mech && mech->getName()) {
	                                log_end("%s hit!", mech->getName());
	                                mech->attack(character);
	                        } else {
	                                log_end("%s hit!", terrain->name);
	                        }
                        }
							
                }
                else if (target == character) {
                        /* Targeting the self is taken to mean "switch to my
                         * next weapon". This allows players to quickly jump to
                         * the next weapon if no target is in range and they
                         * aren't interested in attacking the ground. */
                        cmdwin_spush("skip weapon!");
			// no attack made, so don't increment this_is_nth_attack
                        continue;
                } else {
                        // confirmed_attack_ally:

                        // in combat all npc parties and the player party
                        // should be removed, so only characters reside at the
                        // being layer
                        assert(target->isType(CHARACTER_ID));

                        cmdwin_spush("%s", target->getName());


                        // If the npc is not hostile then get player confirmation.
                        if (! are_hostile(character, target)) {
                                int yesno;
                                cmdwin_spush("attack non-hostile");
                                cmdwin_spush("<y/n>");
                                getkey(&yesno, yesnokey);
                                cmdwin_pop();
                                if (yesno == 'n') {
                                        cmdwin_spush("no");
                                        continue;
                                }
                                cmdwin_spush("yes");
                        }

                        // Strike the target
			this_is_nth_attack++;
                        ctrl_do_attack(character, weapon, target, character->getToHitPenalty());

                        // If we hit a party member then show their new hit
                        // points in the status window
                        if (target->isPlayerControlled())
                                statusRepaint();
                }

                /* Warn the user if out of ammo. Originally this code used
                 * character->getCurrentWeapon() instead of weapon, that may
                 * still be okay now that getToHitPenalty() is outside of this
                 * loop, but not sure why it' would be preferred. */
                if (! character->hasAmmo(weapon))
                        log_msg("%s : %s now out of ammo\n", 
                                     character->getName(), weapon->getName());
                                
		character->decActionPoints(this_wpn_AP);
		//log_msg("DEBUG: after attack, used %d, remaining AP=%d\n", 
		//	this_wpn_AP, character->getActionPoints() );
        }
        
}

static void ctrl_move_character(class Character *character, int dir)
{
        enum MoveResult move_result;
        const char *result = NULL;
        const char *dirstr = directionToString(dir);
        int dx;
        int dy;

        dx = directionToDx(dir);
        dy = directionToDy(dir);

        /* try to move */
        move_result = character->move(dx, dy);

        /* recenter (harmless if move failed) */
        mapCenterView(character->getView(), character->getX(),
                      character->getY());

        /* if moved ok then no message to print */
        if (MovedOk == move_result) {
                return;
        }
        
        /* otherwise we'll print something */
        log_begin("");

        switch (move_result) {
        case OffMap:
                result = "no place to go!";
                break;
        case ExitedMap:
                result = "exit!";
                character->endTurn();
                break;
        case EngagedEnemy:
                cmdwin_spush("enter combat!");
                break;
        case WasOccupied:
                result = "occupied!";
                break;
        case WasImpassable:
        {
                /* If the move failed because something impassable is there
                 * then check for a mech and try to handle it. This is good
                 * enough to automatically open unlocked doors.
                 */

                class Object *mech;
                int newx, newy;
                
                newx = character->getX() + dx;
                newy = character->getY() + dy;
                
                mech = place_get_object(character->getPlace(), newx, newy, 
                                        mech_layer);
                
				if (mech && mech->getObjectType()->canHandle())
				{
					mech->getObjectType()->handle(mech, character);
					character->decActionPoints(kern_intvar_get("AP_COST:handle_mechanism"));
					mapSetDirty();
					result = "handled!";
				} 
				else if (mech && mech->getObjectType()->canBump())
				{
					result = "";
					mech->getObjectType()->bump(mech, character);
					mapSetDirty();			
				}
				else
				{                
					result = "impassable!";
				}
        }
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
        default:
                break;
        }

        log_continue("%s: %s - %s", character->getName(), dirstr, result);
        log_end("");
}

static int ctrl_character_key_handler(struct KeyHandler *kh, int key, 
                                       int keymod)
{
        extern int G_latency_start;
        int dir;

        class Character *character = (class Character *) kh->data;
        class Object *portal;

        G_latency_start = SDL_GetTicks();

        Session->subject = character;

        /* First process commands which should not be affected by the keystroke
         * hooks. */

        /* Commands which are only enabled in developer mode */
        if (DeveloperMode) {
                switch (key) {

                case KEY_CTRL_T:
                        cmd_terraform(character->getPlace(), character->getX(),
                                      character->getY());
                        break;
                        
		case KEY_CTRL_O:
                        cmd_save_current_place(character->getPlace() );
                        break;
                        
                case KEY_CTRL_Z:
                        mapTogglePeering();
                        break;
                        
                case KEY_CTRL_E:
                        cmdDeveloperEval(Session);
                        break;
                }
        }

        switch (key) {

        case KEY_CTRL_S:
                cmdSave();
                break;

        case KEY_CTRL_R:
                cmdReload();
                Session->subject = NULL;
                return true;
      
        case SDLK_F10:
                cmdSettings();
                break;
                        
        case 'f':
            if (cmdToggleFollowMode()) {
                if (! character->isLeader()) {
                    character->endTurn();
                }
            } else {
                foogod_set_title("Round Robin: %s", character->getName());
                foogodRepaint();
            }
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
            if (cmdSetSoloMode(key - SDLK_1)) {
                character->endTurn();                        
            }
            break;

        case SDLK_0:
                // ----------------------------------------------------
                // Exit solo mode.
                // ----------------------------------------------------
                player_party->enableRoundRobinMode();
                //in general, switching to roundrobin mode means
                //you need all the turns you gan get, so dont waste this one
                //character->endTurn();
                break;

        default:
                break;
        }

        // Don't run the keystroke hook until we get here. Keystroke
        // effects should not affect the special ctrl charactes
        // (otherwise something like being stuck in a web can prevent a
        // user from reloading a game).
        character->runHook(OBJ_HOOK_KEYSTROKE, 0);
        if (character->isTurnEnded()) {
                Session->subject = NULL;
                return true;
        }


        switch (key) {

#if CONFIG_DIAGONAL_MOVEMENT
        case KEY_NORTHWEST:
        case KEY_NORTHEAST:
        case KEY_SOUTHWEST:
        case KEY_SOUTHEAST:
#endif   /* CONFIG_DIAGONAL_MOVEMENT */
        case KEY_NORTH:
        case KEY_WEST:
        case KEY_EAST:
        case KEY_SOUTH:
                dir = keyToDirection(key);
                ctrl_move_character(character, dir);
                break;


#if CONFIG_DIAGONAL_MOVEMENT
        case KEY_SHIFT_NORTHWEST:
        case KEY_SHIFT_NORTHEAST:
        case KEY_SHIFT_SOUTHWEST:
        case KEY_SHIFT_SOUTHEAST:
#endif   /* CONFIG_DIAGONAL_MOVEMENT */
        case KEY_SHIFT_NORTH:
        case KEY_SHIFT_EAST:
        case KEY_SHIFT_SOUTH:
        case KEY_SHIFT_WEST:

                // ----------------------------------------------------
                // Pan the camera.
                // ----------------------------------------------------
                        
                key &= ~KEY_SHIFT; /* clear shift bit */
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

        case 'd':
                cmdDrop(character);
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


        case 'g':
                cmdGet(character);
                break;
        case 'h':
                cmdHandle(character);
                break;
        case 'k':
                cmd_camp_in_town(character);
                break;
        case 'l':
                cmdLoiter(character);
                break;
        case 'm':
                cmdMixReagents(character);
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
                if (player_party->getPartyControlMode()==PARTY_CONTROL_FOLLOW)
                        cmdReady(NULL);
                else
                        cmdReady(character);
                break;
        case 's':
                cmdSearch(character);
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
#ifdef USE_SKILLS
        case 'y':
                cmdYuse(character);
                break;
#endif
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
        case '?':
                cmdHelp();
                break;

	case SDLK_ESCAPE:
        // SAM: Removed the ESC-to-exit-combat keybinding.
        //      Using ESC for both exit-from-UI-things and exit-combat-map
        //      meant it was easy to exit, and miss your loot.
        //      
                if (!place_is_wilderness_combat(character->getPlace()))
                {
		    // log_msg("");
		    break;
                }

                if (place_contains_hostiles(character->getPlace(), character))
                {
		    // log_msg("");
		    break;
                }
		log_msg("To exit this combat map, use '<', \nor walk off the edge of the map.");
		break;

        case '<':
        // case SDLK_ESCAPE // SAM: Removed the ESC-to-exit-combat keybinding, see above.
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
                cmdwin_push("%s:", character->getName());
        }

        Session->subject = NULL;

        return character->isTurnEnded();
}

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

#if 0
static void ctrl_unready_all_weapons(class Character *character)
{
		int armsIndex = 0;
        for (class ArmsType * weapon = character->enumerateWeapons(&armsIndex); 
             weapon != NULL; weapon = character->getNextWeapon(&armsIndex)) {
                character->unready(weapon);
        }
}

static void ctrl_ready_melee_weapons(class Character *character)
{
        class Container *container = character->getContainer();
        struct inv_entry *ie;
}

static int ctrl_switch_to_melee_weapon(class Character *character)
{
        /* unready all weapons */
        ctrl_unready_all_weapons(character);

        /* ready any melee weapons */
        ctrl_ready_melee_weapons(character);

        /* ready any shields if hands are left open */

        /* ready any missile weapons if hands are left open */

}
#endif

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

		this_is_nth_attack++;
                ctrl_do_attack(character, weapon, target, character->getToHitPenalty());
                
                // sum up AP requirement
                this_wpn_AP = weapon->getRequiredActionPoints();
                if (this_wpn_AP > slowest_attack_AP)
                {
	              	slowest_attack_AP = this_wpn_AP;
                }
                total_AP += this_wpn_AP;
                attacked = true;

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
        
			if (!character->canSee(target))
			{
				if (! character->pathfindTo(target->getPlace(),
						target->getX(),
						target->getY()))
				{
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

void ctrl_character_ui(class Character *character)
{
        struct KeyHandler kh;
        /* Setup cmdwin prompt for first entry to the control loop */
        cmdwin_clear();
        cmdwin_push("%s:", character->getName());

        /* Push the key handler and enter the event loop until character is
         * done with turn */
        kh.fx = &ctrl_character_key_handler;
        kh.data = character;
        eventPushKeyHandler(&kh);
        G_turnaround = SDL_GetTicks() - G_turnaround_start;
        eventHandle();
        G_turnaround_start = SDL_GetTicks();
        eventPopKeyHandler();        
        mapUpdate(REPAINT_IF_DIRTY);
}

void ctrl_party_ui(class PlayerParty *party)
{
        struct KeyHandler kh;

        /* ready the cmdwin prompt */
        cmdwin_clear();
		  cmdwin_push("Party [%d ap]:",party->getActionPoints());
				
        kh.fx = &ctrl_party_key_handler;
        kh.data = party;
        eventPushKeyHandler(&kh);
        G_turnaround = SDL_GetTicks() - G_turnaround_start;
        eventHandle();
        G_turnaround_start = SDL_GetTicks();
        eventPopKeyHandler();
        mapUpdate(REPAINT_IF_DIRTY);
}
