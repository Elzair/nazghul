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
#include "Portal.h"
#include "terrain.h"
#include "sched.h"
#include "session.h"

int G_latency_start = 0;


static int ctrl_party_key_handler(struct KeyHandler *kh, int key, int keymod)
{
        class player_party *party = (class player_party*)kh->data;

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
                        
                case 'q':
                        cmdQuickSave();
                        break;

                case 'r':
                        cmdReload();
                        return true;
                        break;
      
                case 's':
                        cmdSaveTerrainMap(NULL);
                        break;

                case 't':
                        cmdTerraform(NULL);
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
                case 'e':
                        // SAM:
                        // Perhaps this command should be merged with '>' ?
                        party->enter_portal();
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
                        cmdCamp(party);
                        party->endTurn();
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
                        cmdSearch(party->getX(), party->getY());
                        break;
                        /* To enable talking in party mode I need to prompt for
                         * a character to do the talking. */
/*                 case 't': */
/*                         cmdTalk(party->getX(), party->getY()); */
/*                         break; */
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
                        party->endTurn();
                        consolePrint("Pass\n");
                        break;
                case '>':
                        // This key was chosen to be a cognate for '>' in
                        // NetHack and other roguelike games.
                        cmdZoomIn();
                        party->endTurn();
                        break;
                default:
                        break;
                } // switch(key)
        } // !keymod

        /* Return true when done processing commands. */
        return party->isTurnEnded();
}

void ctrl_wander(class Object *object)
{
        // *** left off here ***

	int dx = 0, dy = 0;

	/* Roll for direction */
	dx = random() % 3 - 1;
	if (!dx)
		dy = random() % 3 - 1;

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

	if (party->getAlignment() & player_party->alignment) {
		// This party is friendly to the player, so just wander for now
		// (later I'll add schedules).
		ctrl_wander(party);
		return;
	}

	/* Check if the player is on this map */
	if (Place != party->getPlace()) {
		ctrl_wander(party);
		// Workaround: this is really to handle the case where
		// wandering can't consume any turns because the party is
		// stranded somewhere surrounded by impassable terrain.
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

void ctrl_do_attack(class Character *character, class ArmsType *weapon, class Character *target)
{
        int hit;
        int def;
        int damage;
        int armor;
        bool miss;

        consolePrint("%s attacks %s with %s...", character->getName(), target->getName(), 
                     weapon->getName());

        miss = ! weapon->fire(target, character->getX(), character->getY());
        character->decActionPoints(weapon->getRequiredActionPoints());
        character->useAmmo();

        if (miss) {
                consolePrint("miss!\n");
                return;
        }

        // Roll to hit.
        hit = dice_roll(weapon->getToHitDice());
        def = target->getDefend();
        if (hit < def) {
                consolePrint("barely scratched!\n");
                return;
        } else {
                consolePrint("hit! ");
        }

        // roll for damage
        damage = dice_roll(weapon->getDamageDice());
        armor = target->getArmor();
        consolePrint("Rolled %d damage, %d armor ", damage, armor);
        damage -= armor;
        damage = max(damage, 0);
        consolePrint("for %d total damage, ", damage);
        target->damage(damage);

        consolePrint("%s!\n", target->getWoundDescription());
}


static void ctrl_attack_ui(class Character *character)
{
        int x;
        int y;
        int i;
        class ArmsType *weapon;
        class Character *near;
        class Character *target;
        struct terrain *terrain;
        class Object *mech;
        int dx_to_neighbor[] = { 0, -1, 0, 1 };
        int dy_to_neighbor[] = { -1, 0, 1, 0 };

        consolePrint("Attack!\n");

        // If in follow mode, when the leader attacks automatically switch to
        // turn-based mode.
        if (player_party->getPartyControlMode() == PARTY_CONTROL_FOLLOW) {
                consolePrint("Switching from follow to combat mode\n");
                player_party->enableRoundRobinMode();
        }

        // Loop over all readied weapons
        for (weapon = character->enumerateWeapons(); weapon != NULL; weapon = character->getNextWeapon()) {

                cmdwin_clear();
                cmdwin_print("%s:", character->getName());
                consolePrint("[%s]: ", weapon->getName());
                consoleRepaint();

                // Check ammo
                if (!character->hasAmmo(weapon)) {
                        consolePrint("no ammo!\n");
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
                        for (i = 0; i < 4; i++) {
                                near = (class Character*)place_get_object(character->getPlace(), 
                                                                          character->getX() + dx_to_neighbor[i], 
                                                                          character->getY() + dy_to_neighbor[i], 
                                                                          being_layer);
                                if (near &&
                                    near->isHostile(character->getAlignment()) &&
                                    !near->isIncapacitated()) {
                                        consolePrint("%s interferes!\n", near->getName());
                                        return;
                                }
                        }

                }

                // prompt the user
                cmdwin_clear();
                if (weapon->isMissileWeapon()) {
                        // SAM: It would be nice to get ammo name, too...
                        cmdwin_print("Attack-Fire %s (range %d, %d ammo)-", weapon->getName(), weapon->getRange(), character->hasAmmo(weapon));
                }
                else if (weapon->isThrownWeapon()) {
                        // SAM: It would be nice to get ammo name, too...
                        cmdwin_print("Attack-Throw %s (range %d, %d left)-", weapon->getName(), weapon->getRange(), character->hasAmmo(weapon));
                }
                else {
                        cmdwin_print("Attack-With %s (reach %d)-", weapon->getName(), weapon->getRange() );
                }

                // select the target location
                x = target->getX();
                y = target->getY();


        prompt_for_target:
                // SAM:
                // select_target() might be a more elegant place to put
                // logic to prevent (or require confirm of) attacking self, 
                // party members, etc.
                if (select_target(character->getX(), character->getY(), &x, &y, weapon->getRange()) == -1) {
                        consolePrint("skip\n");
                        continue;
                }

                // Find the new target under the cursor
                target = (class Character *) place_get_object(character->getPlace(), x, y, being_layer);
                character->setAttackTarget(target);
                if (target == NULL) {

                        /* Attack the terrain */
                        terrain = placeGetTerrain(x, y);
                        character->attackTerrain(x, y);
                        cmdwin_print("%s", terrain->name);
                        consolePrint("%s\n", terrain->name);

                        /* Check for a mech */
                        mech = place_get_object(character->getPlace(), x, y, 
                                                mech_layer);
                        if (mech)
                                mech->attack(character);
                }
                else if (target == character) {

                        // -----------------------------------------------------
                        // Don't allow attacking self. This results in a nested
                        // enumerateArms() call when we call getDefend() on
                        // ourself, which messes up the loop we're in right
                        // now. If we really want to support suicide then we'll
                        // need to rework the enumeration code.
                        // -----------------------------------------------------

                        goto prompt_for_target;
                }               // confirm attack self
                else {
                        // confirmed_attack_ally:

                        // in combat all npc parties and the player party
                        // should be removed, so only characters reside at the
                        // being layer
                        assert(target->isType(CHARACTER_ID));

                        cmdwin_print("%s", target->getName());                        
                        consolePrint("attack %s...", target->getName());

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
                        consolePrint("(%s now out of ammo)\n", weapon->getName());

                // Once the player uses a weapon he can't cancel out of the
                // attack and continue his round with a different command.
                character->addExperience(XP_PER_ATTACK);
        }
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

        // -------------------------------------------------------------------
        // Process the special CTRL commands
        // -------------------------------------------------------------------

        if (keymod == KMOD_LCTRL || keymod == KMOD_RCTRL) {

                switch (key) {
      
                case 'q':
                        cmdQuickSave();
                        break;

                case 'r':
                        cmdReload();
                        return true;
                        break;
      
                case 't':
                        cmdTerraform(character);
                        break;

                case 's':
                        cmdSaveTerrainMap(character);
                        break;

                case 'z':
                        mapTogglePeering();
                        break;

                default:
                        break;
                }
        }

        // -------------------------------------------------------------------
        // Process normal commands.
        // -------------------------------------------------------------------

        else {
                switch (key) {

                case KEY_NORTH:
                case KEY_EAST:
                case KEY_SOUTH:
                case KEY_WEST:

                        // ----------------------------------------------------
                        // Move the character.
                        // ----------------------------------------------------

                        dir = keyToDirection(key);

                        consolePrint("%s-", directionToString(dir));

                        switch (character->move(directionToDx(dir),
                                                directionToDy(dir))) {
                        case MovedOk:
                                consolePrint("Ok\n");
                                break;
                        case OffMap:
                                consolePrint("No place to go!\n");
                                break;
                        case ExitedMap:
                                consolePrint("Exit!\n");
                                character->endTurn();
                                break;
                        case EngagedEnemy:
                                break;
                        case WasOccupied:
                                consolePrint("Occupied!\n");
                                break;
                        case WasImpassable:
                                consolePrint("Impassable!\n");
                                break;
                        case SlowProgress:
                                consolePrint("Slow progress!\n");
                                break;
                        case SwitchedOccupants:
                                consolePrint("Switch!\n");
                                break;
                        case NotFollowMode:
                                consolePrint("Must be in Follow Mode!\n");
                                break;
                        case CantRendezvous:
                                consolePrint("Party can't rendezvous!\n");
                                break;
			case CouldNotSwitchOccupants:
			  consolePrint("Can't switch!\n");
			  break;
                        }

                        mapCenterView(character->getView(), character->getX(),
                                      character->getY());
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

                        cmdwin_clear();
                        cmdwin_print("Enter-");
                        
                        portal = place_get_object(character->getPlace(), 
                                                  character->getX(), 
                                                  character->getY(), 
                                                  mech_layer);
                        if (!portal || !portal->canEnter()) {
                                cmdwin_print("Nothing!");
                                break;;
                        }
                        
                        portal->enter(character);
                        break;


                case 'f':

                        // ----------------------------------------------------
                        // Toggle Follow mode on or off. When turning follow
                        // mode off, set all party members to player
                        // control. When turning it on, set all party member to
                        // follow mode but set the leader to player control.
                        // ----------------------------------------------------
                        
                        consolePrint("Follow mode ");
                        if (player_party->getPartyControlMode() == 
                            PARTY_CONTROL_FOLLOW) {
                                consolePrint("OFF\n");
                                player_party->enableRoundRobinMode();
                        } else {
                                consolePrint("ON\n");
                                player_party->enableFollowMode();
                        }
                        character->endTurn();
                        break;

                case 'g':
                        cmdGet(character, 
                               !place_contains_hostiles(
                                       character->getPlace(), 
                                       character->getAlignment()));
                        break;
                case 'h':
                        cmdHandle(character);
                        break;
                case 'k':
                        cmdCamp(character);
                        break;
                case 'o':
                        cmdOpen(character);
                        break;
                case 'q':
                        cmdQuit();
                        break;
                case 'r':
                        cmdReady(character, 0);
                        break;
                case 't':
                        cmdTalk(character);
                        break;
                case 'u':
                        cmdUse(character, 0);
                        break;
                case 'x':
                        consolePrint("examines around\n");
                        cmdXamine(character);
                        break;
                case 'y':
                        cmdYuse(character);
                case 'z':
                        consolePrint("show status\n");
                        cmdZtats(character);
                        break;
                case '@':
                        consolePrint("skylarks a bit");
                        cmdAT(character);
                        break;
                case ' ':
                        cmdwin_print("Pass");
                        consolePrint("Pass\n");
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

                        if (!place_is_wilderness_combat(character->getPlace())) {
                                consolePrint("Must use an exit!\n");
                                consolePrint("%s: ", character->getName());
                                consoleRepaint();
                                break;
                        }

                        if (place_contains_hostiles(character->getPlace(), character->getAlignment())) {
                                consolePrint("Not while foes remain!\n");
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

        }

        return character->isTurnEnded();
}

static void ctrl_enchant_target(class Character *character, 
                                class Character *target)
{
        struct spell *spell;
        int i;
        int distance;

        if (MagicNegated)
                return;

        distance = place_flying_distance(character->getPlace(), 
                                         character->getX(), character->getY(),
                                         target->getX(), target->getY());

        // Enumerate all the known spells for this species
        for (i = 0; i < character->species->n_spells; i++) {

                spell = magic_lookup_spell(&Session->magic, 
                                           character->species->spells[i]);
                assert(spell);

                // Check if the THIS has enough mana
                if (spell->cost > character->getMana()) {
                        continue;
                }

                // Check if the nearest is in range or if the range does not
                // matter for this spell type
                if ((spell->flags & SPELL_RANGE_LIMITED) &&
                    (distance > spell->range)) {
                        continue;
                }

                // Cast the spell
                // gmcnutt: for now use the caster's coordinates, only the
                // summoning spells currently use them.
                consolePrint("%s casts %s on %s.\n", character->getName(), 
                             spell->type->getName(), target->getName());
#if 0
                // revisit
                spell->cast(character, target, 0, character->getX(), 
                            character->getY());
#endif
                character->decActionPoints(spell->action_points);
                if (character->getActionPoints() <= 0)
                        return;
        }

        return;
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
        path = place_find_path(source->getPlace(), &as_info, source->getPmask(), source);
        
        if (!path) {
                return;
        }
        
        if (path->next) {
                source->move(path->next->x - source->getX(), path->next->y - source->getY());
                
        }

        astar_path_destroy(path);                
}



static bool ctrl_attack_target(class Character *character, class Character *target)
{
        int distance;
        bool attacked = false;
        int hit;
        int def;
        int damage;
        int armor;
        bool miss;


        distance = place_flying_distance(character->getPlace(), character->getX(), character->getY(), target->getX(), target->getY());

        for (class ArmsType * weapon = character->enumerateWeapons(); weapon != NULL; weapon = character->getNextWeapon()) {

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

static class Character * ctrl_select_target(class Character *character)
{
        struct list *head;
        struct list *elem;
        class Object *obj;
        class Character *new_target = NULL;
        int min_distance = 1000;
        int distance;

        /* Get a list of all the objects within vision radius. */
        head = place_get_all_objects(character->getPlace());

        /* Walk the list, looking for the nearest hostile character. */
        list_for_each(head, elem) {
                
                obj = outcast(elem, class Object, turn_list);

                /* Skip invalid targets */
                if (obj == character ||
                    !obj->isOnMap() ||
                    obj->isDead() ||
                    !obj->isType(CHARACTER_ID) ||
                    !obj->isHostile(character->getAlignment()) ||
                    !character->canSee(obj))
                        continue;

                /* Remember the closest target */
                distance = place_flying_distance(character->getPlace(), 
                                                 character->getX(), 
                                                 character->getY(), 
                                                 obj->getX(), obj->getY());
                if (distance < min_distance) {
                        min_distance = distance;
                        new_target = (class Character*)obj;
                }
        }

        if (new_target) {
                character->setAttackTarget((class Character*)new_target);
                return new_target;
        }

        // use the old one
        new_target = character->getAttackTarget();

        if (new_target &&
            new_target->isHostile(character->getAlignment()) &&
            new_target != character &&
            new_target->isOnMap() &&
            !new_target->isDead() && 
            new_target->isVisible())
                return new_target;

        // old one invalid now
        character->setAttackTarget(NULL);
        return NULL;
}

static void ctrl_idle(class Character *character)
{
        class Character *target;

        if (character->ai) {
                closure_exec(character->ai, "p", character);
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
        // First try magic.
        // -------------------------------------------------------------------

        ctrl_enchant_target(character, target);
        if (character->isTurnEnded())
                return;
        
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
        kh.fx = &ctrl_character_key_handler;
        kh.data = character;
        eventPushKeyHandler(&kh);
        eventHandle();
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

        
        kh.fx = &ctrl_party_key_handler;
        kh.data = party;
        eventPushKeyHandler(&kh);
        eventHandle();
        eventPopKeyHandler();
        mapUpdate(REPAINT_IF_DIRTY);

}
