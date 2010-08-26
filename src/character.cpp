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
#include "character.h"
#include "conv.h"
#include "dice.h"
#include "effect.h"
#include "gob.h"
#include "map.h"
#include "console.h"
#include "place.h"
#include "Container.h"
#include "Arms.h"
#include "player.h"
#include "status.h"
#include "sound.h"
#include "common.h"
#include "screen.h"
#include "knapsack.h"
#include "occ.h"
#include "species.h"
#include "sched.h"
#include "combat.h"
#include "cmdwin.h"
#include "terrain.h"
#include "cmd.h"
#include "event.h"
#include "vehicle.h"
#include "foogod.h"
#include "ctrl.h"
#include "session.h"
#include "sprite.h"
#include "mmode.h"
#include "log.h"
#include "factions.h"
#include "kern_intvar.h"

#include <string.h>
#include <stdlib.h>
#include <math.h>

/**
 * Used when calculating an evasion vector away from hostiles.
 */
struct evasionVectorInfo {
        class Character *subj; /**< The character doing the evading. */
        int dx;          /**< The x component of the evasion vector. */
        int dy;          /**< The y component of the evasion vector. */
};

static int wrapReady(void *item, void *context)
{
	class ArmsType *type = (class ArmsType *) item;
	class Character *npc = (class Character *) context;
	return (npc->hasAmmo(type) && npc->ready(type) == Character::Readied);
}

static void wrapUnready(void *item, void *context)
{
	class ArmsType *type = (class ArmsType *) item;
	class Character *npc = (class Character *) context;
	npc->unready(type);

}

static void myConsiderArms(struct inv_entry *ie, void *data)
{
	struct knapsack *ks = (struct knapsack *) data;
	if (!ie->type->isType(ARMS_TYPE_ID))
		return;
	class ArmsType *arms = (class ArmsType *) ie->type;
	int val = 2 * dice_average(arms->getDamageDice()) * arms->getRange()
                + 2 * dice_average(arms->getArmorDice())
                + 2 * dice_average(arms->getToDefendDice())
                + dice_average(arms->getToHitDice())
                ;
        if (val) {
                for (int i = 0; 
                     i < ie->count && ks->n_items < MAX_N_ITEMS; 
                     i++, ks->n_items++) {
                        ks->item[ks->n_items] = arms;
                        ks->value[ks->n_items] = val;
                }
        }
}

/**
 * Accumulate the sum of the vectors from hostile foes. This is passed as an
 * argument to place_for_each() to scan for hostiles. It sums up the vectors
 * from hostile foes to the subject in order to calculate the best vector to
 * flee.
 *
 * @param obj An object in the current place.
 * @param data The evasion vector struct which refers to the calling object and
 * accumulates results.
 */
static void getEvasionVectorVisitor(Object *obj, void *data)
{
        struct evasionVectorInfo *info;
        class Character *subj;

        info = (struct evasionVectorInfo *)data;
        subj = info->subj;

        /* Filter out non-beings */
        if (obj->getLayer() != being_layer)
                return;

        /* Filter out non-hostiles */
        if (! are_hostile(subj, (class Being*)obj))
                return;

#if 0
        /* Cheat a bit here, both for performance and to increase the
         * effectiveness of townsfolk trying to flee raiders. */

        /* Filter out objects not in los of the subject */
        if (! place_in_los(subj->getPlace(),subj->getX(),subj->getY(),
                                   obj->getPlace(),obj->getX(),obj->getY()))
                return;

        /* Filter out object not in the vision radius of the subject */
        if (place_flying_distance(subj->getPlace(),subj->getX(),subj->getY(),
                                  obj->getX(),obj->getY())
            > subj->getVisionRadius())
                return;

        /* Filter out invisible objects */
        if (! obj->isVisible())
                return;
#endif
        /* Add the vector going from the hostile to the subject to the
         * cumulative vector. */
        info->dx += (subj->getX() - obj->getX());
        info->dy += (subj->getX() - obj->getX());
}

Character::Character(const char *tag, const char *name, 
                     struct sprite *sprite, 
                     struct species *species, struct occ *occ, 
                     int str, int intl, int dex, 
                     int hpmod, int hpmult, 
                     int mpmod, int mpmult, 
                     int hp, int xp_, 
                     int mp, 
		     int AP_per_round,  // aka speed
		     int lvl
                     )
        : hm(0), xp(xp_), order(-1),
          sleeping(false),
          ac(0), 
          str(str), intl(intl),
          dex(dex), mana(mp), lvl(lvl),
          solo(false), target(NULL),
          rdyArms(NULL),
          fleeing(false), burden(0),
          inCombat(false),
          container(NULL), 
          //sprite(sprite),
          sched_chars_node(0),
          forceContainerDrop(false)
          , fleePathFound(false)
          , fleeX(0)
          , fleeY(0)
          , fleePathFlags(0)
          , currentMmode(0)
          , known(false)
          , taskname(NULL)
          , taskproc(NULL)
          , taskgob(NULL)
          , taskInterruptOnDamage(false)
{
        if (tag) {
                this->tag = strdup(tag);
                assert(this->tag);
        } else {
                this->tag = NULL;
        }

        setName(name);

        plnode = NULL;
	setPlayerControlled(false);	// by default
	this->current_sprite = sprite;
	this->light        = MIN_PLAYER_LIGHT;
	this->party        = 0;
	this->conv         = conv;
	this->species      = species;
	this->occ          = occ;
        if (occ)
                occ_ref(occ);
	this->is_clone     = false;
	this->visible      = 1;
	this->target       = 0;
        this->damage_sound = NULL_SOUND;
        this->charmed      = false;
        this->resting      = false;
        this->loitering    = false;
        this->guarding     = false;
        this->mp_mod       = mpmod;
        this->mp_mult      = mpmult;
        this->hp_mod       = hpmod;
        this->hp_mult      = hpmult;
        this->sched        = NULL;
        this->appt         = 0;
        this->is_leader    = false;
        this->hp           = hp;        
        this->defenseBonus = 0;
        factionSwitch      = 0;
        tmpFaction         = NIL_FACTION;
        ambushedWhileCamping = false;

        setDead(hp <= 0);

        setActivity(NONE);


        initCommon();

	this->hp = min(this->hp, getMaxHp());
	this->mana = min(this->mana, getMaxMana());
	this->AP_per_round = AP_per_round;

        setOnMap(false);
	if (xp == 0)
		this->xp = getXpForLevel(this->lvl);
	if (xp < 0)
	{
		if (this->lvl == 1)
		{
			this->xp = rand() % getXpForLevel(this->lvl + 1);
		}
		else
		{
			this->xp = getXpForLevel(this->lvl) + (rand() % getXpForLevel(this->lvl));
		}
	}
}

Character::Character():hm(0), xp(0), order(-1),
                       sleeping(false),
                       ac(0), 
                       str(0), intl(0),
                       dex(0), mana(0),
		       AP_per_round(kern_intvar_get("AP_TOTAL:normal_human")),
		       lvl(0),
                       playerControlled(true), solo(false),
                       target(NULL),
                       rdyArms(NULL),
                       fleeing(false), burden(0),
                       inCombat(false),
                       container(NULL), 
                       //sprite(0),
                       sched_chars_node(0),
                       forceContainerDrop(false)
                       , fleePathFound(false)
                       , fleeX(0)
                       , fleeY(0)
                       , fleePathFlags(0)
                       , currentMmode(0)
                       , known(false)
                       , taskname(NULL)
                       , taskproc(NULL)
                       , taskgob(NULL)
{
        // This method is probably obsolete now

        plnode = NULL;
	setPlayerControlled(false);	// by default
        setBaseFaction(NIL_FACTION);

	light        = MIN_PLAYER_LIGHT;
	tag          = 0;
	party        = 0;
	conv         = 0;
	species      = 0;
	occ          = 0;
	is_clone     = false;
	visible      = 1;
	occ          = 0;
	target       = 0;
        damage_sound = NULL_SOUND;
        charmed      = false;
        resting      = false;
        loitering    = false;
        guarding     = false;
        mp_mod       = 0;
        mp_mult      = 0;
        hp_mod       = 0;
        hp_mult      = 0;
        sched        = NULL;
        appt         = 0;
        is_leader    = false;
        factionSwitch= 0;
        tmpFaction   = NIL_FACTION;
        ambushedWhileCamping = false;

	AP_per_round = AP_per_round;

        setDead(hp <= 0);

        //assert(place);

        setActivity(NONE);

        // ------------------------------------------------------------------
        // Initially always off-map. Loader will position us, put us in a
        // party, or whatever.
        // ------------------------------------------------------------------

        setPlace(0);
        setX(-1);
        setY(-1);
        setOnMap(false);
}


Character::~Character()
{
        obj_dec_ref_safe(container);

	if (rdyArms != NULL) {
		free(rdyArms);
        }


        if (party)
                party->removeMember(this);

        if (ai)
                closure_unref(ai);

        /* Hack: make sure you deref occ after deleting the
         * container. Currently the container references traps that are built
         * into the occ struct. The container unrefs the traps, and if the occ
         * isn't still holding a ref then you get a double-deallocation and
         * usually a crash. This will get cleaned up when I rework the way NPCs
         * are factoried; the traps won't be kept as part of occ any more. */
        if (occ)
                occ_unref(occ);

        // subtle: use setAttackTarget to unref the target; it will do so
        // safely even if 'this' is the target
        if (target)
                setAttackTarget(NULL);

        /* remove self from session's special list for chars with multi-place
         * schedules */
        if (sched_chars_node)
                session_rm_sched_char(sched_chars_node);
}

static int yes_no_ignore(struct KeyHandler * kh, int key, int keymod)
{
	int *yesno = (int *) kh->data;

	switch (key) {
	case 'y':
	case 'Y':
		*yesno = 'y';
		return 1;
        case 'i':
        case 'I':
            *yesno = 'i';
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

void Character::taskPromptToAbort()
{
    cmdwin_clear();
    log_continue("^c+y%s damaged - abort task?^c-\n", getName());
    log_flush();
    cmdwin_spush("Abort");
    cmdwin_spush("(Y/N/Ignore)");
    int key;
    getkey(&key, yes_no_ignore);
    cmdwin_pop();
    switch (key) {
    case 'y':
        taskAbort();
        cmdwin_spush("abort!");
        break;
    case 'i':
        taskInterruptOnDamage = false;
        cmdwin_spush("ignore");
        break;
    case 'n':
    default:
        cmdwin_spush("no");
        break;
    }
}

void Character::damage(int amount)
{
        if (hp <= 0)
                return;

        // This will run the "on-damage-hook":
        Object::damage(amount);

        // setHP() might call kill(), which calls remove(), which will destroy
        // most objects
        obj_inc_ref(this);
        setHp(hp - amount);

	if (isPlayerControlled()) {
            statusFlash(getOrder(), Red);
            if (!isDead()
                   && engagedInTask() 
                && taskInterruptOnDamage
                ) {
                taskPromptToAbort();
            }
	} else {
            if (!isDead() && inCombat && (getHp() < (getMaxHp() / 4))) {
                setFleeing(true);
            }
	}
        obj_dec_ref(this);
}

void Character::inflictDamage(int amount, class Character *attacker)
{
	bool mightkill = false;
	if (!isDead())
	{
		mightkill = true;
		harm_relations(attacker, this);
	}
	obj_inc_ref(this);
	damage(amount);
	if (mightkill && isDead())
	{
		attacker->addExperience(getExperienceValue());
	}
	obj_dec_ref(this);
}

enum Character::ReadyResult Character::ready(class ArmsType * arms)
{
	bool foundSlotType = false;
	int slot = arms->getSlotMask();

	if (burden + arms->getWeight() > getStrength())
		return TooHeavy;

	for (int i = 0; i < species->n_slots; i++) {

		// Is the slot the right type?
		if ((slot & species->slots[i]) == 0)
			continue;

		foundSlotType = true;

		// Is the slot occupied?
		if (rdyArms[i] != NULL)
			continue;

		// At this point we've found an empty slot of the correct
		// type. If this is a two-handed item then we also need the
		// next slot to be empty.
		if (arms->getNumHands() == 2) {

			if (i >= species->n_slots - 1)
				continue;

			// Is the slot occupied?
			if (rdyArms[i + 1] != NULL)
				continue;

			// Is the slot the right type?
			if ((slot & species->slots[i + 1]) == 0)
				continue;

			rdyArms[i + 1] = arms;
		}
		// Ready the item. Recalculate armour class.
		rdyArms[i] = arms;
		burden += arms->getWeight();

                // Run the "ready" hook, passing in the arms type being readied
                // and the (first) changed equipment slot.
                runHook(OBJ_HOOK_READY_EQUIP, "pd", arms, i);

                // Bugfix: for party members, have to change the party
                // inventory ref here. cmdReady() used to do this, but the
                // script often calls directly here for things like disarm() or
                // acid effects.
                if (isPlayerPartyMember()) {
                        player_party->refInventoryObject(arms);
                }

                // Bugfix: for others, take the item out of personal
                // inventory. 
                else if (container) {
                        container->takeOut(arms, 1);
                }

		return Readied;
	}

	if (foundSlotType)
		return NoAvailableSlot;
	return WrongType;
}

bool Character::unready(class ArmsType * arms)
{
	for (int i = 0; i < species->n_slots; i++) {

		// Is it in this slot?
		if (rdyArms[i] != arms)
			continue;

		// Is this a 2h item (in which case it should be in the next
		// slot, also)?
		if (arms->getNumHands() == 2) {

			assert(i < species->n_slots - 1);
			assert(rdyArms[i + 1] == arms);
			rdyArms[i + 1] = NULL;
		}
		// Unready the item. Recacalculate armour class.
		rdyArms[i] = NULL;
		burden -= arms->getWeight();

                // Run the "uunready" hook, passing in the arms type being
                // unreadied and the (first) changed equipment slot.
                runHook(OBJ_HOOK_UNREADY_EQUIP, "pd", arms, i);

                // Bugfix: for party members, have to change the party
                // inventory ref here. cmdReady() used to do this, but the
                // script often calls directly here for things like disarm() or
                // acid effects.
                if (isPlayerPartyMember()) {
                        player_party->unrefInventoryObject(arms);
                }

                // Bugfix: for others, put the item back into personal
                // inventory. This fixes (e.g.) Haxima's acid effect on NPC's,
                // which first unreadies, and then removes, which is correct
                // for PC's.
                else if (container) {
                        container->add(arms, 1);
                }

		return true;
	}


	return false;
}

const char *Character::getWoundDescription()
{
	static const char *desc[] = {
		"Critical",
		"Heavily wounded",
		"Moderately wounded",
		"Barely wounded",
		"Unscathed"
	};

	if (isDead())
		return "Killed";

	if (isFleeing())
		return "Fleeing";

	return desc[(getHp() * 4) / getMaxHp()];
	return desc[(getHp() * 4) / getMaxHp()];
}

void Character::groupExitTo(struct place *dest_place, int dest_x, int dest_y,
                            struct closure *cutscene)
{
        struct place *oldPlace = getPlace();

        player_party->removeMembers();

        // --------------------------------------------------------------------
        // If the party is in a vehicle check if we need to disembark before
        // exiting. If the destination is not wilderness or it's impassable
        // wilderness then we'll disembark.
        //
        // When we disembark, we want to put the vehicle on the parent place
        // of the place we're leaving.
        // --------------------------------------------------------------------

        class Vehicle *vehicle = party->getVehicle();
        if (vehicle 
            && (! place_is_wilderness(dest_place)
                || ! place_is_passable(dest_place, dest_x, dest_y,
                                       party, 0))) {
                
                assert(getPlace());
                assert(getPlace()->location.place);

                vehicle->setOccupant(0);
                vehicle->relocate(getPlace()->location.place,
                                         getPlace()->location.x,
                                         getPlace()->location.y);
                vehicle = NULL;
        }
                                       

        if (cutscene) {
                mapUpdate(0);
                closure_exec(cutscene, "");
        }

        // --------------------------------------------------------------------
        // If combat is active then run its state machine after removing
        // everybody.
        // --------------------------------------------------------------------

        //if (combat_get_state() != COMBAT_STATE_DONE) {
                combat_analyze_results_of_last_turn();
        //}

        place_exit(oldPlace);

        player_party->relocate(dest_place, dest_x, dest_y, REL_NOSTEP);
        endTurn();
}

enum MoveResult Character::move(int dx, int dy)
{
	int newx, newy;
	class Character *occupant;

        if (isStationary())
                return StationaryObject;

        this->dx = dx;
        this->dy = dy;

        // ------------------------------------------------------------------
        // Let's give this next a try, in order to make the code for teleport
        // spells simpler. If a teleport spell says to teleport the caster,
        // then perhaps it shouldn't have to concern itself with whether the
        // caster is in party mode or member mode. It just tells the caster to
        // move, and the caster then checks its context to see if this means
        // "move the member" or "move the whole party".
        // ------------------------------------------------------------------

        if (!isOnMap()) {
                if (isPlayerControlled()) {
                        return (player_party->move(dx, dy));
                } else {
                        // I don't know how I got here, but I did with an NPC
                        // in wilderness combat, so its party was NULL. Maybe
                        // it was summoned?
                        dbg("%s not on map and not player-controlled...WTF?\n");
                        if (! party)
                                return OffMap;
                        return (party->move(dx, dy) ? MovedOk : WasImpassable);
                }
        }

	// Calculate new coordinates.
	newx = getX() + dx;
	newy = getY() + dy;


	// ------------------------------------------------------------------
	// Is the character walking off the edge of the map? The same rules as
	// entering a portal apply here: the party must be in follow mode, and
        // all other members must be able to pathfind to this location.
        //
        // Addendum: since this is not strictly necessary for wilderness
        // combat, and it is something of an inconvenience to the user, I skip
        // the checks for wilderness combat.
        // ------------------------------------------------------------------

	if (place_off_map(getPlace(), newx, newy)) {

                // -----------------------------------------------------------
                // Npc characters can just step off and will be removed from
                // the game.
                // -----------------------------------------------------------
                
                // Note: the following used to be || isCharmed(), but since I
                // changed charm to be just a temporary faction switch I don't
                // think this is good any more. I don't want charmed player
                // party members getting destroyed.
                if (! isPlayerControlled()) {
                        remove();
                        destroy();
                        endTurn();
                        return ExitedMap;
                }

                if (place_is_wilderness_combat(getPlace())) {
                        remove();
                        endTurn();
                        return ExitedMap;
                }

                if (player_party->get_num_living_members() == 1) {
                        // Force to follow mode to avoid the annoying case
                        // where only one member is in the party and the player
                        // wants to leave a combat map.
                        player_party->enableFollowMode();
                }

                /* Let's try switching the order used to look for an exit
                 * place. Instead of checking the parent first, check for a
                 * neighbor first. */

                // Look for a neighbor in that direction
                int dir = place_off_map_dir(getPlace(),newx, newy);
                struct place *dest_place = place_get_neighbor(getPlace(), dir);
                if (dest_place != NULL) {
                        
                        int dest_x, dest_y;

                        // Check if the neighbor has an edge entrance
                        if (place_get_edge_entrance(dest_place, dir, &dest_x, 
                                                    &dest_y)) {
                                return NoDestination;
                        }

                        // For parties of size 1, force to follow mode
                        if (player_party->getSize() == 1) {
                                player_party->enableFollowMode();
                        }

                        // Ensure in follow mode
                        if (player_party->getPartyControlMode() 
                            != PARTY_CONTROL_FOLLOW) {
                                return NotFollowMode;
                        }
                        
                        // Rendezvous other party members or abort
                        if (!player_party->rendezvous(getPlace(), getX(), 
                                                      getY())) {
                                return CantRendezvous;
                        }

                        // Goto neighbor
                        groupExitTo(dest_place, dest_x, dest_y, NULL);
                        endTurn();
                        return ExitedMap;
                }


                if (place_get_parent(getPlace()) != NULL) {
                
                        if (player_party->getPartyControlMode() 
                            != PARTY_CONTROL_FOLLOW) {
                                return NotFollowMode;
                        }
                        
                        if (!player_party->rendezvous(getPlace(), getX(), 
                                                      getY())) {
                                return CantRendezvous;
                        }
                        
                        groupExitTo(place_get_parent(getPlace()),
                                    place_get_x(getPlace()) + dx, 
                                    place_get_y(getPlace()) + dy,
                                    NULL);
                        
                        endTurn();
                        return ExitedMap;
                }

                return OffMap;
	}

        // ------------------------------------------------------------------
        // Check passability. If commuting then ignore closed doors (and other
        // blocking mechs).
        // ------------------------------------------------------------------

	if (!place_move_is_passable(
                    getPlace(), getX(), getY(), newx, newy, this,
                    PFLAG_MOVEATTEMPT | 
                    (getActivity() == COMMUTING ? PFLAG_IGNOREMECHS : 0))) {
                return WasImpassable;
	}

	// Are the new coordinates already occupied by another character?
	if ((occupant = (class Character *) place_get_object(getPlace(), 
                                                             newx, newy, 
                                                             being_layer))) {
                

		// Is the occupant an enemy?
		if (are_hostile(this, occupant)) {
			return WasImpassable;
		}

		// *** Switch ***

		if (isPlayerControlled() 

                    // don't allow switching with sleeping NPC's; this would
                    // allow the player to kick THEM out of bed!
                    && SLEEPING != occupant->getActivity()
                    //&& occupant->isPlayerControlled() 
                    //&& isSelected()
                        ) {

			// Special case: if both the occupant and this
			// character are player-controlled then have them
			// switch places if possible.  This prevents the
			// situation where the player party is in follow mode
			// and the leader gets boxed in by the other members.
			// Note that in this case I'm ignoring movement cost
			// (pretend that the characters help each other across
			// the rough terrain... yeah, that's it!).

                        // Wait - first have to check if the other character
                        // can occupy this tile (may have different
                        // passability). If this fails then go ahead and
                        // approve the move without switching. Stacking members
                        // of the same party is permitted in order to resolve
                        // certain corner cases which could be very unpleasant
                        // or confusing for the user. (For example: the party
                        // leader is a gazer, the party enters a dungeon, the
                        // portal destination is surrounded by water, the gazer
                        // steps onto the water... oops! Either we allow the
                        // gazer to stack back onto the party or we require the
                        // user to figure out how to switch the party order
                        // just to pick a new leader just to get out of this
                        // mess...)
                        if (!place_is_passable(getPlace(), getX(), getY(), 
                                               occupant, 0)) {
                                relocate(getPlace(), newx, newy);
                                runHook(OBJ_HOOK_MOVE_DONE, "pdd", getPlace(),
                                        newx, newy);
                                decActionPoints(
                                        place_get_diagonal_movement_cost(
                                                getPlace(), 
                                                getX(), getY(),
                                                newx, newy, 
                                                this, (getActivity() == COMMUTING ? PFLAG_IGNOREMECHS : 0)));
                                return MovedOk;
        
                        }

                        // In follow mode only the leader can switch with other
                        // party members, otherwise two or more followers can
                        // spin-in-place.
                        if (occupant->isPlayerControlled()
                            && CONTROL_MODE_FOLLOW == getControlMode()) {
                                return WasOccupied;
                        }

                        switchPlaces(occupant);
			return SwitchedOccupants;
		}

		return WasOccupied;
	}

        runHook(OBJ_HOOK_MOVE_DONE, "pdd", getPlace(), getX(), getY());
        decActionPoints(place_get_diagonal_movement_cost(getPlace(), 
                                                         getX(), getY(), 
                                                         newx, 
                                                         newy, this, (getActivity() == COMMUTING ? PFLAG_IGNOREMECHS : 0)));
	relocate(getPlace(), newx, newy);

        // If this move was visible to the player
	if (mapTileIsVisible(getX(), getY())) {
                // And this is a party member following the player
                if (isPlayerPartyMember()
                    && (CONTROL_MODE_FOLLOW == getControlMode())) {
                        // Don't update the map now, just mark it dirty. This
                        // helps reduce the sluggishness in town maps.
                        mapSetDirty();
                } else {
                        // If this character is being moved by the player and
                        // they are not in view of the camera
                        if (isPlayerControlled() 
                            && ! mapIsInCameraView(getPlace(), getX(), getY())) {
                                // Recenter the camera to keep the character in view.
                                mapCenterCamera(getX(), getY());
                        }
                        // Repaint the map now
                        mapUpdate(0);
                }
	}

	return MovedOk;
}

void Character::remove()
{
        obj_inc_ref(this);
	Object::remove();
	setAttackTarget(this);
        mapSetDirty();

        // ------------------------------------------------------------------
        // Handle changes to party control.
        // ------------------------------------------------------------------

        if (isSolo()) {
                player_party->enableRoundRobinMode();
        } else if (isLeader()) {
                player_party->enableFollowMode();
        }
        obj_dec_ref(this);
}

class ArmsType *Character::enumerateWeapons(int *armsIndex)
{
	*armsIndex = -1;
	class ArmsType *currentArms;
	currentArms = getNextWeapon(armsIndex);
	if (!currentArms)
		currentArms = species->weapon;
	return currentArms;
}

class ArmsType *Character::getNextWeapon(int *armsIndex)
{
	class ArmsType *currentArms;
	do {
		currentArms = getNextArms(armsIndex);
	} while (currentArms != NULL 
                 && dice_average(currentArms->getDamageDice()) <= 0);
	return currentArms;
}

/* If target is null, then it cant be in range.
  If weapon is null, then theres no range limitation, so target is
  considered 'in range' */
bool Character::isAttackTargetInRange(class ArmsType *weapon)
{
	int dx, dy, distance;

	if (target == NULL)
		return false;
		
	if (weapon == NULL)
		return true;

	dx = target->getX() - getX();
	dy = target->getY() - getY();

	if (dx < 0)
		dx = -dx;
	if (dy < 0)
		dy = -dy;

	distance = (dx > dy) ? (dx + (dy >> 1)) : (dy + (dx >> 1));

	return (weapon->getRange() >= distance);
}

class Character *Character::getAttackTarget(class ArmsType *weapon)
{
	// Is the old target still valid?
	if (!target || !target->isOnMap() || target->isDead() ||
	    !isAttackTargetInRange(weapon) || !target->isVisible()) {
		setAttackTarget(this);
		return this;
	}

	return target;
}

bool Character::hasReadied(class ArmsType * arms)
{
	int armsIndex = -1;
	class ArmsType *readied = enumerateArms(&armsIndex);
	while (readied != NULL && readied != arms)
		readied = getNextArms(&armsIndex);
	return readied == arms;
}

class ArmsType *Character::enumerateArms(int *armsIndex)
{
	*armsIndex = -1;
	return getNextArms(armsIndex);
}

class ArmsType *Character::getNextArms(int *armsIndex)
{
	// Advance to the next slot
	// *armsIndex++ doesnt work for some reason...
	*armsIndex=*armsIndex+1;

	// Search remaining slots for a weapon
	for (; *armsIndex < species->n_slots; *armsIndex=*armsIndex+1) {

		// Is anything in this slot?
		if (rdyArms[*armsIndex] == NULL)
			continue;

		// Is this just another slot for the same weapon (happens in
		// the case of multi-slotted weapons like 2h swords)?
		if (*armsIndex > 0 && rdyArms[*armsIndex-1] == rdyArms[*armsIndex] &&
		    rdyArms[*armsIndex]->getNumHands() == 2)
			continue;

		return rdyArms[*armsIndex];
	}
	
	return 0;
}

ArmsType *Character::getArmsInSlot(int slot)
{
        if (slot < 0
            || slot >= species->n_slots)
                return NULL;
        return rdyArms[slot];
}

int Character::hasAmmo (class ArmsType * weapon)
{
        // SAM: Changed this from returning bool to 
        //      returning int (0 for no ammo, n for amount)
	if (weapon->ammoIsUbiquitous())
                return 1;  // One more available, that is.

	if (isPlayerControlled()) {
		struct inv_entry *ie;

		if (weapon->isMissileWeapon()) {
			ie = player_party->inventory->search(weapon->getAmmoType());
                        if (ie == NULL)
                                return 0;  // No ammo
			return ie->count;  // 1 or more
		}
                else if (weapon->isThrownWeapon()) {
			ie = player_party->inventory->search(weapon);
			if (ie == NULL) {
				unready(weapon);
				return 0;  // No more
			}
			assert(ie->count > 0);
			return ie->count;  // 1 or more
		}
		return 1;  // Melee weapons are like ubiquitous
	} else {
                // SAM: Not bothering with quantity of NPC ammo for now
		return (!weapon->isMissileWeapon() ||
                        (container != NULL &&
                         container->search(weapon->getAmmoType())));
	}
} // Character::hasAmmo()

bool Character::hasInInventory (class ObjectType *type)
{
	if (isPlayerControlled()) {
                return player_party->hasInInventory(type);
	} else {
		return (container != NULL &&
                        container->search(type) != NULL);
	}
}

void Character::setLight(int val)
{
	light = max(val, MIN_PLAYER_LIGHT);
        mapSetDirty();
}

void Character::addMana(int delta)
{
	mana += delta;
	mana = max(mana, 0);
        mana = min(mana, getMaxMana());
}

void Character::changeSleep(bool val)
{
        if (sleeping == val)
                return;

	sleeping = val;
        statusRepaint();
        mapSetDirty();

        if (sleeping) {

                // -----------------------------------------------------------
                // Going to sleep.
                // -----------------------------------------------------------
            if (engagedInTask()) {
                taskAbort();
            }


            if (isLeader()) {
                assert(isPlayerControlled());
                player_party->enableFollowMode();
            } else if (isSolo()) {
                assert(isPlayerControlled());
                player_party->enableRoundRobinMode();
            }

        } else {

                if (isPlayerControlled()) {

                        // ----------------------------------------------------
                        // Upon waking up, set this character's control mode
                        // based on the party's control mode.
                        // ----------------------------------------------------

                        assert(! isLeader());
                        assert(! isSolo());
                        
                        switch (player_party->getPartyControlMode()) {

                        case PARTY_CONTROL_FOLLOW:
                                if (this != player_party->get_leader()) {
                                        setControlMode(CONTROL_MODE_FOLLOW);
                                }
                                break;

                        case PARTY_CONTROL_SOLO:
                                setControlMode(CONTROL_MODE_IDLE);
                                break;

                        case PARTY_CONTROL_ROUND_ROBIN:
                                setControlMode(CONTROL_MODE_PLAYER);
                                break;

                        default:
                                assert(false);
                                break;
                        }
                        
                }

        }

}

void Character::awaken(void)
{
        if (isAsleep() 
            && ! isResting()
            && ! isDead()) {
                changeSleep(false);
                log_msg("%s wakes up!", getName());
        }
}

void Character::setFleeing(bool val)
{
	if (fleeing == val)
		return;

	fleeing = val;
        fleePathFound = false;
        fleePathFlags = 0;
        cachedPath = 0;
}

/**
 * Check if it's a good idea to try and move here in the given place. Checks
 * for passability and known hazards.
 */
bool Character::locationIsOk(int x2, int y2)
{
        return (place_is_passable(getPlace(), x2, y2, this, 0)
                && (! place_is_occupied(getPlace(), x2, y2))
                && (! place_is_hazardous(getPlace(), x2, y2)));
}

/**
 * Compute the optimal vector to move away from hostiles.
 *
 * @param dx The x component of the resulting evasion vector.
 * @param dy The y component of the resulting evasion vector.
 */
void Character::getEvasionVector(int *dx, int *dy)
{
        struct evasionVectorInfo info;
        int x2, y2;

        /* Get the vector away from foes. */
        memset(&info, 0, sizeof(info));
        info.subj = this;
        place_for_each_object(getPlace(), getEvasionVectorVisitor, &info);

        /* Normalize the vector. */
        clamp(info.dx, -1, 1);
        clamp(info.dy, -1, 1);

        /* Compute the new location. */
        x2 = getX() + info.dx;
        y2 = getY() + info.dy;

        /* Check if ok. */
        if (locationIsOk(x2, y2)) {
                goto done;
        }

        /* Not ok, try the vertical. */
        if (info.dx) {
                x2 -= info.dx;
                if (locationIsOk(x2, y2)) {
                        goto done;
                }
                x2 += info.dx;
        }

        /* Not ok, try the horizontal. */
        if (info.dy) {
                y2 -= info.dy;
                if (locationIsOk(x2, y2)) {
                        goto done;
                }
                y2 += info.dx;
        }

        /* No place to go. */
        info.dx = 0;
        info.dy = 0;

 done:
        *dx = info.dx;
        *dy = info.dy;
}

static bool movedOk(enum MoveResult result)
{
        switch (result) {
        case NotApplicable:
        case NoDestination:
        case WasImpassable:
        case WasOccupied:
                return false;
        default:
                return true;
        }
}

bool Character::mapHasEdge()
{
        // This is the simple thing to do, and works for towns and wilderness
        // maps. Dungeon rooms could have edges shared with other rooms, but I
        // don't want to deal with that right now, mostly because it
        // complicates the pathfinding heuristics, and I want to get this case
        // right first.
        return (place_get_parent(getPlace()));
}

bool Character::exitMap()
{
        if (getX() == 0) {
                return movedOk(move(-1, 0));
        } else if (getX() == (place_w(getPlace()) - 1)) {
                return movedOk(move(1, 0));
        } else if (getY() == 0) {
                return movedOk(move(0, -1));
        } else if (getY() == (place_h(getPlace()) - 1)) {
                return movedOk(move(0, 1));
        } else {
                return false;
        }
}

bool Character::followFleePath()
{
        return (exitMap()
                || pathfindTo(getPlace(), fleeX, fleeY, fleePathFlags));
}

bool Character::findFleePath()
{
	// Just look for the nearest map edge. Note that this might not be the
	// best choice, because it might not have a path. Get this working
	// first, then get fancy if you want.
	int leftx = getX();
	int rightx = place_w(getPlace()) - getX();
	int boty = getY();
	int topy = place_h(getPlace()) - getY();
	int minx, miny;

	// Is the left edge nearer than the right edge?
	if (leftx < rightx) {
                fleeX = 0;
		minx = leftx;
	} else {
                fleeX = place_w(getPlace()) - 1;
		minx = leftx;
	}

	// Is the top edge nearer than the bottom edge?
	if (boty < topy) {
                fleeY = 0;
		miny = topy;
	} else {
                fleeY = place_h(getPlace()) - 1;
		miny = boty;
	}

        // Is the vertical edge nearer than the horizontal edge?
        if (minx < miny) {
                fleePathFlags = PFLAG_HORZ;
        } else {
                fleePathFlags = PFLAG_VERT;
        }

        fleePathFound = true;
        return true;
}

bool Character::pathfindToMapEdge()
{
        if (fleePathFound
            && followFleePath()) {
                return true;
        }

        if (! findFleePath()) {
                return false;
        }

        return followFleePath();
}

bool Character::fleeToMapEdge()
{
        return (mapHasEdge()
                && pathfindToMapEdge());
}

bool Character::evade()
{
        int dx = 0, dy = 0;
        getEvasionVector(&dx, &dy);
        if (!dx && !dy) {
                return false;
        }

        return movedOk(move(dx, dy));
}

bool Character::flee()
{
        return (!isStationary()
                && (fleeToMapEdge()
                    || evade()));
}

void Character::dropRdyArms()
{
	assert(!isPlayerControlled());

	for (int i = 0; i < species->n_slots; i++) {

		// Anything in this slot?
		if (rdyArms[i] == NULL)
			continue;

                // roll to drop
                if ((rand() % 100) > ITEM_DROP_PROB)
                        continue;

		// Create an object of this type and drop it on the map
		class Object *object = new Object();
		if (!object)
			continue;
		object->init(rdyArms[i]);
		object->relocate(getPlace(), getX(), getY());

		// Unready it
		unready(rdyArms[i]);

	}
}

void Character::unreadyAll()
{
	for (int i = 0; i < species->n_slots; i++) {

                class ArmsType *type = rdyArms[i];

		// Anything in this slot?
		if (!type)
			continue;

		// Unready it
		unready(type);
	}
}

bool Character::dropItems()
{
	assert(!isPlayerControlled());

	if (container == NULL)
		return false;

        if (container->isEmpty())
                return true;

        if (! container->isEmpty()
            && (forceContainerDrop
                || (rand() % 100) <= CHEST_DROP_PROB))
                container->relocate(getPlace(), getX(), getY());
        obj_dec_ref(container);
	container = NULL;

	return true;
}

void Character::kill()
{
    // Why not turn this on again...? At least dropItems?
// 	if (!isPlayerControlled() && isOnMap()) {
// 		dropRdyArms();
// 		dropItems();
// 	}

        // when a PC dies unready all arms so other party members can use them
        if (isPlayerControlled()) {
                unreadyAll();
        }

        if (isOnMap()
            && container
            && forceContainerDrop) {
                container->relocate(getPlace(), getX(), getY());
                obj_dec_ref(container);
                container = NULL;
        }

        if (engagedInTask()) {
            taskAbort();
        }

        if (isPlayerControlled()) {
                log_msg("%s has fallen!!", getName());
        }

	hp = 0;
        setDead(true);

        // Similarly, run the on-death hook
        runHook(OBJ_HOOK_ON_DEATH, 0);
	remove();
}

void Character::useAmmo(class ArmsType *weapon)
{
	if (weapon->ammoIsUbiquitous())
		return;

	if (isPlayerControlled()) {
		struct inv_entry *ie;

		if (weapon->isMissileWeapon()) {
			class ObjectType *ammoType = weapon->getAmmoType();
			takeOut(ammoType, 1);
		} else if (weapon->isThrownWeapon()) {

                        // Handle the case where multiple party members have
                        // all readied the same throwable weapon, and there is
                        // no longer enough to supply them all, by forcing the
                        // character that just threw to unready.
			ie = player_party->inventory->search(weapon);
                        assert(ie);
                        assert(ie->ref <= ie->count);
                        if (ie->ref == ie->count) {
                                unready(weapon);
                                log_msg("%s : %s now out of ammo\n", 
                                        getName(), weapon->getName());
                        }
			takeOut(weapon, 1);
		}
	} else {
		if (weapon->isMissileWeapon()) {
			container->takeOut(weapon->getAmmoType(), 1);
			if (!hasAmmo(weapon)) {
				unready(weapon);
				rearm = true;
			}
		} else if (weapon->isThrownWeapon()) {
			if (container && container->search(weapon)) {
				container->takeOut(weapon, 1);
			} else {
				unready(weapon);
				rearm = true;
			}
		}
	}
}

/*****************************************************************************/

void Character::armThyself(void)
{
	struct knapsack ks;

	//assert(!isPlayerControlled());

	if (container == NULL)
		return;

	// Setup the context to solve this using the knapsack algorithm
	memset(&ks, 0, sizeof(ks));
	ks.item = new void *[MAX_N_ITEMS];
	ks.value = new int[MAX_N_ITEMS];
	ks.solution = new unsigned char[MAX_N_ITEMS];
	if (!ks.item || !ks.value || !ks.solution)
		goto destroy_ks;
	ks.put = wrapReady;
	ks.remove = wrapUnready;
	ks.context = this;

	container->forEach(myConsiderArms, &ks);

	knapsack_solve(&ks);

	// Ready the chosen items
	for (int i = 0; i < ks.n_items; i++) {
		if (!ks.solution[i])
			continue;
		class ArmsType *arms = (class ArmsType *) ks.item[i];
		if (ready(arms) != Character::Readied)
			continue;
	}

      destroy_ks:
	if (ks.item)
		delete [] ks.item;
	if (ks.value)
		delete [] ks.value;
	if (ks.solution)
		delete [] ks.solution;

	rearm = false;
}

bool Character::needToRearm()
{
        // gmcnutt: I currently don't have any way to tell a charmed player
        // party member how to rearm themselves. Player characters pull from
        // party inventory. Non-player characters pull from their own personal
        // inventory. I really need to merge both types into the same behaviour
        // to simplify things.
        if (party == (Party*)player_party)
                return false;

	return rearm;
}

bool Character::initCommon(void)
{
        if (species && species->n_slots > 0) {
                rdyArms = (class ArmsType**)calloc(species->n_slots, sizeof(class ArmsType*));
                assert(rdyArms);
        }

        ai = NULL;

	return true;
}

bool Character::initStock(struct species * species, struct occ * occ,
			  struct sprite * sprite, char *nameStr, int order)
{
	// This method is now only used to initialize cloned characters.

	this->species = species;        
	this->occ = occ;
        if (occ)
                occ_ref(occ);
	this->current_sprite = sprite;

	if (!initCommon())
		return false;

        setName(nameStr);
	this->order = order;

	lvl = 1;		// fixme: hardcoded hack!

	hp   = getMaxHp();
	mana = getMaxMana();
	AP_per_round = species->spd;
        setDead(false);

        defenseBonus = 0;
        
	return true;
}

/*****************************************************************************/

void Character::resurrect(void)
{
	assert(isPlayerControlled());// shotgun assert put here during refactor
	setHp(min(10, getMaxHp()));
        setDead(false);
	statusFlash(getOrder(), Blue);

        // ------------------------------------------------------------------
        // If we're in wilderness mode then we're done. Otherwise we need to
        // put this character near the other party member's on the map.
        // ------------------------------------------------------------------

        if (player_party->isOnMap())
                return;

        assert(player_party->get_leader());

        putOnMap(player_party->get_leader()->getPlace(), 
                 player_party->get_leader()->getX(),
                 player_party->get_leader()->getY(), 4,
                 0);

        assert(isOnMap());

}


struct sprite *Character::getSprite()
{
	if ((isAsleep() || isDead()) && species->sleep_sprite)
		return species->sleep_sprite;
	return current_sprite;
}

void Character::rest(int hours)
{
        int healAmount = HP_RECOVERED_PER_HOUR_OF_REST;
        int manaAmount = MANA_RECOVERED_PER_HOUR_OF_REST;

        /* Partial fix for SF BUG [ 1526910 ] "starvation is lame". Don't let
         * the player completely rest his way out of starvation. */
        if (isPlayerPartyMember()) {
                if (!player_party->food) {
                        healAmount /= 2;
                        manaAmount /= 2;
                }
        }

	while (hours) {
                if (!isDead()) {
                        heal(healAmount);
                        addMana(manaAmount);
                }
		hours--;
	}
}

int Character::getExperienceValue()
{
        int xpval = 0;
        if (species)
                xpval += species->xpval;
        if (occ)
                xpval += occ->xpval;
        return (xpval * lvl);
}

int Character::getXpForLevel(int lvl)
{
	if (lvl == 1)
		return 0;
    return (int)pow((double)2, (double)lvl+6);
}

void Character::addExperience(int amount)
{
	xp += amount;
	if (xp >= getXpForLevel(getLevel()+1)) {
		lvl++;
                log_banner("^c+b%s^c- gains level ^c+g%d^c-!", getName(), lvl);
                if (isPlayerControlled()) {
                        mapFlash(1000);
                }
                setHp(getMaxHp());
                setMana(getMaxMana());
	}
}

int Character::getMaxHp()
{       
        int base = hp_mod + species->hp_mod;
        int mult = hp_mult + species->hp_mult;

        if (occ) {
                base += occ->hp_mod;
                mult += occ->hp_mult;
        }

        mult = max(0, mult);
        
        return (base + getLevel() * mult);
}

int Character::getMaxMana()
{
        int base = mp_mod + species->mp_mod;
        int mult = mp_mult + species->mp_mult;

        if (occ) {
                base += occ->mp_mod;
                mult += occ->mp_mult;
        }

        mult = max(0, mult);
        return base + getLevel() * mult;
}

void Character::changeArmourClass(int delta)
{
	ac += delta;
	ac = max(0, ac);
}

class Object *Character::clone()
{
	char buf[64];
	class Character *clone = new Character();
	if (!clone)
		return NULL;

        if (is_clone)
                snprintf(buf, sizeof(buf), "%s", getName());
        else
                snprintf(buf, sizeof(buf), "%s (clone)", getName());

	clone->initStock(species, occ, current_sprite, buf, 0);
	clone->is_clone = true;

        // clone the readied items
		int armsIndex = 0;
        for (ArmsType *arms = enumerateArms(&armsIndex); arms != NULL; arms = getNextArms(&armsIndex)) {
                clone->ready(arms);
        }

        // NOTE: effects not cloned... that will require a bit of work and
        // testing because effects can have gobs (gobs aren't cloned either)

	return clone;
}

bool Character::isVisible()
{
	return ((visible > 0) && species->visible);
}

bool Character::isShaded()
{
	// Friendly invisible characters are shaded
	return ((! isVisible() && isPlayerControlled()) 
                || Object::isShaded());
}

void Character::describe()
{
    if (Session->subject) {
        const char *diplstr = diplomacy_string(this, Session->subject);
        if (isvowel(diplstr[0]))
            log_continue("an");
        else
            log_continue("a");
        log_continue(" %s", diplstr);
    } else {
        log_continue("an");
    }
    log_continue(" L%d", getLevel());
    if (isKnown()) {
        log_continue(" %s", getName());
    } else {
        if (species && species->name) {
            log_continue(" %s", species->name);
        }
        if (occ && occ->name) {
            log_continue(" %s", occ->name);
        }
    }
    if (!isVisible())
        log_continue(" (invisible)");
    if (isSubmerged()) {
        log_continue(" (submerged)");
    }
}

void Character::examine()
{
        int i;
        int n = 0;
	const char *diplstr = diplomacy_string(this, Session->subject);

        log_continue("%s level %d", diplstr, getLevel());

        if (isKnown()) {
                log_continue(" %s,", getName());
        } else {
                if (species && species->name) {
                        log_continue(" %s", species->name);
                }
                if (occ && occ->name) {
                        log_continue(" %s", occ->name);
                }
        }

        log_continue(" %s [", getWoundDescription());

        for (ArmsType *arms = enumerateArms(&i); arms; 
             arms = getNextArms(&i)) {
                if (n > 0) {
                        log_continue(", ");
                }
                log_continue("%s", arms->getName());
                n++;
        }

        if (!n) {
                log_continue("no arms");
        }

        log_continue("]");
}

sound_t *Character::getDamageSound()
{
        if (damage_sound)
                return damage_sound;
        if (species && species->damage_sound)
                return species->damage_sound;
        return NULL_SOUND;
}

sound_t *Character::get_movement_sound()
{
        if (species)
                return species->movement_sound;
        return NULL_SOUND;
}

bool Character::isType(int classID) {
        if (classID == CHARACTER_ID)
                return true;
        return Object::isType(classID);
}

int Character::getType() {
        return CHARACTER_ID;
}

int Character::getHp() {
        return hp;
}

int Character::getOrder() {
        return order;
}

int Character::getExperience() {
        return xp;
}

unsigned char Character::getStrength() {
        return (species->str + str);
}

unsigned char Character::getIntelligence() {
        return (species->intl + intl);
}

unsigned char Character::getDexterity() {
        return (species->dex + dex);
}

unsigned char Character::getBaseStrength() {
        return (str);
}

unsigned char Character::getBaseIntelligence() {
        return (intl);
}

unsigned char Character::getBaseDexterity() {
        return (dex);
}

void Character::setStrength(unsigned char newstat) {
        str = newstat;
}

void Character::setIntelligence(unsigned char newstat) {
       intl = newstat;
}

void Character::setDexterity(unsigned char newstat) {
	dex = newstat;
}

int Character::getLevel() {
        return lvl;
}

void Character::setLevel(int val) {
        assert(val>0);
		xp = getXpForLevel(val);
        lvl = val;
        setHp(getMaxHp());
        setMana(getMaxMana());
}

bool Character::isDead() {
        return dead;
}

bool Character::isAsleep() {
        return sleeping;
}

bool Character::isIncapacitated() {
        return (!isOnMap() || isDead() || isAsleep());
}

int Character::getArmourClass() {
        return ac;
}

void Character::setHp(int val) 
{
        hp = val;
        hp = clamp(hp, 0, getMaxHp());
        if (hp == 0) {
            kill();
        }
}

bool Character::isPlayerControlled() {
        return playerControlled;
}

void Character::setPlayerControlled(bool val) 
{
    playerControlled = val;
    if (val) {
        ctrl = ctrl_character_ui;
        if (isPlayerPartyMember()) {
            class Character *oldLeader, *newLeader;
            switch (player_party->getPartyControlMode()) {
            case PARTY_CONTROL_FOLLOW:
                oldLeader = player_party->get_leader();
                player_party->chooseNewLeader();
                newLeader = player_party->get_leader();
                if (oldLeader != newLeader) {
                    oldLeader->setControlMode(CONTROL_MODE_FOLLOW);
                }
                if (newLeader != this) {
                    setControlMode(CONTROL_MODE_FOLLOW);
                }
                break;
            case PARTY_CONTROL_SOLO:
                setControlMode(CONTROL_MODE_IDLE);
                break;
            case PARTY_CONTROL_ROUND_ROBIN:
                setControlMode(CONTROL_MODE_PLAYER);
                break;
            }
        }
    } else {
        ctrl = ctrl_character_ai;
        if (isPlayerPartyMember()) {
            if (isLeader()) {
                player_party->chooseNewLeader();
                assert(this != player_party->get_leader());
            }
            if (isSolo()) {
                setSolo(false);
                player_party->enableFollowMode();
            }
            if (engagedInTask()) {
                taskAbort();
            }
        }
        setControlMode(CONTROL_MODE_AUTO);
    }
}

void Character::setAttackTarget(class Character * newtarget) 
{
        if (target == newtarget)
                return;

        if (target && target != this)
                obj_dec_ref(target);

        target = newtarget;

        if (target && target != this)
                obj_inc_ref(target);
}

bool Character::isSolo() {
        return solo;
}

int Character::getLight() {
        return light;
}

int Character::getVisionRadius() {
        return species->vr;
}

int Character::getSpeed() {
    // Returns the character-specific number of 
    // Action Points per round for this character.

    int total_AP;
    int AP_modifier_from_equipped_items = 0;

    if (AP_per_round > 0)
    	total_AP = AP_per_round;
    else
	total_AP = species->spd;

    int armsIndex = 0;
    for (class ArmsType * arms = enumerateArms(&armsIndex); arms != NULL; 
	 arms = getNextArms(&armsIndex)) {
	AP_modifier_from_equipped_items += arms->get_AP_mod();
    }
    total_AP += AP_modifier_from_equipped_items;

    if (total_AP < 1)
	total_AP = 1; // SAM: perhaps revisit this...

    return total_AP;
}

int Character::setSpeed(int val) {
    AP_per_round = val;
    return AP_per_round;
}

int Character::getMana() {
        return mana;
}

void Character::setMana(int val) {
        mana = val;
}

bool Character::isFleeing() {
        return fleeing;
}

void Character::setOrder(int order) {
        this->order = order;
}

void Character::setCombat(bool val) {
        inCombat = val;
}

int Character::getDefend()
{
        int defend = 0;

        if (isAsleep())
                return -3; // hack: hard-coded constant

		int armsIndex=0;
        for (class ArmsType * arms = enumerateArms(&armsIndex); arms != NULL; 
             arms = getNextArms(&armsIndex)) {
                defend += dice_roll(arms->getToDefendDice());
        }
        
        defend += defenseBonus;

        if (isSubmerged()) {
                defend += kern_intvar_get("submerged_def_bonus");
        }

        return defend;
}

int Character::getToHitPenalty()
{
        int penalty = 0;

		int armsIndex=0;
        for (class ArmsType * arms = enumerateArms(&armsIndex);
             arms != NULL; arms = getNextArms(&armsIndex)) {
                int roll = dice_roll(arms->getToHitDice());
                if (roll < 0)
                        penalty += roll;
        }
        
        return penalty;
}

int Character::getBaseAttackBonus(class ArmsType * weapon)
{
	int strbonus = weapon->modifyStrAttack(session_run_query(Session, str_based_attack_query, "p", this));
	int dexbonus = weapon->modifyDexAttack(session_run_query(Session, dex_based_attack_query, "p", this));
	int totalbonus = (strbonus + dexbonus) / (100 * 1000);
	return (1+ totalbonus);
}

int Character::getAttackBonus(class ArmsType * weapon)
{
	return (rand() % getBaseAttackBonus(weapon));
}

int Character::getDamageBonus(class ArmsType * weapon)
{
	int dambonus = weapon->modifyDamageBonus(session_run_query(Session, damage_bonus_query, "p", this))/(100 * 1000);
	return (rand() % (1+ dambonus));
}

int Character::getAvoidBonus()
{
	//hack: dont get any bonus here if you're asleep on the job
	if (isAsleep())
		return 0;
		
	int avoidBonus = session_run_query(Session, defense_bonus_query, "p", this);
	float avoidMod = 1;
	
	//roundabout way of getting data in order to preserve info for stderr
	int armsIndex=0;
	for (class ArmsType * arms = enumerateArms(&armsIndex);
		 arms != NULL; arms = getNextArms(&armsIndex))
	{
		avoidMod = arms->modifyAvoidBonus(avoidMod);
	}
	
	int totalbonus = (int)(avoidBonus * (avoidMod/1000));
	return (rand() % (1+ totalbonus));
}

int Character::getArmor()
{
        int armor = 0;

		int armsIndex=0;
        for (class ArmsType * arms = enumerateArms(&armsIndex);
             arms != NULL; arms = getNextArms(&armsIndex)) {
                armor += dice_roll(arms->getArmorDice());
        }

        // the obsolescent 'armor class' is still used by the 'protect' spell
        // effect
        armor += ac;
        
        // Some species have an armor bonus
        if (species->armor_dice) {
                armor += dice_roll(species->armor_dice);
        }

        return armor;

}

class Party *Character::getParty()
{
        return party;
}

void Character::burn()
{
        damage(DAMAGE_FIRE);
        log_msg("%s burning-%s!", getName(), getWoundDescription());
}

void Character::sleep()
{
        if (isAsleep())
                return;

        changeSleep(true);
        log_msg("%s sleeping!", getName());
}

bool Character::canSee(class Object *obj)
{
        return (obj->getPlace() == getPlace() &&
                place_flying_distance(getPlace(), getX(), getY(), 
                                      obj->getX(), obj->getY()) <= 
                getVisionRadius() &&
                obj->isVisible() &&
                place_in_los(getPlace(), getX(), getY(),
                             obj->getPlace(), obj->getX(), obj->getY()));
}

bool Character::atAppointment()
{
        struct appt *curAppt = &sched->appts[appt];

        if (getX() >= curAppt->x
            && getX() < (curAppt->x + curAppt->w)
            && getY() >= curAppt->y
            && getY() < (curAppt->y + curAppt->h)) {
                return true;
        }
        return false;
}

bool Character::playerIsInMyBed()
{
	struct appt *curAppt = &sched->appts[appt];
	
	if (SLEEPING!=curAppt->act)
	return false;
	
	class Character *sleeper = 
		(class Character*)place_get_object(getPlace(), 
			curAppt->x,
			curAppt->y,
			being_layer);
			
	return (sleeper
		&& sleeper->isPlayerControlled()
		&& sleeper->isResting());
}

//return true if character is next to or on the appointment loc
bool Character::nextToAppointment()
{
   struct appt *curAppt = &sched->appts[appt];	
	
	if (abs(curAppt->x - getX())>1)
	{
		return false;      
	}
	if (abs(curAppt->y - getY())>1)
	{
		return false;      
	} 

	return true;
}

void Character::kickPlayerOutOfMyBed()
{
        struct appt *curAppt = &sched->appts[appt];

        log_msg("Kicked out of bed!");
        player_party->throw_out_of_bed();

        // now switch places with whoever is in bed
       class Character *sleeper = 
               (class Character*)place_get_object(getPlace(), 
                                                  curAppt->x,
                                                  curAppt->y,
                                                  being_layer);
       assert(sleeper);
       switchPlaces(sleeper);

       assert(atAppointment());
       setActivity(curAppt->act);
}

void Character::switchPlaces(class Being *occupant)
{
        int oldx = getX();
        int oldy = getY();
        int newx = occupant->getX();
        int newy = occupant->getY();

        // Save these before calling remove because remove()
        // automatically resets these to defaults (for sane
        // reasons... I think)
        class Character *oldTarget = target;
        struct place *oldPlace = getPlace();
        bool wasSolo = isSolo();
        
        //remove();
        occupant->relocate(oldPlace, oldx, oldy);
        relocate(oldPlace, newx, newy);
        decActionPoints(place_get_diagonal_movement_cost(getPlace(), 
                                                         oldx, oldy,
                                                         newx, newy,
                                                         this, 0));
        setAttackTarget(oldTarget);
        setSolo(wasSolo);
}

bool Character::commute()
{
	int tx, ty;

        // Note: this could be improved a bit by caching the results of the
        // rectangle search below. Since the path is cached I'm not sure how
        // big of a gain it would be.

        struct appt *curAppt = &sched->appts[appt];

        // Check if the commute is over
        if (atAppointment()) {
                setActivity(curAppt->act);
                return true;
        }

        // Else search for an open place in the appointment rectangle
        for (ty = curAppt->y; ty < curAppt->y + curAppt->h; ty++) {
                for (tx = curAppt->x; tx < curAppt->x + curAppt->w;  tx++) {

                        if (!place_is_passable(getPlace(), tx, ty, this, 
                                               PFLAG_IGNOREMECHS) ||
                            place_is_hazardous(getPlace(), tx, ty))
                                continue;
                        
                        // Try to go there. Adjacent-to-there is not good
                        // enough; we will get stuck repeatedly trying to
                        // pathfind if the last step is impassable (Bug
                        // 1734069).
                        if (!pathfindTo(getPlace(), tx, ty, 
                                        PFLAG_ADJACENTNOTOK|PFLAG_IGNOREMECHS)) {
                                continue;
                        }
                        
                        // Check if the commute is over.
                        if (atAppointment()) {
                                setActivity(curAppt->act);
                        }

                        return true;
                }
        }

        // Special case: if the appointment is the character's bed, and
        // pathfinding failed because the player is sleeping in it, then kick
        // the player out of bed
			if (playerIsInMyBed())
			{
				if (nextToAppointment())
				{
					//evict the player if we are close enough
					kickPlayerOutOfMyBed();
				}
				else
				{
					//try to reach bed, clambering over other beings as necessary
					pathfindTo(getPlace(), curAppt->x, curAppt->y, PFLAG_IGNOREBEINGS | PFLAG_IGNOREMECHS);
				}
				return true;
			}

        dbg("%s cannot find path to [%d %d %d %d] while commuting\n", 
               getName(), 
               sched->appts[appt].x, 
               sched->appts[appt].y, 
               sched->appts[appt].w, 
               sched->appts[appt].h);

        return false;
}

void Character::synchronize()
{
        struct appt *cur_appt = 0;

	if (!sched || sched->n_appts == 0)
		return;
		
	if (isDead())
		return;

        cur_appt = sched_get_appointment(sched, Session->clock.hour,
                                         Session->clock.min);

        if (getPlace()
            && getPlace() != cur_appt->place) {
                introduce();
                return;
        } 

        /* Iff the character is not already in their appointment rectangle then
         * relocate them to the upper left corner of their appointment. The ULC
         * is better than the center because it's more obvious to the schedule
         * designer that the ULC needs to be passable terrain. If they are
         * already in their appointment then don't relocate them, because it
         * looks funny when you reload the game.
         */
        if ((getX() < cur_appt->x)
            || (getX() >= (cur_appt->x + cur_appt->w))
            || (getY() < cur_appt->y)
            || (getY() >= (cur_appt->y + cur_appt->h))) {
                relocate(cur_appt->place, cur_appt->x, cur_appt->y);
        }
	setActivity(cur_appt->act);
        appt = cur_appt->index;
}

void Character::introduce()
{
        assert(sched);
		
		if (isDead())
			return;

        struct appt *newAppt = sched_get_appointment(sched, 
                                                     Session->clock.hour,
                                                     Session->clock.min);
        if (getPlace() != newAppt->place) {

                // Since this code just "teleports" the character, make sure
                // the player isn't looking.

                // Introduce a character into the place as part of its
                // schedule. For now, just drop it on its location as in
                // synchronize(). DO NOT try to use object::relocate(). It's
                // seriously overextended and has a bunch of logic that will
                // prevent NPC's from relocating.
                obj_inc_ref(this);
                if (getPlace()) {
                        place_remove_object(getPlace(), this);
                }
                setPlace(newAppt->place);
                setX(newAppt->x);
                setY(newAppt->y);
                place_add_object(newAppt->place, this);
                obj_dec_ref(this);
                setActivity(newAppt->act);
                appt = newAppt->index;
        }
}

void Character::getAppointment()
{

        int nextAppt = appt + 1;

        /* Special case: the last appointment of the day is over when the clock
           rolls over at midnight. We can detect clock rollover by checking if
           the current time is BEFORE the start of the current appt. */
        if (nextAppt == sched->n_appts) {
                if (Session->clock.hour < sched->appts[appt].hr) {
                        appt = 0;
                }
        }

        /* Normal case: check if the clock time exceeds the start time of our
           next appt. */
        else if (Session->clock.hour >= sched->appts[nextAppt].hr &&
                 Session->clock.min >= sched->appts[nextAppt].min) {
                appt = nextAppt;
        }

        if (atAppointment()) {
                setActivity(sched->appts[appt].act);
        } else {
                setActivity(COMMUTING);
        }

}

void Character::exec()
{
        int points_last_loop;
        class Character *leader;
        bool noHostiles = false;
        bool appointmentChecked = false;
                
        //printf("exec %s\n", getName());

        startTurn();
        
        if (isDead() || ! isOnMap() ||action_points <= 0) {
                endTurn();
                return;
        }

        if (isResting()) {

                // -----------------------------------------------------------
                // Every hour until the wakeup alarm goes off have the
                // character rest a little.
                //
                // The first character to wakeup to the alarm clock will wake
                // up the party.
                // -----------------------------------------------------------
                
                assert(isAsleep());

                if (clock_alarm_is_expired(&rest_alarm)) {
                        rest(1);
                        clock_alarm_set(&rest_alarm, 60);
                }

                if (clock_alarm_is_expired(&wakeup_alarm)) {

                        if (! isPlayerPartyMember()) {
                                endResting();
                        } else {
                                log_begin_group();
                                log_msg("Done resting...");
                                endResting();

                                if (player_party->isCamping())
                                        player_party->endCamping();
                                else if (player_party->isResting())
                                        player_party->endResting();

                                log_end_group();
                        }
                }

                endTurn();
                return;
        }

        else if (isGuarding()) {

                // -----------------------------------------------------------
                // Every hour have the guard repair the vehicle by some amount.
                //
                // When guarding is over the guard will wake up the party.
                // -----------------------------------------------------------

                class Vehicle *vehicle = player_party->getVehicle();
                if (clock_alarm_is_expired(&rest_alarm)) {
                        if (isPlayerControlled() &&
                            vehicle &&
                            vehicle->getHp() < 
                            vehicle->getMaxHp()) {
                                vehicle->heal(
                                        vehicle->getMaxHp() / 
                                        10);
                                foogodRepaint();
                                log_begin("%s repairs ", getName());
                                vehicle->describe();
                                log_end(".");
                        }
                        clock_alarm_set(&rest_alarm, 60);
                }

                if (clock_alarm_is_expired(&wakeup_alarm)) {
                        endGuarding();

                        if (isPlayerPartyMember()) {
                                if (player_party->isCamping())
                                        player_party->endCamping();
                                else if (player_party->isResting())
                                        player_party->endResting();
                        }
                }
                endTurn();
                return;
        }

        else if (isLoitering()) {
                if (clock_alarm_is_expired(&wakeup_alarm)) {
                        if (! isPlayerPartyMember()) {
                                endLoitering();
                        } else {
                                log_begin_group();
                                log_msg("Done loitering...");
                                endLoitering();

                                if (player_party->isLoitering())
                                        player_party->endLoitering();
                                log_end_group();
                        }
                }
                endTurn();
                return;
        }

        // ------------------------------------------------------------------
        // Check for cases that prevent the character from taking a turn. Note
        // that if the character is sleeping he will still take a turn iff the
        // sleep is part of his schedule.
        // ------------------------------------------------------------------

        if (isAsleep() && getActivity() != SLEEPING) {
                
                if (ambushedWhileCamping &&
                    ((rand() % 100) < PROB_AWAKEN)) {
                        endCamping();
                } else {
                        endTurn();
                        return;
                }
        }

        switch (getControlMode()) {
                
        case CONTROL_MODE_AUTO:

                // Loop until the turn is over or the character stops using
                // action points.
                points_last_loop = 0;
                while (! isTurnEnded() &&
                       getActionPoints() != points_last_loop) {
                        points_last_loop = action_points;

                        // Lookup this character's schedule. We only need to do
                        // it once per turn; the clock won't change in the
                        // loop. But we want to do it inside the loop so that
                        // if there's no AP the schedule won't change the
                        // character's state. This was specifically added so
                        // that An Zu could wake up sleeping NPC's for a few
                        // rounds by waking them up and socking them with an AP
                        // debt to keep them docile for a bit.
                        if (sched && ! appointmentChecked) {
                                getAppointment();
                                appointmentChecked = true;
                        }

                        switch (getActivity()) {
                        case COMMUTING:
                                // pathfind to next appointment
                                commute();
                                break;
                        case EATING:
                        case SLEEPING:
                                // do nothing
                                break;
                        case FIGHTING:
                        default:
                                // call the AI
                                ctrl(this);
                                break;
                        }
                }
                break;

        case CONTROL_MODE_PLAYER:

                /* Highlight the character & prompt the user */
                select(true);

                // Update name in foogod window
                if (PARTY_CONTROL_ROUND_ROBIN == player_party->getPartyControlMode()) {
                    foogod_set_title("Round Robin: %s", getName());
                    foogodRepaint();
                }

                // If the character is out-of-site then change the camera to
                // focus on the character.
                if (! mapIsInCameraView(getPlace(), getX(), getY())) {
                        mapCenterCamera(getX(), getY());
                        mapUpdate(0);
                }

                /* Hand control over to the player */
                ctrl(this);

                /* Increment the turn count once per combat round. */
                if (isSolo() || isLeader()) {
                        session_inc_turn_count();
                        foogodRepaint();
                }

                if (Session->reloaded)
                        /* Hack: this object has been destroyed. Leave
                         * now. Don't touch a thing. */
                        return;

                /* Un-highlight the character */
                select(false);

                break;


        case CONTROL_MODE_FOLLOW:

                // -----------------------------------------------------------
                // Follow the party leader.
                // -----------------------------------------------------------

                leader = player_party->get_leader();

                assert(leader);
                assert(this != leader);

                // -----------------------------------------------------------
                // Loop until the leader is one tile away, we run out of action
                // points, or we stop using action points (this last occurs
                // when we can't find a path)
                // -----------------------------------------------------------

                points_last_loop = 0;

                // Since this character is in follow mode it's keystroke hook
                // hasn't been run yet. Do it now so that effects like
                // paralysis work properly on PC's in follow mode.
                runHook(OBJ_HOOK_KEYSTROKE, 0);
                if (isTurnEnded()) {
                        break;
                }

                noHostiles = ! place_contains_hostiles(getPlace(), this);

                while (1 < place_flying_distance(Place, getX(), getY(), 
                                                 leader->getX(), 
                                                 leader->getY()) 
                       && (noHostiles || ! isTurnEnded())
                       && getActionPoints() != points_last_loop
                        ) {

                        points_last_loop = getActionPoints();

                        // ----------------------------------------------------
                        // Take a step toward the leader, recompute
                        // line-of-sight and repaint to show the action.
                        // ----------------------------------------------------

                        pathfindTo(leader->getPlace(), 
                                   leader->getX(), 
                                   leader->getY(),
                                   PFLAG_IGNOREMECHS
                                   |PFLAG_IGNORECOMPANIONS
                                   |PFLAG_IGNORESTEPTRIG
                                );
                        mapCenterView(getView(), getX(), getY());
                        mapSetDirty();
                }

                // In follow mode don't accumulate action point depth. This
                // leads to annoying laggardliness in player party members.
                setActionPoints(0);
                break;
 
        case CONTROL_MODE_IDLE:
                break;

        case CONTROL_MODE_TASK:
            assert(taskproc);
            if (isPlayerControlled()) {
                log_msg("%s continues %s...", getName(), getTaskName());
            }
            closure_exec(taskproc, "pl", this, taskgob->p);
            break;

        default:
                assert(false);
                break;
        }

        endTurn();
}

void Character::setSolo(bool val)
{
        if (solo == val)
                return;

        if (val) {
            assert(isPlayerControlled());
            if (engagedInTask()) {
                taskAbort();
            }
            solo = val;
            attachCamera(true);
            setControlMode(CONTROL_MODE_PLAYER);
            log_msg("%s goes solo.", getName());
            mapCenterCamera(getX(), getY());
            mapSetDirty();
        } else {
            solo = val;
            attachCamera(false);
            setControlMode(CONTROL_MODE_IDLE);
        }
}

void Character::unCharm()
{
        // Check for illegal request
        if (0 == factionSwitch) {
                warn("%s:uncharm:factionSwitch 0", getName());
                return;
        }

        // Decrement the faction switch.
        factionSwitch--;

        // If the faction switch is still on then there are other charms in
        // effect and we don't want to disturb them. NOTE: the last faction
        // used with charm will remain in effect until the factionSwitch falls
        // to zero. This will match most expected behavior related to multiple
        // charm effects.
        if (factionSwitch)
                return;

        // Is this an NPC or a party member?
        if (! isPlayerPartyMember()) {
                
                // Revert the NPC to AI-control
                setControlMode(CONTROL_MODE_AUTO);
                
                // Remove the NPC's map view, if any
                if (NULL != getView()) {
                        rmView();
                        mapDestroyView(getView());
                        setView(NULL);
                }

                // Switch the controller back to the AI
                //ctrl = ctrl_character_ai;
                
        } else {

                // Set the party member's control mode based on the party's
                // current control mode.
                switch (player_party->getPartyControlMode()) {
                case PARTY_CONTROL_ROUND_ROBIN:
                        setControlMode(CONTROL_MODE_PLAYER);
                        break;
                case PARTY_CONTROL_SOLO:
                        setControlMode(CONTROL_MODE_IDLE);
                        break;
                case PARTY_CONTROL_FOLLOW:
                        if (isLeader()) {
                                setControlMode(CONTROL_MODE_PLAYER);
                        } else {
                                setControlMode(CONTROL_MODE_FOLLOW);
                        }
                        break;
                }
        }
}

void Character::charm(int newFaction)
{

        if (isDead())
                return;

        // Set the temporary faction (possibly clobbering the previous one).
        tmpFaction = newFaction;
        
        if (isPlayerPartyMember())
                // Switch the party member to be AI-controlled.
                setControlMode(CONTROL_MODE_AUTO);
        else {
                // Add a map view for the non-party member
                setView(mapCreateView());
                addView();
                
                // Switch the non-party member to be player-controlled
                setControlMode(CONTROL_MODE_PLAYER);

                // Switch the controller over to the player
                // NOTE: this needs a bit more work to be nicely done
                // ctrl = ctrl_character_ui;
        }

        // Increment the faction switch (must do this AFTER calling
        // setControlMode())
        factionSwitch++;

}

bool Character::isCharmed()
{
        return getCurrentFaction() != getBaseFaction();
}

bool Character::isPlayerPartyMember()
{
        // If player party has not been created yet it's safe to assume that
        // for now this is not a party member.
        if (!Session || ! player_party) {
                return false;
        }
        return (class Object*)party == (class Object*)player_party;
}

void Character::setControlMode(enum control_mode mode)
{
        // -------------------------------------------------------------------
        // Player party calls in here to switch between follow, round-robin and
        // solo modes. Don't want to change the control mode of charmed
        // member's in this case. Always need to uncharm before switching
        // control modes.
        // -------------------------------------------------------------------

        if (isCharmed())
                return;

        control_mode = mode;

        switch (mode) {
        case CONTROL_MODE_AUTO:
                ctrl = ctrl_character_ai;
                break;
        case CONTROL_MODE_PLAYER:
        case CONTROL_MODE_IDLE:
        case CONTROL_MODE_FOLLOW:
                ctrl = ctrl_character_ui;
                break;
        case CONTROL_MODE_TASK:
            break;
        }
}


bool Character::add(ObjectType *type, int amount)
{
        if (isPlayerPartyMember()) {
                return player_party->add(type, amount);
        } else if (container) {
                container->add(type, amount);
                return true;
        }

        return false;
}


bool Character::takeOut(ObjectType *type, int amount)
{
        if (isPlayerPartyMember()) {
                return player_party->takeOut(type, amount);
        } else if(container) {
                return container->takeOut(type, amount);                
        }
        return false;

}
bool Character::addFood(int quantity)
{
        if (isPlayerPartyMember()) {
                player_party->addFood(quantity);
                return true;
        }
        return false;
}

bool Character::addGold(int quantity)
{
        if (isPlayerPartyMember()) {
                player_party->addGold(quantity);
                return true;
        }
        return false;
}

void Character::beginResting(int hours)
{
        assert(hours > 0);

        if (isDead())
                return;

        clock_alarm_set(&wakeup_alarm, hours * 60);
        clock_alarm_set(&rest_alarm, 60);
        resting = true;
        changeSleep(true);
}

void Character::endResting()
{
        resting = false;
        setControlMode(CONTROL_MODE_PLAYER);
        awaken();
}

bool Character::isResting()
{
        return resting;
}

void Character::beginLoitering(int hours)
{
        assert(hours > 0);

        if (isDead())
                return;

        clock_alarm_set(&wakeup_alarm, hours * 60);
        loitering = true;
}

void Character::endLoitering()
{
        loitering = false;
        if (! engagedInTask()) {
            setControlMode(CONTROL_MODE_PLAYER);
        }
}

bool Character::isLoitering()
{
        return loitering;
}

void Character::beginCamping(int hours)
{
        ambushedWhileCamping = false;
        beginResting(hours);
}

void Character::endCamping()
{
        ambushedWhileCamping = false;
        endResting();
}

bool Character::isCamping()
{
        return isResting();
}

void Character::ambushWhileCamping()
{
        resting = false;
        ambushedWhileCamping = true;

        if (!isAsleep())
                return;

        if ((rand() % 100) < PROB_AWAKEN) {
                endCamping();
        }

}

void Character::beginGuarding(int hours)
{
        assert(hours > 0);
        assert(!isAsleep());
        assert(!isDead());

        clock_alarm_set(&wakeup_alarm, hours * 60);
        clock_alarm_set(&rest_alarm, 60);
        setControlMode(CONTROL_MODE_IDLE);
        guarding  = true;
}

void Character::endGuarding()
{
        guarding = false;
        setControlMode(CONTROL_MODE_PLAYER);
}

bool Character::isGuarding()
{
        return guarding;
}

void Character::heal(int amount)
{
        amount = min(amount, getMaxHp() - hp);
        hp += amount;
        if (isPlayerControlled() && amount)
                statusFlash(getOrder(), Blue);
        /* fixme: if sufficiently healed should stop fleeing */
}

void Character::setLeader(bool val)
{
        if (is_leader == val)
                return;

        if (is_leader) {
                attachCamera(false);
                if (isPlayerControlled()) {
                    setControlMode(CONTROL_MODE_IDLE);
                }
        } else {
                attachCamera(true);
                setControlMode(CONTROL_MODE_PLAYER);
                //consolePrint("%s is now party leader.\n", getName());
                mapSetPlace(getPlace());
                mapCenterCamera(getX(), getY());
                mapSetDirty();
        }

        is_leader = val;
}

bool Character::canBeLeader()
{
        return (! isDead() && isOnMap() && ! isAsleep() && ! isCharmed() && isPlayerControlled()
                && (getActionPoints() > -(2*getActionPointsPerTurn()))
                && ! engagedInTask()
            );
}

bool Character::isLeader()
{
        return is_leader;
}

bool Character::isCompanionOf(class Object *other)
{
        // ------------------------------------------------------------------
        // Do the simple thing for now. This is only used in
        // the context of player party follow mode.
        // ------------------------------------------------------------------

        return isPlayerPartyMember() && other->isPlayerPartyMember();
}

bool Character::joinPlayer(void)
{
        class Party *old_party = party;

        if (old_party == player_party)
                return false;

        if (NULL != old_party) {
                old_party->removeMember(this);
        }
        
        if (player_party->addMember(this)) {
                addView();
                // Turn off any prior AI.
                setAI(NULL);
                return true;
        }

        if (NULL != old_party) {
                old_party->addMember(this);
        }

        setSchedule(NULL);

        return false;
}

void Character::leavePlayer(void)
{
        if (!isPlayerPartyMember())
                return;
        if (isSolo()) {
                assert(isPlayerControlled());
                player_party->enableRoundRobinMode();
        }

        bool wasLeader = isLeader();

        unreadyAll();

        player_party->removeMember(this);
        log_msg("%s leaves the party", getName());
        if (wasLeader) {
                player_party->enableFollowMode();
        }
}

int Character::getActivity()
{
        if (place_contains_hostiles(getPlace(), this))
                return FIGHTING;

        if (! sched)
                return NONE;
        getAppointment();
        if (atAppointment())
                return sched->appts[appt].act;
        return COMMUTING;
}

void Character::setActivity(int val)
{
        activity = val;
        changeSleep(activity == SLEEPING);
}

bool Character::canWanderTo(int newx, int newy)
{
        // If this party is on a schedule then limit wandering to the
        // area specied in the current appt.
        if (sched) {
                if (newx < sched->appts[appt].x ||
                    newx > (sched->appts[appt].x + sched->appts[appt].w - 1) ||
                    newy < sched->appts[appt].y ||
                    newy > (sched->appts[appt].y + sched->appts[appt].h) - 1)
                        return false;
        }

        return true;
}

void char_dtor(void *val)
{
        delete (class Character*)val;
}

void char_save(save_t *save, void *val)
{
        ((class Character*)val)->save(save);
}

void Character::save(struct save *save)
{
        class ArmsType *arms;

        if (saved == save->session_id) {
                save->write(save, "%s\n", tag);
                return;
        }

        saved = save->session_id;

        // Create it within a 'let' block
        save->enter(save, "(let ((kchar ");

        if (getGob()) {
                // wrap the declaration in a call to bind the object to the
                // gob 
                save->enter(save, "(bind\n");

                if (getForceContainerDrop()) {
                        save->enter(save, "(kern-char-force-drop");
                }
        }

        // Create the object within a 'let' block
        save->enter(save, "(let ((kchar ");
        save->enter(save, "(kern-mk-char\n");
        if (this->tag) {
                save->write(save, "\'%s  ; tag\n", this->tag );
        } else {
                save->write(save, "nil  ; tag\n");
        }
        save->write(save, "\"%s\"  ; name\n", this->getName());
        save->write(save, "%s  ; species\n",  this->species->tag);
        save->write(save, "%s  ; occ\n", this->occ ? this->occ->tag : "nil");
        sprite_save(current_sprite, save);
        /*save->write(save, "%s\n", sprite_get_tag(this->sprite));*/
        save->write(save, "%d  ; BaseFaction\n", getBaseFaction());
        save->write(save, "%d %d %d  ; str, int, dex\n",    str, intl, dex);
        save->write(save, "%d %d  ;    hp_mod, hp_mult\n",  this->hp_mod,    this->hp_mult);
        save->write(save, "%d %d  ;    mp_mod, mp_mult\n",  this->mp_mod,    this->mp_mult);
        save->write(save, "%d %d  ;    HP, XP\n",           this->getHp(),   this->getExperience());
        save->write(save, "%d  ;    mana\n",        this->getMana()  );
        save->write(save, "%d  ;    AP_per_round\n", this->AP_per_round );
        save->write(save, "%d  ;    level\n",       this->getLevel() );
        save->write(save, "#%c  ; dead?\n", isDead() ? 't' : 'f');

        if (conv != NULL) {
                conv_save(conv, save);
        } else {
                save->write(save, "nil  ; conv\n");
        }

        save->write(save, "%s  ; sched\n", sched? sched->tag : "nil");

        if (ai != NULL) {
                closure_save(ai, save);
        } else
                save->write(save, "nil  ; ai\n");

        // Items in personal inventory.
        if (!container) {
                save->write(save, "nil  ; inventory\n");
        } else {
                container->save(save);
        }

        // Readied items. Subtle: if this character has a container then these
        // items have already been saved.
		int armsIndex=0;
	arms = this->enumerateArms(&armsIndex);
        if (! arms) {
                save->write(save, "nil  ; readied arms\n");
        } else {
                save->enter(save, "(list\n");
                while (arms != NULL) {
                        save->write(save, "%s\n", arms->getTag());
                        arms = this->getNextArms(&armsIndex);
                }
                save->exit(save, ")  ; readied arms\n\n");
        }

        // Hooks
        Object::saveHooks(save);

        // Close the <var-list> part of the 'let' block
        save->exit(save, "))) ;; end ((kchar ...)\n");

        if (isSubmerged()) {
                save->write(save, "(kern-obj-set-submerged kchar #t)\n");
        }

        // Close the 'let' block
        save->exit(save, "kchar) ;; end (let ...)\n");

        if (getForceContainerDrop()) {
                save->exit(save, "#t) ;; kern-char-force-drop\n");
        }

        if (getGob()) {

                // save the gob list
                gob_save(getGob(), save);

                // end the bind call
                save->exit(save, ") ;; bind\n");
        }
        
        // close the args assignment section of the let block
        save->write(save, ")) ");

        if (isKnown()) {
                save->write(save, "(kern-char-set-known kchar #t)\n");
        }

        // Only save AP debt, else when the game reloads and the character
        // restarts its turn it will carry over what is saved as an unwarranted
        // bonus.
        if (getActionPoints() < 0) {
            save->write(save, "(kern-obj-set-ap kchar %d)\n", getActionPoints());
        }

        // save the task, if any
        if (taskproc) {
            save->write(save, "(kern-char-task-continue kchar \"%s\" ", getTaskName());
            closure_save(taskproc, save);
            if (taskgob) {
                gob_save(taskgob, save);
            } else {
                save->write(save, " nil");
            }
            save->write(save, ")\n");
        }

        // close the 'let' block
        save->exit(save, "kchar)\n");
}

void Character::setSchedule(struct sched *val)
{

        sched = val;

        if (sched) {
                sched_chars_node = session_add_sched_char(Session, this);
        } else {
                // Bugfix: if a character is eating and the schedule is set to
                // NULL then the character will be stuck eating (and doing
                // nothing else!).
                setActivity(NONE);
        }

        // Bugfix: if an npc with a schedule is added to the player party
        // either during the game or on reload, and it has a schedule, then the
        // next time the player enters a town the scheduling code will mess
        // with the party member.
        if (! sched && sched_chars_node) {
                session_rm_sched_char(sched_chars_node);
                sched_chars_node = 0;
        }
}

bool Character::tryToRelocateToNewPlace(struct place *newplace, 
                                        int newx, int newy,
                                        struct closure *closure)
{
        // NPCs and charmed PCs are not allowed to change places because I
        // don't want them dissappearing into the ether.
        //
        // Addendum: this needs to work for characters with multi-place
        // schedules. If I only forbid entry to the wilderness this should work
        // as originally intended.
        if (place_is_wilderness(newplace)
            && (! isPlayerControlled())) {
                return false;
        }

        // -----------------------------------------------------------------
        // At this point I know the character is player-controlled, so I can
        // print informative messages.
        // -----------------------------------------------------------------

        if (player_party->get_num_living_members() == 1) {
                // Force to follow mode to avoid the annoying case
                // where only one member is in the party and the player
                // wants to leave a combat map.
                player_party->enableFollowMode();
        }

        if (player_party->getPartyControlMode() != PARTY_CONTROL_FOLLOW) {
                log_msg("Exit - must be in follow mode!");
                return false;
        }

        if (player_party->get_leader() != this)
                return false;

        if (!player_party->rendezvous(getPlace(), getX(), getY())) {
                log_msg("Exit - party can't rendezvous!");
                return false;
        }

        groupExitTo(newplace, newx, newy, closure);

        return true;

}

void Character::addDefense(int val)
{
        defenseBonus += val;
}

struct mmode *Character::getMovementMode()
{
        if (currentMmode) {
                return currentMmode;
        } else {
                return species->mmode;
        }
}

void Character::setMovementMode(struct mmode *mmode)
{
        currentMmode = mmode;
}

void Character::setCurrentFaction(int faction)
{
#if 1
    Being::setCurrentFaction(faction);

    // We need to implement charm differently. This is the wrong place to do
    // it.
#else
        if (! isPlayerPartyMember()) {
                Being::setCurrentFaction(faction);
        } else {
                if (faction != player_party->getBaseFaction()) {
                        // player party member charmed

                        // Fix for [ 1425039 ] Solo mode + possession = lockup:
                        // if in solo mode, switch party to follow mode
                        if (isSolo()) {
                                setSolo(false);
                                player_party->enableFollowMode();
                        }

                        setControlMode(CONTROL_MODE_AUTO);
                        
                        // subtle: in this case, do this AFTER calling
                        // setControlMode() (charmed character's ignore control
                        // mode changes)
                        Being::setCurrentFaction(faction);

                } else {
                        // subtle: in this case, do this BEFORE calling
                        // setControlMode() (opposite of above)
                        Being::setCurrentFaction(faction);

                        // player party member uncharmed
                        switch (player_party->getPartyControlMode()) {
                        case PARTY_CONTROL_ROUND_ROBIN:
                                setControlMode(CONTROL_MODE_PLAYER);
                                break;
                        case PARTY_CONTROL_SOLO:
                                setControlMode(CONTROL_MODE_IDLE);
                                break;
                        case PARTY_CONTROL_FOLLOW:
                                if (isLeader()) {
                                        setControlMode(CONTROL_MODE_PLAYER);
                                } else {
                                        setControlMode(CONTROL_MODE_FOLLOW);
                                }
                                break;
                        }
                }
        }
#endif
}

class Container* Character::getInventoryContainer()
{
        /* for player-controlled maybe return party inventory? */
        if (isPlayerControlled())
                return player_party->inventory;
        return container;
}

void Character::setInventoryContainer(class Container *val)
{
        // Blow away the old one
        if (container) {
                assert(container->isEmpty());
                obj_dec_ref(container);
        }

        container = val;
        if (container) {
                obj_inc_ref(container);
        }
}

void Character::setAI(struct closure *val)
{
        // out with the old
        if (ai) {
                closure_unref(ai);
                ai = NULL;
        }
        
        // in with the new
        if (val) {
                closure_ref(val);
                ai = val;
        }
}

struct closure *Character::getAI()
{
        return ai;
}

void Character::setDead(bool val)
{
        dead = val;
}

bool Character::getForceContainerDrop()
{
        return forceContainerDrop;
}

void Character::setForceContainerDrop(bool val)
{
        forceContainerDrop = val;
}

bool Character::isStationary()
{
        return species->stationary;
}

bool Character::isKnown()
{
        return known;
}

void Character::setKnown(bool val)
{
        known = val;
}

const char * Character::getTaskName()
{
    return taskname ? taskname : "a nameless task";
}

void Character::taskCleanup()
{
    if (taskname) {
        free(taskname);
        taskname = NULL;
    }

    if (taskproc) {
        closure_unref(taskproc);
        taskproc = NULL;
    }

    if (taskgob) {
        gob_unref(taskgob);
        taskgob = NULL;
    }

    if (isPlayerPartyMember()) {
        class Character *oldLeader, *newLeader;
        switch (player_party->getPartyControlMode()) {
        case PARTY_CONTROL_FOLLOW:
            oldLeader = player_party->get_leader();
            player_party->chooseNewLeader();
            newLeader = player_party->get_leader();
            if (oldLeader != newLeader) {
                oldLeader->setControlMode(CONTROL_MODE_FOLLOW);
            }
            if (newLeader != this) {
                setControlMode(CONTROL_MODE_FOLLOW);
            }
            break;
        case PARTY_CONTROL_SOLO:
            setControlMode(CONTROL_MODE_IDLE);
            break;
        case PARTY_CONTROL_ROUND_ROBIN:
            setControlMode(CONTROL_MODE_PLAYER);
            break;
        }
    }
}

void Character::taskAbort()
{
    log_msg("%s aborts %s!", getName(), getTaskName());
    taskCleanup();
}

void Character::taskSetup(char *name_arg, struct closure *proc_arg, struct gob *gob_arg)
{
    assert(! engagedInTask());

    if (name_arg) {
        taskname = strdup(name_arg);
    }

    assert(proc_arg);
    taskproc = proc_arg;
    closure_ref(taskproc);

    if (gob_arg) {
        taskgob = gob_arg;
        gob_ref(gob_arg);
    }

    if (isPlayerPartyMember()) {
        switch (player_party->getPartyControlMode()) {
        case PARTY_CONTROL_FOLLOW:
            if (isLeader()) {
                player_party->chooseNewLeader();
                assert(this != player_party->get_leader());
            }
            break;
        case PARTY_CONTROL_SOLO:
            if (isSolo()) {
                setSolo(false); // sets CONTROL_MODE_IDLE
                player_party->enableFollowMode();
            }
            break;
        case PARTY_CONTROL_ROUND_ROBIN:
            break;
        }
    }

    setControlMode(CONTROL_MODE_TASK);
    taskInterruptOnDamage = true;
}

void Character::taskBegin(char *name_arg, struct closure *proc_arg, struct gob *gob_arg)
{
    taskSetup(name_arg, proc_arg, gob_arg);
    log_msg("%s begins %s.", getName(), getTaskName());
    endTurn();
}

void Character::taskContinue(char *name_arg, struct closure *proc_arg, struct gob *gob_arg)
{
    if (isPlayerControlled()) {
        log_msg("%s continues %s.", getName(), getTaskName());
    }

    taskSetup(name_arg, proc_arg, gob_arg);
}

void Character::taskEnd()
{
    log_msg("%s completes %s!", getName(), getTaskName());
    taskCleanup();
}

bool Character::engagedInTask()
{
    return taskproc != NULL;
}
