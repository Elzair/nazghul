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
#include "map.h"
#include "console.h"
#include "place.h"
#include "Container.h"
#include "Arms.h"
#include "player.h"
#include "status.h"
#include "sound.h"
#include "common.h"
#include "Trap.h"
#include "screen.h"
#include "Loader.h"
#include "knapsack.h"
#include "occ.h"
#include "species.h"
#include "sched.h"
#include "Mech.h"
#include "combat.h"
#include "Spell.h"
#include "cmdwin.h"
#include "terrain.h"
#include "cmd.h"
#include "event.h"
#include "portal.h"
#include "vehicle.h"
#include "foogod.h"

#include <string.h>
#include <stdlib.h>
#include <math.h>

static bool myUnreadyDepletedThrownWeapon(class Character * pc, void *data)
{
	assert(pc->isPlayerControlled());
	struct inv_entry *ie = (struct inv_entry *) data;
	assert(ie->type->isType(ARMS_TYPE_ID));
	if (pc->unready((class ArmsType *) ie->type))
		ie->ref--;
	return false;
}

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
	int val = arms->getDamageMax() * arms->getRange() +
	    arms->getArmorMax();
	for (int i = 0; i < ie->count; i++, ks->n_items++) {
		ks->item[ks->n_items] = arms;
		ks->value[ks->n_items] = val;
	}
}

Character::Character():name(0), hm(0), xp(0), order(-1),
                       sleeping(false),
                       ac(0), armsIndex(-1),
                       str(0), intl(0),
                       dex(0), mana(0), lvl(0), poisoned(false),
                       playerControlled(true), solo(false),
                       currentArms(NULL), target(NULL),
                       rdyArms(NULL),
                       fleeing(false), fleeX(0), fleeY(0), burden(0),
                       native_alignment(0), inCombat(false),
                       container(NULL), sprite(0), n_rest_credits(0), elevated(false)
{
	list_init(&plist);
	list_init(&llist);
	setPlayerControlled(false);	// by default

	light        = MIN_PLAYER_LIGHT;
	tag          = 0;
	party        = 0;
	conv         = 0;
	sched        = 0;
	species      = 0;
	occ          = 0;
	is_clone     = false;
	visible      = 1;
	occ          = 0;
	target       = 0;
	path         = 0;
        damage_sound = NULL;
        charmed      = false;
        resting      = false;
        guarding     = false;
        mp_mod       = 0;
        mp_mult      = 0;
        hp_mod       = 0;
        hp_mult      = 0;
        sched        = NULL;
        conv         = NULL;
        activity     = NONE;
        appt         = 0;
        is_leader    = false;
}


Character::~Character()
{
	if (name)
		free(name);
	if (view) {
#if 0
		mapRmView(view);
#else
                rmView();
#endif
		free(view);
	}

	if (container)
		delete container;

	if (rdyArms != NULL)
		free(rdyArms);

        if (damage_sound)
                free(damage_sound);
}

void Character::damage(int damage)
{
        if (hp <= 0)
                return;

        soundPlay(get_damage_sound(), SOUND_MAX_VOLUME);

        // Hack: add a clone effect for slimes
        if (player_party->getContext() & CONTEXT_COMBAT &&
            species->effects & EFFECT_CLONE &&
            (rand() % 4) == 0) {
                printf("Cloning %s\n", getName());
                class Character *copy = clone(this);
                assert(copy);
                class NpcParty *copy_party = new NpcParty();
                assert(copy_party);
                copy->setAlignment(this->getAlignment());
                copy_party->init(copy);
                if (!combatAddNpcParty(copy_party, 0, 0, true, getPlace(), getX(), getY())) {
                        delete copy_party;
                        delete copy;                                
                } else {
                        consolePrint("%s cloned at [%d %d]...", 
                                     getName(),
                                     copy->getX(), copy->getY());
                }
        }

        damage = min(damage, hp);
	hp -= damage;
	if (hp <= 0)
		kill();

	if (isPlayerControlled()) {
		statusFlash(getOrder(), Red);
	} else {
		if (!isDead() && inCombat && getHp() < (getMaxHp() / 4))
			setFleeing(true);
	}
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
		return true;
	}

	return false;
}

char *Character::getWoundDescription()
{
	static char *desc[] = {
		"Critical!",
		"Heavily wounded!",
		"Moderately wounded!",
		"Barely wounded!",
		"Unscathed!"
	};

	if (isDead())
		return "Killed!";

	if (isFleeing())
		return "Fleeing!";

	return desc[(getHp() * 4) / getMaxHp()];
}

static void uncharm_victims(class Object * obj, void *data)
{
        if (obj->isCharmed())
                obj->unCharm();
}

void Character::groupExitTo(struct place *dest_place, int dest_x, int dest_y)
{
        place_for_each_object(getPlace(), uncharm_victims, NULL);
        player_party->removeMembers();        
#if 0
        mapRmView(ALL_VIEWS);
        mapAddView(player_party->view);
        mapSetDirty();
#endif
        player_party->relocate(dest_place, dest_x, dest_y);
        endTurn();
}

enum Character::MoveResult Character::move(int dx, int dy)
{
	int newx, newy;
	class Character *occupant;

        // ---------------------------------------------------------------------
        // Let's give this next a try, in order to make the code for teleport
        // spells simpler. If a teleport spell says to teleport the caster,
        // then perhaps it shouldn't have to concern itself with whether the
        // caster is in party mode or member mode. It just tells the caster to
        // move, and the caster then checks its context to see if this means
        // "move the member" or "move the whole party".
        // ---------------------------------------------------------------------

        if (!isOnMap()) {
                if (isPlayerControlled()) {

                        // -----------------------------------------------------
                        // Hack: if dx or dy is greater than 1 then set the
                        // 'teleport' argument to the player party move() to
                        // true.
                        // -----------------------------------------------------

                        return (player_party->move(dx, dy, (dx > 1 || dy > 1)) ? MovedOk : WasImpassable);
                } else {
                        return (party->move(dx, dy) ? MovedOk : WasImpassable);
                }
        }

	// Calculate new coordinates.
	newx = getX() + dx;
	newy = getY() + dy;


	// ---------------------------------------------------------------------
	// Is the character walking off the edge of the map? The same rules as
	// entering a portal apply here: the party must be in follow mode, and
        // all other members must be able to pathfind to this location.
        //
        // Addendum: since this is not strictly necessary for wilderness combat,
        // and it is something of an inconvenience to the user, I skip the
        // checks for wilderness combat.
	// ---------------------------------------------------------------------

	if (place_off_map(getPlace(), newx, newy)) {

	  // -------------------------------------------------------------------
	  // Npc characters can just step off and will be removed from
	  // the game.
	  // -------------------------------------------------------------------

	  if (! isPlayerControlled() || isCharmed()) {
                        remove();
                        destroy();
                        return ExitedMap;
	  }

                if (place_is_wilderness_combat(getPlace())) {
                        remove();
                        return ExitedMap;
                }

                if (place_get_parent(getPlace()) == NULL)
                        return OffMap;
                
                if (player_party->getPartyControlMode() != PARTY_CONTROL_FOLLOW) {
                        return NotFollowMode;
                }
                
                if (!combat_rendezvous_party(player_party->n_pc * 2)) {
                        return CantRendezvous;
                }

                groupExitTo(place_get_parent(getPlace()), place_get_x(getPlace()), place_get_y(getPlace()));

		return ExitedMap;
	}

        // ---------------------------------------------------------------------
        // Check passability. If commuting then ignore closed doors (and other
        // blocking mechs).
        // ---------------------------------------------------------------------

	if (!place_is_passable(getPlace(), newx, newy, getPmask(), activity == COMMUTING ? PFLAG_IGNOREMECHS : 0)) {
		return WasImpassable;
	}

	// Are the new coordinates already occupied by another character?
	if ((occupant = (class Character *) place_get_object(getPlace(), newx, newy, being_layer))) {


                

		// Is the occupant an enemy?
		if (!(getAlignment() & occupant->getAlignment())) {
			return WasImpassable;
		}

		// *** Switch ***

		if (isPlayerControlled() && 
                    occupant->isPlayerControlled() &&
		    isSelected()) {

			// Special case: if both the occupant and this
			// character are player-controlled then have them
			// switch places if possible.  This prevents the
			// situation where the player party is in follow mode
			// and the leader gets boxed in by the other members.
			// Note that in this case I'm ignoring movement cost
			// (pretend that the characters help each other across
			// the rough terrain... yeah, that's it!).

			int oldx, oldy;
			class Character *oldTarget;
			struct place *oldPlace;
			bool wasSolo;

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
                        if (!place_is_passable(getPlace(), getX(), getY(), occupant->getPmask(), 0)) {
                                relocate(getPlace(), newx, newy);
                                decActionPoints(place_get_movement_cost(getPlace(), newx, newy));
                                return MovedOk;
        
                        }


			oldx = getX();
			oldy = getY();

                        // Save these before calling remove because remove()
                        // automatically resets these to defaults (for sane
                        // reasons... I think)
			oldTarget = target;
			oldPlace = getPlace();
			wasSolo = isSolo();

			//remove();
			occupant->relocate(oldPlace, oldx, oldy);
			relocate(oldPlace, newx, newy);
                        decActionPoints(place_get_movement_cost(getPlace(), getX(), getY()));
			setAttackTarget(oldTarget);
                        setSolo(wasSolo);

			return SwitchedOccupants;
		}

		return WasOccupied;
	}

	relocate(getPlace(), newx, newy);
        decActionPoints(place_get_movement_cost(getPlace(), newx, newy));

	return MovedOk;
}

void Character::remove()
{
	Object::remove();
	setAttackTarget(this);

        // ---------------------------------------------------------------------
        // Handle changes to party control.
        // ---------------------------------------------------------------------

        if (isSolo()) {
                assert(isPlayerControlled());
                player_party->enableRoundRobinMode();
        } else if (isLeader()) {
                assert(isPlayerControlled());
                player_party->enableFollowMode();
        }
}

class ArmsType *Character::enumerateWeapons(void)
{
	armsIndex = -1;
	currentArms = NULL;
	getNextWeapon();
	if (!currentArms)
		currentArms = species->weapon;
	return currentArms;
}

class ArmsType *Character::getNextWeapon(void)
{
	do {
		getNextArms();
	} while (currentArms != NULL && currentArms->getDamageMax() <= 0);
	return currentArms;
}

bool Character::isAttackTargetInRange()
{
	int dx, dy, distance;

	if (target == NULL || currentArms == NULL)
		return false;

	dx = target->getX() - getX();
	dy = target->getY() - getY();

	if (dx < 0)
		dx = -dx;
	if (dy < 0)
		dy = -dy;

	distance = (dx > dy) ? (dx + (dy >> 1)) : (dy + (dx >> 1));

	return (currentArms->getRange() >= distance);
}

class Character *Character::getAttackTarget(void)
{
	// Is the old target still valid?
	if (!target || !target->isOnMap() || target->isDead() ||
	    !isAttackTargetInRange() || !target->isVisible()) {
		target = this;
		return this;
	}

	return target;
}

bool Character::hasReadied(class ArmsType * arms)
{
	class ArmsType *readied = enumerateArms();
	while (readied != NULL && readied != arms)
		readied = getNextArms();
	return readied == arms;
}

class ArmsType *Character::enumerateArms(void)
{
	armsIndex = -1;
	currentArms = NULL;
	getNextArms();
	return currentArms;
}

class ArmsType *Character::getNextArms(void)
{
	// Advance to the next slot
	armsIndex++;

	// Search remaining slots for a weapon
	for (; armsIndex < species->n_slots; armsIndex++) {

		// Is anything in this slot?
		if (rdyArms[armsIndex] == NULL)
			continue;

		// Is this just another slot for the same weapon (happens in
		// the case of multi-slotted weapons like 2h swords)?
		if (currentArms == rdyArms[armsIndex] &&
		    currentArms->getNumHands() == 2)
			continue;

		currentArms = rdyArms[armsIndex];
		if (!currentArms)
			continue;

		return currentArms;
	}

	currentArms = NULL;
	return 0;
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
			ie = player_party->search_inventory(weapon->
							    getMissileType());
                        if (ie == NULL)
                                return 0;  // No ammo
			return ie->count;  // 1 or more
		}
                else if (weapon->isThrownWeapon()) {
			ie = player_party->search_inventory(weapon);
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
                         container->search(weapon->getMissileType())));
	}
} // Character::hasAmmo()

void Character::changeLight(int delta)
{
	light += delta;
	light = max(light, MIN_PLAYER_LIGHT);
	mapRecomputeLos(view);
}

void Character::changeMana(int delta)
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

                // -------------------------------------------------------------
                // Going to sleep.
                // -------------------------------------------------------------

                if (isLeader()) {
                        assert(isPlayerControlled());
                        player_party->enableFollowMode();
                } else if (isSolo()) {
                        assert(isPlayerControlled());
                        player_party->enableRoundRobinMode();
                }

        } else {

                if (isPlayerControlled()) {

                        // -----------------------------------------------------
                        // Upon waking up, set this character's control mode
                        // based on the party's control mode.
                        // -----------------------------------------------------

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

#if 0
        // Not sure this is relevant any more...

        // ---------------------------------------------------------------------
        // Special case: if this character just woke up, and it is a player
        // party member, and it is the only member on the map then it will be
        // the party leader. Make sure that it is player-controlled.
        // ---------------------------------------------------------------------

        if (! val                              &&
            isPlayerControlled()               &&
            isOnMap()                          &&
            this == player_party->get_leader() &&
            getControlMode() != CONTROL_MODE_PLAYER) {

                setControlMode(CONTROL_MODE_PLAYER);
                consolePrint("%s is the new party leader.\n", this->getName());
        }
#endif
}

void Character::awaken(void)
{
        changeSleep(false);
        consolePrint("%s wakes up!\n", getName());
}

void Character::setFleeing(bool val)
{
	if (fleeing == val)
		return;

	fleeing = val;

	if (!fleeing) {
		fleeX = fleeY = 0;
		return;
	}

	// Pick a direction to flee. Don't bother pathfinding, just look for
	// the nearest edge and move toward it. After all, the Character is
	// supposed to be fleeing in a panic so it doesn't have to be too smart
	// about it.

	int leftx = getX();
	int rightx = place_w(getPlace()) - getX();
	int topy = getY();
	int boty = place_h(getPlace()) - getY();
	int dx, minx, dy, miny;

	// Is the left edge nearer than the right edge?
	if (leftx < rightx) {
		dx = -1;
		minx = leftx;
	} else {
		dx = 1;
		minx = leftx;
	}

	// Is the top edge nearer than the bottom edge?
	if (topy < boty) {
		dy = -1;
		miny = topy;
	} else {
		dy = 1;
		miny = boty;
	}

	// Flee horizontally?
	if (minx < miny) {
		fleeX = dx;
	} else {
		fleeY = dy;
	}
}

enum Character::MoveResult Character::flee()
{
	return move(fleeX, fleeY);
}

void Character::attackTerrain(int x, int y)
{
	class ArmsType *weapon = getCurrentWeapon();
	weapon->fire(getPlace(), getX(), getY(), x, y);
	useAmmo();
}

void Character::rejuvenate(void)
{
	// Clear all conditions and restore health and mana to full capacity.

	setAlignment(party->getAlignment());
	heal(getMaxHp());
	mana = lvl * HP_PER_LVL;
	if (isAsleep())
		awaken();
}

void Character::dropRdyArms()
{
	assert(!isPlayerControlled());

	for (int i = 0; i < species->n_slots; i++) {

		// Anything in this slot?
		if (rdyArms[i] == NULL)
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

bool Character::dropItems()
{
	assert(!isPlayerControlled());

	if (container == NULL)
		return false;
	container->relocate(getPlace(), getX(), getY());

	// I have to set this to NULL so that we don't double-delete the
	// container: once when the Character destructor calls and once when
	// the player opens the container or exits the map.
	container = NULL;

	return true;
}

void Character::kill()
{
	if (!isPlayerControlled() && isOnMap()) {
		dropRdyArms();
		dropItems();
	}

	hp = 0;
	remove();
}

void Character::initItems()
{
	assert(!isPlayerControlled());

	// If the character's occupation does not carry a container then this
	// character will not have any items.
	if (!occ || !occ->container)
		return;

	// Allocate a container to hold the items. (Note: the occupation keeps
	// the container TYPE, here we are allocating an instance of one and
	// initializing it to that type).
	container = new Container();
	if (container == NULL)
		return;
	container->init(occ->container);

	// Check if this occupation traps their containers. If so then randomly
	// choose one of the traps from the list.
	if (occ->n_traps) {

		int roll = random() % occ->n_traps;
		container->setTrap(occ->traps[roll]);
	}
	// Enumerate all the items this occupation tends to carry and roll to
	// include it (and how many of it to include) in the container.
	for (int i = 0; i < occ->n_items; i++) {

		if ((random() % 100) > occ->items[i].prob)
			continue;

		int n = random() % occ->items[i].n_max + 1;
		container->add(occ->items[i].type, n);
	}
}

void Character::useAmmo()
{
	class ArmsType *weapon = getCurrentWeapon();
	if (weapon->ammoIsUbiquitous())
		return;

	if (isPlayerControlled()) {
		struct inv_entry *ie;

		if (weapon->isMissileWeapon()) {
			class ArmsType *missileType = weapon->getMissileType();
			ie = player_party->search_inventory(missileType);
			player_party->remove_from_inventory(ie, 1);
		} else if (weapon->isThrownWeapon()) {
			ie = player_party->search_inventory(weapon);
			if (ie->count == 1) {

				// Multiple characters might have the same
				// thrown weapon readied. Although they will
				// eventually discover that they have no more
				// ammo, I want to unready them all now because
				// the 'Ztats' command will show them as
				// readied, and this would deceive the user,
				// who would think that although inventory is
				// exhausted the individual characters have one
				// remaining in their hands.

				player_party->for_each_member (myUnreadyDepletedThrownWeapon, ie);
			}
			player_party->remove_from_inventory(ie, 1);

		}
	} else {
		if (weapon->isMissileWeapon()) {
			container->subtract(weapon->getMissileType(), 1);
			if (!hasAmmo(weapon)) {
				unready(weapon);
				rearm = true;
			}
		} else if (weapon->isThrownWeapon()) {
			if (container->search(weapon) != NULL) {
				container->subtract(weapon, 1);
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

	assert(!isPlayerControlled());

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
			assert(0);
		container->subtract(arms, 1);
	}

      destroy_ks:
	if (ks.item)
		delete ks.item;
	if (ks.value)
		delete ks.value;
	if (ks.solution)
		delete ks.solution;

	rearm = false;
}

bool Character::needToRearm()
{
        // gmcnutt: I currently don't have any way to tell a charmed player
        // party member how to rearm themselves. Player characters pull from
        // party inventory. Non-player characters pull from their own personal
        // inventory. I really need to merge both types into the same behaviour
        // to simplify things.
        if (party == (NpcParty*)player_party)
                return false;

	return rearm;
}

bool Character::initCommon(void)
{
	// *** Basic ***

	// Normally Object::init() does this but it takes the layer from the
	// type and Character's have no special ObjectType.

	container_link.key = being_layer;

	// *** Slots ***

	rdyArms = new class ArmsType *[species->n_slots];
	memset(rdyArms, 0, species->n_slots * sizeof(class ArmsType *));

	return true;
}

bool Character::initStock(struct species * species, struct occ * occ,
			  struct sprite * sprite, char *name, int order,
			  int alignment)
{
	// This is to initialize characters being automatically generated to
	// fill out an NPC party. I think of them as "stock" characters since
	// they don't have much in the way of individual personality.

	this->species = species;
	this->occ = occ;
	this->sprite = sprite;

	if (!initCommon())
		return false;

	this->name = strdup(name);
	if (!this->name)
		return false;
	this->order = order;
	this->native_alignment = alignment;

	str = species->str;
	intl = species->intl;
	dex = species->dex;
	ac = species->ac;
	lvl = 1;		// fixme: hardcoded hack!

	hp = getMaxHp();
	mana = getMaxMana();

	initItems();
	armThyself();

	return true;
}

/*****************************************************************************/

void Character::cure(void)
{
	assert(isPlayerControlled());	// shotgun assert put here during
	// refactor
	setPoison(false);
	statusFlash(getOrder(), Blue);
}

void Character::resurrect(void)
{
	assert(isPlayerControlled());	// shotgun assert put here during refactor
	setHp(min(10, getMaxHp()));
	statusFlash(getOrder(), Blue);

        // ---------------------------------------------------------------------
        // If we're in wilderness mode then we're done. Otherwise we need to
        // put this character near the other party member's on the map.
        // ---------------------------------------------------------------------

        if (player_party->isOnMap())
                return;

        assert(player_party->get_leader());

        putOnMap(player_party->get_leader()->getPlace(), 
                 player_party->get_leader()->getX(),
                 player_party->get_leader()->getY(), 4);

        assert(isOnMap());

}

bool Character::load(class Loader * loader)
{
	char cond, *species_tag = 0, *occ_tag = 0, *sprite_tag = 0,
	    *type_tag = 0, *conv_tag = 0, *sched_tag = 0;
	class ArmsType *arms;

	// *** Parse Attributes ***

	// Parse up to the "ready" section. Before we can go any further we
	// need to call initCommon() so that it can allocate the slots before
	// we start trying to put things in them.

	if (!(loader->getWord(&tag) &&
	      loader->matchToken('{') &&
	      loader->matchWord("name") &&
	      loader->getString(&name) &&
	      loader->matchWord("species") &&
	      loader->getWord(&species_tag) &&
	      loader->matchWord("occ") &&
	      loader->getWord(&occ_tag) &&
	      loader->matchWord("sprite") &&
	      loader->getWord(&sprite_tag) &&

	      loader->matchWord("str") &&
	      loader->getInt(&str) &&
	      loader->matchWord("intl") &&
	      loader->getInt(&intl) &&
	      loader->matchWord("dex") &&
	      loader->getInt(&dex) &&
              
              loader->matchWord("hp_mod") &&
              loader->getInt(&hp_mod) &&
              loader->matchWord("hp_mult") &&
              loader->getInt(&hp_mult) &&
              loader->matchWord("mp_mod") &&
              loader->getInt(&mp_mod) &&
              loader->matchWord("mp_mult") &&
              loader->getInt(&mp_mult) &&
              loader->matchWord("hit_mod") &&
              loader->getInt(&hit_mod) &&
              loader->matchWord("def_mod") &&
              loader->getInt(&def_mod) &&
              loader->matchWord("dam_mod") &&
              loader->getInt(&dam_mod) &&
              loader->matchWord("arm_mod") &&
              loader->getInt(&arm_mod) &&

	      loader->matchWord("hp") &&
	      loader->getInt(&hp) &&
	      loader->matchWord("xp") &&
	      loader->getInt(&xp) &&
	      loader->matchWord("cond") &&
	      loader->getChar(&cond) &&
	      loader->matchWord("magic") &&
	      loader->getInt(&mana) &&
	      loader->matchWord("lvl") &&
	      loader->getInt(&lvl) &&
	      loader->matchWord("conv") && loader->getWord(&conv_tag)))
		return false;

	// *** Resolve Tags ***

	if (!(species = (struct species *) loader->lookupTag(species_tag,
							     SPECIES_ID))) {
		loader->setError("Invalid SPECIES tag '%s'", species_tag);
		goto fail;
	}

	if (!(occ = (struct occ *) loader->lookupTag(occ_tag, OCC_ID))) {
		loader->setError("Invalid OCC tag '%s'", occ_tag);
		goto fail;
	}

	if (!(sprite = (struct sprite *) loader->lookupTag(sprite_tag,
							   SPRITE_ID))) {
		loader->setError("Invalid SPRITE tag '%s'", sprite_tag);
		goto fail;
	}

	if (strcmp(conv_tag, "null") &&
	    (!(conv =
	       (struct conv *) loader->lookupTag(conv_tag,
						 CONVERSATION_TYPE_ID)))) {
		loader->setError("Invalid CONV tag '%s'", conv_tag);
		goto fail;
	}
	// *** Common Init ***

	if (!initCommon()) {
		loader->setError("Memory allocation failure");
		goto fail;
	}

	switch (cond) {
	case 'D':
		kill();
		break;
	case 'P':
		setPoison(true);
		break;
	case 'S':
		changeSleep(true);
		break;
	case 'G':
	default:
		setPoison(false);
		break;
	}

	// *** Ready Arms ***

	if (!loader->matchWord("readied") || !loader->matchToken('{'))
		goto fail;

	while (!loader->matchToken('}')) {
		if (!loader->getWord(&type_tag)) {
			loader->setError("Invalid armaments type tag '%s'",
					 type_tag);
			goto fail;
		}
		arms = (class ArmsType *) loader->lookupTag(type_tag,
							    ARMS_TYPE_ID);
		if (!arms || !arms->isType(ARMS_TYPE_ID)) {
			snprintf(loader->error, sizeof(loader->error),
				 "%s not an armament type", type_tag);
			free(type_tag);
			goto fail;
		}
		free(type_tag);
		ready(arms);
	}

	// *** Optional Parameters ***

        while (!loader->matchToken('}')) {

                if (loader->matchWord("sched")) {
                        if (!loader->getWord(&sched_tag))
                                goto fail;
                        if (!(sched = (struct sched *) 
                              loader->lookupTag(sched_tag,
                                                SCHEDULE_ID)))
                        {
                                loader->setError("Invalid SCHED tag '%s'", 
                                                 sched_tag);
                                goto fail;
                        }
                } else if (loader->matchWord("damage_sound")) {
                        if (!loader->getString(&damage_sound))
                                goto fail;

                } else {
                        loader->setError("Error in CHAR: unknown field '%s'",
                                         loader->getLexeme());
                        goto fail;
                }
	}

	// *** Constrain Attributes ***

	hp = min(hp, getMaxHp());
	mana = min(mana, getMaxMana());

	// *** Cleanup ***

	free(species_tag);
	free(occ_tag);
	free(sprite_tag);

        return true;

      fail:
	if (species_tag)
		free(species_tag);
	if (occ_tag)
		free(occ_tag);
	if (sprite_tag)
		free(sprite_tag);
	if (conv_tag)
		free(conv_tag);
	if (sched_tag)
		free(sched_tag);
	return false;
}

struct sprite *Character::getSprite()
{
	if ((isAsleep() || isDead()) && species->sleep_sprite)
		return species->sleep_sprite;
	return sprite;
}

void Character::setRestCredits(int hours)
{
	n_rest_credits = hours;
}

void Character::addRestCredits(int delta_hours)
{
	n_rest_credits += delta_hours;
	n_rest_credits = min(n_rest_credits, MAX_USEFUL_REST_HOURS_PER_DAY);
}

int Character::getRestCredits(void)
{
	return n_rest_credits;
}

void Character::rest(int hours)
{
	while (n_rest_credits && hours) {
                if (!isDead()) {
                        heal(HP_RECOVERED_PER_HOUR_OF_REST);
                        changeMana(MANA_RECOVERED_PER_HOUR_OF_REST);
                }
		n_rest_credits--;
		hours--;
	}
}

void Character::addExperience(int amount)
{
	double lxp;

	xp += amount;
	lxp = pow(2, lvl + 7);
	if (xp >= lxp) {
		lvl++;		// elevate to the next level
		setElevated(true);
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
        
        return (base + getLevel() * mult);
}

void Character::changeArmourClass(int delta)
{
	ac += delta;
	ac = max(0, ac);
}

class Character *Character::clone(class Character * character)
{
	char buf[64];
	class Character *clone = new Character();
	if (!clone)
		return NULL;

	snprintf(buf, sizeof(buf), "clone of %s", character->getName());
	clone->initStock(character->species, character->occ, character->sprite,
			 buf, 0, 0);
	clone->is_clone = true;
	return clone;
}

bool Character::isVisible()
{
	return ((visible > 0) && species->visible);
}

bool Character::isShaded()
{
	// Friendly invisible characters are shaded
	// return (!isHostile(Player.alignment) && !isVisible());
	return isPlayerControlled();
}

void Character::setVisible(bool val)
{
	if (val)
		visible++;
	else
		visible--;
}

void Character::describe(int count)
{
	if (isvowel(species->name[0]))
		consolePrint("an ");
	else
		consolePrint("a ");
	consolePrint("%s", species->name);
        if (occ && occ->name)
                consolePrint(" %s", occ->name);
        if (!isVisible())
                consolePrint(" (invisible)");
}

void Character::relocate(struct place *place, int x, int y)
{
	class Mech *mech;

	Object::relocate(place, x, y);

#if 0
        // ---------------------------------------------------------------------
        // The following duplicates what you'll currently find in
        // player_party::relocate(). I considered moving it to the base
        // Object::relocate() but not all objects have views. Any object with a
        // camera attached probably does. Something to consider. It would save
        // us an extra mapUpdate().
        // ---------------------------------------------------------------------

        if (isCameraAttached()) {
                mapCenterView(getView(), getX(), getY());
                mapRecomputeLos(getView());
                mapSetDirty();
        }
#endif

	mech = (class Mech *) place_get_object(place, x, y, mech_layer);
	if (mech)
		mech->activate(MECH_STEP);

}

char *Character::get_damage_sound()
{
        if (damage_sound)
                return damage_sound;
        if (species && species->damage_sound)
                return species->damage_sound;
        return 0;
}

char *Character::get_movement_sound()
{
        if (species)
                return species->movement_sound;
        return 0;
}

bool Character::isType(int classID) {
        if (classID == CHARACTER_ID)
                return true;
        return Object::isType(classID);
}

int Character::getType() {
        return CHARACTER_ID;
}

char *Character::getName() {
        return name;
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

unsigned char Character::getLevel() {
        return lvl;
}

bool Character::isDead() {
        return (hp == 0);
}

bool Character::isPoisoned() {
        return poisoned;
}

bool Character::isAsleep() {
        return sleeping;
}

bool Character::isIncapacitated() {
        return (!isOnMap() || isDead() || isAsleep());
}

int Character::getPmask() {
        return species->pmask;
}

int Character::getArmourClass() {
        return ac;
}

void Character::setPoison(bool val) {
        if (val && 
            species->immunities & EFFECT_POISON) {
                return;
        }
        poisoned = val;
}

void Character::setHp(int hp) {
        this->hp = hp;
}

bool Character::isPlayerControlled() {
        return playerControlled;
}

void Character::setPlayerControlled(bool val) {
        playerControlled = val;
}

void Character::setAttackTarget(class Character * target) {
        this->target = target;
}

class ArmsType *Character::getCurrentWeapon() {
        return currentArms;
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
        return species->spd;
}

int Character::getMana() {
        return mana;
}

bool Character::isFleeing() {
        return fleeing;
}

int Character::getFleeDx() {
        return fleeX;
}

int Character::getFleeDy() {
        return fleeY;
}

void Character::setName(char *name) {
        this->name = strdup(name);
}

void Character::setOrder(int order) {
        this->order = order;
}

int Character::getAlignment() {
        if (charmed)
                return charmed_alignment;
        return native_alignment;
}

void Character::setAlignment(int val) {
        native_alignment = val;
}

bool Character::isHostile(int alignment) {
        return ((getAlignment() & alignment) == 0);
}

void Character::setCombat(bool val) {
        inCombat = val;
}

bool Character::wasElevated(void) {
        return elevated;
}

void Character::setElevated(bool val) {
        elevated = val;
}

int Character::getDefend()
{
        int defend = 0;

        if (isAsleep())
                return -3; // hack: hard-coded constant

        for (class ArmsType * arms = enumerateArms(); arms != NULL; arms = getNextArms()) {
                defend += arms->getDefend();
        }
        
        return defend;
}

int Character::getArmor()
{
        int armor = 0;

        for (class ArmsType * arms = enumerateArms();
             arms != NULL; arms = getNextArms()) {
                armor += arms->getArmor();
        }

        // the obsolescent 'armor class' is still used by the 'protect' spell
        // effect
        armor += ac;
        
        return armor;

}

class NpcParty *Character::getParty()
{
        return party;
}

void Character::burn()
{
        damage(DAMAGE_FIRE);
        consolePrint("%s burning-%s!\n", getName(), getWoundDescription());
        consoleRepaint();
}

void Character::poison()
{
        setPoison(true);
        damage(DAMAGE_POISON);
        consolePrint("%s poisoned-%s!\n", getName(), getWoundDescription());
        consoleRepaint();
}

void Character::sleep()
{
        changeSleep(true);
        consolePrint("%s sleeping!\n", getName());
        consoleRepaint();
}

bool Character::canSee(class Object *obj)
{
        return (obj->getPlace() == getPlace() &&
                place_flying_distance(getPlace(), getX(), getY(), obj->getX(), obj->getY()) <= getVisionRadius() &&
                obj->isVisible());
}

void Character::ai_select_target(void)
{
        struct list *head;
        struct list *elem;
        class Object *obj;
        class Object *new_target = NULL;
        int min_distance = 1000;
        int distance;

        /* Get a list of all the objects within vision radius. */
        head = place_get_all_objects(getPlace());

        /* Walk the list, looking for the nearest hostile character. */
        list_for_each(head, elem) {
                
	  obj = outcast(elem, class Object, turn_list);

                /* Skip invalid targets */
                if (obj == this ||
                    !obj->isOnMap() ||
                    obj->isDead() ||
                    !obj->isType(CHARACTER_ID) ||
                    !obj->isHostile(getAlignment()) ||
                    !canSee(obj))
                        continue;

                /* Remember the closest target */
                distance = place_flying_distance(getPlace(), getX(), getY(), obj->getX(), obj->getY());
                if (distance < min_distance) {
                        min_distance = distance;
                        new_target = obj;
                }
        }

        if (new_target) {
                setAttackTarget((class Character*)new_target);
                return;
        }

        if (target &&
            target->isHostile(getAlignment()) &&
            target != this &&
            target->isOnMap() &&
            !target->isDead() && 
            target->isVisible())
                return;

        target = NULL;
}

void Character::enchant_target()
{
        class Spell *spell;
        int i;
        int distance;

        if (MagicNegated)
                return;

        distance = place_flying_distance(getPlace(), getX(), getY(), target->getX(), target->getY());

        // Enumerate all the known spells for this this
        for (i = 0; i < species->n_spells; i++) {

                spell = species->spells[i];

                // Check if the THIS has enough mana
                if (spell->cost > getMana()) {
                        continue;
                }

                // Check if the nearest is in range or if the range does not
                // matter for this spell type
                if (distance > spell->range &&
                    ! (spell->effects & EFFECT_SUMMON)) {
                        continue;
                }

                // Cast the spell
                // gmcnutt: for now use the caster's coordinates, only the
                // summoning spells currently use them.
                consolePrint("%s casts %s on %s.\n", getName(), spell->getName(), target->getName());
                spell->cast(this, target, 0, getX(), getY());
                decActionPoints(spell->getRequiredActionPoints());
                if (getActionPoints() <= 0)
                        return;
        }

        return;
}

void Character::attack_target(class ArmsType *weapon)
{
        int hit;
        int def;
        int damage;
        int armor;
        bool miss;

        miss = ! weapon->fire(target, getX(), getY());
        decActionPoints(weapon->getRequiredActionPoints());
        useAmmo();

        if (miss) {
                consolePrint("miss!\n");
                return;
        }

        // Roll to hit.
        hit = dice_roll(2, 6) + weapon->getHit();
        def = dice_roll(2, 6) + target->getDefend();
        if (hit < def) {
                consolePrint("miss!\n");
                return;
        } else {
                consolePrint("hit! ");
        }

        // roll for damage
        damage = weapon->getDamage();
        armor = target->getArmor();
        consolePrint("Rolled %d damage, %d armor ", damage, armor);
        damage -= armor;
        damage = max(damage, 0);
        consolePrint("for %d total damage, ", damage);
        target->damage(damage);

        consolePrint("%s!\n", target->getWoundDescription());
}

bool Character::ai_attack_target()
{
        int distance;
        bool attacked = false;

        distance = place_flying_distance(getPlace(), getX(), getY(), target->getX(), target->getY());

        for (class ArmsType * weapon = enumerateWeapons(); weapon != NULL; weapon = getNextWeapon()) {

                if (distance > weapon->getRange()) {
                        continue;
                }

                if (!hasAmmo(weapon)) {
                        continue;
                }

                if (distance <= 1 && weapon->isMissileWeapon()) {
                        // Handle missile weapon interference
                        continue;
                }

                consolePrint("%s attacks %s with %s...", getName(), target->getName(), weapon->getName());
                attack_target(weapon);
                attacked = true;
                statusRepaint();

                if (target->isDead())
                        break;

                if (getActionPoints() <= 0)
                        break;
        }

        if (((class Character *) this)->needToRearm())
                ((class Character *) this)->armThyself();

        return attacked;
}

void Character::pathfind_to(class Object *target)
{
        struct astar_node *path;
        struct astar_search_info as_info;

        /* Find a path to the nearest member */
        memset(&as_info, 0, sizeof (as_info));
        as_info.x0 = getX();
        as_info.y0 = getY();
        as_info.x1 = target->getX();
        as_info.y1 = target->getY();
        as_info.flags = PFLAG_IGNORECOMPANIONS;
        path = place_find_path(Place, &as_info, getPmask(), this);
        
        if (!path) {
                return;
        }
        
        if (path->next) {
                move(path->next->x - getX(), path->next->y - getY());
                
        }

        astar_path_destroy(path);                
}

void Character::player_controlled_attack()
{
        int x;
        int y;
        int i;
        class ArmsType *weapon;
        class Character *near;
        struct terrain *terrain;
        class Mech *mech;
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
        for (weapon = enumerateWeapons(); weapon != NULL; weapon = getNextWeapon()) {

                cmdwin_clear();
                cmdwin_print("%s:", getName());
                consolePrint("[%s]: ", weapon->getName());
                consoleRepaint();

                // Check ammo
                if (!hasAmmo(weapon)) {
                        consolePrint("no ammo!\n");
                        continue;
                }

                /* Get the target. It's important to do this every time the
                 * loop because the last iteration may have killed the previous
                 * target, or it may be out of range of the weapon. The
                 * getAttackTarget routine will reevaluate the current
                 * target. */
                getAttackTarget();


                /* Check the four adjacent tiles for hostiles who will
                 * interfere with a missile weapon */
                if (weapon->isMissileWeapon()) {
                        for (i = 0; i < 4; i++) {
                                near = (class Character*)place_get_object(getPlace(), getX() + dx_to_neighbor[i], getY() + dy_to_neighbor[i], being_layer);
                                if (near &&
                                    near->isHostile(getAlignment()) &&
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
                        cmdwin_print("Attack-Fire %s (range %d, %d ammo)-", weapon->getName(), weapon->getRange(), hasAmmo(weapon));
                }
                else if (weapon->isThrownWeapon()) {
                        // SAM: It would be nice to get ammo name, too...
                        cmdwin_print("Attack-Throw %s (range %d, %d left)-", weapon->getName(), weapon->getRange(), hasAmmo(weapon));
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
                if (select_target(getX(), getY(), &x, &y, weapon->getRange()) == -1) {
                        consolePrint("skip\n");
                        continue;
                }

                // Find the new target under the cursor
                target = (class Character *) place_get_object(Place, x, y, being_layer);
                if (target == NULL) {

                        /* Attack the terrain */
                        terrain = placeGetTerrain(x, y);
                        attackTerrain(x, y);
                        cmdwin_print("%s", terrain->name);
                        consolePrint("%s\n", terrain->name);

                        /* Check for a mech */
                        mech = (class Mech *) place_get_object(Place, x, y, mech_layer);
                        if (mech)
                                mech->activate(MECH_ATTACK);
                }
                else if (target == this) {

                        // -----------------------------------------------------
                        // Don't allow attacking self. This results in a nested
                        // enumerateArms() call when we call getDefend() on
                        // ourself, which messes up the loop we're in right
                        // now. If we really want to support suicide then we'll
                        // need to rework the enumeration code.
                        // -----------------------------------------------------

                        goto prompt_for_target;
#if 0
                        int yesno;

                        // Don't allow targeting self, unless perhaps with
                        // comfirmation.
                        cmdwin_print("Confirm Attack Self-Y/N?");
                        getkey(&yesno, yesnokey);
                        cmdwin_backspace(4);
                        if (yesno == 'y') {
                                cmdwin_print("Yes!");
                                goto confirmed_attack_self;
                        }
                        else {
                                cmdwin_print("No!");
                                continue;
                        }
#endif
                }               // confirm attack self
                else {
//                      confirmed_attack_self:
                        // confirmed_attack_ally:

                        // in combat all npc parties and the player party
                        // should be removed, so only characters reside at the
                        // being layer
                        assert(target->isType(CHARACTER_ID));

                        cmdwin_print("%s", target->getName());                        
                        consolePrint("attack %s...", target->getName());

                        // Strike the target
                        attack_target(weapon);

                        // If we hit a party member then show their new hit
                        // points in the status window
                        if (target->isPlayerControlled())
                                statusRepaint();
                }

                // Warn the user if out of ammo
                if (NULL == getCurrentWeapon() ||
                    false == hasAmmo(getCurrentWeapon()))
                        consolePrint("(%s now out of ammo)\n", weapon->getName());

                // Once the player uses a weapon he can't cancel out of the
                // attack and continue his round with a different command.
                addExperience(XP_PER_ATTACK);
        }
}

static bool character_key_handler(struct KeyHandler *kh, int key, int keymod)
{
        int dir;
        class Character *character = (class Character *) kh->data;
        static int unshift[] = { KEY_NORTH, KEY_SOUTH, KEY_EAST, KEY_WEST };
        class Portal *portal;
        class Character *solo_member;


        // ---------------------------------------------------------------------
        // Process the special CTRL commands
        // ---------------------------------------------------------------------

        if (keymod == KMOD_LCTRL || keymod == KMOD_RCTRL) {

                switch (key) {
      
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

        // ---------------------------------------------------------------------
        // Process normal commands.
        // ---------------------------------------------------------------------

        else {
                switch (key) {

                case KEY_NORTH:
                case KEY_EAST:
                case KEY_SOUTH:
                case KEY_WEST:

                        // ------------------------------------------------------
                        // Move the character.
                        // ------------------------------------------------------

                        dir = keyToDirection(key);

                        consolePrint("%s-", directionToString(dir));

                        switch (character->move(directionToDx(dir), directionToDy(dir))) {
                        case Character::MovedOk:
                                consolePrint("Ok\n");
                                break;
                        case Character::OffMap:
                                consolePrint("No place to go!\n");
                                break;
                        case Character::ExitedMap:
                                consolePrint("Exit!\n");
                                character->endTurn();
                                break;
                        case Character::EngagedEnemy:
                                break;
                        case Character::WasOccupied:
                                consolePrint("Occupied!\n");
                                break;
                        case Character::WasImpassable:
                                consolePrint("Impassable!\n");
                                break;
                        case Character::SlowProgress:
                                consolePrint("Slow progress!\n");
                                break;
                        case Character::SwitchedOccupants:
                                consolePrint("Switch!\n");
                                break;
                        case Character::NotFollowMode:
                                consolePrint("Must be in Follow Mode!\n");
                                break;
                        case Character::CantRendezvous:
                                consolePrint("Party can't rendezvous!\n");
                                break;
			case Character::CouldNotSwitchOccupants:
			  consolePrint("Can't switch!\n");
			  break;
                        }

                        mapCenterView(character->getView(), character->getX(), character->getY());
                        mapRecomputeLos(character->getView());
                        break;


                case KEY_SHIFT_NORTH:
                case KEY_SHIFT_EAST:
                case KEY_SHIFT_SOUTH:
                case KEY_SHIFT_WEST:

                        // ------------------------------------------------------
                        // Pan the camera.
                        // ------------------------------------------------------
                        
                        key = unshift[(key - KEY_SHIFT_NORTH)];
                        dir = keyToDirection(key);
                        mapMoveCamera(directionToDx(dir), directionToDy(dir));
                        mapSetDirty();
                        break;


                case 'a':
                        character->player_controlled_attack();
                        break;

                case 'c':
                        cmdCastSpell(character);
                        break;


                case 'e':

                        // ------------------------------------------------------
                        // Enter a portal. For this to work a portal must exist
                        // here, the party must be in follow mode, and all the
                        // party members must be able to rendezvous at this
                        // character's position.
                        // ------------------------------------------------------

                        cmdwin_clear();
                        cmdwin_print("Enter-");
                        
                        portal = place_get_portal(character->getPlace(), character->getX(), character->getY());
                        if (!portal) {
                                cmdwin_print("Nothing!");
                                break;;
                        }
                        
                        if (player_party->getPartyControlMode() != PARTY_CONTROL_FOLLOW) {
                                cmdwin_print("Must be in follow mode!");
                                break;
                        }
                        
                        if (!combat_rendezvous_party(player_party->n_pc * 2)) {
                                cmdwin_print("Party must be together!");
                                break;
                        }
                        
                        // ------------------------------------------------------
                        // It's safe to enter the portal.
                        // ------------------------------------------------------

                        character->groupExitTo(portal->getToPlace(), portal->getToX(), portal->getToY());
                        cmdwin_print("Ok");
                        break;


                case 'f':

                        // ------------------------------------------------------
                        // Toggle Follow mode on or off. When turning follow
                        // mode off, set all party members to player
                        // control. When turning it on, set all party member to
                        // follow mode but set the leader to player control.
                        // ------------------------------------------------------
                        
                        consolePrint("Follow mode ");
                        if (player_party->getPartyControlMode() == PARTY_CONTROL_FOLLOW) {
                                consolePrint("OFF\n");
                                player_party->enableRoundRobinMode();
                        } else {
                                consolePrint("ON\n");
                                player_party->enableFollowMode();
                        }
                        character->endTurn();
                        break;

                case 'g':
                        cmdGet(character, !place_contains_hostiles(character->getPlace(), character->getAlignment()));
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
                        cmdTalk(character->getX(), character->getY());
                        break;
                case 'u':
                        cmdUse(character, 0);
                        break;
                case 'x':
                        consolePrint("examines around\n");
                        cmdXamine(character);
                        break;
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

                        // ------------------------------------------------------
                        // Put a character in solo mode.
                        // ------------------------------------------------------
                        
                        solo_member = player_party->getMemberAtIndex(key - SDLK_1);
                        if (solo_member != NULL             &&
                            !solo_member->isIncapacitated() &&
                            solo_member->isOnMap()) {
                                player_party->enableSoloMode(solo_member);
                                character->endTurn();
                        }
                        break;

                case SDLK_0:
                        // ------------------------------------------------------
                        // Exit solo mode.
                        // ------------------------------------------------------
                        player_party->enableRoundRobinMode();
                        character->endTurn();
                        break;


                case '<':
                        // ------------------------------------------------------
                        // Quick exit from wilderness combat. The current place
                        // must be the special wildernss combat place and it
                        // must be empty of hostile characters or this fails.
                        // ------------------------------------------------------

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

                        // -----------------------------------------------------
                        // This next call is to make sure the "Victory" and
                        // "Defeated" messages are printed properly. I don't
                        // *think* it has any other interesting side-effects in
                        // this case.
                        // -----------------------------------------------------

                        combat_analyze_results_of_last_turn();

                        // ------------------------------------------------------
                        // Remove all party members.
                        // ------------------------------------------------------

                        player_party->removeMembers();

                        character->endTurn();

                        break;

                default:
                        break;
                }

        }

        return character->isTurnEnded();
}

bool Character::gotoSpot(int mx, int my)
{
	// Common routine used by work() and commute().
	struct astar_node *path;
	struct astar_node *next;
	struct astar_search_info as_info;
	int dx;
	int dy;
        bool ret = true;

	/* Look for a path. */
	memset(&as_info, 0, sizeof(as_info));
	as_info.x0 = getX();
	as_info.y0 = getY();
	as_info.x1 = mx;
	as_info.y1 = my;
	as_info.flags = PFLAG_IGNOREMECHS | PFLAG_IGNORECOMPANIONS;
	path = place_find_path(getPlace(), &as_info, getPmask(), this);

	if (!path)
		return false;

        //dump_path(path);

	/* The first node in the path is the starting location. Get the next
	 * step. */
	next = path->next;
	if (next) {

		/* Get the movement vector */
		dx = next->x - getX();
		dy = next->y - getY();

		/* Attempt to move */
		switch (move(dx, dy)) {
                case Character::MovedOk:
                case Character::ExitedMap:
                case Character::EngagedEnemy:
                case Character::SlowProgress:
                case Character::SwitchedOccupants:
                        ret = true;
                        break;
                case Character::OffMap:
                case Character::WasOccupied:
                case Character::WasImpassable:
		case Character::NotFollowMode:
		case Character::CouldNotSwitchOccupants:
		case Character::CantRendezvous:
                        ret = false;
                        break;
                }                        
	}

	/* Cleanup */
	astar_path_destroy(path);

	return ret;
}

bool Character::commute()
{
	int tx, ty;

        // ---------------------------------------------------------------------
        // Search for an open place in the appointed rectangle where the
        // character can go to.
        // ---------------------------------------------------------------------

        for (ty = sched->appts[appt].y; ty < sched->appts[appt].y + sched->appts[appt].h; ty++) {
                for (tx = sched->appts[appt].x; tx < sched->appts[appt].x + sched->appts[appt].w; tx++) {

                        if (!place_is_passable(getPlace(), tx, ty, getPmask(), PFLAG_IGNOREMECHS) ||
                            place_is_hazardous(getPlace(), tx, ty))
                                continue;
                        
                        if (!gotoSpot(tx, ty)) {
                                continue;
                        }
                        
                        // -----------------------------------------------------
                        // Check if the commute is over.
                        // -----------------------------------------------------

                        if (getX() == tx && getY() == ty) {
                                activity = sched->appts[appt].act;
                        }

                        return true;
                        
                }
        }

        printf("%s cannot find path to [%d %d %d %d] while commuting\n", 
               getName(), 
               sched->appts[appt].x, 
               sched->appts[appt].y, 
               sched->appts[appt].w, 
               sched->appts[appt].h);

        return false;
}

void Character::wander()
{
	int dx = 0, dy = 0;

	/* Roll for direction */
	dx = random() % 3 - 1;
	if (!dx)
		dy = random() % 3 - 1;

	if (dx || dy) {

		// If this party is on a schedule then limit wandering to the
		// area specied in the current appt.
		if (sched) {
			int newx, newy;
			newx = getX() + dx;
			newy = getY() + dy;
			if (newx < sched->appts[appt].x ||
			    newx > (sched->appts[appt].x +
				    sched->appts[appt].w - 1) ||
			    newy < sched->appts[appt].y ||
			    newy > (sched->appts[appt].y +
				    sched->appts[appt].h) - 1)
				return;
		}

                // -------------------------------------------------------------
                // Do a quick check here if this would take the character off
                // the map. If so, then don't do it. Can't have NPC's wandering
                // off out of town...
                // -------------------------------------------------------------

                if (place_off_map(getPlace(), getX() + dx, getY() + dy) ||
                    place_is_hazardous(getPlace(), getX() + dx, getY() + dy))
                        return;

		move(dx, dy);
	}
}

void Character::synchronize()
{
	int hr, min;

	if (!sched || sched->n_appts == 0)
		return;

	for (appt = 0; appt < sched->n_appts; appt++) {
		hr = sched->appts[appt].hr;
		min = sched->appts[appt].min;

		if (hr > Clock.hour || (Clock.hour == hr && min > Clock.min)) {
			break;
		}
	}

	// The loader must ensure that the first appt in every schedule starts
	// at hour zero.
	assert(appt);

	// Back up to the previous appt.
	appt--;

        // ---------------------------------------------------------------------
        // Drop the character in the upper left corner of their roaming
        // rectangle. The ULC is better than the center because it's more
        // obvious to the schedule designer that the ULC needs to be passable
        // terrain.
        // ---------------------------------------------------------------------

	relocate(getPlace(), sched->appts[appt].x, sched->appts[appt].y);

	activity = sched->appts[appt].act;
}

void Character::getAppointment()
{

        int nextAppt = appt + 1;

        // ---------------------------------------------------------------------
        // Special case: the last appointment of the day is over when the clock
        // rolls over at midnight. We can detect clock rollover by checking if
        // the time is BEFORE the start of the current appt.
        // ---------------------------------------------------------------------

        if (nextAppt == sched->n_appts) {
                if (Clock.hour < sched->appts[appt].hr) {
                        activity = COMMUTING;
                        appt = 0;
                }
        }

        // ---------------------------------------------------------------------
        // Normal case: check if the clock time exceeds the start time of our
        // next appt.
        // ---------------------------------------------------------------------

        else if (Clock.hour >= sched->appts[nextAppt].hr &&
                 Clock.min >= sched->appts[nextAppt].min) {
                activity = COMMUTING;
                appt = nextAppt;
        }
}

void Character::execIdle()
{
        // ---------------------------------------------------------------------
        // If they see an enemy they'll engage. Otherwise they just wander
        // uselessly within the rectangular area imposed by their schedule (or
        // freely if they have no schedule).
        // ---------------------------------------------------------------------

        ai_select_target();
        if (!target) {
                wander();
                return;
        }
        
        // ---------------------------------------------------------------------
        // A bit confusing here next. If the NPC can't see a target I still let
        // them pathfind. Why? Because the target might be "remembered" - maybe
        // they were visible last turn and they just stepped out of LOS.
        // ---------------------------------------------------------------------
        
        if (!canSee(target)) {
                pathfind_to(target);
                return;
        }
        
        // ---------------------------------------------------------------------
        // First try magic.
        // ---------------------------------------------------------------------

        enchant_target();
        if (isTurnEnded())
                return;
        
        // ---------------------------------------------------------------------
        // Then try force.
        // ---------------------------------------------------------------------

        if (!ai_attack_target())
                pathfind_to(target);
}

void Character::execAutoMode()
{
        // ---------------------------------------------------------------------
        // Fleeing overrides the current activity (maybe it should be an
        // activity?)
        // ---------------------------------------------------------------------

        if (isFleeing()) {
                flee();
                return;
        }
        
        // ---------------------------------------------------------------------
        // What we do in auto mode depends on the character's "activity", which
        // is set by the schedule. Character's with no schedule are always
        // "idle", which means they wander aimlessly and act agressive toward
        // perceived enemies. Think of them as juvenile delinquents.
        // ---------------------------------------------------------------------

        switch (activity) {
        case COMMUTING:
                commute();
                break;
        case EATING:
                break;
        default:
                execIdle();
                break;
        }
        
}

void Character::exec(struct exec_context *context)
{
        int points_last_loop;
        struct KeyHandler kh;
        class Character *leader;

        startTurn();
        
        if (isDead()) {
                endTurn();
                return;
        }

        if (isResting()) {

                // -------------------------------------------------------------
                // Every hour until the wakeup alarm goes off have the
                // character rest a little.
                //
                // The first character to wakeup to the alarm clock will wake
                // up the party.
                // -------------------------------------------------------------
                
                assert(isAsleep());

                if (clock_alarm_is_expired(&rest_alarm)) {
                        if (getRestCredits())
                                rest(1);
                        clock_alarm_set(&rest_alarm, 60);
                }

                if (clock_alarm_is_expired(&wakeup_alarm)) {
                        endResting();

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

        else if (isGuarding()) {

                // -------------------------------------------------------------
                // Every hour have the guard repair the vehicle by some amount.
                //
                // When guarding is over the guard will wake up the party.
                // -------------------------------------------------------------

                if (clock_alarm_is_expired(&rest_alarm)) {
                        if (isPlayerControlled() &&
                            player_party->vehicle &&
                            player_party->vehicle->getHp() < player_party->vehicle->getMaxHp()) {
                                player_party->vehicle->heal(player_party->vehicle->getMaxHp() / 10);
                                foogodRepaint();
                                consolePrint("%s repairs ", getName());
                                player_party->vehicle->describe(1);
                                consoleNewline();
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

        if (isIncapacitated() ||
            action_points <= 0) {
                endTurn();
                return;
        }

        switch (getControlMode()) {
                
        case CONTROL_MODE_AUTO:

                // -------------------------------------------------------------
                // Lookup this character's schedule (do it outside the loop
                // because we only need to do it once per turn - the clock won't
                // change in the loop).
                // -------------------------------------------------------------

                if (sched)
                        getAppointment();

                // -------------------------------------------------------------
                // Loop until the turn is over or the character stops using
                // action points.
                // -------------------------------------------------------------

                points_last_loop = 0;
                while (! isTurnEnded() > 0 &&
                       getActionPoints() != points_last_loop) {
                        
                        points_last_loop = action_points;

                        execAutoMode();

                }
                break;

        case CONTROL_MODE_PLAYER:

                /* Highlight the character & prompt the user */
                select(true);
                cmdwin_clear();
                cmdwin_print("%s:", getName());
                consolePrint("\n%s: ", getName());
                consoleRepaint();
                
                /* Hand control over to the player */
                kh.fx = &character_key_handler;
                kh.data = this;
                eventPushKeyHandler(&kh);
                eventHandle();
                eventPopKeyHandler();

                /* Un-highlight the character */
                select(false);

                break;


        case CONTROL_MODE_FOLLOW:

                // -------------------------------------------------------------
                // Follow the party leader.
                // -------------------------------------------------------------

                leader = player_party->get_leader();

                assert(leader);
                assert(this != leader);

                // -------------------------------------------------------------
                // Loop until the leader is one tile away, we run out of action
                // points, or we stop using action points (this last occurs
                // when we can't find a path)
                // -------------------------------------------------------------

                points_last_loop = 0;

                while (1 < place_flying_distance(Place, getX(), getY(), leader->getX(), leader->getY()) && 
                       ! isTurnEnded() &&
                       getActionPoints() != points_last_loop) {

                        points_last_loop = getActionPoints();

                        // -----------------------------------------------------
                        // Take a step toward the leader, recompute
                        // line-of-sight and repaint to show the action.
                        // -----------------------------------------------------

                        pathfind_to(leader);
                        mapCenterView(getView(), getX(), getY());
                        mapRecomputeLos(getView());
                        mapSetDirty();
                }
                break;
 
        case CONTROL_MODE_IDLE:
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

        solo = val;

        if (solo) {
                attachCamera(true);
                setControlMode(CONTROL_MODE_PLAYER);
                consolePrint("%s goes solo.\n", getName());
                mapCenterCamera(getX(), getY());
                mapSetDirty();
        } else {
                attachCamera(false);
                setControlMode(CONTROL_MODE_IDLE);
        }
}

void Character::charm(int new_alignment)
{
        if (isDead())
                return;

        if (new_alignment != native_alignment) {

                charmed = true;

                if (isPlayerPartyMember())
                        Object::setControlMode(CONTROL_MODE_AUTO);
                else {
                        // -----------------------------------------------------
                        // Create and add a view for this object.
                        // -----------------------------------------------------

                        setView(mapCreateView());
                        addView();
                        Object::setControlMode(CONTROL_MODE_PLAYER);
                }

                consolePrint("%s is charmed!\n", getName());

        } else {

                charmed = false;

                if (! isPlayerPartyMember()) {

                        // -----------------------------------------------------
                        // Revert npc party member to AI control
                        // -----------------------------------------------------
                        Object::setControlMode(CONTROL_MODE_AUTO);
                        if (NULL != getView()) {
                                rmView();
                                mapDestroyView(getView());
                                setView(NULL);
                        }

                } else {

                        // -----------------------------------------------------
                        // Figure out what control mode a newly un-charmed
                        // player party member needs to use.
                        // -----------------------------------------------------

                        switch (player_party->getPartyControlMode()) {
                        case PARTY_CONTROL_ROUND_ROBIN:
                                Object::setControlMode(CONTROL_MODE_PLAYER);
                                break;
                        case PARTY_CONTROL_SOLO:
                                Object::setControlMode(CONTROL_MODE_IDLE);
                                break;
                        case PARTY_CONTROL_FOLLOW:
                                if (isLeader()) {
                                        Object::setControlMode(CONTROL_MODE_PLAYER);
                                } else {
                                        Object::setControlMode(CONTROL_MODE_FOLLOW);
                                }
                                break;
                        }
                }

                consolePrint("%s is un-charmed!\n", getName());
        }

        charmed_alignment = new_alignment;

}

bool Character::isCharmed()
{
        return charmed;
}

bool Character::isNativelyHostile(int alignment)
{
        return (0 == (native_alignment & alignment));
}

bool Character::isPlayerPartyMember()
{
        return (class Object*)party == (class Object*)player_party;
}

void Character::unCharm()
{
        charm(native_alignment);

        // ---------------------------------------------------------------------
        // Make passing out a normal aspect of charm recovery. This helps make
        // the recovery obvious to the player.
        // ---------------------------------------------------------------------

        changeSleep(true);
}

void Character::setControlMode(enum control_mode mode)
{
        if (!isCharmed())
                Object::setControlMode(mode);
}

void Character::applyExistingEffects()
{
        // ---------------------------------------------------------------------
        // Poison
        // ---------------------------------------------------------------------

	if (isPoisoned()) {
                damage(DAMAGE_POISON);
                if (isDead())
                        return;
	}

        // ---------------------------------------------------------------------
        // Sleep. If the party is not intentionally resting then roll to
        // awaken.
        //
        // FIXME: PROB_AWAKEN should not be a constant.
        //
        // ---------------------------------------------------------------------

        if (isAsleep() && 
            (! isPlayerPartyMember() ||
             ! player_party->isResting())) {

                consolePrint("%s sleeping...", getName());
                if ((random() % 100) < PROB_AWAKEN) {
                        awaken();
                        consolePrint("awakes!");
                }
                consolePrint("\n");
        }
}

bool Character::addToInventory(class Object *item)
{
        if (isPlayerPartyMember()) {
                return player_party->addToInventory(item);                
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

void Character::beginCamping(int hours)
{
        beginResting(hours);
}

void Character::endCamping()
{
        endResting();
}

bool Character::isCamping()
{
        return isResting();
}

void Character::ambushWhileCamping()
{
        resting = false;

        if (!isAsleep())
                return;

        if ((random() % 100) < PROB_AWAKEN) {
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

// -----------------------------------------------------------------------------
// These next seem a bit hackish. They certainly look ungainly. Some of this
// might be helped if the player party and Npc parties did not have different
// classes. This whole thing was prompted by missile spells which need to know
// the location the missile is being shot from. And of course this depends on if
// the character is on the map or in the party on the map (part of the problem
// is silly: I can't figure out a good name for "in the party but on the map").
// -----------------------------------------------------------------------------

struct place *Character::getPlace()
{
        if (isPlayerControlled()) {
                if (player_party->isOnMap())
                        return player_party->getPlace();
        } else {
                if (party->isOnMap())
                        return party->getPlace();
        }

        return Object::getPlace();
}

int Character::getX()
{
        if (isPlayerControlled()) {
                if (player_party->isOnMap())
                        return player_party->getX();
        } else {
                if (party->isOnMap())
                        return party->getX();
        }

        return Object::getX();
        
}

int Character::getY()
{
        if (isPlayerControlled()) {
                if (player_party->isOnMap())
                        return player_party->getY();
        } else {
                if (party->isOnMap())
                        return party->getY();
        }

        return Object::getY();
}

void Character::setLeader(bool val)
{
        if (is_leader == val)
                return;

        if (is_leader) {
                attachCamera(false);
                setControlMode(CONTROL_MODE_IDLE);
        } else {
                attachCamera(true);
                setControlMode(CONTROL_MODE_PLAYER);
                consolePrint("%s is now party leader.\n", getName());
                mapCenterCamera(getX(), getY());
                mapSetDirty();
        }

        is_leader = val;
}

bool Character::isLeader()
{
        return is_leader;
}

bool Character::isCompanionOf(class Object *other)
{
        // ---------------------------------------------------------------------
        // Do the simple thing for now. This is only used in
        // the context of player party follow mode.
        // ---------------------------------------------------------------------

        return isPlayerPartyMember() && other->isPlayerPartyMember();
}

struct conv *Character::getConversation()
{
        if (NULL == conv)
                return party->getConversation();

        return conv;
}

void Character::clearAlignment(int alignment)
{
        native_alignment &= ~alignment;
}

bool Character::joinPlayer(void)
{
        class NpcParty *old_party = party;

        if (NULL != old_party) {
                old_party->removeMember(this);
        }
        
        if (player_party->addMember(this)) {
#if 0                
                mapAddView(getView());
                mapSetRadius(getView(), min(getVisionRadius(), MAX_VISION_RADIUS));
                mapRecomputeLos(getView());
#else
                addView();
#endif
                return true;
        }

        if (NULL != old_party) {
                old_party->addMember(this);
        }

        return false;
}
