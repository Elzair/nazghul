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

Character::Character():name(0), hm(0), xp(0), order(-1), hp(0),
                       sleep(false),
                       ac(0), armsIndex(-1),
                       str(0), intl(0),
                       dex(0), mana(0), lvl(0), poison(false),
                       playerControlled(true), solo(false), view(0),
                       killedNotifier(NULL), currentArms(NULL), target(NULL),
                       rdyArms(NULL),
                       fleeing(false), fleeX(0), fleeY(0), burden(0),
                       alignment(0), inCombat(false),
                       container(NULL), sprite(0), n_rest_credits(0), elevated(false)
{
	view = mapCreateView();
	list_init(&plist);
	list_init(&llist);
	light = MIN_PLAYER_LIGHT;	// ok?
	setPlayerControlled(false);	// by default

	tag = 0;
	party = 0;
	conv = 0;
	sched = 0;
	species = 0;
	occ = 0;
	is_clone = false;
	visible = 1;
	occ = 0;
	quarry = 0;
	path = 0;
        damage_sound = 0;

        mp_mod  = 0;
        mp_mult = 0;
        hp_mod  = 0;
        hp_mult = 0;
}

Character::~Character()
{
	if (name)
		free(name);
	if (view) {
		mapRmView(view);
		free(view);
	}

	if (container)
		delete container;

	if (rdyArms != NULL)
		free(rdyArms);

        if (damage_sound)
                free(damage_sound);
}

void Character::changeHp(int delta)
{
	if (delta < 0) {
		soundPlay(get_damage_sound(), SOUND_MAX_VOLUME);

                // Hack: add a clone effect for slimes
                if (player_party->context == CONTEXT_COMBAT &&
                    species->effects & EFFECT_CLONE &&
                    (rand() % 4) == 0) {
                        printf("Cloning %s\n", getName());
                        class Character *copy = clone(this);
                        assert(copy);
                        class NpcParty *copy_party = new NpcParty();
                        assert(copy_party);
                        copy->setAlignment(this->getAlignment());
                        copy_party->init(copy);
                        if (!combatAddNpcParty(copy_party, 0, 0, true, getX(), 
                                               getY())) {
                                delete copy_party;
                                delete copy;                                
                        } else {
                                consolePrint("%s cloned at [%d %d]...", 
                                             getName(),
                                             copy->getX(), copy->getY());
                        }
                }
        }
	else {
		delta = min(delta, getMaxHp() - hp);
	}

	hp += delta;
	if (hp <= 0)
		kill();


	if (playerControlled && delta) {
		statusFlash(getOrder(), (delta < 0) ? Red : Blue);
	} else {
		/* To simplify things, if the character is in a party whose
		 * home is the current place then do not flee. */
		if (!isDead() &&
		    getHp() < (getMaxHp() / 4) && party->isHome(getPlace()))
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

enum Character::MoveResult Character::move(int dx, int dy)
{
	int newx, newy;
	class Character *occupant;

	// Calculate new coordinates.
	newx = getX() + dx;
	newy = getY() + dy;

	// Is the character walking off the edge of the map?
	if (place_off_map(getPlace(), newx, newy)) {
                if (place_get_parent(getPlace()) == NULL)
                        return OffMap;
		remove();
		return ExitedMap;
	}
	// Are the new coordinates passable?
	if (!place_is_passable(getPlace(), newx, newy, getPmask(), 0)) {
		return WasImpassable;
	}
	// Are the new coordinates already occupied by another character?
	if ((occupant = (class Character *) place_get_object(getPlace(), newx,
							     newy,
							     being_layer))) {
		// Is the occupant an enemy?
		if (!(getAlignment() & occupant->getAlignment())) {
			return WasImpassable;
		}
		// *** Switch ***

		if (isPlayerControlled() && occupant->isPlayerControlled() &&
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
                        if (!place_is_passable(getPlace(), getX(), getY(),
                                               occupant->getPmask(), 0)) {
                                // Roll for movement cost
                                if ((placeGetMovementCost(newx, newy) > 0) &&
                                    (random() % 100) > 
                                    (100 / placeGetMovementCost(newx, newy)))
                                        return SlowProgress;

                                // Ok, you can move.
                                relocate(getPlace(), newx, newy);
                                
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

			remove();
			occupant->relocate(oldPlace, oldx, oldy);
			relocate(oldPlace, newx, newy);
			setAttackTarget(oldTarget);
			setSolo(wasSolo);

			return SwitchedOccupants;
		}

		return WasOccupied;
	}

        if (CONFIG_MOVEMENT_COST_FOR_CHARACTERS) {
                // gmcnutt: turned this off because it is plain annoying.
                // Roll for movement cost
                if ((placeGetMovementCost(newx, newy) > 0) &&
                    (random() % 100) > (100 / placeGetMovementCost(newx, newy)))
                        return SlowProgress;
        }

	// Ok, you can move.
	relocate(getPlace(), newx, newy);

	return MovedOk;
}

void Character::remove()
{
	Object::remove();
	setAttackTarget(this);
	setSolo(false);
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

bool Character::setSolo(bool val)
{
	if (val == solo)
		return solo;

	if (val && isIncapacitated())
		return false;

	solo = val;
	return solo;
}

int Character::hasAmmo (class ArmsType * weapon)
{
        // SAM: Changed this from returning bool to 
        //      returning int (0 for no ammo, n for amount)
	if (weapon->ammoIsUbiquitous())
                return 1;  // One more available, that is.

	if (playerControlled) {
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
	sleep = val;
}

void Character::awaken(void)
{
	sleep = false;
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
	changeHp(getMaxHp());
	mana = lvl * HP_PER_LVL;
	if (isAsleep())
		awaken();
}

void Character::dropRdyArms()
{
	assert(!playerControlled);

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
	assert(!playerControlled);

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
	if (!playerControlled && getPlace()) {
		printf("Dropping arms on %s\n", getPlace()->name);
		dropRdyArms();
		dropItems();
	}
	hp = 0;
	remove();
	if (inCombat)
		combatKIA(this);
}

void Character::initItems()
{
	assert(!playerControlled);

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

	if (playerControlled) {
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

				player_party->
				    for_each_member
				    (myUnreadyDepletedThrownWeapon, ie);
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

	assert(!playerControlled);

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
	this->alignment = alignment;

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

void Character::heal(void)
{
	assert(playerControlled);	// shotgun assert put here during
	// refactor
	changeHp(getMaxHp());
}

void Character::cure(void)
{
	assert(playerControlled);	// shotgun assert put here during
	// refactor
	setPoison(false);
	statusFlash(getOrder(), Blue);
}

void Character::resurrect(void)
{
	assert(playerControlled);	// shotgun assert put here during
	// refactor
	setHp(min(10, getMaxHp()));
	statusFlash(getOrder(), Blue);
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
                        changeHp(HP_RECOVERED_PER_HOUR_OF_REST);
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

struct mview *Character::getView() {
        return view;
}

bool Character::isDead() {
        return (hp == 0);
}

bool Character::isPoisoned() {
        return poison;
}

bool Character::isAsleep() {
        return sleep;
}

bool Character::isOnMap() {
        return getPlace() != 0;
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
        poison = val;
}

void Character::setHp(int hp) {
        this->hp = hp;
}

int Character::attack(int damage) {
        if (isDead())
                return 0;
        damage -= getArmourClass();
        if (damage < 0)
                damage = 0;
        changeHp(-damage);
        addExperience(XP_PER_DEFEND);
        return damage;
}

bool Character::isPlayerControlled() {
        return playerControlled;
}

void Character::setPlayerControlled(bool val) {
        playerControlled = val;
}

void Character::setKilledNotifier(void (*cb) (class Character * c)) {
        killedNotifier = cb;
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
        return alignment;
}

void Character::setAlignment(int val) {
        alignment = val;
}

bool Character::isHostile(int alignment) {
        return ((this->alignment & alignment) == 0);
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

        for (class ArmsType * arms = enumerateArms();
             arms != NULL; arms = getNextArms()) {
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
