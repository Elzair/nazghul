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

#include <string.h>

#include "Spell.h"
#include "Loader.h"
#include "character.h"
#include "Arms.h"		// for class Missile
#include "common.h"
#include "cmd.h"
#include "play.h"
#include "place.h"
#include "player.h"
#include "console.h"
#include "combat.h"
#include "wind.h"
#include "wq.h"
#include "Field.h"
#include "Mech.h"
#include "dup_constants.h"
#include "Missile.h"

struct spell_info {
	class Spell *spell;
	class Character *caster;
	bool done;
};

class Spell *SpellTree = NULL;

char *Spell_words[MAX_SPELL_WORDS];

static void Spell_expire_invisibility(struct wq_job *job, struct list *wq)
{
	struct spell_info *info = (struct spell_info *) job->data;
	info->caster->setVisible(true);
	consolePrint("%s expired\n", info->spell->getName());
	delete info;
}

Spell::Spell()
{
	left = 0;
	right = 0;
	code = 0;
	cost = 0;
	range = 0;
	effects = 0;
	context = 0;
	strength = 0;
	duration = 0;
	level = 0;
	target = 0;
	missile = 0;
	n_reagents = 0;
	reagents = 0;
	n_parms = 0;
	parms = 0;
        required_action_points = 1;
}

bool Spell::isType(int id)
{
	return id == SPELL_TYPE_ID;
}

int Spell::getType()
{
	return SPELL_TYPE_ID;
}

static bool Spell_insert(class Spell * spell)
{
	class Spell *parent = 0;
	class Spell *current = SpellTree;

	while (current) {

		parent = current;

		// Spell names must be unique
		if (strcmp(spell->code, current->code) == 0)
			return false;

		if (strcmp(spell->code, current->code) < 0)
			current = current->left;
		else
			current = current->right;
	}

	if (!parent) {
		SpellTree = spell;
	} else if (strcmp(spell->code, parent->code) < 0) {
		parent->left = spell;
	} else {
		parent->right = spell;
	}

	return true;
}

int Spell_init(void)
{
	memset(Spell_words, 0, sizeof(Spell_words));
	return 0;
}

bool Spell_load_magic_words(class Loader * loader)
{
	char *word, letter;
	unsigned int i;
	bool ret;

	if (!loader->matchToken('{'))
		goto fail;

	for (i = 0; i < array_sz(Spell_words); i++) {
		if (!loader->getWord(&word))
			goto fail;
		letter = word[0] - 'A';
		if (Spell_words[letter]) {
			loader->setError("Cannot define '%s' as a MAGIC WORD "
					 "because the MAGIC WORD '%s' "
					 "already begins with the letter '%c'",
					 word, Spell_words[letter], letter);
			free(word);
			goto fail;
		}
		Spell_words[letter] = word;
	}

	if (!loader->matchToken('}'))
		goto fail;

	ret = true;

      done:
	return ret;

      fail:
	for (i = 0; i < array_sz(Spell_words); i++) {
		if (Spell_words[i]) {
			free(Spell_words[i]);
			Spell_words[i] = 0;
		}
	}

	ret = false;

	goto done;
}

static class Reagent **load_reagents(class Loader * loader, int *n)
{
	char *tag;
	class Reagent **set = 0;
	int index;

	// base case
	if (loader->matchToken('}')) {
		if (*n) {
			set = new class Reagent *[*n];
			if (!set)
				loader->setError("Memory allocation failed");
		}
		return set;
	}
	// recursive case
	if (!loader->getWord(&tag))
		return 0;

	index = *n;
	(*n)++;

	set = load_reagents(loader, n);
	if (!set) {
		free(tag);
		return 0;
	}

	set[index] = (class Reagent *) loader->lookupTag(tag, REAGENT_TYPE_ID);
	if (!set[index]) {
		loader->setError("Invalid REAGENT tag '%s'", tag);
		free(tag);
		delete set;
		return 0;
	}

	return set;
}

static void **load_parms_tags(class Loader * loader, int *n)
{
	char *tag;
	void **set = 0;
	int index;

	// base case
	if (loader->matchToken('}')) {
		if (*n) {
			set = new void *[*n];
			if (!set)
				loader->setError("Memory allocation failed");
		}
		return set;
	}
	// recursive case
	if (!loader->getWord(&tag))
		return 0;

	index = *n;
	(*n)++;

	set = load_parms_tags(loader, n);
	if (!set) {
		free(tag);
		return 0;
	}

	set[index] = tag;

	return set;
}

bool Spell::bindTags(class Loader * loader)
{
	int i;
	char *stag;

	for (i = 0; i < n_parms; i++) {
		stag = (char *) parms[i];

		// Note the subtle assumption here: if a spell is going to make
		// use of the generic 'parms' list then it may only have one
		// effect. Spells which combine effects will fail to produce
		// anything out of the parms list.

		switch (effects) {
		case EFFECT_REPEL:
			parms[i] = loader->lookupTag(stag, SPECIES_ID);
			if (!parms[i]) {
				loader->setError("Invalid SPECIES tag '%s' "
						 "in SPELL '%s'", stag, tag);
				free(stag);
				return false;
			}
			free(stag);
			break;
		case EFFECT_SUMMON:
			parms[i] = loader->lookupTag(stag, NPCPARTY_TYPE_ID);
			if (!parms[i]) {
				loader->setError("Invalid PARTY tag '%s' "
						 "in SPELL '%s'", stag, tag);
				free(stag);
				return false;
			}
			free(stag);
			break;
		case EFFECT_DESTROY:
			parms[i] = loader->lookupTag(stag, OBJECT_TYPE_ID);
			if (!parms[i]) {
				loader->setError("Invalid OBJECT tag '%s' "
						 "in SPELL '%s'", stag, tag);
				free(stag);
				return false;
			}
			free(stag);
			break;
		case EFFECT_WIND:
			// Allow only field types
			parms[i] = loader->lookupTag(stag, OBJECT_TYPE_ID);
			if (!parms[i]) {
				loader->setError("Invalid OBJECT tag '%s' "
						 "in SPELL '%s'", stag, tag);
				free(stag);
				return false;
			}
			if (!((class ObjectType *) parms[i])->
			    isType(FIELD_TYPE_ID)) {
				loader->setError("EFFECT_WIND requires OBJECT"
						 " %s to be a field type in "
						 "spell %s", stag, tag);
				free(stag);
				return false;

			}
			free(stag);
			break;
		default:
			break;
		}

	}

	return true;
}

bool Spell::load(class Loader * loader)
{
	char *missile_tag = 0, *context_code = 0, *ptr;
	bool ret = true;

	if (!ObjectType::load(loader))
		return false;

	// Parse
	if (!loader->matchWord("code") ||
	    !loader->getWord(&code) ||
	    !loader->matchWord("cost") ||
	    !loader->getInt(&cost) ||
	    !loader->matchWord("range") ||
	    !loader->getInt(&range) ||
	    !loader->matchWord("missile") ||
	    !loader->getWord(&missile_tag) ||
	    !loader->matchWord("effects") ||
	    !loader->getBitmask(&effects) ||
	    !loader->matchWord("strength") ||
	    !loader->getInt(&strength) ||
	    !loader->matchWord("duration") ||
	    !loader->getInt(&duration) ||
	    !loader->matchWord("context") ||
	    !loader->getWord(&context_code) ||
	    !loader->matchWord("level") ||
	    !loader->getInt(&level) ||
	    !loader->matchWord("target") ||
	    !loader->getInt(&target) ||
	    !loader->matchWord("reagents") || !loader->matchToken('{'))
		goto fail;

	reagents = load_reagents(loader, &n_reagents);
	if (!reagents)
		goto fail;

	if (!loader->matchWord("parms") || !loader->matchToken('{'))
		goto fail;

	// hack: since PARMS are usually defined after spells I can only load
	// the tags for now. Later the load process must call the bindTags()
	// method to resolve these tags to actual parms.
	parms = load_parms_tags(loader, &n_parms);
	if (n_parms && !parms)
		goto fail;

	if (!loader->matchToken('}'))
		goto fail;

	// Bind the missile. Fixme: the way this works really bugs me: do I
	// really need an instance of a Missile or wouldn't the ArmsType be
	// sufficient?
	if (strcmp(missile_tag, "null")) {

		class ArmsType *missile_type;
		missile_type =
		    (class ArmsType *) loader->lookupTag(missile_tag,
							 ARMS_TYPE_ID);
		if (!missile_type) {
			loader->setError("Invalid MISSILE tag '%s'",
					 missile_tag);
			goto fail;
		}

		missile = new Missile();
		if (missile == NULL) {
			loader->setError("Memory allocation failure while "
					 "loading SPELL: failed to allocated "
					 "Missile.");
			goto fail;
		}

		missile->init(missile_type);
	}
	// Interpret the context
	ptr = context_code;
	while (*ptr) {
		switch (*ptr) {
		case 'C':
		case 'c':
			context |= CONTEXT_COMBAT;
			break;
		case 'W':
		case 'w':
			context |= CONTEXT_WILDERNESS;
			break;
		case 'T':
		case 't':
			context |= CONTEXT_TOWN;
			break;
		default:
			// ignore others (should warn, but have no way to do
			// that right now)
			break;
		}
		ptr++;
	}

	if (!Spell_insert(this)) {
		loader->setError("SPELL name '%s' already registered",
				 getName());
		goto fail;
	}

      done:
	if (missile_tag)
		free(missile_tag);
	if (context_code)
		free(context_code);
	return ret;

      fail:
	ret = false;
	goto done;

}

Spell::~Spell()
{
	if (code)
		free(code);
	if (reagents)
		delete(reagents);
	if (missile)
		delete(missile);
}

static bool Spell_reverse_effects_to_character(class Character * character,
					       void *data)
{
	class Spell *spell = (class Spell *) data;
	if (spell->effects & EFFECT_ARMOUR)
		character->changeArmourClass(-spell->strength);

	return false;
}

static void Spell_expire_effects_to_character(struct wq_job *job,
					      struct list *wq)
{
	class Spell *spell = (class Spell *) job->data;
	player_party->for_each_member(Spell_reverse_effects_to_character,
				      spell);
	free(job);
	consolePrint("%s spell expired!\n", spell->getName());
}

static bool Spell_apply_effects_to_character(class Character * character,
					     void *data)
{
	class Spell *spell = (class Spell *) data;
	if (spell->effects & EFFECT_ARMOUR)
		character->changeArmourClass(spell->strength);
	return false;
}

static void Spell_cast_repel(class Object * obj, void *data)
{
	struct spell_info *info;
	class Character *character;
	int i;

	if (!obj->isType(CHARACTER_ID))
		return;

	character = (class Character *) obj;
	info = (struct spell_info *) data;

	// Do not apply to allies...
	if (!character->isHostile(info->caster->getAlignment()))
		return;

	// Special csse: if no species are specied then apply to all non-allied
	// characters regardless of species.
	if (info->spell->n_parms == 0) {
		character->setFleeing(true);
		info->spell->success = Spell::ok;
		return;
	}

	for (i = 0; i < info->spell->n_parms; i++) {
		if (character->species ==
		    (struct species *) info->spell->parms[i]) {
			character->setFleeing(true);
			info->spell->success = Spell::ok;
			break;
		}
	}

}

static void Spell_apply_tremor(class Object * obj, void *data)
{
	class Character *character;
	struct spell_info *info;

	if (!obj->isType(CHARACTER_ID))
		return;

	character = (class Character *) obj;
	info = (struct spell_info *) data;

	// If the character is an ally of the caster then skip.
	if (!info->caster->isHostile(character->getAlignment()))
		return;

	// If the character is asleep then roll to wake them up.
	if (character->isAsleep()) {
		if (!(random() % 4) || info->done)
			character->awaken();
		return;
	}
	// If the character is awake then roll to knock them unconscious, doing
	// damage.
	if (!(random() % 4)) {
		character->changeSleep(true);
		character->damage(info->spell->strength);
	}
}

static void Spell_apply_confuse(class Object * obj, void *data)
{
	class Character *character;
	class Character *caster;

	if (!obj->isType(CHARACTER_ID))
		return;

	character = (class Character *) obj;
	caster = (class Character *) data;

	// If the character is an ally of the caster then skip.
	if (!caster->isHostile(character->getAlignment()))
		return;

	character->setAlignment(0);
}

static void Spell_cast_wind(int x, int y, class FieldType * type, int direction)
{
	int dx, dy, i, p, p1, p2;
	class Field *field;
	bool done;

	dx = directionToDx(direction);
	dy = directionToDy(direction);

	i = 1;

	switch (direction) {
	case NORTH:
	case SOUTH:
		p1 = p2 = x;

		done = false;

		while (!done) {

			done = true;

			y += dy;
			p1 -= 1;
			p2 += 1;

			// Drop a field at every point along the line
			for (p = p1; p <= p2; p++) {
				if (place_off_map(Place, p, y))
					continue;
				done = false;
				field = new Field();
				if (field == NULL)
					continue;
				field->init(type);
				field->relocate(Place, p, y);
			}

			// Repaint and pause to give the player time to admire
			// the effect.
			mapUpdate(0);
			SDL_Delay(10);

			// Remove all the fields except for ones which occupy
			// the same tile as a being. Leave those after applying
			// their effects.  They will expire automatically after
			// the usual time delay.
			for (p = p1; p <= p2; p++) {
				if (place_off_map(Place, p, y))
					continue;
				class Character *character;
				character = (class Character *)
				    place_get_object(Place, p, y, being_layer);
				if (character) {
					int effects = type->getEffects();
					if (effects & EFFECT_POISON)
						character->setPoison(true);
					if (effects & EFFECT_SLEEP)
						character->changeSleep(true);
					if (effects &
					    (EFFECT_BURN | EFFECT_DAMAGE))
						character->
						    damage(DAMAGE_FIRE);
					continue;
				}

				field = (class Field *) place_get_object(Place,
									 p, y,
									 field_layer);
				assert(field);
				field->destroy();
				delete field;
			}

			mapUpdate(0);
		}

		break;
	case EAST:
	case WEST:
		p1 = p2 = y;

		done = false;

		while (!done) {

			done = true;

			x += dx;
			p1 -= 1;
			p2 += 1;

			// Drop a field at every point along the line
			for (p = p1; p <= p2; p++) {
				if (place_off_map(Place, x, p))
					continue;
				done = false;
				field = new Field();
				if (field == NULL)
					continue;
				field->init(type);
				field->relocate(Place, x, p);
			}

			// Repaint and pause to give the player time to admire
			// the effect.
			mapUpdate(0);
			SDL_Delay(10);

			// Remove all the fields except for ones which occupy
			// the same tile as a being. Leave those after applying
			// their effects.  They will expire automatically after
			// the usual time delay.
			for (p = p1; p <= p2; p++) {
				if (place_off_map(Place, x, p))
					continue;
				class Character *character;
				character = (class Character *)
				    place_get_object(Place, x, p, being_layer);
				if (character) {
					int effects = type->getEffects();
					if (effects & EFFECT_POISON)
						character->setPoison(true);
					if (effects & EFFECT_SLEEP)
						character->changeSleep(true);
					if (effects &
					    (EFFECT_BURN | EFFECT_DAMAGE))
						character->
						    damage(DAMAGE_FIRE);
					continue;
				}

				field = (class Field *) place_get_object(Place,
									 x, p,
									 field_layer);
				assert(field);
				field->destroy();
				delete field;
			}

			mapUpdate(0);
		}

		break;
	default:
		break;
	}
}

enum Spell::cast_result Spell::cast(class Character * caster,
				    class Object * target,
				    int direction, int tx, int ty)
{
	class Character *character = (class Character *) target;
	class Mech *mech = (class Mech *) target;

	success = no_effect;

	// Check if negate magic is in effect (natural spells are not affected
	// by negate magic).
	if (MagicNegated > 0 && !(effects & EFFECT_NATURAL)) {
		success = magic_negated;
		return success;
	}
	// Charge the caster.
	caster->changeMana(-cost);
        caster->decActionPoints(getRequiredActionPoints());

	if (missile &&
	    (this->target == SPELL_TARGET_CHARACTER ||
	     this->target == SPELL_TARGET_LOCATION)) {

		// Fire the missile. Field types are attributes of the missile,
		// so this method also takes care of dropping a field on the
		// target's location if applicable.

		if (this->target == SPELL_TARGET_CHARACTER) {
			tx = target->getX();
			ty = target->getY();
		}

                // -------------------------------------------------------------
                // Next I have to decide which location the missile should
                // originate from. This is simple, right? Just use the caster's
                // coordinates. But what if the caster's party is in party mode
                // in the wilderness?  Then I have to use the party coordinates.
                // -------------------------------------------------------------


		missile->setPlace(Place);
		missile->animate(caster->getX(), caster->getY(), tx, ty, 0);
		if (!missile->hitTarget())
			return missed_target;
                success = ok;
	}
	// Apply any effects of this spell. First I test for the mutually
	// exclusive effects which cannot be combined with one another (because
	// they both make use of the parms list).

	if (effects == EFFECT_REPEL) {
		struct spell_info info;
		info.caster = caster;
		info.spell = this;
		place_for_each_object(Place, Spell_cast_repel, &info);

	} else if (effects == EFFECT_SUMMON) {
		int i, dx, dy;
		for (i = 0; i < n_parms; i++) {
			class NpcParty *party;
			class NpcPartyType *type;

			type = (class NpcPartyType *) parms[i];
			party = (class NpcParty *) type->createInstance();
			party->setAlignment(caster->getAlignment());
			party->createMembers();

			// Randomly pick a direction for the summoned party.
			dx = 0;
			dy = 0;
			do {
				dx = random() % 3 - 1;
				if (!dx)
					dy = random() % 3 - 1;
			} while (!dx && !dy);

                        // -----------------------------------------------------
                        // If the spell does not specify a target then default
                        // to the caster's location.
                        // -----------------------------------------------------

                        if (this->target != SPELL_TARGET_LOCATION) {
                                tx = caster->getX();
                                ty = caster->getY();
                        }
                        
                        if (!combatAddNpcParty(party, dx, dy, true, caster->getPlace(), tx, ty)) {
				success = no_room_on_battlefield;
                                delete party;
                        } else {
				success = ok;
                        }
                        mapSetDirty();
		}

	} else if (effects == EFFECT_DESTROY) {

		// For EFFECT_DESTROY the (x,y) coordinates give the location
		// and the parms identify the object types to search for and
		// destroy. I only pick one thing to destroy per cast.
		int i;
		class ObjectType *type;
		class Object *obj;

		for (i = 0; i < n_parms; i++) {
			type = (class ObjectType *) parms[i];
			obj = place_get_object(Place, tx, ty, type->getLayer());
			if (obj) {
				obj->destroy();
				delete obj;
				break;
			}
		}
	} else if (effects == EFFECT_WIND &&
		   player_party->getContext() & CONTEXT_COMBAT) {
		class FieldType *field;
		field = (class FieldType *) parms[0];
		if (field) {
			Spell_cast_wind(caster->getX(), caster->getY(),
					field, direction);
			success = ok;
		}
	}

	if (effects & EFFECT_TELEPORT) {
		if (direction == UP || direction == DOWN) {
                        teleport_vertically(caster, direction);
		} else {
			teleport_horizontally(caster, direction);
		}
	}
	if (effects & EFFECT_ARMOUR &&
	    this->target == SPELL_TARGET_ALL_PARTY_MEMBERS) {
		player_party->for_each_member(Spell_apply_effects_to_character,
					      this);
		wqCreateJob(&TurnWorkQueue, Turn + duration, 0, this,
			    Spell_expire_effects_to_character);
		success = ok;

	}
	if (effects & EFFECT_POISON && character && !character->isPoisoned()) {
		success = ok;
		character->setPoison(true);
	}
	if (effects & EFFECT_CURE && character && character->isPoisoned()) {
		success = ok;
		character->setPoison(false);
	}
	if (effects & EFFECT_SLEEP && character && !character->isAsleep()) {
		success = ok;
		character->changeSleep(true);
	}
	if (effects & EFFECT_AWAKEN && character && character->isAsleep()) {
		success = ok;
		character->awaken();
	}
	if (effects & EFFECT_CHARM && character) {
		success = ok;
		character->charm(caster->getAlignment());
	}
	if (effects & (EFFECT_DAMAGE | EFFECT_BURN) && character) {
		success = ok;
		character->damage(strength * caster->getLevel());
	}
	if (effects & EFFECT_LIGHT && character) {
		success = ok;
		effectLight("spell", strength, duration, character);
	}
	if (effects & EFFECT_REVEAL) {
		success = ok;
		effectReveal(getName(), duration);
	}
	if (effects & EFFECT_QUICK) {
		success = ok;
		effectQuicken(getName(), duration);
	}
	if (effects & EFFECT_HEAL && character &&
	    character->getHp() < character->getMaxHp()) {
		success = ok;
		character->heal(strength);
	}
	if (effects & EFFECT_RESTORE && character &&
	    character->getMana() < character->getMaxMana()) {
		success = ok;
		character->changeMana(strength);
	}
	if (effects & EFFECT_UNLOCK && mech) {
		// hack: use the strength field to distinguish between
		// different intents.
		switch (strength) {
		case 1:
			if (mech->activate(MECH_UNLOCK))
				success = ok;
			break;
		case 2:
			if (mech->activate(MECH_MAGIC_UNLOCK))
				success = ok;
			break;
		case -1:
			if (mech->activate(MECH_LOCK))
				success = ok;
			break;
		case -2:
			if (mech->activate(MECH_MAGIC_LOCK))
				success = ok;
			break;
		default:
			break;
		}
	}
	if (effects & EFFECT_LOCATE) {
		success = ok;
		if (player_party->getContext() & CONTEXT_COMBAT)
			consolePrint("Location is %s [%d %d]\n", Place->name,
				     caster->getX(), caster->getY());
		else
			consolePrint("Location is %s [%d %d]\n", Place->name,
				     player_party->getX(),
				     player_party->getY());
	}
	if (effects & EFFECT_WIND_CHANGE) {
		success = ok;
		windSetDirection(direction, duration * caster->getLevel());
	}
	if (effects & EFFECT_NEGATE) {
		success = ok;
		effectNegateMagic(getName(), duration);
	}
	if (effects & EFFECT_SHOW_TERRAIN) {
		success = ok;
		effectShowTerrain(getName(), duration);
		mapSetDirty();
	}
	if (effects & EFFECT_TREMOR) {
		int i;
		struct spell_info info;
		success = ok;
		info.spell = this;
		info.caster = caster;
		info.done = false;
		for (i = 0; i < duration; i++) {
			place_for_each_object(Place, Spell_apply_tremor, &info);
			mapJitter(true);
			mapUpdate(0);
		}
		info.done = true;
		place_for_each_object(Place, Spell_apply_tremor, &info);
		mapJitter(false);
		mapUpdate(0);
	}
	if (effects & EFFECT_CONFUSE) {
		success = ok;
		place_for_each_object(Place, Spell_apply_confuse, caster);
	}
	if (effects & EFFECT_PEER) {
		int key;
		mapPeer(true);
		mapUpdate(0);
		getkey(&key, anykey);
		mapPeer(false);
		mapUpdate(0);
		success = ok;
	}
	if ((effects & EFFECT_CLONE) && 
            character                &&
	    player_party->getContext() & CONTEXT_COMBAT) {
		class Character *clone = character->clone(character);
		if (clone) {
			class NpcParty *party = new NpcParty();
			if (!party) {
				delete clone;
			} else {
				clone->setAlignment(caster->getAlignment());

                                // ---------------------------------------------
                                // Clones need to be reworked a bit in order to
                                // put them under player control, so just leave
                                // them in auto mode.
                                // ---------------------------------------------

				party->init(clone);
				if (!combatAddNpcParty(party, 0, 0, true, caster->getPlace(), tx, ty)) {
                                        delete party;
                                        success = no_room_on_battlefield;
                                }
                                mapSetDirty();
				success = ok;
			}
		}
	}
	if (effects & EFFECT_INVISIBLE && character && character->isVisible()) {
		struct spell_info *info = new struct spell_info;
		if (info) {
			character->setVisible(false);
			if (!character->isVisible()) {
                                mapSetDirty();
				info->caster = character;
				info->spell = this;
				success = ok;
				wqCreateJob(&TurnWorkQueue, Turn + duration, 0, info, Spell_expire_invisibility);
			} else {
				delete info;
			}
		}
	}
	if (effects & EFFECT_TIME_STOP) {
                effectTimeStop(getName(), duration);
		success = ok;
	}
	if (effects & EFFECT_RESURRECT && character) {
		character->resurrect();
		if (!character->isDead())
			success = ok;
	}
	if (effects & EFFECT_GATE_TRAVEL) {
		if (player_party->try_to_enter_moongate(0))
			success = ok;
	}
	return success;
}

class Spell *Spell_lookup_by_code(char *name)
{
	class Spell *spell = SpellTree;

	while (spell) {
		if (!strcmp(name, spell->code))
			return spell;
		if (strcmp(name, spell->code) < 0)
			spell = spell->left;
		else
			spell = spell->right;
	}

	return 0;
}

void Spell::teleport_horizontally(class Character * caster, int direction)
{
	int dx, dy, distance;
	distance = random() % (range + 1) + 1;

	dx = directionToDx(direction) * distance;
	dy = directionToDy(direction) * distance;

	// Don't allow teleporting off the edge of a map.
	if (!Place->wraps) {
		int oldx, oldy, newx, newy;

                oldx = caster->getX();
                oldy = caster->getY();

		newx = oldx + dx;
		newy = oldy + dy;
		place_clip_to_map(caster->getPlace(), &newx, &newy);
		dx = newx - oldx;
		dy = newy - oldy;
	}

        if (caster->move(dx, dy) == Character::MovedOk) {
                success = ok;
        } else {
                success = teleport_failed;
        }
}

void Spell::teleport_vertically(class Character * caster, int direction)
{
	struct move_info minfo;
	struct combat_info cinfo;

	memset(&minfo, 0, sizeof(minfo));

	if (direction == UP) {
		minfo.place = caster->getPlace()->above;
	} else {
		minfo.place = caster->getPlace()->below;
	}

	minfo.x = caster->getX();
	minfo.y = caster->getY();

	switch (player_party->check_move_to(&minfo)) {

	case move_ok:
	case move_enter_auto_portal:
	case move_enter_moongate:
                player_party->removeMembers();
#if 0
                mapRmView(ALL_VIEWS);
                mapAddView(player_party->view);
                mapSetDirty();
#endif
		player_party->relocate(minfo.place, minfo.x, minfo.y);
		success = ok;
		break;

	case move_enter_combat:
                player_party->removeMembers();
		memset(&cinfo, 0, sizeof(cinfo));
		cinfo.move = &minfo;
		player_party->move_to_wilderness_combat(&cinfo);
		success = ok;
		break;

	case move_null_place:
	case move_occupied:
	case move_impassable:
		success = teleport_failed;
		break;

	default:
		// no others expected
		assert(false);
	}
}
