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
/* Concept and design hashed out with Sam Glasby */

#include "Mech.h"
#include "Loader.h"
#include "console.h"
#include "terrain.h"
#include "place.h"
#include "dup_constants.h"
#include "wq.h"
#include "lexer.h"
#include "map.h"
#include "terrain_map.h"

#if 0 // debug
static void dump_state(struct mech_state *state)
{
        printf("  name=%s\n",   state->name);
        printf(" pmask=0x%x\n", state->pmask);
        printf(" light=%d\n",   state->light);
        printf("opaque=%d\n",   state->opaque);
}
#endif

struct mech_transition *MechType::load_transitions(class Loader * loader,
						   int *n)
{
	char *from_tag = 0, *to_tag = 0;
	struct mech_transition *set = 0;
	struct mech_transition tmp;
	int index, state;

	// *** base case ***

	if (loader->matchToken('}')) {
		if (*n) {
			set = new struct mech_transition[*n];
			if (!set)
				loader->setError("Memory allocation failed");
		}
		return set;
	}
	// *** recursive case ***

	memset(&tmp, 0, sizeof(tmp));
	if (!loader->matchWord("state") ||
	    !loader->getString(&from_tag) ||
	    !loader->matchWord("event") ||
	    !loader->getInt(&tmp.method) ||
	    !loader->matchWord("next_state") || !loader->getString(&to_tag))
		goto fail;

	// bind the "from" state
	for (state = 0; state < n_states; state++) {
		if (!strcmp(from_tag, states[state].name)) {
			tmp.from = &states[state];
			break;
		}
	}
	if (!tmp.from) {
		loader->setError("Invalid state %s", from_tag);
		goto fail;
	}
	// bind the "to" state
	for (state = 0; state < n_states; state++) {
		if (!strcmp(to_tag, states[state].name)) {
			tmp.to = &states[state];
			break;
		}
	}
	if (!tmp.to) {
		loader->setError("Invalid state %s", to_tag);
		goto fail;
	}
	if (!loader->matchWord("actions"))
		goto fail;

	if (!(tmp.actions = load_response_chain(loader)))
		goto fail;

	// recur

	index = *n;
	(*n)++;

	set = load_transitions(loader, n);
	if (!set) {
		return 0;
	}

	set[index] = tmp;

      fail:
	if (from_tag)
		free(from_tag);
	if (to_tag)
		free(to_tag);

	return set;
}

static bool load_state(class Loader * loader, struct mech_state *state)
{
	char *sprite_tag = 0;

	if (!loader->matchToken('{'))
		goto fail;
	while (!loader->matchToken('}')) {
		if (loader->matchWord("name")) {
			if (!loader->getString(&state->name))
				goto fail;
		} else if (loader->matchWord("sprite")) {
			if (!loader->getWord(&sprite_tag))
				goto fail;
		} else if (loader->matchWord("pmask")) {
			if (!loader->getInt(&state->pmask))
				goto fail;
		} else if (loader->matchWord("light")) {
			if (!loader->getInt(&state->light))
				goto fail;
		} else if (loader->matchWord("opaque")) {
			if (!loader->getBool(&state->opaque))
				goto fail;
		} else if (loader->matchWord("invisible")) {
			if (!loader->getBool(&state->invisible))
				goto fail;                        
		} else {
			loader->setError("Invalid state field: '%s'",
					 loader->lexer->lexeme);
			goto fail;
		}
	}

	if (sprite_tag != NULL) {
		if (strcmp(sprite_tag, "null")) {
			state->sprite = (struct sprite *) loader->
			    lookupTag(sprite_tag, SPRITE_ID);
			if (!state->sprite) {
				loader->setError("Invalid SPRITE tag %s",
						 sprite_tag);
				goto fail;
			}
		}
	}
	free(sprite_tag);

	return true;

      fail:
	if (state->name)
		free(state->name);
	if (sprite_tag)
		free(sprite_tag);
	return false;
}

static struct mech_state *load_states(class Loader * loader, int *n,
				      struct mech_state *def)
{
	char *sprite_tag = 0;
	struct mech_state *set = 0;
	struct mech_state tmp;
	int index;

	// base case
	if (loader->matchToken('}')) {
		if (*n) {
			set = new struct mech_state[*n];
			if (!set)
				loader->setError("Memory allocation failed");
		}
		return set;
	}
	// recursive case
	tmp = *def;
	if (!load_state(loader, &tmp))
		goto fail;

    // dump_state(def);  // For debug purposes

	index = *n;
	(*n)++;

	set = load_states(loader, n, def);
	if (!set)
		return 0;

	set[index] = tmp;

	return set;
      fail:
	if (tmp.name)
		free(tmp.name);
	if (sprite_tag)
		free(sprite_tag);
	return 0;
}

/*****************************************************************************/

MechType::MechType()
{
	n_states = 0;
	states = NULL;
	n_transitions = 0;
	transitions = NULL;
}

MechType::~MechType()
{
	if (states)
		delete states;
	if (transitions) {
		for (int i = 0; i < n_transitions; i++)
			response_chain_destroy(transitions[i].actions);
		delete transitions;
	}
}

class Object *MechType::createInstance()
{
	class Mech *obj = new Mech();
	if (obj)
		obj->init(this);
	return obj;
}

bool MechType::load(class Loader * loader)
{
	struct mech_state def;

	if (!loader->getWord(&tag) ||
	    !loader->matchToken('{') ||
	    !loader->matchWord("name") ||
	    !loader->getString(&name) || !loader->matchWord("defaults"))
		return false;

	memset(&def, 0, sizeof(def));
    // printf("Loading default state for MECH %s:\n", tag);
	if (!load_state(loader, &def))
		return false;

	if (!loader->matchWord("states") || !loader->matchToken('{'))
		return false;

	states = load_states(loader, &n_states, &def);
	if (!states)
		return false;

	if (!loader->matchWord("transitions") || !loader->matchToken('{'))
		return false;

	transitions = load_transitions(loader, &n_transitions);
	if (!transitions)
		return false;

	if (!loader->matchToken('}'))
		return false;

	this->layer = mech_layer;

	return true;
}

/*****************************************************************************/

Mech::Mech()
{
	state = 0;		// changed on load anyway
	port = NULL;
	conv.speaker = NULL;
	conv.mech = this;
	conv.amount = 0;
        activating = false;
}

Mech::~Mech()
{
}

bool Mech::load(class Loader * loader)
{
	class MechType *type;
	int i;

	// MECH declarations appear standalone and then 
	char *state_name = 0;

	type = getObjectType();

	if (!Object::load(loader))
		return false;

	if (!script_tag) {
		loader->setError("A mech requires a tag so it can be "
				 "connected to other mechs");
		return false;
	}

	if (!loader->getString(&state_name))
		return false;

	for (i = 0; i < type->n_states; i++) {
		if (!strcmp(type->states[i].name, state_name)) {
			state = &type->states[i];
			break;
		}
	}

	if (i == type->n_states) {
		loader->setError("Invalid state name '%s'", state_name);
		free(state_name);
		return false;
	}

	free(state_name);

	return true;
}

bool Mech::activate(int method)
{
	int i;
	class MechType *type;
	bool recompute;

	// printf("activate %s %d\n", getName(), method);

	type = getObjectType();

	for (i = 0; i < type->n_transitions; i++) {
		struct mech_transition *trans = &type->transitions[i];
		if (trans->from == state && trans->method == method) {

                        if (activating) {
                                printf("%s: circular loop detected!\n",
                                       getName());
                                return false;
                        }

                        activating = true;
			execute_response_chain(trans->actions, &conv);
                        activating = false;

			// If the state change will affect opacity then update
			// LOS on all the views.
			recompute = (trans->to->opaque != state->opaque);

			// advance to the new state
			state = trans->to;

			if (recompute)
				mapRecomputeLos(ALL_VIEWS);

			mapSetDirty();
			return true;
		}
	}

	return false;
}				// Mech::activate()

struct sprite *Mech::getSprite()
{
	if (state) {
		return state->sprite;
	}
	return NULL;
}

#ifdef USE_OLD_MECH_GETNAME
char *Mech::getName()
{
	if (state)
		return state->name;
	return "unknown";
}
#endif

int Mech::getPmask()
{
	if (state) {
		return state->pmask;
        }
        printf("%s has no state\n", getName());
	return 0;		// SAM: Stateless Mechs are not passable?
}

int Mech::getLight()
{
	if (state)
		return state->light;
	return 0;
}

bool Mech::is_opaque(void)
{
	if (state)
		return state->opaque;
	return false;
}

bool Mech::isVisible(void)
{
        if (state)
                return !state->invisible;
        return true;
}

void Mech::describe(int count)
{
        char *name = getName();

        if (state && state->name) {
		if (isvowel(state->name[0]))
			consolePrint("an ");
		else
			consolePrint("a ");
                consolePrint("%s ", state->name);
        } else {
		if (isvowel(name[0]))
			consolePrint("an ");
		else
			consolePrint("a ");
        }

        consolePrint(name);
}
