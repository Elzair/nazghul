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

#include "species.h"
#include "Loader.h"
#include "occ.h"
#include "sprite.h"
#include "common.h"

#include <string.h>
#include <stdlib.h>

static class Spell **loadSpells(class Loader * loader, int *n)
{
	char *tag;
	class Spell **set;
	int index;

	// base case
	if (loader->matchToken('}')) {
		set = new class Spell *[*n];
		if (!set)
			loader->setError("Memory allocation failed");
		return set;
	}
	// recursive case
	if (!loader->getWord(&tag))
		return 0;

	index = *n;
	(*n)++;

	set = loadSpells(loader, n);
	if (!set) {
		free(tag);
		return 0;
	}

	set[index] = (class Spell *) loader->lookupTag(tag, SPELL_TYPE_ID);
	if (!set[index]) {
		loader->setError("Invalid spell type '%s'", tag);
		free(tag);
		delete set;
		return 0;
	}

	return set;
}

static int *loadSlots(class Loader * loader, int *n)
{
	int *set;
	int index, slot;

	// base case
	if (loader->matchToken('}')) {
		set = new int[*n];
		if (!set)
			loader->setError("Memory allocation failed");
		return set;
	}
	// recursive case
	if (!loader->getInt(&slot))
		return 0;

	index = *n;
	(*n)++;

	set = loadSlots(loader, n);
	if (!set) {
		return 0;
	}

	set[index] = slot;

	return set;
}

static struct occ **loadOccs(class Loader * loader, int *n)
{
	char *tag;
	struct occ **set;
	int index;

	// base case
	if (loader->matchToken('}')) {
		set = new struct occ *[*n];
		if (!set)
			loader->setError("Memory allocation failed");
		return set;
	}
	// recursive case
	if (!loader->getWord(&tag))
		return 0;

	index = *n;
	(*n)++;

	set = loadOccs(loader, n);
	if (!set) {
		free(tag);
		return 0;
	}

	set[index] = (struct occ *) loader->lookupTag(tag, OCC_ID);
	if (!set[index]) {
		loader->setError("Invalid OCC type '%s'", tag);
		free(tag);
		delete set;
		return 0;
	}

	return set;
}

void speciesDestroy(struct species *species)
{
	if (!species)
		return;
	if (species->tag)
		free(species->tag);
	if (species->name)
		free(species->name);
	if (species->occs)
		delete species->occs;
	if (species->slots)
		delete species->slots;
	if (species->spells)
		delete species->spells;
        if (species->damage_sound)
                free(species->damage_sound);
        if (species->movement_sound)
                free(species->movement_sound);
}

struct species *speciesLoad(class Loader * loader)
{
	struct species *species = 0;
	char *stag = 0, *wtag = 0;

	if (!(species = new struct species))
		goto fail;

	memset(species, 0, sizeof(struct species));

	if (!loader->getWord(&species->tag) ||
	    !loader->matchToken('{') ||
	    !loader->matchWord("name") ||
	    !loader->getString(&species->name) ||
	    !loader->matchWord("str") ||
	    !loader->getInt(&species->str) ||
	    !loader->matchWord("int") ||
	    !loader->getInt(&species->intl) ||
	    !loader->matchWord("dex") ||
	    !loader->getInt(&species->dex) ||
	    !loader->matchWord("spd") ||
	    !loader->getInt(&species->spd) ||
	    !loader->matchWord("vr") ||
	    !loader->getInt(&species->vr) ||
	    !loader->matchWord("pmask") || 
            !loader->getBitmask(&species->pmask))
		goto fail;

	if (!loader->matchWord("sleep_sprite") ||
	    !loader->getWord(&stag) ||
	    (strcmp(stag, "null") && 
             !(species->sleep_sprite =
               (struct sprite *) loader->lookupTag(stag,
                                                   SPRITE_ID))))
	{
		loader->setError("Invalid sprite tag '%s'", stag);
		goto fail;
	}

	if (!loader->matchWord("weapon") ||
	    !loader->getWord(&wtag) ||
	    (!(species->weapon =
	       (class ArmsType *) loader->lookupTag(wtag, ARMS_TYPE_ID)))) {
		loader->setError("Invalid arms tag '%s'", wtag);
		goto fail;
	}

	if (!loader->matchWord("visible") ||
	    !loader->getBool(&species->visible))
		goto fail;

	if (!loader->matchWord("occs") ||
	    !loader->matchToken('{') ||
	    !(species->occs = loadOccs(loader, &species->n_occs)) ||
	    !loader->matchWord("slots") ||
	    !loader->matchToken('{') ||
	    !(species->slots = loadSlots(loader, &species->n_slots)) ||
	    !loader->matchWord("spells") ||
	    !loader->matchToken('{') ||
	    !(species->spells = loadSpells(loader, &species->n_spells)))
		goto fail;

        // Optional fields

        while (!loader->matchToken('}')) {

                if (loader->matchWord("damage_sound")) {
                        if (!loader->getString(&species->damage_sound))
                                goto fail;
                } else if (loader->matchWord("movement_sound")) {
                        if (!loader->getString(&species->movement_sound))
                                goto fail;
                } else if (loader->matchWord("effects")) {
                        if (!loader->getBitmask(&species->effects))
                                goto fail;
                } else if (loader->matchWord("immunities")) {
                        if (!loader->getBitmask(&species->immunities))
                                goto fail;
                } else {
                        loader->setError("Error in SPECIES: unknown field "
                                         "'%s'", loader->getLexeme());
                        goto fail;
                }
	}

      done:
	if (stag)
		free(stag);
	if (wtag)
		free(wtag);

	return species;

      fail:
	speciesDestroy(species);
	species = 0;
	goto done;
}
