/* Copyright (c) 2002 Gordon McNutt */

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
	    !loader->matchWord("pmask") || !loader->getBitmask(&species->pmask))
		goto fail;

	if (!loader->matchWord("sleep_sprite") ||
	    !loader->getWord(&stag) ||
	    (strcmp(stag, "null") && !(species->sleep_sprite =
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

	if (!loader->matchToken('}'))
		goto fail;

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
