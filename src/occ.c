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

#include "occ.h"
#include "Loader.h"
#include "object.h"
#include "sprite.h"

#include <string.h>
#include <stdlib.h>

static class ArmsType **loadArms(class Loader * loader, int *n)
{
	char *tag;
	class ArmsType **arms;
	int index;

	// base case
	if (loader->matchToken('}')) {
		arms = new class ArmsType *[*n];
		if (!arms)
			loader->setError("Memory allocation failed");
		return arms;
	}
	// recursive case
	if (!loader->getWord(&tag))
		return 0;

	index = *n;
	(*n)++;

	arms = loadArms(loader, n);
	if (!arms) {
		free(tag);
		return 0;
	}

	arms[index] = (class ArmsType *) loader->lookupTag(tag, ARMS_TYPE_ID);
	if (!arms[index]) {
		loader->setError("Invalid arms type '%s'", tag);
		free(tag);
		delete arms;
		return 0;
	}

	return arms;
}

static struct typical_items *loadItems(class Loader * loader, int *n)
{
	char *tag;
	struct typical_items *set;
	int index, n_max, prob;

	// base case
	if (loader->matchToken('}')) {
		set = new struct typical_items[*n];
		if (!set)
			loader->setError("Memory allocation failed");
		return set;
	}
	// recursive case
	if (!loader->getWord(&tag) ||
	    !loader->getInt(&prob) || !loader->getInt(&n_max))
		return 0;

	index = *n;
	(*n)++;

	set = loadItems(loader, n);
	if (!set) {
		free(tag);
		return 0;
	}

	set[index].type =
	    (class ObjectType *) loader->lookupTag(tag, OBJECT_TYPE_ID);
	if (!set[index].type) {
		loader->setError("Invalid object type '%s'", tag);
		free(tag);
		delete set;
		return 0;
	}

	set[index].prob = prob;
	set[index].n_max = n_max;

	return set;
}

static class TrapType **loadTraps(class Loader * loader, int *n)
{
	char *tag;
	class TrapType **set;
	int index;

	// base case
	if (loader->matchToken('}')) {
		set = new class TrapType *[*n];
		if (!set)
			loader->setError("Memory allocation failed");
		return set;
	}
	// recursive case
	if (!loader->getWord(&tag))
		return 0;

	index = *n;
	(*n)++;

	set = loadTraps(loader, n);
	if (!set) {
		free(tag);
		return 0;
	}

	set[index] = (class TrapType *) loader->lookupTag(tag, TRAP_TYPE_ID);
	if (!set[index]) {
		loader->setError("Invalid trap type '%s'", tag);
		free(tag);
		delete set;
		return 0;
	}

	return set;
}

void occDestroy(struct occ *occ)
{
	if (!occ)
		return;
	if (occ->tag)
		free(occ->tag);
	if (occ->name)
		free(occ->name);
	if (occ->arms)
		delete occ->arms;
	if (occ->items)
		delete occ->items;
}

struct occ *occLoad(class Loader * loader)
{
	struct occ *occ = 0;
	char *ctag = 0;

	if (!(occ = new struct occ))
		goto fail;

	memset(occ, 0, sizeof(struct occ));

	if (!loader->getWord(&occ->tag) ||
	    !loader->matchToken('{') ||
	    !loader->matchWord("name") ||
	    !loader->getString(&occ->name) ||
	    !loader->matchWord("magic") || 
            !loader->getFloat(&occ->magic) ||

            !loader->matchWord("hp_mod") ||
            !loader->getInt(&occ->hp_mod) ||
            !loader->matchWord("hp_mult") ||
            !loader->getInt(&occ->hp_mult) ||
            !loader->matchWord("mp_mod") ||
            !loader->getInt(&occ->mp_mod) ||
            !loader->matchWord("mp_mult") ||
            !loader->getInt(&occ->mp_mult) ||
            !loader->matchWord("hit_mod") ||
            !loader->getInt(&occ->hit_mod) ||
            !loader->matchWord("def_mod") ||
            !loader->getInt(&occ->def_mod) ||
            !loader->matchWord("dam_mod") ||
            !loader->getInt(&occ->dam_mod) ||
            !loader->matchWord("arm_mod") ||
            !loader->getInt(&occ->arm_mod))
		goto fail;

	if (!loader->matchWord("container") ||
	    !loader->getWord(&ctag) ||
	    !(occ->container =
	      (class ObjectType *) loader->lookupTag(ctag, OBJECT_TYPE_ID))) {
		if (strcmp(ctag, "null")) {
			loader->setError("Invalid container tag '%s'", ctag);
			goto fail;
		}
	}

	if (!loader->matchWord("traps") ||
	    !loader->matchToken('{') ||
	    !(occ->traps = loadTraps(loader, &occ->n_traps)) ||
	    !loader->matchWord("arms") ||
	    !loader->matchToken('{') ||
	    !(occ->arms = loadArms(loader, &occ->n_arms)) ||
	    !loader->matchWord("items") ||
	    !loader->matchToken('{') ||
	    !(occ->items = loadItems(loader, &occ->n_items)))
		goto fail;

	if (!loader->matchToken('}'))
		goto fail;

      done:
	if (ctag)
		free(ctag);

	return occ;

      fail:
	occDestroy(occ);
	occ = 0;
	goto done;
}
