/* Copyright (c) 2002 Gordon McNutt */
#include "sched.h"
#include "Loader.h"

#include <string.h>
#include <stdlib.h>

struct appt *load_appts(class Loader * loader, int *n)
{
	struct appt *set;
	int index;
	struct appt tmp;

	if (loader->matchToken('}')) {
		set = new struct appt[*n];
		if (set)
			memset(set, 0, *n * sizeof(struct appt));
		return set;
	}

	if (!loader->getInt(&tmp.hr) ||
	    !loader->getInt(&tmp.min) ||
	    !loader->getInt(&tmp.x) ||
	    !loader->getInt(&tmp.y) ||
	    !loader->getInt(&tmp.w) ||
	    !loader->getInt(&tmp.h) || !loader->getInt(&tmp.act))
		return 0;

	index = *n;
	(*n)++;

	// Special case: the first appt MUST start at hour zero, and I must
	// enforce that here so that other code can safely assume so.

	if (!index && tmp.hr && tmp.min) {
		loader->setError("First appointment in a SCHED *must* start "
				 "at hour zero, minute zero");
		return 0;
	}

	if (!(set = load_appts(loader, n)))
		return 0;

	set[index] = tmp;

	return set;
}

void schedDestroy(struct sched *sched)
{
	if (!sched)
		return;
	if (sched->tag)
		free(sched->tag);
	if (sched->appts)
		delete sched->appts;
	delete sched;
}

struct sched *schedLoad(class Loader * loader)
{
	struct sched *sched;

	sched = new struct sched;
	if (!sched)
		return 0;
	memset(sched, 0, sizeof(*sched));

	if (!loader->getWord(&sched->tag)) {
		delete sched;
		return 0;
	}

	if (!loader->matchToken('{') ||
	    !(sched->appts = load_appts(loader, &sched->n_appts)))
		goto fail;

	return sched;

      fail:
	schedDestroy(sched);
	return 0;
}
