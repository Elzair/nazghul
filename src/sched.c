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
#include "sched.h"
#include "Loader.h"
#include "common.h"

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

        /* Check that the start time is within the time supported by the
         * day. */
        if (tmp.min >= MINUTES_PER_HOUR ||
            tmp.hr >= HOURS_PER_DAY) {
                loader->setError("Error in SCHED appointment %d: %02d:%02d is not a valid time.\n"
                                 "Valid times are from 00:00 to %02d:%02d",
                                 index + 1, tmp.hr, tmp.min, HOURS_PER_DAY - 1, MINUTES_PER_HOUR - 1);
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
