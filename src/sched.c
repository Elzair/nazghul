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
#include "common.h"

#include <assert.h>
#include <string.h>
#include <stdlib.h>

char *activity_names[NUM_ACTIVITIES] = {
        "idle",
        "working",
        "sleeping",
        "commuting",
        "eating"
};

int sched_name_to_activity(char *activity_name)
{
        int i;
        for (i = 0; i < NUM_ACTIVITIES; i++) {
                if (! strcmp(activity_name, activity_names[i]))
                        return i;
        }
        return -1;
}

char *sched_activity_to_name(int activity)
{
        if (activity < NUM_ACTIVITIES &&
            activity >= 0)
                return activity_names[activity];
        return NULL;
}

struct sched *sched_new(char *tag, int n_appts)
{
        struct sched *sched;

        sched = (struct sched*)calloc(1, sizeof(*sched));
        assert(sched);

        sched->tag = strdup(tag);
        assert(sched->tag);

        sched->n_appts = n_appts;
        sched->appts = (struct appt*)calloc(n_appts, sizeof(struct appt));
        assert(sched->appts);

        return sched;
}

void sched_del(struct sched *sched)
{
	if (!sched)
		return;
	if (sched->tag)
		free(sched->tag);
	if (sched->appts)
		free(sched->appts);
        free(sched);
}
