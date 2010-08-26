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
#include "scheme-private.h"

#include <assert.h>
#include <string.h>
#include <stdlib.h>

const char *activity_names[NUM_ACTIVITIES] = {
        "idle",
        "working",
        "sleeping",
        "commuting",
        "eating",
        "drunk",
        "fighting"
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

const char *sched_activity_to_name(int activity)
{
        if (activity < NUM_ACTIVITIES &&
            activity >= 0)
                return activity_names[activity];
        return NULL;
}

struct sched *sched_new(char *tag, int n_appts)
{
        struct sched *sched;
        int i;

        sched = (struct sched*)calloc(1, sizeof(*sched));
        assert(sched);

        sched->tag = strdup(tag);
        assert(sched->tag);

        sched->n_appts = n_appts;
        sched->appts = (struct appt*)calloc(n_appts, sizeof(struct appt));
        assert(sched->appts);

        for (i = 0; i < n_appts; i++)
                sched->appts[i].index = i;

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

static struct place *sched_scheme_sym_to_place(scheme *sc, pointer sym)
{
        pointer pair;

        assert(sym);
        assert(sc);
        assert(sc->vptr->is_symbol(sym));

        pair = sc->vptr->find_slot_in_env(sc, sc->envir, sym, 1);
        assert(sc->vptr->is_pair(pair));

        return (struct place*)sc->vptr->pair_car(sc->vptr->pair_cdr(pair));
}

struct appt *sched_get_appointment(struct sched *sched, int hr, int min)
{
        int i = 0;
        struct appt *appt = 0;

        assert(hr >= 0);
        assert(hr <= 23);

        for (i = 0; i < sched->n_appts; i++) {
                if (hr < sched->appts[i].hr)
                        break;
        }

        assert(i);
        assert(i <= sched->n_appts);

        appt = &sched->appts[i-1];

        /* resolve the place symbol to a place */
        if (! appt->place) {
                appt->place = sched_scheme_sym_to_place(sched->sc, 
                                                        appt->place_sym);
                assert(appt->place);
        }

        return appt;
}

struct place *sched_appt_get_place(struct sched *sched,
                                         struct appt *appt)
{
        if (! appt->place) {
                appt->place = sched_scheme_sym_to_place(sched->sc, 
                                                        appt->place_sym);
                assert(appt->place);
        }
        return appt->place;
}
