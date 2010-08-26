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
#ifndef sched_h
#define sched_h

/* NPC schedules */

#include "list.h"
#include "scheme.h"           /* for pointer */
#include "scheme-private.h"   /* for scheme */
#include "place.h"

/* Reserved activities (if you add one don't forget to update the
 * activity_names[] array in sched.c) */
#define NONE           0
#define WORKING        1
#define SLEEPING       2
#define COMMUTING      3
#define EATING         4
#define DRUNK          5
#define FIGHTING       6
#define NUM_ACTIVITIES 7

struct appt {
        int hr, min, x, y, w, h, act;
        int index;             /* into sched array */
        pointer place_sym;     /* scheme var name for place */   
        struct place *place;   /* appt place */
};
struct sched {
        /* struct list list; */
        char *tag;
        int n_appts;
        struct appt *appts;
        scheme *sc;
};

extern struct sched *sched_new(char *tag, int n_appts);
extern void sched_del(struct sched*);
extern int sched_name_to_activity(char *activity_name);
extern const char *sched_activity_to_name(int activity);
extern struct appt *sched_get_appointment(struct sched *sched, 
                                          int hr, int min);
extern struct place *sched_appt_get_place(struct sched *sched, 
                                          struct appt *appt);


#endif
