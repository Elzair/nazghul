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

/* Reserved activities */
#define NONE           0
#define WORKING        1
#define SLEEPING       2
#define COMMUTING      3
#define EATING         4
#define NUM_ACTIVITIES 5

struct appt {
        int hr, min, x, y, w, h, act;
};
struct sched {
        /* struct list list; */
        char *tag;
        int n_appts;
        struct appt *appts;
};

extern struct sched *sched_new(char *tag, int n_appts);
extern void sched_del(struct sched*);
extern int sched_name_to_activity(char *activity_name);
extern char *sched_activity_to_name(int activity);

#endif
