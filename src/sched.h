/* Copyright (c) 2002 Gordon McNutt */
#ifndef sched_h
#define sched_h

/* NPC schedules */

#include "list.h"

struct appt {
        int hr, min, x, y, w, h, act;
};
struct sched {
        struct list list;
        char *tag;
        int n_appts;
        struct appt *appts;
};

struct sched *schedLoad(class Loader * loader);

#endif
