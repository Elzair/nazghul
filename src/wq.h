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
#ifndef wq_h
#define wq_h

#include "macros.h"
#include "list.h"

BEGIN_DECL

struct wq_job {
        struct list list;
        int tick;
        int period;
        void *data;
        void (*run) (struct wq_job *, struct list * wq);        
};

extern void wqAddJob(struct list *wq, struct wq_job *job);
extern void wqRunToTick(struct list *wq, int tick);
extern void wqCreateJob(struct list *wq, int tick, int period, void *data, 
                        void (*run) (struct wq_job *, struct list * wq));
extern void wq_job_del(struct wq_job *);
extern void wqReschedule(struct list *wq, struct wq_job *job);

END_DECL

#endif
