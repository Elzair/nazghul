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
#include "wq.h"
#include "common.h"

void wqAddJob(struct list *wq, struct wq_job *newJob)
{
	struct list *list;
	struct wq_job *aJob;

        //dbg("wqAddJob: %08lx\n", newJob);


	list_for_each(wq, list) {
		aJob = outcast(list, struct wq_job, list);
		if (aJob->tick > newJob->tick)
			break;
	}

	list_add_aux(list->prev, list, &newJob->list);


}

void wqRunToTick(struct list *wq, int tick)
{
	struct list *list;

	list = wq->next;
	while (list != wq) {

		struct list *tmp;
		struct wq_job *aJob;

		aJob = outcast(list, struct wq_job, list);
		if (aJob->tick > tick)
			break;

		tmp = list->next;
		list_remove(list);
                //dbg("wqRunToTick: %08lx\n", aJob);
                aJob->run(aJob, wq);
		list = tmp;

	}
}

void wqCreateJob(struct list *wq, int tick, int period, void *data,
		 void (*run) (struct wq_job *, struct list * wq))
{
	struct wq_job *job;

	job = (struct wq_job *) malloc(sizeof(struct wq_job));
	if (!job) {
		warn("Failed to allocate job");
		return;
	}

	job->tick = tick;
	job->period = period;
	job->data = data;
	job->run = run;

        //dbg("wqCreateJob: %08lx\n", job);

	wqAddJob(wq, job);
}

void wq_job_del(struct wq_job *job)
{
        list_remove(&job->list);
        free(job);
}

void wqReschedule(struct list *wq, struct wq_job *job)
{
	job->tick += job->period;
	wqAddJob(wq, job);
}
