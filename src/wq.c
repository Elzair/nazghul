/* Copyright (c) 2002 Gordon McNutt */
#include "wq.h"
#include "util.h"

struct list TickWorkQueue;
struct list TurnWorkQueue;

void wqInit(void)
{
	list_init(&TickWorkQueue);
	list_init(&TurnWorkQueue);
}

void wqAddJob(struct list *wq, struct wq_job *newJob)
{
	struct list *list;
	struct wq_job *aJob;

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

	wqAddJob(wq, job);
}

void wqReschedule(struct list *wq, struct wq_job *job)
{
	job->tick += job->period;
	wqAddJob(wq, job);
}
