/* Copyright (c) 2002 Gordon McNutt */
#ifndef wq_h
#define wq_h

#ifdef __cplusplus
extern "C" {
#endif

#include "list.h"

	struct wq_job {
								struct list list;
								int tick;
								int period;
								void *data;
		void (*run) (struct wq_job *, struct list * wq);
	};

	extern struct list TickWorkQueue;
	extern struct list TurnWorkQueue;

	extern void wqInit(void);
	extern void wqAddJob(struct list *wq, struct wq_job *job);
	extern void wqRunToTick(struct list *wq, int tick);
	extern void wqCreateJob(struct list *wq, int tick, int period,
				void *data, void (*run) (struct wq_job *,
							 struct list * wq));
	extern void wqReschedule(struct list *wq, struct wq_job *job);

#ifdef __cplusplus
}
#endif

#endif
