/* Copyright (c) 2002 Gordon McNutt */
#ifndef olist_h
#define olist_h

#ifdef __cplusplus
extern "C" {
#endif

#include "list.h"

#define olist_init(ol) list_init(&(ol)->list)

	struct olist {
		struct list list;
		int key;
	};

	extern struct olist *olist_lookup(struct olist *head, int key,
					  int first);
	extern void olist_add(struct olist *head, struct olist *node);

#ifdef __cplusplus
}
#endif
#endif
