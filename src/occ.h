/* Copyright (c) 2002 Gordon McNutt */
#ifndef occ_h
#define occ_h

#include "list.h"

struct typical_items {
	int prob;
	class ObjectType *type;
	int n_max;
};

struct occ {
	char *tag;
	struct list list;
	char *name;
	float magic;
	class ObjectType *container;
	int n_arms;
	class ArmsType **arms;
	int n_items;
	struct typical_items *items;
	int n_traps;
	class TrapType **traps;
};

extern struct occ *occLoad(class Loader *);
extern void occDestroy(struct occ *occ);

#endif
