// Copyright (c) 2003 Gordon McNutt
#ifndef formation_h
#define formation_h

#include "list.h"

struct formation_entry {
	int x;
	int y;
};

struct formation {
	struct list list;
	char *tag;
	int n;
	struct formation_entry *entry;
};

#endif
