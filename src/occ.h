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
