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
#include "olist.h"
#include "common.h"

struct olist *olist_lookup(struct olist *head, int key, int first)
{
	struct list *l;
	struct olist *i, *last = 0;

	list_for_each(&head->list, l) {
		i = outcast(l, struct olist, list);
		if (i->key == key) {
			if (first)
				return i;
			last = i;
		} else if (last != 0)
			return last;
	}
	return last;
}

void olist_add(struct olist *head, struct olist *node)
{
	struct list *l;
	struct olist *i;

	list_for_each(&head->list, l) {
		i = outcast(l, struct olist, list);
		if (i->key > node->key)
			break;
	}

	list_add_aux(l->prev, l, &node->list);
}
