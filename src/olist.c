/* Copyright (c) 2002 Gordon McNutt */
#include "olist.h"
#include "util.h"

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
