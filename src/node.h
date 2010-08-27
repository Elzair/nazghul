/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2002, 2003, 2004 Gordon McNutt
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 *
 * Gordon McNutt
 * gmcnutt@users.sourceforge.net
 */

#ifndef node_h
#define node_h

#include "macros.h"
#include "olist.h"

BEGIN_DECL

struct node {
        struct olist olist;
        void *ptr;
        int ref;
};


#define nodelst(n) (&((n)->olist.list))
#define nodekeyedlst(n) (&((n)->olist))
#define node_next(n) ((struct node *)((n)->olist.list.next))
#define node_prev(n) ((struct node *)((n)->olist.list.prev))
#define node_key(n) ((n)->olist.key)
#define node_add(n1,n2) list_add(nodelst(n1), nodelst(n2))
#define node_addref(n) ((n)->ref++)
#define node_add_keyed(n1, n2) olist_add(nodekeyedlst(n1), nodekeyedlst(n2))
#define node_add_tail(n1,n2) list_add_tail(nodelst(n1), nodelst(n2))
#define node_for_each(head,ptr) \
        for ((ptr) = node_next(head); (ptr) != (head); (ptr) = node_next(ptr))
#define node_init(n) { list_init(nodelst(n)); (n)->ptr = NULL; }
#define node_list_empty(n) (list_empty(nodelst(n)))
#define node_remove(n) list_remove(nodelst(n))
#define node_switch(a,b) (list_switch(nodelst(a), nodelst(b)))
#define node_lookup(n, key) (struct node*)(olist_lookup(nodekeyedlst(n),(key), 0))

extern struct node *node_new(void *data);
extern struct node *node_new_keyed(void *data, int key);
extern void node_unref(struct node *node);
extern void node_foldr(struct node *node,
                       void (*fx)(struct node *node, void *data),
                       void *data);
extern int node_list_len(struct node *head);
extern void node_list_unlink_and_unref(struct node *head);
END_DECL

#endif /* node_h */
