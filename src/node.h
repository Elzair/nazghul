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

#include "list.h"

struct node {
        struct node *next;
        struct node *prev;
        void *ptr;
        int ref;
};

#define node_add(n1,n2) list_add(n1, n2)
#define node_init(n) { list_init(n); (n)->ptr = NULL; }
#define node_add_tail(n1,n2) list_add_tail(n1,n2)
#define node_remove(n) list_remove(n)
#define node_addref(n) ((n)->ref++)

extern struct node *node_new(void *data);
extern void node_unref(struct node *node);


#endif /* node_h */
