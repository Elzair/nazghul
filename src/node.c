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

#include "node.h"

#include <assert.h>
#include <stdlib.h>

struct node *node_new(void *data)
{
        struct node *node;

        node = (struct node *)calloc(1, sizeof(*node));
        assert(node);
        node->ptr = data;
        node->ref = 1;
        return node;
}

struct node *node_new_keyed(void *data, int key)
{
        struct node *node;

        node = node_new(data);
        node_key(node) = key;

        return node;
}

void node_unref(struct node *node)
{
        assert(node->ref);
        node->ref--;
        if (! node->ref)
                free(node);
}

void node_foldr(struct node *head,
                void (*fx)(struct node *node, void *data),
                void *data)
{
        struct node *i;
        struct node *p;

        i = node_next(head);

        while (i != head) {
                p = i;
                i = node_next(i);

                fx(p, data);
        }
}

int node_list_len(struct node *head)
{
        struct node *node = node_next(head);
        int n = 0;
        while (node != head) {
                node = node_next(node);
                n++;
        }
        return n;
}

void node_list_unlink_and_unref(struct node *head)
{
        struct node *node = node_next(head);
        while (node != head) {
                struct node *tmp = node_next(node);
                node_remove(node);
                node_unref(node);
                node = tmp;
        }
}
