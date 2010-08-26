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
#include "astar.h"
#include "list.h"
#include "heap.h"

//#define DEBUG
#include "debug.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define COORD_TO_INDEX(x,y,w) ((y)*(w)+(x))

/**
 * The number of neighbors to search. If you want to allow NPC's to have
 * diagonal movement, set this to 8. Otherwise set it to 4.
 */
#define CONFIG_NEIGHBORS 8
#define MAX_DEPTH 1000	/* hack to limit search time on large places */

static struct heap *schedule;	/* Priority queue of nodes to explore */
static struct tree *found;	/* Nodes with a known path, ordered by location
				 */

/* Internal ******************************************************************/

#ifdef ASTAR_DBG
#define dump_node(n) \
         dbg("%p={x=%d, y=%d, next=%p, len=%d, cost=%d, "\
         "goodness=%d, order.key=%d}", (n), (n)->x, (n)->y, (n)->next, \
         (n)->len, (n)->cost, (n)->goodness, (n)->order.key)
#else
#define dump_node(n)
#endif

static inline void astar_search_init(void)
{
	heap_clean(schedule);
}

static inline struct astar_node *astar_node_create(int x, int y, int cost,
						   int goodness,
						   struct astar_node *next,
						   int location)
{
	struct astar_node *node;

	node = (struct astar_node *) malloc(sizeof(struct astar_node));
	if (!node)
		return 0;
	memset(node, 0, sizeof(struct astar_node));

	node->x               = x;
	node->y               = y;
	node->cost            = cost;
	node->goodness        = goodness;
	node->next            = next;
        node->order.key_type  = tree_i_key;
	node->order.key.i_key = location;

        if (next)
                node->depth = next->depth + 1;

	dump_node(node);
	return node;
}

void astar_node_destroy(struct astar_node *node)
{
	free(node);
}

static struct astar_node *astar_path_reverse_aux(struct astar_node *node,
						 struct astar_node *prev)
{
	struct astar_node *ret;

	dump_node(node);

	node->len = prev->len + 1;
	assert(node->len < 1000);	/* debug */
	if (node->next)
		ret = astar_path_reverse_aux(node->next, node);
	else
		ret = node;
	node->next = prev;

	return ret;
}

static struct astar_node *astar_path_reverse(struct astar_node *node)
{
	struct astar_node *ret;

	dump_node(node);

	/* simple case of only one node in the path */
	if (!node->next)
		return node;

	node->len = 0;
	ret = astar_path_reverse_aux(node->next, node);
	node->next = 0;

	return ret;
}

static inline int astar_schedule(struct astar_node *node)
{

	assert(!node->scheduled);

	/* Insert the node in the priority queue. */
	if (heap_insert(schedule, &node->goodness))
		return -1;

	node->scheduled = 1;

	/* Insert it in the tree of nodes with known paths. */
	tree_insert(&found, &node->order);

	return 0;
}

static inline struct astar_node *astar_schedule_extract(void)
{
	struct astar_node *node;

	/* Pull off the top of the heap (caller must ensure non-empty) */
	node = heap_entry(heap_extract(schedule), struct astar_node, goodness);
	node->scheduled = 0;
	return node;
}

static void astar_explored_remove(struct astar_node *path)
{
	struct astar_node *ptr;

	/* Purge the 'found' tree of all nodes in the path */
	for (ptr = path; ptr; ptr = ptr->next)
		tree_delete(&found, &ptr->order);
}

static void astar_cleanup_aux(struct tree *root)
{
	/* Recursively attack the subtrees */
	if (root->left)
		astar_cleanup_aux(root->left);
	if (root->right)
		astar_cleanup_aux(root->right);

	/* Destroy the root */
	astar_node_destroy(tree_entry(root, struct astar_node, order));
}

static void astar_cleanup(void)
{
	if (found) {
		astar_cleanup_aux(found);
		found = 0;
	}
}

static inline struct astar_node *astar_old_route(int location)
{
	struct tree *tree = tree_i_search(found, location);
	if (tree)
		return tree_entry(tree, struct astar_node, order);
	return 0;
}

static inline void astar_replace_route(struct astar_node *node, int x, int y,
				       int cost, int goodness,
				       struct astar_node *next, int location)
{
	/* Replace the old route values with the new ones. */
	node->x = x;
	node->y = y;
	node->cost = cost;
	node->goodness = goodness;
	node->next = next;
	node->order.key.i_key = location;

	/* If the old route was scheduled but not explored yet then fixup the
	 * priority queue to reflect the new goodness of the route. */
	if (node->scheduled) {
		heapify(schedule, 0);
		return;
	}

	/* Otherwise reschedule the route to be explored again. */
	heap_insert(schedule, &node->goodness);
	node->scheduled = 1;
}

static inline void
astar_schedule_neighbor(struct astar_node *node, 
                        struct astar_search_info *info)
{
	struct astar_node *ptr;
	int location;

        /* 'cost' is an attribute of the path leading to this node. When we
         * find two paths leading to the same node, cost helps us decide which
         * path to take. */
	int cost = 0;

        /* 'goodness' is an attribute both of a node and of the path leading to
         * that node. It is important because it allows us to prioritize nodes,
         * so we can search the more promising nodes first. */
	int goodness = 0;

	info->heuristic(info, &goodness, &cost, node->x, node->y);

        /* Cost is cumulative along a path, so add the parent node's cost. */
        cost += node->cost;

        /* Nodes with cheaper paths leading to them are better than nodes with
         * costlier paths. */
        goodness -= cost;

	location = COORD_TO_INDEX(info->x0, info->y0, info->width);

	/* Check if we already have a route to this location. */
	if ((ptr = astar_old_route(location))) {

		/* If the old route is better than skip this neighbor. */
		if (goodness <= ptr->goodness)
			return;

		/* If the new route is better than replace the old with the new
		 * and reschedule. */
		astar_replace_route(ptr, info->x0, info->y0, cost, goodness,
				    node, location);
		return;
	}

	/* This is a new route. */
	ptr = astar_node_create(info->x0, info->y0, cost, goodness, node,
				location);
	if (!ptr) {
		err("Allocation failed");
		return;
	}

	if (astar_schedule(ptr)) {
                err("Schedule failed!");
                exit(-1);                
        }
}

int astar_init(void)
{
	return ((schedule = heap_create(64)) ? 0 : -1);
}

void astar_quit(void)
{
	if (schedule)
		heap_destroy(schedule);
}

struct astar_node *astar_search(struct astar_search_info *info)
{
	struct astar_node *node;
	int row;
	int col;
        int cost = 0;
        int goodness = 0;

	astar_search_init();

        info->heuristic(info, &goodness, &cost, info->x0, info->y0);
	node = astar_node_create(info->x0, info->y0, cost, goodness, NULL, 
                                 COORD_TO_INDEX(info->x0, info->y0, 
                                                info->width));

	astar_schedule(node);

	while (!heap_empty(schedule)) {

                //astar_dump_schedule();
		node = astar_schedule_extract();

		/* Check if this node is the target location */
		if ((info->flags & ASTAR_HORZ || node->x == info->x1) &&
		    (info->flags & ASTAR_VERT || node->y == info->y1)) {

			/* Reverse the path to get a pointer to the start */
			node = astar_path_reverse(node);

			/* Remove the nodes in the path from the tree so that
			 * we don't free them with the rest */
			astar_explored_remove(node);

			goto done;
		}

                /* Check if this node is at max depth */
                if (node->depth == MAX_DEPTH ||
                    (info->limit_depth && node->depth == info->max_depth)) {
                        continue;
                }

		/* Check the four non-diagonal neighbors of this node */
		for (row = 0, info->y0 = node->y - 1; row < 3;
		     row++, info->y0++) {

			/* Wrap y-coord if applicable */
			if (info->wraps)
				info->y0 = ((info->y0 + info->height) %
					    info->height);

			for (col = 0, info->x0 = node->x - 1; col < 3;
			     col++, info->x0++) {

#if (CONFIG_NEIGHBORS==4)
				/* skip diagonals and center */
				if (((row * 3 + col) % 2) == 0) {
					continue;
				}
#elif (CONFIG_NEIGHBORS==8)
				/* skip center */
				if ((row == col) && (row == 1)) {
					continue;
				}
#else
#error CONFIG_NEIGHBORS undefined or has bad value
#endif

				/* Wrap x-coord if applicable */
				if (info->wraps)
					info->x0 = ((info->x0 + info->width) %
						    info->width);

				/* Skip this neighbor if it's not a valid
				 * location (impassable, off-map, etc) */
				if (!info->is_valid_location(info->context,
                                                             node->x,
                                                             node->y,
							     info->x0,
							     info->y0))
					continue;

				astar_schedule_neighbor(node, info);

			}
		}

	}

	node = 0;
      done:
	astar_cleanup();
	return node;

}

void astar_path_destroy(struct astar_node *node)
{
	if (node->next)
		astar_path_destroy(node->next);
	astar_node_destroy(node);
}

void astar_dbg_dump_path(struct astar_node *path)
{
        while (path) {
                dbg("(%d, %d)", path->x, path->y);
                path = path->next;
        }
}
