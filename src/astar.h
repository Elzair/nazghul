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
#ifndef astar_h
#define astar_h

#include "macros.h"

BEGIN_DECL

#include "tree.h"

#define ASTAR_HORZ (1 << 0)
#define ASTAR_VERT (1 << 1)

struct astar_search_info {
        int x0;
        int x1;
        int y0;
        int y1;
        int flags;
        int (*is_valid_location) (void *context, int fx, int fy, int x, int y);
        void (*heuristic) (struct astar_search_info * info, 
                           int *goodness, int *cost, int fx, int fy);
        unsigned int width;
        unsigned int height;
        int wraps;
        void *context;
        bool limit_depth;
        int max_depth;
};

struct astar_node {
        struct tree order;
        struct astar_node *next;
        int cost;
        int goodness;
        int len;
        int x;
        int y;
        int depth;
        unsigned char scheduled:1;
};

/**
 * Initialize the astar lib before first use.
 */
extern int astar_init(void);

/**
 * Cleanup the lib after last use.
 */
extern void astar_quit(void);

/**
 * Given two locations on a map, find the best route from the first to
 * the second.  Returns a list of nodes representing the best route or
 * null if no route can be found (or the alg runs out of memory before
 * finding a route).  The caller should use astar_path_destroy() when
 * done using the path, or astar_node_destroy on each node of the path.
 *
 * WARNING: the info struct may be modified during this call
 */
extern struct astar_node *astar_search(struct astar_search_info *info);

/**
 * Free the memory used to contain a path returned from astar_search().
 */
extern void astar_path_destroy(struct astar_node *node);

/**
 * Free the memory for a single node.
 */
extern void astar_node_destroy(struct astar_node *node);

/**
 * Dump the path for dbg.
 */
extern void astar_dbg_dump_path(struct astar_node *path);

END_DECL

#endif
