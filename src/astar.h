/* Copyright (c) 2002 Gordon McNutt */
#ifndef astar_h
#define astar_h

#ifdef __cplusplus
extern "C" {
#endif

#include "tree.h"

#define ASTAR_HORZ (1 << 0)
#define ASTAR_VERT (1 << 1)

	struct astar_search_info {
		int x0;
		int x1;
		int y0;
		int y1;
		int flags;
		int (*is_valid_location) (void *context, int x, int y);
		int (*heuristic) (struct astar_search_info * info);
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
#ifdef __cplusplus
}
#endif
#endif
