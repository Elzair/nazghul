/* Copyright (c) 2002 Gordon McNutt */
#ifndef knapsack_h
#define knapsack_h

#define MAX_N_ITEMS 100

struct knapsack {

	int n_items;		/* The number of items to consider */

	void **item;		/* Array of pointers to the items under
				 * consideration. Memory managed by caller. */

	int *value;		/* Array of values of the items (in the same
				 * order as the items). Memory managed by *
				 * caller. */

	unsigned char *solution;	/* Array representing the set of items
					 * chosen. If and only if the value of
					 * * solution[i] is * nonzero, then *
					 * item[i] was chosen. Memory managed *
					 * by * caller. */

	int (*put) (void *context, void *item);	/* Attempt to put the item in
						 * the 'knapsack' and return
						 * nonzero if successful or
						 * zero if it failed. */

	void (*remove) (void *context, void *item);	/* Remove an item from
							 * the knapsack. */

	void *context;		/* Used as the 'context' parameter to the 'put'
				 * callback */
};

extern int knapsack_solve(struct knapsack *problem);

#endif
