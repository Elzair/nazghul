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
