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
#include "knapsack.h"

//#define DEBUG
#ifdef DEBUG
#include <stdio.h>
#endif

static unsigned char cur_in[MAX_N_ITEMS];
static int max;
static int cur_max;

#ifdef DEBUG
static void dump_problem(struct knapsack *prob)
{
	int i;
	printf("-------------------------\n");
	printf("max=%d\n", max);
	for (i = 0; i < prob->n_items; i++) {
		printf("%d %d %d\n", i, prob->value[i], prob->solution[i]);
	}
}
#endif

static int ks_solve(struct knapsack *prob)
{
	int i;

#ifdef DEBUG
	dump_problem(prob);
#endif

	/* For each item... */
	for (i = 0; i < prob->n_items; i++) {

		if (cur_in[i])
			/* The item is already in the knapsack */
			continue;

		if (prob->put(prob->item[i], prob->context)) {

			/* The item fits. To decide if the item belongs in the
			 * * final solution consider two subproblems: one where
			 * * the item is included in the solution, and one where
			 * * the item is not. */

			cur_in[i] = 1;
			cur_max += prob->value[i];

			if (max < cur_max) {
				/* This solution is better than the old * one.
				 * Make a copy. */
				int j;
				max = cur_max;
				for (j = 0; j < prob->n_items; j++)
					prob->solution[j] = cur_in[j];
			}

			/* Consider the subproblem where the item is included */
			ks_solve(prob);

			/* Consider the subproblem where the item is NOT *
			 * included (by continuing at this level without it) */
			cur_in[i] = 0;
			cur_max -= prob->value[i];
			prob->remove(prob->item[i], prob->context);
		}
	}

#if  0
	/* remove any remaining items */
	for (i = 0; i < prob->n_items; i++) {
		if (cur_in[i])
			prob->remove(prob->item[i], prob->context);
	}
#endif

	return 0;
}

int knapsack_solve(struct knapsack *prob)
{
	int i;

	/* Sanity checks */
	if (!prob || prob->n_items < 0 || prob->n_items > MAX_N_ITEMS)
		return -1;

	/* Initialize */
	for (i = 0; i < prob->n_items; i++) {
		prob->solution[i] = 0;
		cur_in[i] = 0;
	}
	max = 0;
	cur_max = 0;

	return ks_solve(prob);
}
