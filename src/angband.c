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
/* Note: this is a derived work from the Angband 3.0.0 source file cave.c,
 * which had the following license header attached:
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */
#include "debug.h"
#include "los.h"

#include <stdlib.h>
#include <stdio.h>

/* Wrapper/Stubs for cave.c code *********************************************/

#include <string.h>

typedef unsigned char byte;
typedef signed short int s16b;
typedef unsigned short int u16b;
typedef unsigned int u32b;

#define MAKE(ptr,type) { \
    (ptr) = (type *)malloc(sizeof(type)); \
    if (! (ptr)) \
        return -1; \
    memset((ptr), 0, sizeof(type)); \
}

#define KILL(ptr) free((ptr))

/*
 * Convert a "location" (Y,X) into a "grid" (G)
 */
#define GRID(Y,X,W) \
	((W) * (Y) + (X))

/*
 * Convert a "grid" (G) into a "location" (Y)
 */
#define GRID_Y(G,W) \
	((int)((G) / (W)))

/*
 * Convert a "grid" (G) into a "location" (X)
 */
#define GRID_X(G,W) \
	((int)((G) % (W)))

/*
 * Maximum possible sight radius minus one.
 */
#define MAX_SIGHT 19		/* 19 */

/* From xtra2.c **************************************************************/

/*
 * Current "comp" function for ang_sort()
 */
int (*ang_sort_comp) (const void *u, const void *v, int a, int b);

/*
 * Current "swap" function for ang_sort()
 */
void (*ang_sort_swap) (void *u, void *v, int a, int b);

/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort_aux(void *u, void *v, int p, int q)
{
	int z, a, b;

	/* Done sort */
	if (p >= q)
		return;

	/* Pivot */
	z = p;

	/* Begin */
	a = p;
	b = q;

	/* Partition */
	while (1) {
		/* Slide i2 */
		while (!(*ang_sort_comp) (u, v, b, z))
			b--;

		/* Slide i1 */
		while (!(*ang_sort_comp) (u, v, z, a))
			a++;

		/* Done partition */
		if (a >= b)
			break;

		/* Swap */
		(*ang_sort_swap) (u, v, a, b);

		/* Advance */
		a++, b--;
	}

	/* Recurse left side */
	ang_sort_aux(u, v, p, b);

	/* Recurse right side */
	ang_sort_aux(u, v, b + 1, q);
}

/*
 * Angband sorting algorithm -- quick sort in place
 *
 * Note that the details of the data we are sorting is hidden,
 * and we rely on the "ang_sort_comp()" and "ang_sort_swap()"
 * function hooks to interact with the data, which is given as
 * two pointers, and which may have any user-defined form.
 */
void ang_sort(void *u, void *v, int n)
{
	/* Sort the array */
	ang_sort_aux(u, v, 0, n - 1);
}

/* Modified from cave.c ******************************************************/

/*
 * Approximate distance between two points.
 *
 * When either the X or Y component dwarfs the other component,
 * this function is almost perfect, and otherwise, it tends to
 * over-estimate about one grid per fifteen grids of distance.
 *
 * Algorithm: hypot(dy,dx) = max(dy,dx) + min(dy,dx) / 2
 */
int distance(int y1, int x1, int y2, int x2)
{
	int ay, ax;

	/* Find the absolute y/x distance components */
	ay = (y1 > y2) ? (y1 - y2) : (y2 - y1);
	ax = (x1 > x2) ? (x1 - x2) : (x2 - x1);

	/* Hack -- approximate the distance */
	return ((ay > ax) ? (ay + (ax >> 1)) : (ax + (ay >> 1)));
}

/*
 * Maximum number of grids in a single octant
 */
//#define VINFO_MAX_GRIDS 161
#define VINFO_MAX_GRIDS 147

/*
 * Maximum number of slopes in a single octant
 */
//#define VINFO_MAX_SLOPES 126
#define VINFO_MAX_SLOPES 113

/*
 * Mask of bits used in a single octant
 */
//#define VINFO_BITS_3 0x3FFFFFFF
#define VINFO_BITS_3 0x0001FFFF
#define VINFO_BITS_2 0xFFFFFFFF
#define VINFO_BITS_1 0xFFFFFFFF
#define VINFO_BITS_0 0xFFFFFFFF

/*
 * The 'vinfo_type' structure. There is one of these for each possible grid
 * location in an octant.
 */
struct vinfo_type {
	s16b grid[8]; /* the location of this grid cell in each of the 8
                       * octants */
	u32b bits_3;  /* the rays which strike this grid cell */
	u32b bits_2;  /* the rays which strike this grid cell */
	u32b bits_1;  /* the rays which strike this grid cell */
	u32b bits_0;  /* the rays which strike this grid cell */

	struct vinfo_type *next_0; /* grid to the right */
	struct vinfo_type *next_1; /* grid to the right and down (usually) */

	byte y;
	byte x;
	byte d;
	byte r;
};

/*
 * Slope scale factor
 */
#define SCALE 100000L

/*
 * Temporary data used by "vinfo_init()"
 */
struct vinfo_hack {
	int num_slopes;
	long slopes[VINFO_MAX_SLOPES];
	long slopes_min[MAX_SIGHT + 1][MAX_SIGHT + 1];
	long slopes_max[MAX_SIGHT + 1][MAX_SIGHT + 1];
};

/*
 * Sorting hook -- comp function -- array of long's (see below)
 *
 * We use "u" to point to an array of long integers.
 */
static int ang_sort_comp_hook_longs(const void *u, const void *v, int a, int b)
{
	long *x = (long *) (u);

	/* Unused parameter */
	(void) v;

	return (x[a] <= x[b]);
}

/*
 * Sorting hook -- comp function -- array of long's (see below)
 *
 * We use "u" to point to an array of long integers.
 */
static void ang_sort_swap_hook_longs(void *u, void *v, int a, int b)
{
	long *x = (long *) (u);

	long temp;

	/* Unused parameter */
	(void) v;

	/* Swap */
	temp = x[a];
	x[a] = x[b];
	x[b] = temp;
}

/*
 * Save a slope
 */
static void vinfo_init_aux(struct vinfo_hack *hack, int y, int x, long m)
{
	int i;

	/* Handle "legal" slopes */
	if ((m > 0) && (m <= SCALE)) {
		/* Look for that slope */
		for (i = 0; i < hack->num_slopes; i++) {
			if (hack->slopes[i] == m)
				break;
		}

		/* New slope */
		if (i == hack->num_slopes) {
			/* Paranoia */
			if (hack->num_slopes >= VINFO_MAX_SLOPES) {
				err("Too many slopes (%d)!", VINFO_MAX_SLOPES);
				exit(-1);
			}

			/* Save the slope, and advance */
			hack->slopes[hack->num_slopes++] = m;
		}
	}

	/* Track slope range */
	if (hack->slopes_min[y][x] > m)
		hack->slopes_min[y][x] = m;
	if (hack->slopes_max[y][x] < m)
		hack->slopes_max[y][x] = m;
}

/*
 * Initialize the "vinfo" array
 *
 * Full Octagon (radius 20), Grids=1149
 *
 * Quadrant (south east), Grids=308, Slopes=251
 *
 * Octant (east then south), Grids=161, Slopes=126
 *
 * This function assumes that VINFO_MAX_GRIDS and VINFO_MAX_SLOPES
 * have the correct values, which can be derived by setting them to
 * a number which is too high, running this function, and using the
 * error messages to obtain the correct values.
 *
 * Note: I eliminated the static vinfo object and replaced it with one
 * dynamically allocated and associated with each instance of an angband
 * los object. That's why we now pass the vinfo array in.
 *
 * Also, as per my comments above the GRID macros (see above), converting
 * coordinates to grid indices is no longer hardcoded for a 256-wide grid
 * array. The width of the grid array is now dynamic and passed in as
 * a parameter.
 */
int vinfo_init(struct vinfo_type *vinfo, int width)
{
	int i, g;
	int y, x;
	long m;
	struct vinfo_hack *hack;
	int num_grids = 0;
	int queue_head = 0;
	int queue_tail = 0;
	struct vinfo_type *queue[VINFO_MAX_GRIDS * 2];

        /* Max width of a single octant (center of grid to edge of LOS) */
        const int max_sight = width/2;

	/* Make hack */
	MAKE(hack, struct vinfo_hack);

	/* Analyze grids */
	for (y = 0; y <= max_sight; ++y) {
		for (x = y; x <= max_sight; ++x) {
			/* Skip grids which are out of sight range */
			if (distance(0, 0, y, x) > max_sight)
				continue;

			/* Default slope range */
			hack->slopes_min[y][x] = 999999999;
			hack->slopes_max[y][x] = 0;

			/* Paranoia */
			if (num_grids >= VINFO_MAX_GRIDS) {
				err("Too many grids (%d >= %d)!",
				    num_grids, VINFO_MAX_GRIDS);
				exit(-1);
			}

			/* Count grids */
			num_grids++;

			/* Slope to the top right corner */
			m = SCALE * (1000L * y - 500) / (1000L * x + 500);

			/* Handle "legal" slopes */
			vinfo_init_aux(hack, y, x, m);

			/* Slope to top left corner */
			m = SCALE * (1000L * y - 500) / (1000L * x - 500);

			/* Handle "legal" slopes */
			vinfo_init_aux(hack, y, x, m);

			/* Slope to bottom right corner */
			m = SCALE * (1000L * y + 500) / (1000L * x + 500);

			/* Handle "legal" slopes */
			vinfo_init_aux(hack, y, x, m);

			/* Slope to bottom left corner */
			m = SCALE * (1000L * y + 500) / (1000L * x - 500);

			/* Handle "legal" slopes */
			vinfo_init_aux(hack, y, x, m);
		}
	}

	/* Enforce maximal efficiency */
	if (num_grids < VINFO_MAX_GRIDS) {
		warn("Too few grids (%d < %d)!\n", num_grids, VINFO_MAX_GRIDS);
	}

	/* Enforce maximal efficiency */
	if (hack->num_slopes < VINFO_MAX_SLOPES) {
		warn("Too few slopes (%d < %d)!\n",
		    hack->num_slopes, VINFO_MAX_SLOPES);
	}

	/* Sort slopes numerically */
	ang_sort_comp = ang_sort_comp_hook_longs;

	/* Sort slopes numerically */
	ang_sort_swap = ang_sort_swap_hook_longs;

	/* Sort the (unique) slopes */
	ang_sort(hack->slopes, NULL, hack->num_slopes);

	/* Enqueue player grid */
	queue[queue_tail++] = &vinfo[0];

	/* Process queue */
	while (queue_head < queue_tail) {
		int e;

		/* Index */
		e = queue_head++;

		if (e == VINFO_MAX_GRIDS)
			break;

		/* Main Grid */
		g = vinfo[e].grid[0];

		/* Location */
		y = GRID_Y(g, width);
		x = GRID_X(g, width);

		/* compute grid offsets */
		vinfo[e].grid[0] = GRID(+y, +x, width);
		vinfo[e].grid[1] = GRID(+x, +y, width);
		vinfo[e].grid[2] = GRID(+x, -y, width);
		vinfo[e].grid[3] = GRID(+y, -x, width);
		vinfo[e].grid[4] = GRID(-y, -x, width);
		vinfo[e].grid[5] = GRID(-x, -y, width);
		vinfo[e].grid[6] = GRID(-x, +y, width);
		vinfo[e].grid[7] = GRID(-y, +x, width);

		/* Analyze slopes */
		for (i = 0; i < hack->num_slopes; ++i) {
			m = hack->slopes[i];

			/* Memorize intersection slopes (for non-player-grids) */
			if ((e > 0) &&
			    (hack->slopes_min[y][x] < m) &&
			    (m < hack->slopes_max[y][x])) {
				switch (i / 32) {
				case 3:
					vinfo[e].bits_3 |= (1L << (i % 32));
					break;
				case 2:
					vinfo[e].bits_2 |= (1L << (i % 32));
					break;
				case 1:
					vinfo[e].bits_1 |= (1L << (i % 32));
					break;
				case 0:
					vinfo[e].bits_0 |= (1L << (i % 32));
					break;
				}
			}
		}

		/* Default */
		vinfo[e].next_0 = &vinfo[0];

		/* Grid next child */
		if (distance(0, 0, y, x + 1) <= max_sight) {
			g = GRID(y, x + 1, width);

			if (queue[queue_tail - 1]->grid[0] != g) {
				vinfo[queue_tail].grid[0] = g;
				queue[queue_tail] = &vinfo[queue_tail];
				queue_tail++;
			}

			vinfo[e].next_0 = &vinfo[queue_tail - 1];
		}

		/* Default */
		vinfo[e].next_1 = &vinfo[0];

		/* Grid diag child */
		if (distance(0, 0, y + 1, x + 1) <= max_sight) {
			g = GRID(y + 1, x + 1, width);

			if (queue[queue_tail - 1]->grid[0] != g) {
				vinfo[queue_tail].grid[0] = g;
				queue[queue_tail] = &vinfo[queue_tail];
				queue_tail++;
			}

			vinfo[e].next_1 = &vinfo[queue_tail - 1];
		}

		/* Hack -- main diagonal has special children */
		if (y == x)
                        vinfo[e].next_0 = vinfo[e].next_1;

		/* Extra values */
		vinfo[e].y = y;
		vinfo[e].x = x;
		vinfo[e].d = ((y > x) ? (y + x / 2) : (x + y / 2));
		vinfo[e].r = ((!y) ? x : (!x) ? y : (y == x) ? y : 0);
	}

	/* Verify maximal bits XXX XXX XXX */
	if (((vinfo[1].bits_3 | vinfo[2].bits_3) != VINFO_BITS_3) ||
	    ((vinfo[1].bits_2 | vinfo[2].bits_2) != VINFO_BITS_2) ||
	    ((vinfo[1].bits_1 | vinfo[2].bits_1) != VINFO_BITS_1) ||
	    ((vinfo[1].bits_0 | vinfo[2].bits_0) != VINFO_BITS_0)) {
		warn("Incorrect bit masks!\n");
	}

	/* Kill hack */
	KILL(hack);

	/* Success */
	return (0);
}

/* gmcnutt: variable los */
#ifndef USE_VLOS
#define USE_VLOS 1
#endif

/* gmcnutt: max ray strength for vlos */
#define MAX_RAY_STRENGTH 12

/* gmcnutt: for vlos, track ray strength in each octant */
static unsigned char ray_strength[VINFO_MAX_SLOPES];

#define OPAQUE(val)  ((val) == (MAX_RAY_STRENGTH))
#define VISIBLE(val) ((val)  < (MAX_RAY_STRENGTH))

static void check_ray_set(u32b *ray_set, u32b ray_mask, int ray_index, unsigned char tile_val, int diag)
{
        int ray_bit = 1;
        unsigned int ray_num;

        /* walk the rays */
        for (ray_num = 0; ray_num < sizeof(*ray_set)*8; ray_num++) {
                                                
                /* if this ray strikes this grid */
                if (ray_bit & ray_mask) {
                                                        
                        /* decrement ray
                         * strength by grid
                         * opacity */
                        int ray_val = ray_strength[ray_index];
                        ray_val -= tile_val;

			// Due to the way the rays are cast
			// (Bresenham algorithm), odd effects
			// were seen on perfect diagonals.
			// 
			// A perfect diagonal (degrees 45, 135, 225, 315)
			// gets treated as distance n, rather than n * sqrt(2)
			// and as such, a "spike" of longer LOS on the diagonals
			// was seen.
			// 
			// To avoid this artifact, we aproximate additional 
			// ray strength reduction to correct this.
			// Thus far we have reduced by (n * 1), 
			// so we reduce by (n * 0.5) more, for a total of 1.5.
			// This is a fair approximation of sqrt(2) ~ 1.41,
			// and gets rid of the LOS "spikes"
			// 
                        if (ray_index==(VINFO_MAX_SLOPES-1)) {
                                ray_val -= (tile_val / 2);
                        }

                        if (ray_val < 0) {
                                ray_val = 0;
                        }

                        if (!ray_val) {
                                *ray_set &= ~ray_bit;
                        }

                        ray_strength[ray_index] = ray_val;
                }
                                                
                ray_bit <<=1;
                ray_index++;
        }
}

/*
 * Calculate the complete field of view using a new algorithm
 *
 * If "view_g" and "temp_g" were global pointers to arrays of grids, as
 * opposed to actual arrays of grids, then we could be more efficient by
 * using "pointer swapping".
 *
 * Note the following idiom, which is used in the function below.
 * This idiom processes each "octant" of the field of view, in a
 * clockwise manner, starting with the east strip, south side,
 * and for each octant, allows a simple calculation to set "g"
 * equal to the proper grids, relative to "pg", in the octant.
 *
 *   for (o2 = 0; o2 < 8; o2++)
 *   ...
 *         g = pg + p->grid[o2];
 *   ...
 *
 *
 * Normally, vision along the major axes is more likely than vision
 * along the diagonal axes, so we check the bits corresponding to
 * the lines of sight near the major axes first.
 *
 * We use the "temp_g" array (and the "CAVE_TEMP" flag) to keep track of
 * which grids were previously marked "CAVE_SEEN", since only those grids
 * whose "CAVE_SEEN" value changes during this routine must be redrawn.
 *
 * This function is now responsible for maintaining the "CAVE_SEEN"
 * flags as well as the "CAVE_VIEW" flags, which is good, because
 * the only grids which normally need to be memorized and/or redrawn
 * are the ones whose "CAVE_SEEN" flag changes during this routine.
 *
 * Basically, this function divides the "octagon of view" into octants of
 * grids (where grids on the main axes and diagonal axes are "shared" by
 * two octants), and processes each octant one at a time, processing each
 * octant one grid at a time, processing only those grids which "might" be
 * viewable, and setting the "CAVE_VIEW" flag for each grid for which there
 * is an (unobstructed) line of sight from the center of the player grid to
 * any internal point in the grid (and collecting these "CAVE_VIEW" grids
 * into the "view_g" array), and setting the "CAVE_SEEN" flag for the grid
 * if, in addition, the grid is "illuminated" in some way.
 *
 * This function relies on a theorem (suggested and proven by Mat Hostetter)
 * which states that in each octant of a field of view, a given grid will
 * be "intersected" by one or more unobstructed "lines of sight" from the
 * center of the player grid if and only if it is "intersected" by at least
 * one such unobstructed "line of sight" which passes directly through some
 * corner of some grid in the octant which is not shared by any other octant.
 * The proof is based on the fact that there are at least three significant
 * lines of sight involving any non-shared grid in any octant, one which
 * intersects the grid and passes though the corner of the grid closest to
 * the player, and two which "brush" the grid, passing through the "outer"
 * corners of the grid, and that any line of sight which intersects a grid
 * without passing through the corner of a grid in the octant can be "slid"
 * slowly towards the corner of the grid closest to the player, until it
 * either reaches it or until it brushes the corner of another grid which
 * is closer to the player, and in either case, the existanc of a suitable
 * line of sight is thus demonstrated.
 *
 * It turns out that in each octant of the radius 20 "octagon of view",
 * there are 161 grids (with 128 not shared by any other octant), and there
 * are exactly 126 distinct "lines of sight" passing from the center of the
 * player grid through any corner of any non-shared grid in the octant.  To
 * determine if a grid is "viewable" by the player, therefore, you need to
 * simply show that one of these 126 lines of sight intersects the grid but
 * does not intersect any wall grid closer to the player.  So we simply use
 * a bit vector with 126 bits to represent the set of interesting lines of
 * sight which have not yet been obstructed by wall grids, and then we scan
 * all the grids in the octant, moving outwards from the player grid.  For
 * each grid, if any of the lines of sight which intersect that grid have not
 * yet been obstructed, then the grid is viewable.  Furthermore, if the grid
 * is a wall grid, then all of the lines of sight which intersect the grid
 * should be marked as obstructed for future reference.  Also, we only need
 * to check those grids for whom at least one of the "parents" was a viewable
 * non-wall grid, where the parents include the two grids touching the grid
 * but closer to the player grid (one adjacent, and one diagonal).  For the
 * bit vector, we simply use 4 32-bit integers.  All of the static values
 * which are needed by this function are stored in the large "vinfo" array
 * (above), which is machine generated by another program.  XXX XXX XXX
 *
 * Hack -- The queue must be able to hold more than VINFO_MAX_GRIDS grids
 * because the grids at the edge of the field of view use "grid zero" as
 * their children, and the queue must be able to hold several of these
 * special grids.  Because the actual number of required grids is bizarre,
 * we simply allocate twice as many as we would normally need.  XXX XXX XXX
 */
void update_view(struct los *los)
{
	int pg;
	int g;
	int o2;
	struct vinfo_type *vinfo = (struct vinfo_type *) los->data;

	/* Set pg to be the center */
	pg = los->h / 2 * los->w + los->w / 2;

	/* Initialize the mask to make everything unseen */
	memset(los->vmask, 0, los->w * los->h);

	/* Scan each octant */
	for (o2 = 0; o2 < 8; o2++) {
		struct vinfo_type *p;

                if (USE_VLOS) {
                        /* Initially all rays at max strength */
                        memset(ray_strength, MAX_RAY_STRENGTH, sizeof(ray_strength));
                }

		/* Last added */
		struct vinfo_type *last = &vinfo[0];

		/* Grid queue */
		int queue_head = 0;
		int queue_tail = 0;
		struct vinfo_type *queue[VINFO_MAX_GRIDS * 2];

		/* Slope bit vector */
		u32b bits0 = VINFO_BITS_0;
		u32b bits1 = VINFO_BITS_1;
		u32b bits2 = VINFO_BITS_2;
		u32b bits3 = VINFO_BITS_3;

		/* Reset queue */
		queue_head = queue_tail = 0;

		/* Initial grids */
		queue[queue_tail++] = &vinfo[1];
		queue[queue_tail++] = &vinfo[2];

		/* Process queue */
		while (queue_head < queue_tail) {

			/* Dequeue next grid */
			p = queue[queue_head++];

			/* Limit LOS to the radius if given (creating a circle
			 * of visibility centered on the middle). Otherwise
			 * limit it to the view dimensions (making the entire
			 * square window potentially visible). */
			if (los->r > 0) {
				if (p->d > los->r)
					continue;
			} else {
				if (p->x > los->w / 2 || p->y > los->h / 2)
					continue;
			}

			/* If none of the unblocked rays strike this grid then
			 * skip it. */
			if (!((bits0 & (p->bits_0)) ||
			      (bits1 & (p->bits_1)) ||
			      (bits2 & (p->bits_2)) || (bits3 & (p->bits_3))))
				continue;

			/* Extract grid value XXX XXX XXX */
			g = pg + p->grid[o2];

			/* If this grid is out-of-bounds then skip it (this
			 * happens on children of edge grids */
			if (g < 0 || g >= (los->w * los->h))
				continue;

			/* This grid is visible so unmask it. */
			los->vmask[g] = 1;

			/* If this grid is opaque then mask out the rays which
			 * strike it and do NOT queue it's children. */
			{

                                int ray_index = 0;
                                unsigned char tile_val = los->alpha[g];
                                int diag = abs(GRID_Y(g,los->w))==abs(GRID_X(g,los->w));
                                
                                /* check rays in first set */
                                if (bits0 & (p->bits_0)) {
                                        check_ray_set(&bits0, p->bits_0, ray_index, tile_val, diag);
                                }
                                ray_index += (sizeof(bits0)*8);

                                /* check rays in next set */
                                if (bits1 & (p->bits_1)) {
                                        check_ray_set(&bits1, p->bits_1, ray_index, tile_val, diag);
                                }
                                ray_index += (sizeof(bits1)*8);

                                /* check rays in next set */
                                if (bits2 & (p->bits_2)) {
                                        check_ray_set(&bits2, p->bits_2, ray_index, tile_val, diag);
                                }
                                ray_index += (sizeof(bits2)*8);

                                /* check rays in next set */
                                if (bits3 & (p->bits_3)) {
                                        check_ray_set(&bits3, p->bits_3, ray_index, tile_val, diag);
                                }
                                ray_index += (sizeof(bits3)*8);
                        }

			/* This grid is transparent so queue it's children */
			if (last != p->next_0)
				queue[queue_tail++] = last = p->next_0;
			if (last != p->next_1)
				queue[queue_tail++] = last = p->next_1;

		} // while (queue_head < queue_tail)

	} // for (octant o2)

} // update_view()


/* los lib stuff *************************************************************/

static void ANGBAND_destroy(struct los *los)
{
	free(los->data);
}

static void ANGBAND_compute(struct los *los)
{
	update_view(los);
	los->vmask[(los->w * los->h) / 2] = 1;	/* make center always
                                                 * visible */
}

int ANGBAND_Init(struct los *los)
{
	struct vinfo_type *vinfo;

        if (los->data) {
                free(los->data);
        }

	vinfo = (struct vinfo_type *) malloc((VINFO_MAX_GRIDS + 1) *
					     sizeof(struct vinfo_type));
	if (!vinfo)
		return -1;
	memset(vinfo, 0, (VINFO_MAX_GRIDS + 1) * sizeof(struct vinfo_type));

	if (vinfo_init(vinfo, los->w) < 0)
		return -1;

	los->data = vinfo;
	los->destroy = ANGBAND_destroy;
	los->compute = ANGBAND_compute;

	return 0;

}
