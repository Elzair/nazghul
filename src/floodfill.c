/* Copyright (c) 2002 Gordon McNutt */
#include "los.h"

#include <string.h>

static void FLOODFILL_destroy(struct los *los)
{
}

static int floodfillCheckDistance(struct los *los, int mx, int my)
{
	int d;
	int x;
	int y;

	/* Limit LOS to the radius if given (creating a circle of visibility
	 * centered on the player). Otherwise limit it to the view dimensions
	 * (making the entire map window potentially visible). */
	if (los->r <= 0)
		return (!(mx < 0 || mx >= los->w || my < 0 || my >= los->h));

	/* Convert coordinates to a system where the origin is in the center of 
	 * the los area */
	x = mx - los->w / 2;
	y = my - los->h / 2;

	/* And force coordinates to absolute values so we can use the simple
	 * distance formula */
	x = (x < 0 ? -x : x);
	y = (y < 0 ? -y : y);

	/* Use a quick-and-dirty distance formula (stolen from angband los
	 * algorithm) */
	d = ((y > x) ? (y + x / 2) : (x + y / 2));

	return (d <= los->r);
}

static void FLOODFILL_computeRecursive(struct los *los, int vx, int vy,
				       int mx, int my)
{
	int vindex;
	int mindex;
	int vnx;
	int vny;
	int mnx;
	int mny;

	/* If these are not valid viewing coordinates then return. This happens
	 * when we hit the edges of the los area. */
	if (vx < 0 || vx >= los->w || vy < 0 || vy >= los->h)
		return;

	vindex = vy * los->w + vx;
	mindex = my * los->w + mx;

	/* If this tile has already been visited then return */
	if (los->vmask[vindex] != 'u')
		return;

	/* If this tile is outside of the radius then return */
	if (!floodfillCheckDistance(los, mx, my))
		return;

	/* Mark this tile as visible. */
	los->vmask[vindex] = 1;

	/* If this tile is opaque and not the tile the player is standing on
	 * then return. */
	if (!los->alpha[mindex] && ((vx != los->w / 2) || (vy != los->h / 2)))
		return;

	/* revisit -- consider skipping diagonal neighbors */
	/* revisit -- we could skip some neighbors if we knew which direction
	 * our parent was. We could also precheck neighbors before recurring,
	 * saving us some function call overhead */
	/* For each of the eight neighboring tiles, call the recursive
	 * floodfill function */
	for (vny = vy - 1, mny = my - 1; vny <= vy + 1; vny++, mny++) {
		for (vnx = vx - 1, mnx = mx - 1; vnx <= vx + 1; vnx++, mnx++) {
			FLOODFILL_computeRecursive(los, vnx, vny, mnx, mny);
		}
	}

}

static void FLOODFILL_compute(struct los *los)
{
	int i;
	int len;

	len = los->w * los->h * sizeof(unsigned char);

	/* Fill the mask with the 'unvisited' flag */
	memset(los->vmask, 'u', len);

	/* Start the recursion with the center tile */
	FLOODFILL_computeRecursive(los, los->w / 2, los->h / 2,
				   los->w / 2, los->h / 2);

	/* Replace all 'unvisited' and 'visited' flags with 'nonvisible' */
	for (i = 0; i < len; i++) {
		switch (los->vmask[i]) {
		case 0:
		case 1:
			break;
		default:
			los->vmask[i] = 0;
		}
	}

}

struct los *FLOODFILL_Init(struct los *los)
{
	los->destroy = FLOODFILL_destroy;
	los->compute = FLOODFILL_compute;
	return 0;
}
