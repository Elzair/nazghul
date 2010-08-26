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
#ifndef los_h
#define los_h

#ifdef __cplusplus
extern "C" {
#endif

#define ALPHA_TRANSPARENT 0
#define ALPHA_OPAQUE      12

	struct los {

		/*** Public ***/

		/**
                 * Width and height specify the maximum area over which an
                 * algorithm will compute line of sight.
                 */
		int w;
		int h;

		/**
                 * Radius is an optional parameter. Use it to limit the max
                 * visible range from the center of the vmask. No matter how
                 * large you make this, the width and height parameters (above)
                 * still enforce a max limit on visible range. But you can use
                 * it to shrink the visible range below what the width and
                 * height allow.
                 */
		int r;

		/**
                 * 'alpha' points to a 2d array. Each element in the array
                 * corresponds to a tile on the visible map. The value in each
                 * element indicates how transparent the corresponding tile
                 * is. For example, a value of 0 indicates that the tile is
                 * perfectly opaque and blocks all visibility beyond it,
                 * whereas a value of 1 indicates that the tile is perfectly
                 * transparent and does not hamper visibility at all.
                 *
                 * Although the los engine is responsible for allocating and
                 * freeing this buffer, the caller is responsible for filling
                 * it in before calling the compute() routine.
                 */
		unsigned char *alpha;

		/**
                 * 'vmask' also points to a 2d array. Again, each element
                 * corresponds to a tile. However, this array stores the result
                 * of the last los computation.  It's value give the relative
                 * visibility of a tile. For example, 0 means the tile is
                 * completely obscured, whereas 1 means the tile is completely
                 * visible.
                 *
                 * The los engine is responsible for allocating and freeing
                 * this buffer, and also filling it. The caller should consider
                 * it read-only (but it doesn't really matter).
                 */
		unsigned char *vmask;

		/**
                 * Run the los algorithm and return the results in
                 * 'vmask'. Note that both 'alpha' and 'vmask' must refer to
                 * valid pointers.
                 */
		void (*compute) (struct los * los);

		/*** Private ***/

		/**
                 * Free the resources associated with an los engine. This is
                 * for internal use only -- clients should use los_destroy()
                 * below instead.
                 */
		void (*destroy) (struct los * los);

		void *data;

	};

	/**
         * Create an los object.  'algorithm' specifies the name of the los
         * algorithm to use (e.g., "angband" or "floodfill").  'width' and
         * 'height' are used to size the alpha and vmask buffers.  'radius' is
         * used by some algorithms to limit line-of-sight to something less
         * than 'width' or 'height' (angband uses it, floodfill currently does
         * not).  Returns an los struct or null if one can't be created to meet
         * the request.
         */
	extern struct los *los_create(const char *algorithm, int width, int height,
				      int radius);

	/**
         * Free the resources used by an los object.
         */
	extern void los_destroy(struct los *los);

#ifdef __cplusplus
}
#endif
#endif
