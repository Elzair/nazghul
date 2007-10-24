/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2007 Gordon McNutt
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

#include "templ.h"
#include "closure.h"

#include <assert.h>
#include <config.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <string.h>

/* unchecked array accesses */
#define templ_get_unch(templ,x,y) ((templ)->grid[templ_i((templ),(x),(y))])
#define templ_set_unch(templ,x,y,v) ((templ)->grid[templ_i((templ),(x),(y))]=(v))

/* translate to coords relative to template upper left corner */
#define templ_translate(templ,x,y) \
  do {                           \
    (x) = (x) - (templ)->ux;      \
    (y) = (y) - (templ)->uy;      \
  } while (0) 

#define templ_in_range(templ,x,y)         \
  (((x) >= 0) && ((x) < (templ)->w)      \
    && ((y) >= 0) && ((y) < (templ)->h))

static void templ_del(struct templ *templ)
{
        assert(!templ->refcount);
        free(templ->grid);
        free(templ);
}

struct templ *templ_new(int w, int h)
{
        struct templ *templ = (struct templ*)calloc(1, sizeof(*templ));
        assert(templ);
        templ->grid = (char*)calloc(w * h, sizeof(char));
        assert(templ->grid);
        templ->w = w;
        templ->h = h;
        templ->refcount = 1;
        return templ;
}

struct templ *templ_new_from_range(int rad)
{
        int ww = rad * 2 + 1;
        struct templ *templ = templ_new(ww, ww);
        int i = 0, x, y;

        /* mark all the template cells within the radius */
        for (y = 0; y < ww; y++) {
                int dy = (y>rad) ? (y-rad) : (rad-y); /* abs(y-rad) */
                for (x = 0; x < ww; x++, i++) {
                        int dx = (x>rad) ? (x-rad) : (rad-x); /* abs(x-rad) */

                        /* angband quick-and-dirty distance formula: */
                        int d = ((dy>dx) ? (dy+(dx>>1)) : (dx+(dy>>1)));

                        if (d <= rad) {
                                templ->grid[i] = 1;
                        }
                }
        }
        
        return templ;
}

void templ_ref(struct templ *templ)
{
        templ->refcount++;
}

void templ_unref(struct templ *templ)
{
        assert(templ->refcount > 0);
        templ->refcount--;
        if (!templ->refcount) {
                templ_del(templ);
        }
}

char templ_get(struct templ *templ, int x, int y)
{
        templ_translate(templ, x, y);
        if (! templ_in_range(templ, x, y)) {
                return 0;
        }

        return templ_get_unch(templ, x, y);
}

int templ_set(struct templ *templ, int x, int y, char val)
{
        templ_translate(templ, x, y);
        if (! templ_in_range(templ, x, y)) {
                return -1;
        }
        templ_set_unch(templ, x, y, val);
        return 0;
}

void templ_set_origin(struct templ *templ, int x, int y)
{
        templ->ux = x - templ->w / 2;
        templ->uy = y - templ->h / 2;
}

void templ_for_each(struct templ *grd, 
                    int (*cb)(struct templ *, int, int, void *),
                    void *data)
{
        int x1, y1, x2, y2;
        templ_ref(grd);
        for (y1 = 0; y1 < grd->h; y1++) {
                y2 = y1 + grd->uy;
                for (x1 = 0; x1 < grd->w; x1++) {

                        /* run the cb only on tiles in the templ */
                        if (templ_get_unch(grd, x1, y1)) {
                                x2 = x1 + grd->ux;
                                if (cb(grd, x2, y2, data)) {
                                        goto done;
                                }
                        }
                }
        }
 done:
        templ_unref(grd);
}
