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
#ifndef templ_h
#define templ_h

/* A templ is a grid of cells that are either on (1) or off (0). 'templ' is
 * short for 'template', which is a reserved word in c++. */
struct templ {
        int refcount;    /* reference count                      */
        int ux, uy;      /* upper left corner                    */
        int w, h;        /* width and height                     */
        char *grid;      /* cells                                */
};

#define templ_i(templ,x,y) ((y)*(templ)->w+(x))
#define templ_x(templ,i) ((i)%(templ)->w)
#define templ_y(templ,i) ((i)/(templ)->w)

extern struct templ *templ_new(int w, int h);
extern struct templ *templ_new_from_range(int rad);
extern void templ_ref(struct templ *grd);
extern void templ_unref(struct templ *grd);
extern char templ_get(struct templ *grd, int x, int y);
extern int templ_set(struct templ *grd, int x, int y, char val);
extern void templ_set_origin(struct templ *grd, int x, int y);
extern void templ_for_each(struct templ *grd, 
                           int (*cb)(struct templ *tmple, int x, int y, void *data),
                           void *data);

#endif
