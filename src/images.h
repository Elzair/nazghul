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
#ifndef images_h
#define images_h

#include "macros.h"

BEGIN_DECL

#include "list.h"

#include <SDL.h>

struct images {
        char *tag;
        char *fname;
        int w;		/* in pixels of the images */
        int h;		/* in pixels of the images */
        int rows;	/* in the image file */
        int cols;	/* in the image file */
        int offx;	/* pixels separating images */
        int offy;	/* pixels separating images */
        SDL_Surface *images;
        SDL_Surface *faded;	/* faded copy of image set for
                                 * sem-transparent sprites */
};

extern struct images *images_new(const char *tag, int w, int h, int rows, int cols, 
                                 int offx, int offy, const char *fname);
extern int images_convert2display(struct images *images);
extern void images_del(struct images *images);
extern int images_fade(struct images *images);

END_DECL

#endif
