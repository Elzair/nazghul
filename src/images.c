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
#include "images.h"
#include "screen.h"
#include "common.h"

#include <assert.h>
#include <SDL_image.h>

void images_del(struct images *images)
{
        if (images->tag)
                free(images->tag);
        if (images->fname)
                free(images->fname);
	if (images->images)
		SDL_FreeSurface(images->images);
	if (images->faded)
		SDL_FreeSurface(images->faded);
	delete images;
}


int images_fade(struct images *images)
{

	if (images->faded)
		return 0;

	images->faded = screenCreateSurface(images->images->w,
					    images->images->h);
	if (images->faded == NULL)
		return -1;

	memcpy(images->faded->pixels, images->images->pixels,
	       images->faded->pitch * images->faded->h);

	screen_fade_surface(images->faded, 1);
	return 0;
}

struct images *images_new(char *tag, int w, int h, int rows, int cols, 
                          int offx, int offy, char *fname)
{
	struct images *images;
	SDL_Surface *tmp;
	char *filename;

	images = new struct images;
        assert(images);
	memset(images, 0, sizeof(*images));

        images->tag     = strdup(tag);
        assert(images->tag);

        images->fname   = strdup(fname);
        assert(images->fname);

        images->w       = w;
        images->h       = h;
        images->offx    = offx;
        images->offy    = offy;
        images->rows    = rows;
        images->cols    = cols;


	filename = dirConcat(IncludeDir,fname);
	if (filename) {
		images->images = IMG_Load(filename);
		free(filename);
	} else {
		images->images = IMG_Load(fname);
	}
	if (!images->images) {
                // BUG: Mac OS X fails to load PNG images here, but GIF works
                // OK.  This could be a libPNG, libSDL, or libSDL_Image bug. In
                // the meantime, better error logging is helpful.
                printf("IMG_Load() failed to load file '%s' because '%s'.\n", 
                       fname, SDL_GetError() );
                assert(false);
                // err("IMG_Load: %s", SDL_GetError()); The err() macro acted
                // strangely, emitting different error text than the above
                // would indicate.  Perhaps that is a clue to the bug?
                // 
                // Perhaps the err() macro, or the call above, 
                // differ from the nazghul-0.2.0 release and recent CVS?
	}

	/* Make magenta the transparent color */
	if (SDL_SetColorKey(images->images, SDL_SRCCOLORKEY,
			    SDL_MapRGB(images->images->format,
				       0xff, 0x00, 0xff)) < 0) {
		err("SDL_SetColorKey: %s", SDL_GetError());
                goto fail;
	}

	/* Convert to video format for faster blitting */
	if ((tmp = SDL_DisplayFormat(images->images)) == NULL) {
		err("SDL_DisplayFormat: %s", SDL_GetError());
                goto fail;
	}

	SDL_FreeSurface(images->images);
	images->images = tmp;

	return images;

      fail:
	images_del(images);
	return NULL;
}
