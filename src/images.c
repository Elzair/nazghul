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
#include "util.h"
#include "Loader.h"
#include "screen.h"

#include <SDL_image.h>

void images_destroy(struct images *images)
{
	if (images->images)
		SDL_FreeSurface(images->images);
	if (images->faded)
		SDL_FreeSurface(images->faded);
	delete images;
}

struct images *images_load(class Loader * loader)
{
	struct images *images;
	SDL_Surface *tmp;
	char *fname = 0;

	if ((images = new struct images) == NULL)
		return NULL;

	memset(images, 0, sizeof(*images));

	if (!loader->getWord(&images->tag) ||
	    !loader->matchToken('{') ||
	    !loader->getIntKeyValue("image_width", &images->w) ||
	    !loader->getIntKeyValue("image_height", &images->h) ||
	    !loader->getIntKeyValue("file_rows", &images->rows) ||
	    !loader->getIntKeyValue("file_cols", &images->cols) ||
	    !loader->getIntKeyValue("file_offx", &images->offx) ||
	    !loader->getIntKeyValue("file_offy", &images->offy) ||
	    !loader->getStringKeyValue("file", &fname) ||
	    !loader->matchWord("sprites") || !loader->matchToken('{'))
		goto fail;

	images->images = IMG_Load(fname);
	if (!images->images) {
		loader->setError("IMG_Load: %s", SDL_GetError());
		free(fname);
		goto fail;
	}
	free(fname);

	/* Make magenta the transparent color */
	if (SDL_SetColorKey(images->images, SDL_SRCCOLORKEY,
			    SDL_MapRGB(images->images->format,
				       0xff, 0x00, 0xff)) < 0) {
		loader->setError("SDL_SetColorKey: %s", SDL_GetError());
		goto fail;
	}

	/* Convert to video format for faster blitting */
	if ((tmp = SDL_DisplayFormat(images->images)) == NULL) {
		loader->setError("SDL_DisplayFormat: %s", SDL_GetError());
		goto fail;
	}

	SDL_FreeSurface(images->images);
	images->images = tmp;

	return images;

      fail:
	images_destroy(images);
	return NULL;
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
