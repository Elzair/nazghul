/* Copyright (c) 2002 Gordon McNutt */
#ifndef images_h
#define images_h

#ifdef __cplusplus
extern "C" {
#endif

#include "list.h"

#include <SDL/SDL.h>

	struct images {
		struct list list;
		char *tag;
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

	extern struct images *images_load(class Loader *);
	extern void images_destroy(struct images *images);
	extern int images_fade(struct images *images);

#ifdef __cplusplus
}
#endif
#endif
