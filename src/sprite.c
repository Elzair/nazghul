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
#include "sprite.h"
#include "images.h"
#include "screen.h"
#include "list.h"
#include "wq.h"
#include "map.h"
#include "common.h"
#include "console.h"
#include "status.h"
#include "Loader.h"
#include "cmdwin.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

struct {
	struct list list;
	struct wq_job animationJob;
} Sprite;

static void myPaintWave(struct sprite *sprite, int frame, int x, int y)
{
	SDL_Rect src;
	SDL_Rect dest;

	frame = (frame + sprite->frame) % sprite->n_frames;

	// Offset the index into the current sequence
	frame += sprite->sequence * sprite->n_frames;

	/* Wave sprites are painted in two blits. The first blit copies
	 * everything below the wavecrest to the top part of the onscreen tile. 
	 * * The second blit copies everything above the wavecrest to the
	 * bottom part of the onscreen tile. This gives the appearance of a
	 * wave rolling over the tile in a direction opposite the wavefront. */

	src = sprite->frames[frame];
	src.y += sprite->wavecrest;	/* fixme -- only works because source
					 * image has one column of sprites */
	src.h -= sprite->wavecrest;

	dest.x = x;
	dest.y = y;
	dest.w = sprite->images->w;
	dest.h = src.h;

	screenBlit(sprite->surf, &src, &dest);

	src = sprite->frames[frame];
	src.h = sprite->wavecrest;

	dest.x = x;
	dest.y = dest.y + sprite->images->h - sprite->wavecrest;
	dest.w = sprite->images->w;
	dest.h = src.h;

	screenBlit(sprite->surf, &sprite->frames[frame], &dest);

}

static void myPaintNormal(struct sprite *sprite, int frame, int x, int y)
{
	SDL_Rect dest;

	dest.x = x;
	dest.y = y;
	dest.w = sprite->images->w;
	dest.h = sprite->images->h;

	frame = (frame + sprite->frame) % sprite->n_frames;
	frame += sprite->sequence * sprite->n_frames;

	screenBlit(sprite->surf, &sprite->frames[frame], &dest);

}

static void myPaintRotated(struct sprite *sprite, int frame, int x, int y)
{
	SDL_Rect dest;

	dest.x = x;
	dest.y = y;
	dest.w = sprite->images->w;
	dest.h = sprite->images->h;

	frame = (frame + sprite->frame) % sprite->n_frames;

	// Offset the index into the current sequence
	frame += sprite->sequence * sprite->n_frames;

	screenBlit(sprite->surf, &sprite->frames[frame], &dest);
}

static void myAdvanceFrame(struct sprite *sprite)
{
	if (sprite->n_frames > 1)
		sprite->frame = (sprite->frame + 1) % sprite->n_frames;

	sprite->wavecrest = ((sprite->wavecrest - 1) + sprite->images->h) %
	    sprite->images->h;
}

#if 0
static void myRotateFrame(SDL_Surface * srcSurface, SDL_Rect * srcRect,
			  SDL_Surface * destSurface, int destFrame)
{
	/* If I decide to change screen depth I'll need to use a different data 
	 * size for each pixel. */
#if SCREEN_DEPTH == 8
	typedef Uint8 MY_PIXEL;
#elif SCREEN_DEPTH == 16
	typedef Uint16 MY_PIXEL;
#elif SCREEN_DEPTH == 32
	typedef Uint32 MY_PIXEL;
#endif
	MY_PIXEL *srcPixel, *destPixel;
	int row, col;

	/* Find the start of the source pixels */
	srcPixel = (MY_PIXEL *) srcSurface->pixels;
	srcPixel += srcRect->y * srcSurface->pitch + srcRect->x;

	/* Find the start of the destination pixels */
	destPixel = (MY_PIXEL *) destSurface->pixels;
	destPixel += destFrame * destSurface->pitch * srcRect->h;

	SDL_LockSurface(srcSurface);
	SDL_LockSurface(destSurface);

	/* Convert row-column to column-row (not quite a rotate, but if the
	 * image is symmetric it works) */
	for (row = 0; row < srcRect->h; row++) {
		for (col = 0; col < srcRect->w; col++) {
			destPixel[col * destSurface->pitch + row] =
			    srcPixel[row * srcSurface->pitch + col];
		}
	}

	SDL_UnlockSurface(destSurface);
	SDL_UnlockSurface(srcSurface);
}

static void myRotateSprite(struct sprite *sprite)
{
	int width, height, depth;
	Uint32 flags, rmask, gmask, bmask, amask;
	int frame, n_total_frames;

	/* Create a new surface large enough to hold rotated copies of all the
	 * frames. */
	n_total_frames = sprite->n_sequences * sprite->n_frames;
	flags = sprite->images->images->flags;
	width = sprite->images->w;
	height = sprite->images->h * n_total_frames;
	depth = sprite->images->images->format->BitsPerPixel;
	rmask = sprite->images->images->format->Rmask;
	gmask = sprite->images->images->format->Gmask;
	bmask = sprite->images->images->format->Bmask;
	amask = sprite->images->images->format->Amask;

	sprite->surf = SDL_CreateRGBSurface(flags, width, height, depth, rmask,
					    gmask, bmask, amask);
	if (!sprite->surf) {
		perror_sdl("SDL_CreateRGBSurface");
		return;
	}

	sprite->shade = false;

	/* Hack: to display properly the new surface must have the same palette
	 * as the images surface. I don't see any easy way to do that in SDL,
	 * but the following works quite nicely. I do need to keep track of the
	 * original palette so that SDL can delete it if and when I tell it to
	 * free the new surface. */
	sprite->savedPalette = sprite->surf->format->palette;
	sprite->surf->format->palette = sprite->images->images->format->palette;

	/* For each frame copy the bits from the original surface to the new
	 * one, rotating them as we go. */
	for (frame = 0; frame < n_total_frames; frame++) {
		myRotateFrame(sprite->images->images, &sprite->frames[frame],
			      sprite->surf, frame);
		sprite->frames[frame].x = 0;
		sprite->frames[frame].y = frame * sprite->images->h;
	}
}
#endif

struct sprite *spriteCreate(char *tag, int n_frames, int index,
			    enum SpriteStyle style, struct images *images,
			    int n_sequences, int facings)
{
	struct sprite *sprite;
	int i;
	int row;
	int col;
	int col_width;
	int row_height;
	int frame;
	int n_total_frames;

	CREATE(sprite, struct sprite, 0);

	sprite->tag = strdup(tag);
	sprite->n_frames = n_frames;
	sprite->index = index;
	sprite->style = style;
	sprite->images = images;
	sprite->frame = 0;
	sprite->wavecrest = images->h;
	sprite->facing = 0;
	sprite->sequence = 0;
	sprite->n_sequences = n_sequences;
	sprite->facings = facings;

	n_total_frames = n_frames * n_sequences;

	sprite->frames = (SDL_Rect *) malloc(sizeof(SDL_Rect) * n_total_frames);
	if (!sprite->frames) {
		spriteDestroy(sprite);
		return 0;
	}

	col_width = (images->w + images->offx);
	row_height = (images->h + images->offy);

	for (i = 0, frame = index; i < n_total_frames; i++, frame++) {
		col = frame % images->cols;
		row = frame / images->cols;
		sprite->frames[i].x = col * col_width + images->offx;
		sprite->frames[i].y = row * row_height + images->offy;
		sprite->frames[i].w = images->w;
		sprite->frames[i].h = images->h;
	}

#if 0
	// crashes at bpp other than 8
	if (sprite->style == RotatedSprite) {
		myRotateSprite(sprite);
	}
#endif
	return sprite;
}

struct sprite *sprite_load(class Loader * loader, struct images *images)
{
	struct sprite *sprite;
	int i;
	int n_total_frames;
	int col_width;
	int row_height;
	int frame;
	int col;
	int row;

	if ((sprite = new struct sprite) == NULL)
		return NULL;
	memset(sprite, 0, sizeof(*sprite));

	if (!loader->getWord(&sprite->tag) ||
	    !loader->matchToken('{') ||
	    !loader->getIntKeyValue("frames", &sprite->n_frames) ||
	    !loader->getIntKeyValue("index", &sprite->index) ||
	    !loader->matchWord("style"))
		goto fail;

	// Parse the style
	if (loader->matchWord("normal")) {
		sprite->style = NormalSprite;
	} else if (loader->matchWord("wave")) {
		sprite->style = WaveSprite;
	} else if (loader->matchWord("rotate")) {
		sprite->style = RotatedSprite;
	} else {
		loader->setError("Error loading SPRITE '%s': invalid style",
				 sprite->tag);
		goto fail;
	}

	if (!loader->getIntKeyValue("facings", &sprite->facings) ||
	    !loader->matchToken('}'))
		goto fail;

	// Calculate the number of facings.
	for (i = 0; i < NUM_PLANAR_DIRECTIONS; i++)
		sprite->n_sequences++;

	// Sprites which do not support different facings nonetheless have one
	// sequence.
	sprite->n_sequences = max(sprite->n_sequences, 1);

	sprite->images = images;
	sprite->wavecrest = images->h;
	sprite->frame = 0;
	sprite->facing = 0;
	sprite->sequence = 0;
	sprite->surf = images->images;

	// Allocate and initialize the rect structures which index into the
	// image. One rect per frame of animation.
	n_total_frames = sprite->n_frames * sprite->n_sequences;
	sprite->frames = new SDL_Rect[n_total_frames];
	if (!sprite->frames) {
		spriteDestroy(sprite);
		return 0;
	}

	col_width = (images->w + images->offx);
	row_height = (images->h + images->offy);

	for (i = 0, frame = sprite->index; i < n_total_frames; i++, frame++) {
		col = frame % images->cols;
		row = frame / images->cols;
		sprite->frames[i].x = col * col_width + images->offx;
		sprite->frames[i].y = row * row_height + images->offy;
		sprite->frames[i].w = images->w;
		sprite->frames[i].h = images->h;
	}

	return sprite;

      fail:
	spriteDestroy(sprite);
	return NULL;
}

void spriteDestroy(struct sprite *sprite)
{
	if (sprite->frames)
		delete sprite->frames;
	if (sprite->surf) {
		SDL_FreeSurface(sprite->surf);
	}
	delete sprite;
}

static void myRunAnimationJob(struct wq_job *job, struct list *wq)
{
	spriteAdvanceFrames();
	// consoleRepaintCursor();
	cmdwin_repaint_cursor();
	statusRepaint();
	// mapRepaintView(NULL, REPAINT_ACTIVE);
	wqReschedule(wq, job);
}

void spritePaint(struct sprite *sprite, int frame, int x, int y)
{
	switch (sprite->style) {
	case WaveSprite:
		myPaintWave(sprite, frame, x, y);
		break;
	case RotatedSprite:
		myPaintRotated(sprite, frame, x, y);
		break;
	case NormalSprite:
	default:
		myPaintNormal(sprite, frame, x, y);
		break;
	}
}

void spriteStartAnimation(struct list *wq, int tick)
{
	Sprite.animationJob.tick = tick;
	Sprite.animationJob.period = ANIMATION_TICKS;
	Sprite.animationJob.run = myRunAnimationJob;
	wqAddJob(wq, &Sprite.animationJob);
}

void spriteInit(void)
{
	list_init(&Sprite.list);
}

void spriteAdd(struct sprite *sprite)
{
	list_add(&Sprite.list, &sprite->list);
}

struct sprite *spriteLookup(char *tag)
{
	struct list *elem;

	list_for_each(&Sprite.list, elem) {
		struct sprite *sprite = list_entry(elem, struct sprite, list);
		if (!strcmp(sprite->tag, tag)) {
			return sprite;
		}
	}

	return 0;
}

void spriteAdvanceFrames(void)
{
	struct list *elem;

	list_for_each(&Sprite.list, elem) {
		struct sprite *sprite = list_entry(elem, struct sprite, list);
		myAdvanceFrame(sprite);
	}

	// mapMarkAsDirty(ALL_VIEWS);
	mapSetDirty();
}

int spriteSetFacing(struct sprite *sprite, int facing)
{
	int bit, i;

	if (facing == SPRITE_DEF_FACING) {
		sprite->sequence = 0;
		return 0;
	}
	// facing supported?
	if ((sprite->facings & (1 << facing)) == 0)
		return -1;

	sprite->facing = facing;
	sprite->sequence = 0;

	// Find the sequence
	for (i = 0; i < facing; i++) {
		bit = (1 << i);
		if (sprite->facings & bit)
			sprite->sequence++;
	}

	return 0;
}

int sprite_fade(struct sprite *sprite)
{
	if (sprite->faded)
		return 0;

	if (sprite->images->images->format->palette) {
		if (sprite->images->faded == NULL &&
		    images_fade(sprite->images) < 0)
			return -1;
		sprite->surf = sprite->images->faded;
	} else {
		SDL_SetAlpha(sprite->images->images, SDL_SRCALPHA, 128);
	}

	sprite->faded = 1;
	return 0;
}

void sprite_unfade(struct sprite *sprite)
{
	if (sprite->images->images->format->palette)
		sprite->surf = sprite->images->images;
	else
		SDL_SetAlpha(sprite->images->images, SDL_SRCALPHA,
			     SDL_ALPHA_OPAQUE);
	sprite->faded = 0;
}
