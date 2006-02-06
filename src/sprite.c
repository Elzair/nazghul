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
#include "cmdwin.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

struct {
	struct list list;
        int ticks_to_next_animation;
} Sprite;

static int sprite_zoom_factor = 1;
static unsigned int sprite_ticks = 0;

static void myPaintWave(struct sprite *sprite, int frame, int x, int y)
{
	SDL_Rect src;
	SDL_Rect dest;
        int wavecrest;

        frame = (frame + sprite_ticks) % sprite->n_frames;

	// Offset the index into the current sequence
	frame += sprite->sequence * sprite->n_frames;

        // Subtle: when rendering wave sprites zoomed, we'll get artifacts due
        // to roundoff errors in integer division. Unless we align the
        // wavecrest to the zoom factor. So for example, if we zoom at a factor
        // of two then the wavecrest must be a multiple of 2. Since we only
        // support a zoom factor of 2 right now, the simplest thing to do is
        // always use 2.
        wavecrest = (sprite_ticks * 2) % sprite->images->h;
        wavecrest = sprite->images->h - wavecrest; // make it roll south

	/* Wave sprites are painted in two blits. The first blit copies
	 * everything below the wavecrest to the top part of the onscreen tile.
	 * The second blit copies everything above the wavecrest to the
	 * bottom part of the onscreen tile. This gives the appearance of a
	 * wave rolling over the tile in a direction opposite the wavefront. */

	src = sprite->frames[frame];
	src.y += wavecrest;	/* fixme -- only works because source
                                 * image has one column of sprites */
	src.h -= wavecrest;

	dest.x = x;
	dest.y = y;
	dest.w = sprite->images->w;
	dest.h = src.h;

	screenBlit(sprite->surf, &src, &dest);

	src = sprite->frames[frame];
	src.h = wavecrest;

	dest.x = x;
	dest.y = dest.y + (sprite->images->h - wavecrest) / 
                sprite_zoom_factor;
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

        frame = (frame + sprite_ticks) % sprite->n_frames;
	frame += sprite->sequence * sprite->n_frames;

	screenBlit(sprite->surf, &sprite->frames[frame], &dest);

}

void sprite_del(struct sprite *sprite)
{
        if (sprite->tag)
                free(sprite->tag);
	if (sprite->frames)
		delete [] sprite->frames;
        if (sprite->decor)
                sprite_del(sprite->decor);
	delete sprite;
}

void spritePaint(struct sprite *sprite, int frame, int x, int y)
{
        while (sprite) {

                if (sprite->wave) {
                        myPaintWave(sprite, frame, x, y);
                } else {
                        myPaintNormal(sprite, frame, x, y);
                }
                
                sprite = sprite->decor;
        }
}

void spriteAdvanceTicks(int ticks)
{
        Sprite.ticks_to_next_animation -= ticks;
        if (Sprite.ticks_to_next_animation <= 0) {
                spriteAdvanceFrames();
                cmdwin_repaint_cursor();
                statusRepaint();
                Sprite.ticks_to_next_animation += AnimationTicks;
        }
}

int spriteInit(void)
{
        Sprite.ticks_to_next_animation = 0;
        return 0;
}

void spriteAdvanceFrames(void)
{
        sprite_ticks++;
	mapSetDirty();

}

int spriteGetFacing(struct sprite *sprite)
{
        return sprite->facing;
}

int spriteSetFacing(struct sprite *sprite, int facing)
{
	int bit, i;

	if (facing == SPRITE_DEF_FACING) {
		sprite->sequence = 0;
		return 0;
	}
	// facing supported?
	if ((sprite->facings & (1 << facing)) == 0) {
                dbg("warn: spriteSetFacing: facing=%d invalid for sprite %s\n",
                    facing, sprite->tag);
		return -1;
        }

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

void spriteZoomOut(int factor)
{
        sprite_zoom_factor *= factor;
}

extern void spriteZoomIn(int factor)
{
        sprite_zoom_factor /= factor;
}

struct sprite * sprite_new(char *tag, int frames, int index, int wave, 
                           int facings, struct images *images)
{
	struct sprite *sprite;
        int n_total_frames;
        int col_width;
        int row_height;
	int i;
        int frame;
        int col;
        int row;

	sprite = new struct sprite;
        assert(sprite);
	memset(sprite, 0, sizeof(*sprite));

        if (tag)
                sprite->tag       = strdup(tag);
        sprite->n_frames  = frames;
        sprite->index     = index;
        sprite->facings   = facings;
	sprite->facing    = 0;
	sprite->sequence  = 0;
        sprite->images    = images;
	sprite->surf      = images->images;
        sprite->wave      = !!wave;

	// Allocate and initialize the rect structures which index into the
	// image. One rect per frame of animation. Note that 'facings' is a
	// bitmask, not a count. Sprites that don't have different facings
	// specify 'facings' as zero, so for these assume we'll want one
	// sequence of frames. Sprites that do support facings will need as
	// many sequences as there are directions supported by the game.
	n_total_frames = sprite->n_frames * (sprite->facings ? NUM_PLANAR_DIRECTIONS : 1);
	sprite->frames = new SDL_Rect[n_total_frames];
        assert(sprite->frames);

	col_width      = (images->w + images->offx);
	row_height     = (images->h + images->offy);

	for (i = 0, frame = sprite->index; i < n_total_frames; i++, frame++) {
		col = frame % images->cols;
		row = frame / images->cols;
		sprite->frames[i].x = col * col_width + images->offx;
		sprite->frames[i].y = row * row_height + images->offy;
		sprite->frames[i].w = images->w;
		sprite->frames[i].h = images->h;
	}        

	return sprite;
}

struct sprite *spriteClone(struct sprite *orig)
{
        return sprite_new(NULL, orig->n_frames, orig->index, orig->wave, 
                          orig->facings, orig->images);
}

void spriteAppendDecoration(struct sprite *base, struct sprite *decor)
{
        assert(base);
        while (base->decor) {
                base = base->decor;
        }
        base->decor = spriteClone(decor);
}
