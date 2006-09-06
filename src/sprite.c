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

struct sprite {
        struct list list;       // for storage while loading
        char *tag;
        int n_frames;           // per sequence
        int index;              // location in the image set
        SDL_Rect *frames;       // all frames (sequences must be in order)
        struct images *images;  // image set
        SDL_Surface *surf;      // current source of images
        int facing;             // current facing sequence
        int facings;            // bitmap of supported facing sequences
        int sequence;           // current animation sequence
        struct sprite *decor;   // decoration sprites
        Uint32 tint;            /* optional color tint */
        int faded  : 1;	        // render sprite sem-transparent
        int wave   : 1;         /* vertical roll */
        int tinted : 1;         /* apply tint color */
};

struct {
	struct list list;
        int ticks_to_next_animation;
} Sprite;

static int sprite_zoom_factor = 1;
static unsigned int sprite_ticks = 0;

static void sprite_blit_tinted(SDL_Surface *source, SDL_Rect *from, 
                               SDL_Rect *to, Uint32 tint)
{
	int dx, dy, di, sx, sy, si, spitch, dpitch;
	Uint32 *dpix, *spix;
        Uint8 tint_red, tint_grn, tint_blu;
        Uint8 pix_red, pix_grn, pix_blu, pix_alpha;
        SDL_Surface *tmp = 0;

	tmp = SDL_CreateRGBSurface(source->flags,
				   from->w, from->h,
				   source->format->BitsPerPixel,
				   source->format->Rmask,
				   source->format->Gmask,
				   source->format->Bmask,
				   source->format->Amask);
	if (tmp == NULL) {
		perror_sdl("SDL_CreateRGBSurface");
		return;
	}

        /* Isolate the color components of the tint (this assumes the tint has
         * the same pixel format as the source...) */
        tint_red = (tint & source->format->Rmask) >> source->format->Rshift;
        tint_grn = (tint & source->format->Gmask) >> source->format->Gshift;
        tint_blu = (tint & source->format->Bmask) >> source->format->Bshift;

	dpix = (Uint32 *) tmp->pixels;
	spix = (Uint32 *) source->pixels;

	dpitch = tmp->pitch / tmp->format->BytesPerPixel;
	spitch = source->pitch / source->format->BytesPerPixel;

	for (dy = 0; dy < from->h; dy++) {
		sy = dy;
		for (dx = 0; dx < from->w; dx++) {
			sx = dx;
			di = (dy * dpitch + dx);
			si = (sy + from->y) * spitch + (sx + from->x);

                        /* Isolate the color components of the pixel. */
                        pix_red = (spix[si] & source->format->Rmask) >> source->format->Rshift;
                        pix_grn = (spix[si] & source->format->Gmask) >> source->format->Gshift;
                        pix_blu = (spix[si] & source->format->Bmask) >> source->format->Bshift;
                        pix_alpha = (spix[si] & source->format->Amask) >> source->format->Ashift;

                        /* Average the tint and pixel colors. */
                        pix_red = (pix_red + tint_red) / 2;
                        pix_grn = (pix_grn + tint_grn) / 2;
                        pix_blu = (pix_blu + tint_blu) / 2;

                        /* Recombine them, along with the original alpha
                         * component, into the destination pixel. */
                        dpix[di] = (pix_red << tmp->format->Rshift
                                    | pix_grn << tmp->format->Gshift
                                    | pix_blu << tmp->format->Bshift
                                    | pix_alpha << tmp->format->Ashift);
                }
        }
        

        screenBlit(tmp, NULL, to);
        SDL_FreeSurface(tmp);
}

static void sprite_blit_faded(SDL_Surface *source, SDL_Rect *from, 
                              SDL_Rect *to)
{
	int dx, dy, di, sx, sy, si, spitch, dpitch;
	Uint32 *dpix, *spix, pixel;
        Uint8 pix_alpha;
        SDL_Surface *tmp = 0;

	tmp = SDL_CreateRGBSurface(source->flags,
				   from->w, from->h,
				   source->format->BitsPerPixel,
				   source->format->Rmask,
				   source->format->Gmask,
				   source->format->Bmask,
				   source->format->Amask);
	if (tmp == NULL) {
		perror_sdl("SDL_CreateRGBSurface");
		return;
	}

	dpix = (Uint32 *) tmp->pixels;
	spix = (Uint32 *) source->pixels;

	dpitch = tmp->pitch / tmp->format->BytesPerPixel;
	spitch = source->pitch / source->format->BytesPerPixel;

	for (dy = 0; dy < from->h; dy++) {
		sy = dy;
		for (dx = 0; dx < from->w; dx++) {
			sx = dx;
			di = (dy * dpitch + dx);
			si = (sy + from->y) * spitch + (sx + from->x);

                        /* Cut alpha component in half. */
                        pixel = spix[si];
                        pix_alpha = (pixel & source->format->Amask) >> source->format->Ashift;
                        pix_alpha /= 2;
                        pixel &= ~source->format->Amask;
                        pixel |= (pix_alpha << source->format->Ashift);

                        /* Assign result. */
                        dpix[di] = pixel;
                }
        }
        
        screenBlit(tmp, NULL, to);
        SDL_FreeSurface(tmp);
}

static void sprite_paint_wave(struct sprite *sprite, int frame, int x, int y)
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

        if (sprite->tinted) {
                sprite_blit_tinted(sprite->surf, &src,
                                   &dest, sprite->tint);
        } else if (sprite->faded) {
                sprite_blit_faded(sprite->surf,  &sprite->frames[frame], 
                                  &dest);
        } else {
                screenBlit(sprite->surf, &src, &dest);
        }

	src = sprite->frames[frame];
	src.h = wavecrest;

	dest.x = x;
	dest.y = dest.y + (sprite->images->h - wavecrest) / 
                sprite_zoom_factor;
	dest.w = sprite->images->w;
	dest.h = src.h;
        
        if (sprite->tinted) {
                sprite_blit_tinted(sprite->surf, &sprite->frames[frame], 
                                   &dest, sprite->tint);
        } else if (sprite->faded) {
                sprite_blit_faded(sprite->surf,  &sprite->frames[frame], 
                                  &dest);
        } else {
                screenBlit(sprite->surf, &sprite->frames[frame], &dest);
        }

}

static void sprite_paint_normal(struct sprite *sprite, int frame, int x, int y)
{
	SDL_Rect dest;

	dest.x = x;
	dest.y = y;
	dest.w = sprite->images->w;
	dest.h = sprite->images->h;

        frame = (frame + sprite_ticks) % sprite->n_frames;
	frame += sprite->sequence * sprite->n_frames;

        if (sprite->tinted) {
                sprite_blit_tinted(sprite->surf, &sprite->frames[frame], 
                                   &dest, sprite->tint);
        } else if (sprite->faded) {
                sprite_blit_faded(sprite->surf,  &sprite->frames[frame], 
                                  &dest);
        } else {
                screenBlit(sprite->surf, &sprite->frames[frame], &dest);
        }

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

void sprite_paint(struct sprite *sprite, int frame, int x, int y)
{
        while (sprite) {

                if (sprite->wave) {
                        sprite_paint_wave(sprite, frame, x, y);
                } else {
                        sprite_paint_normal(sprite, frame, x, y);
                }
                
                sprite = sprite->decor;
        }
}

void sprite_advance_ticks(int ticks)
{
        Sprite.ticks_to_next_animation -= ticks;
        if (Sprite.ticks_to_next_animation <= 0) {
                sprite_advance_frames();
                cmdwin_repaint_cursor();
                statusRepaint();
                Sprite.ticks_to_next_animation += AnimationTicks;
        }
}

int sprite_init(void)
{
        Sprite.ticks_to_next_animation = 0;
        return 0;
}

void sprite_advance_frames(void)
{
        sprite_ticks++;
	mapSetDirty();

}

int sprite_get_facing(struct sprite *sprite)
{
        return sprite->facing;
}

int sprite_set_facing(struct sprite *sprite, int facing)
{
	int bit, i;

	if (facing == SPRITE_DEF_FACING) {
		sprite->sequence = 0;
		return 0;
	}
	// facing supported?
	if ((sprite->facings & (1 << facing)) == 0) {
                dbg("warn: sprite_set_facing: facing=%d invalid for sprite %s\n",
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
	sprite->faded = 1;
	return 0;
}

void sprite_unfade(struct sprite *sprite)
{
	sprite->faded = 0;
}

void sprite_zoom_out(int factor)
{
        sprite_zoom_factor *= factor;
}

extern void sprite_zoom_in(int factor)
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

struct sprite *sprite_clone(struct sprite *orig)
{
        struct sprite *sprite = sprite_new(NULL, orig->n_frames, 
                                           orig->index, orig->wave, 
                                           orig->facings, orig->images);
        sprite->tint = orig->tint;
        sprite->tinted = orig->tinted;
        return sprite;
}

void sprite_append_decoration(struct sprite *base, struct sprite *decor)
{
        assert(base);
        while (base->decor) {
                base = base->decor;
        }
        base->decor = sprite_clone(decor);
}

char *sprite_get_tag(struct sprite *sprite)
{
        return sprite->tag;
}

int sprite_is_faded(struct sprite *sprite)
{
        return sprite->faded;
}

int sprite_can_face(struct sprite *sprite, int facing)
{
        return (sprite->facings & (1 << facing));
}

void sprite_tint(struct sprite *sprite, Uint32 tint)
{
        sprite->tint = tint;
        sprite->tinted = 1;
}
