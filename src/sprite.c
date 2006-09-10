/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2002, 2003 Gordon McNutt
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
#include "cmdwin.h"
#include "images.h"
#include "list.h"
#include "map.h"
#include "screen.h"
#include "session.h"
#include "sprite.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* rsurf - wraps a surface with a reference count so sprites can share them
 * without fear of premature calls to SDL_FreeSurface(). */
struct rsurf {
        int ref;              /* reference count                     */
        SDL_Surface *surf;    /* underlying surface                  */
        char custom : 1;      /* NOT referenced by any struct images */
};

/* sprite - animation sequence with different facings */
struct sprite {
        char *tag;              /* Script variable name for the sprite.    */
        int n_frames;           /* per sequence                            */
        int n_total_frames;     /* n_frames x # facings                    */
        SDL_Rect *frames;       /* all frames (sequences must be in order) */
        struct rsurf *rsurf;    /* source of image                         */
        int facing;             /* current facing sequence                 */
        int facings;            /* bitmap of supported facing sequences    */
        int sequence;           /* current animation sequence              */
        struct sprite *decor;   /* decoration sprites                      */
        Uint32 tint;            /* optional color tint                     */
        int w_pix, h_pix;       /* frame dimensions (in pixels)            */
        int faded  : 1;	        /* render sprite sem-transparent           */
        int wave   : 1;         /* vertical roll                           */
        int tinted : 1;         /* apply tint color                        */
};

struct {
        int ticks_to_next_animation;
} Sprite;

static int sprite_zoom_factor = 1;
static unsigned int sprite_ticks = 0;

static struct rsurf *sprite_rsurf_new(SDL_Surface *surf)
{
        struct  rsurf *rsurf;

        rsurf = (struct rsurf*)calloc(1, sizeof(*rsurf));
        if (!rsurf)
                return 0;
        rsurf->ref = 1;
        rsurf->surf = surf;
        return rsurf;
}

static void sprite_rsurf_unref(struct rsurf *rsurf)
{
        assert(rsurf->ref > 0);
        rsurf->ref--;
        if (!rsurf->ref) {
                if (rsurf->surf && rsurf->custom) {
                        SDL_FreeSurface(rsurf->surf);
                }
                free(rsurf);
        }
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
                        pix_alpha = ((pixel & source->format->Amask) 
                                     >> source->format->Ashift);
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

	/* Offset the index into the current sequence */
	frame += sprite->sequence * sprite->n_frames;

        // Subtle: when rendering wave sprites zoomed, we'll get artifacts due
        // to roundoff errors in integer division. Unless we align the
        // wavecrest to the zoom factor. So for example, if we zoom at a factor
        // of two then the wavecrest must be a multiple of 2. Since we only
        // support a zoom factor of 2 right now, the simplest thing to do is
        // always use 2.
        wavecrest = (sprite_ticks * 2) % sprite->h_pix;
        wavecrest = sprite->h_pix - wavecrest; // make it roll south

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
	dest.w = sprite->w_pix;
	dest.h = src.h;

        if (sprite->faded) {
                sprite_blit_faded(sprite->rsurf->surf,  &sprite->frames[frame],
                                  &dest);
        } else {
                screenBlit(sprite->rsurf->surf, &src, &dest);
        }

	src = sprite->frames[frame];
	src.h = wavecrest;

	dest.x = x;
	dest.y = dest.y + (sprite->h_pix - wavecrest) / 
                sprite_zoom_factor;
	dest.w = sprite->w_pix;
	dest.h = src.h;
        
        if (sprite->faded) {
                sprite_blit_faded(sprite->rsurf->surf,  &sprite->frames[frame],
                                  &dest);
        } else {
                screenBlit(sprite->rsurf->surf, &sprite->frames[frame], &dest);
        }

}

static void sprite_paint_normal(struct sprite *sprite, int frame, int x, int y)
{
	SDL_Rect dest;

	dest.x = x;
	dest.y = y;
	dest.w = sprite->w_pix;
	dest.h = sprite->h_pix;

        frame = (frame + sprite_ticks) % sprite->n_frames;
	frame += sprite->sequence * sprite->n_frames;

        if (sprite->faded) {
                sprite_blit_faded(sprite->rsurf->surf,  &sprite->frames[frame],
                                  &dest);
        } else {
                screenBlit(sprite->rsurf->surf, &sprite->frames[frame], &dest);
        }

}

static void sprite_tint_image(SDL_Surface *source, SDL_Rect *from, 
                              SDL_Surface *dest, SDL_Rect *to, Uint32 tint)
{
	int dx, dy, di, sx, sy, si, spitch, dpitch;
	Uint32 *dpix, *spix;
        Uint8 tint_red, tint_grn, tint_blu;
        Uint8 pix_red, pix_grn, pix_blu, pix_alpha;

        /* Isolate the color components of the tint (this assumes the tint has
         * the same pixel format as the source...) */
        tint_red = (tint & source->format->Rmask) >> source->format->Rshift;
        tint_grn = (tint & source->format->Gmask) >> source->format->Gshift;
        tint_blu = (tint & source->format->Bmask) >> source->format->Bshift;

	dpix = (Uint32 *) dest->pixels;
	spix = (Uint32 *) source->pixels;

	dpitch = dest->pitch / dest->format->BytesPerPixel;
	spitch = source->pitch / source->format->BytesPerPixel;

	for (dy = 0; dy < from->h; dy++) {
		sy = dy;
		for (dx = 0; dx < from->w; dx++) {
			sx = dx;
			di = (dy * dpitch + dx);
			si = (sy + from->y) * spitch + (sx + from->x);

                        /* Isolate the color components of the pixel. */
                        pix_red = ((spix[si] & source->format->Rmask) 
                                   >> source->format->Rshift);
                        pix_grn = ((spix[si] & source->format->Gmask) 
                                   >> source->format->Gshift);
                        pix_blu = ((spix[si] & source->format->Bmask) 
                                   >> source->format->Bshift);
                        pix_alpha = ((spix[si] & source->format->Amask) 
                                     >> source->format->Ashift);

                        /* Average the tint and pixel colors. */
                        pix_red = (pix_red + tint_red) / 2;
                        pix_grn = (pix_grn + tint_grn) / 2;
                        pix_blu = (pix_blu + tint_blu) / 2;

                        /* Recombine them, along with the original alpha
                         * component, into the destination pixel. */
                        dpix[di] = (pix_red << dest->format->Rshift
                                    | pix_grn << dest->format->Gshift
                                    | pix_blu << dest->format->Bshift
                                    | pix_alpha << dest->format->Ashift);
                }
        }
}

static struct sprite * sprite_new_internal(int frames, int facings)
{
	struct sprite *sprite;

	sprite = (struct sprite*)calloc(1, sizeof(*sprite));
        if (!sprite)
                return 0;

        sprite->n_frames  = frames;
        sprite->facings = facings;
        sprite->n_total_frames = (sprite->n_frames 
                                  * (sprite->facings ? 
                                     NUM_PLANAR_DIRECTIONS : 1));

	// Allocate and initialize the rect structures which index into the
	// image. One rect per frame of animation. Note that 'facings' is a
	// bitmask, not a count. Sprites that don't have different facings
	// specify 'facings' as zero, so for these assume we'll want one
	// sequence of frames. Sprites that do support facings will need as
	// many sequences as there are directions supported by the game.
	
	sprite->frames = (SDL_Rect*)calloc(sprite->n_total_frames, 
                                           sizeof(SDL_Rect));
        if (!sprite->frames) {
                goto abort;
        }

	return sprite;

 abort:
        sprite_del(sprite);
        return 0;
}

void sprite_del(struct sprite *sprite)
{
        if (sprite->tag)
                free(sprite->tag);
	if (sprite->frames)
		free(sprite->frames);
        if (sprite->decor)
                sprite_del(sprite->decor);
        sprite_rsurf_unref(sprite->rsurf);

        free(sprite);
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
                dbg("warn: sprite_set_facing: facing=%d invalid for "\
                    "sprite %s\n",
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
        int col_width;
        int row_height;
	int i;
        int frame;
        int col;
        int row;

        /* Allocate it. */
	sprite = sprite_new_internal(frames, facings);
        if (!sprite)
                return 0;

        /* Dupe the tag if applicable. */
        if (tag) {
                if (!(sprite->tag = strdup(tag)))
                        goto abort;
        }

        /* Create a new refcounted surf. */
        if (!(sprite->rsurf = sprite_rsurf_new(images->images)))
                goto abort;


        /* Fill out the rest of the basic fields. */
        sprite->wave = !!wave;
        sprite->w_pix = images->w;
        sprite->h_pix = images->h;

        /* Fill out the frames based on the index and image info. */
	col_width = (images->w + images->offx);
	row_height = (images->h + images->offy);
	for (i = 0, frame = index; 
             i < sprite->n_total_frames; 
             i++, frame++) {
		col = frame % images->cols;
		row = frame / images->cols;
		sprite->frames[i].x = col * col_width + images->offx;
		sprite->frames[i].y = row * row_height + images->offy;
		sprite->frames[i].w = images->w;
		sprite->frames[i].h = images->h;
	}        

	return sprite;

 abort:
        sprite_del(sprite);
        return 0;
}

struct sprite *sprite_clone(struct sprite *orig, char *tag)
{
        SDL_Rect *frames;

        /* Allocate it. */
        struct sprite *sprite = sprite_new_internal(orig->n_frames, 
                                                    orig->facings);
        if (! sprite) {
                return 0;
        }

        /* Remember the frames pointer before we wipe it out with the copy. */
        frames = sprite->frames;

        /* Copy the sprite info. */
        memcpy(sprite, orig, sizeof(*orig));

        /* Copy the frames. */
        sprite->frames = frames;
        memcpy(sprite->frames, orig->frames, 
               sprite->n_total_frames * sizeof(sprite->frames[0]));

        /* Bump the refcount on the surface. */
        sprite->rsurf->ref++;

        /* Dupe the tag if applicable. */
        if (tag) {
                sprite->tag = strdup(tag);
        } else if (orig->tag) {
                sprite->tag = strdup(orig->tag);
        }

        return sprite;
}

void sprite_append_decoration(struct sprite *base, struct sprite *decor)
{
        assert(base);
        while (base->decor) {
                base = base->decor;
        }
        base->decor = sprite_clone(decor, decor->tag);
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
        SDL_Surface *dest = 0;
        SDL_Surface *source = sprite->rsurf->surf;
        SDL_Rect to;
        int i;

        /* Create a new surface so that the original (which may be shared with
         * other sprites) is not tinted. */
	dest = SDL_CreateRGBSurface(source->flags,
                                    sprite->w_pix * sprite->n_total_frames,
                                    sprite->h_pix,
                                    source->format->BitsPerPixel,
                                    source->format->Rmask,
                                    source->format->Gmask,
                                    source->format->Bmask,
                                    source->format->Amask);
        if (!dest) {
		perror_sdl("SDL_CreateRGBSurface");
		return;
        }

        /* Make a tinted copy of the surface. */
        to.x = 0;
        to.y = 0;
        to.w = sprite->w_pix;
        to.h = sprite->h_pix;
        for (i = 0; i < sprite->n_total_frames; i++) {
                to.x = i * sprite->w_pix;

                /* Tint the frame image. */
                sprite_tint_image(sprite->rsurf->surf, &sprite->frames[i],
                                  dest, &to, tint);

                /* Fixup the frames as we go. */
                sprite->frames[i] = to;
        }

        /* Stash the surface in a new refcounted surf wrapper. */
        sprite->rsurf = sprite_rsurf_new(dest);
        sprite->rsurf->custom = 1;

        /* Set the tint info. */
        sprite->tint = tint;
        sprite->tinted = 1;
}

/* sprite_save - save to file for reload. */
void sprite_save(struct sprite *sprite, struct save *save)
{
        /* For simple sprites just save the tag. */
        if (!sprite->decor) {
                assert(sprite->tag);
                save->write(save, "%s ", sprite->tag);
                return;
        }

        /* For composite sprites */
        save->write(save, ("(mk-composite-sprite (list "));
        while (sprite) {
                assert(sprite->tag);
                save->append(save, "%s ", sprite->tag);
                sprite = sprite->decor;
        }
        save->append(save, "))\n");
}

static void sprite_apply_matrix_to_image(SDL_Surface *source, SDL_Rect *from, 
                                         SDL_Surface *dest,  SDL_Rect *to,
                                         float matrix[4][3])
{
        Uint32 *dpix, *spix;
        int dx, dy, di, sx,  sy, si, spitch,  dpitch;
        Uint8 in_red, in_grn, in_blu, in_alpha, out_red, out_grn, out_blu, 
                out_alpha;
        int ired, igrn, iblu;

        assert(source->format->BytesPerPixel==4);

        spix = (Uint32*)source->pixels;
        dpix = (Uint32*)dest->pixels;

        dpitch = dest->pitch / dest->format->BytesPerPixel;
        spitch = source->pitch / source->format->BytesPerPixel;

        for (dy = 0; dy < from->h; dy++) {
                sy = dy;
                for (dx = 0; dx < from->w; dx++) {
                        sx = dx;
                        di = (dy + to->y) * dpitch + (dx + to->x);
                        si = (sy + from->y) * spitch + (sx + from->x);

                        /* For speed, skip blank pixels. */
                        if (!spix[si]) {
                                continue;
                        }

                        /* Extract the alpha component of the source pixel. */
                        in_alpha = ((spix[si] & source->format->Amask) 
                                     >> source->format->Ashift);

                        /* For speed, skip transparent pixels. */
                        if (! in_alpha) {
                                continue;
                        }

                        /* Extract the color components of the source pixel. */
                        in_red = ((spix[si] & source->format->Rmask) 
                                   >> source->format->Rshift);
                        in_grn = ((spix[si] & source->format->Gmask) 
                                   >> source->format->Gshift);
                        in_blu = ((spix[si] & source->format->Bmask) 
                                   >> source->format->Bshift);

                        /* Run the matrix conversion. */
                        ired = (int)((in_red * matrix[0][0])
                                     + (in_grn * matrix[0][1])
                                     + (in_blu * matrix[0][2])
                                     + matrix[3][0]);

                        igrn = (int)((in_red * matrix[1][0])
                                     + (in_grn * matrix[1][1])
                                     + (in_blu * matrix[1][2])
                                     + matrix[3][1]);
                        
                        iblu = (int)((in_red * matrix[2][0])
                                     + (in_grn * matrix[2][1])
                                     + (in_blu * matrix[2][2])
                                     + matrix[3][2]);

                        out_alpha = in_alpha;

                        /* Clamp the result to the allowed range. */
                        out_red = clamp(ired, 0, 
                                        (int)(dest->format->Rmask 
                                              >> dest->format->Rshift));
                        out_grn = clamp(igrn, 0, 
                                        (int)(dest->format->Gmask 
                                              >> dest->format->Gshift));
                        out_blu = clamp(iblu, 0, 
                                        (int)(dest->format->Bmask 
                                              >> dest->format->Bshift));

                        /* Recombine them, along with the original alpha
                         * component, into the destination pixel. */
                        dpix[di] = (out_red << dest->format->Rshift
                                    | out_grn << dest->format->Gshift
                                    | out_blu << dest->format->Bshift
                                    | out_alpha << dest->format->Ashift);
                }
        }
}

/* sprite_apply_matrix - applies a color conversion matrix. This is good for
 * converting monotone or grayscale images into other tones. The matrix is
 * applied as follows:
 *
 * r = R*m[0][0] + G*m[0][1] + B*m[0][2] + m[3][0]
 * g = R*m[1][0] + G*m[1][1] + B*m[1][2] + m[3][1]
 * b = R*m[2][0] + G*m[2][1] + B*m[2][2] + m[3][2]
 *
 */
void sprite_apply_matrix(struct sprite *sprite, float matrix[4][3])
{
        SDL_Surface *dest = 0;
        SDL_Surface *source = sprite->rsurf->surf;
        SDL_Rect to;
        int i;

        /* Create a new surface so that the original (which may be shared with
         * other sprites) is not altered. */
	dest = SDL_CreateRGBSurface(source->flags,
                                    sprite->w_pix * sprite->n_total_frames,
                                    sprite->h_pix,
                                    source->format->BitsPerPixel,
                                    source->format->Rmask,
                                    source->format->Gmask,
                                    source->format->Bmask,
                                    source->format->Amask);
        if (!dest) {
		perror_sdl("SDL_CreateRGBSurface");
		return;
        }

        /* Apply the correction to each frame of the sprite. This could be
         * optimized a bit by applying the matrix in one pass to the entire set
         * of frames, since they are always contiguous in the image. */
        to.x = 0;
        to.y = 0;
        to.w = sprite->w_pix;
        to.h = sprite->h_pix;
        for (i = 0; i < sprite->n_total_frames; i++) {
                to.x = i * sprite->w_pix;

                /* Apply to the image. */
                sprite_apply_matrix_to_image(sprite->rsurf->surf, 
                                             &sprite->frames[i],
                                             dest, &to, matrix);

                /* Fixup the frames as we go. */
                sprite->frames[i] = to;
        }

        /* Stash the surface in a new refcounted surf wrapper. */
        sprite->rsurf = sprite_rsurf_new(dest);
        sprite->rsurf->custom = 1;
}
