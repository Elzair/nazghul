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
#ifndef sprite_h
#define sprite_h

#include "macros.h"

BEGIN_DECL

#include "list.h"

#include <SDL.h> /* for Uint32 */


#define SPRITE_DEF_FACING -1

struct images;
struct sprite;
struct save;

extern int sprite_init(void);
extern void sprite_paint(struct sprite *sprite, int frame, int x, int y);
extern void sprite_paint_frame(struct sprite *sprite, int frame, int x, int y);
extern void sprite_advance_frames(void);
extern int sprite_set_facing(struct sprite *sprite, int direction);
extern int sprite_get_facing(struct sprite *sprite);

extern int sprite_fade(struct sprite *sprite);
extern void sprite_unfade(struct sprite *sprite);
extern void sprite_zoom_out(int factor);
extern void sprite_zoom_in(int factor);
extern void sprite_advance_ticks(int ticks);
extern void sprite_append_decoration(struct sprite *sprite, 
                                     struct sprite *decor);

/* sprite_clone - clone an existing sprite and give it a new tag. */
extern struct sprite *sprite_clone(struct sprite *orig, const char *new_tag);

extern struct sprite * sprite_new(const char *tag, int frames, int index, int wave, 
                                  int facings, struct images *image);
extern void sprite_del(struct sprite *sprite);
extern char *sprite_get_tag(struct sprite *sprite);
extern int sprite_is_faded(struct sprite *sprite);
extern int sprite_can_face(struct sprite *sprite, int facing);

/* sprite_save - save to file for reload. */
extern void sprite_save(struct sprite *sprite, struct save *save);

/* sprite_apply_matrix - applies a color conversion matrix. This is good for
 * converting monotone or grayscale images into other tones. The matrix is
 * applied as follows:
 *
 * r = R*m[0][0] + G*m[0][1] + B*m[0][2] + m[3][0]
 * g = R*m[1][0] + G*m[1][1] + B*m[1][2] + m[3][1]
 * b = R*m[2][0] + G*m[2][1] + B*m[2][2] + m[3][2]
 *
 */
void sprite_apply_matrix(struct sprite *sprite, float matrix[4][3]);

/**
 * Remove all decorations from the sprite and discard them (see
 * sprite_append_decoration()). Useful for rebuilding decorated sprites from
 * scratch.
 *
 * @param sprite The sprite to strip.
 */
extern void sprite_strip_decorations(struct sprite *sprite);

/**
 * Blit one sprite over another. The images of the destination sprite will be
 * copied and then modified by the blit, so you don't have to worry about other
 * sprites that refer to the same images. The two sprites should have the same
 * number of frames and the same dimensions or the results are not defined. The
 * modification will not be saved with the game, so it needs to be redone at
 * load time.
 *
 * @param dest The sprite that will be modified.
 * @param src The sprite that will blit over the other one. It won't be
 * modified.
 *
 */
extern void sprite_blit_over(struct sprite *dest, 
                             struct sprite *src);
                             
extern int sprite_num_frames(struct sprite *sprite);
extern int sprite_facings_list(struct sprite *sprite);

extern void sprite_paint_direct(struct sprite *sprite, int frame, SDL_Rect *dest);

END_DECL

#endif
