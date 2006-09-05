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

extern int sprite_init(void);
extern void sprite_paint(struct sprite *sprite, int frame, int x, int y);
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
extern struct sprite *sprite_clone(struct sprite *orig);
extern struct sprite * sprite_new(char *tag, int frames, int index, int wave, 
                                  int facings, struct images *image);
extern void sprite_del(struct sprite *sprite);
extern char *sprite_get_tag(struct sprite *sprite);
extern int sprite_is_faded(struct sprite *sprite);
extern int sprite_can_face(struct sprite *sprite, int facing);
extern void sprite_tint(struct sprite *sprite, Uint32 tint);

END_DECL

#endif
