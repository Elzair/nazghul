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

#ifdef __cplusplus
extern "C" {
#endif

#include "list.h"

#include <SDL/SDL.h>

#define SPRITE_DEF_FACING -1

        struct images;
        struct sprite;

        enum SpriteStyle {
                NormalSprite,
                WaveSprite,
                RotatedSprite,
        };

        struct sprite {
                struct list list;       // for storage while loading
                char *tag;              // for lookups while loading
                int n_frames;           // per sequence
                int frame;              // current frame in the sequence
                int index;              // location in the image set
                int wavecrest;          // for wave sprites
                SDL_Rect *frames;       // all frames (sequences must be in
                                        // order)
                struct images *images;  // image set
                enum SpriteStyle style; // wave, etc.
                SDL_Surface *surf;      // current source of images
                int facing;             // current facing sequence
                int facings;            // bitmap of supported facing sequences
                int n_sequences;        // number of animation sequences
                int sequence;           // current animation sequence
		int faded:1;	// render sprite sem-transparent
        };

        extern struct sprite *spriteCreate(char *tag, int n_frames, int index, 
                                           enum SpriteStyle style, 
                                           struct images *images,
                                           int n_facings, int facings);
        extern void spriteDestroy(struct sprite *sprite);
        extern void spriteInit(void);
        extern void spriteAdd(struct sprite *sprite);
        extern struct sprite *spriteLookup(char *tag);
        extern void spritePaint(struct sprite *sprite, int frame, int x, int y);
        extern void spriteStartAnimation(struct list *wq, int start_tick);

        extern void spriteAdvanceFrames(void);
        extern int spriteSetFacing(struct sprite *sprite, int direction);
	extern struct sprite *sprite_load(class Loader * loader,
                                          struct images *images);
        extern int sprite_fade(struct sprite *sprite);
        extern void sprite_unfade(struct sprite *sprite);

#ifdef __cplusplus
}
#endif

#endif
