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

#include <SDL.h>

#define SPRITE_DEF_FACING -1

struct images;
struct sprite;

#define SPRITE_STYLE_NORMAL 0
#define SPRITE_STYLE_WAVE   1
#define SPRITE_STYLE_ROTATE 2

struct sprite {
        struct list list;       // for storage while loading
        char *tag;
        int n_frames;           // per sequence
        int index;              // location in the image set
        SDL_Rect *frames;       // all frames (sequences must be in order)
        struct images *images;  // image set
        int style;              // wave, etc.
        SDL_Surface *surf;      // current source of images
        int facing;             // current facing sequence
        int facings;            // bitmap of supported facing sequences
        int sequence;           // current animation sequence
        int faded:1;	        // render sprite sem-transparent
        int wave : 1;
};

extern int spriteInit(void);
extern void spriteAdd(struct sprite *sprite);
extern struct sprite *spriteLookup(char *tag);
extern void spritePaint(struct sprite *sprite, int frame, int x, int y);

extern void spriteAdvanceFrames(void);
extern int spriteSetFacing(struct sprite *sprite, int direction);
//extern struct sprite *sprite_load(class Loader * loader, struct images *images);
extern int sprite_fade(struct sprite *sprite);
extern void sprite_unfade(struct sprite *sprite);
extern void spriteZoomOut(int factor);
extern void spriteZoomIn(int factor);
extern void spriteAdvanceTicks(int ticks);

END_DECL

// The new sprite interface created with the new loader code.
extern struct sprite * sprite_new(char *tag, int frames, int index, int wave, 
                                  int facings, struct images *image);
extern void sprite_del(struct sprite *sprite);

#endif
