/* Copyright (c) 2002 Gordon McNutt */
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
