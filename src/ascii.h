/* Copyright (c) 2002 Gordon McNutt */
#ifndef ascii_h
#define ascii_h

#include <SDL/SDL.h>

#ifdef __cplusplus
extern "C" {
#endif

	struct images;

	extern void asciiSetImages(struct images *images, int offset);
	extern void asciiPaint(char c, int x, int y, SDL_Surface * surface);
	extern void asciiInvert(void);
	extern void asciiUninvert(void);

#ifdef __cplusplus
}
#endif
#endif
