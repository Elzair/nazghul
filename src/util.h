/* Copyright (c) 2002 Gordon McNutt */
#ifndef util_h
#define util_h

#ifdef __cplusplus
extern "C" {
#endif

#include "common.h"

	extern char *mmap_file(char *file, int *len);

	static inline int util_point_in_rect(int px, int py, int rx, int ry,
					     int rw, int rh) {
		return (px >= rx && px <= (rx + rw) && py >= ry
			&& py <= (ry + rh));
	}
	extern char SDL_keysym_to_ascii(SDL_keysym * keysym);

#ifdef __cplusplus
}
#endif

#endif
