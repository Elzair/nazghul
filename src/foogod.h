/* Copyright (c) 2002 Gordon McNutt */
#ifndef foogod_h
#define foogod_h

#ifdef __cplusplus
extern "C" {
#endif

/* Food-Gold-Date window */

	extern void foogodInit(void);
	extern void foogodRepaint(void);
	extern void foogodAdvanceTurns();
	extern void foogod_set_y(int y);
	extern int foogod_get_y(void);

#ifdef __cplusplus
}
#endif
#endif
