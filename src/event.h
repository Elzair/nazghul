/* Copyright (c) 2002 Gordon McNutt */
#ifndef event_h
#define event_h

#ifdef __cplusplus
extern "C" {
#endif

#include "list.h"

#include <SDL.h>

#define BUTTON_LEFT     0
#define BUTTON_MIDDLE   1
#define BUTTON_RIGHT    2

/* Map directions to the numeric keypad */
#define KEY_NORTHWEST   SDLK_KP7
#define KEY_NORTH       SDLK_KP8
#define KEY_NORTHEAST   SDLK_KP9
#define KEY_WEST        SDLK_KP4
#define KEY_HERE        SDLK_KP5
#define KEY_EAST        SDLK_KP6
#define KEY_SOUTHWEST   SDLK_KP1
#define KEY_SOUTH       SDLK_KP2
#define KEY_SOUTHEAST   SDLK_KP3
#define KEY_UP          KEY_NORTH
#define KEY_DOWN        KEY_SOUTH
#define KEY_RIGHT       KEY_EAST
#define KEY_LEFT        KEY_WEST

#define KEY_SHIFT_NORTH (SDLK_LAST + 1)
#define KEY_SHIFT_SOUTH (SDLK_LAST + 2)
#define KEY_SHIFT_EAST  (SDLK_LAST + 3)
#define KEY_SHIFT_WEST  (SDLK_LAST + 4)

	struct TickHandler {
		struct list list;
		 bool(*fx) (struct TickHandler * handler);
		void *data;
	};

	struct KeyHandler {
		struct list list;
		 bool(*fx) (struct KeyHandler * handler, int key);
		void *data;
	};

	struct QuitHandler {
		struct list list;
		 bool(*fx) (struct QuitHandler * handler);
		void *data;
	};

	struct MouseHandler {
		struct list list;
		 bool(*fx) (struct MouseHandler * handler, int button, int x,
			    int y);
		void *data;
	};

	extern int eventInit(void);
	extern void eventHandle(void);
	extern void eventPushKeyHandler(struct KeyHandler *keyh);
	extern void eventPopKeyHandler(void);
	extern void eventPushTickHandler(struct TickHandler *keyh);
	extern void eventPopTickHandler(void);
	extern void eventPushQuitHandler(struct QuitHandler *keyh);
	extern void eventPopQuitHandler(void);
	extern void eventPushMouseHandler(struct MouseHandler *keyh);
	extern void eventPopMouseHandler(void);
	extern void eventAddHook(void (*fx) (void));

#ifdef __cplusplus
}
#endif
#endif
