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


// SAM: Using this typedef below, and in play.c
typedef void (*v_funcpointer_ii) (int x, int y);

  struct KeyHandler {
    struct list list;
    bool(*fx) (struct KeyHandler * handler, int key);
    void *data;
    // SAM: I hope this is not a butchery...
    v_funcpointer_ii each_point_func;
    v_funcpointer_ii each_target_func;
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
