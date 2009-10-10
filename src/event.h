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

#include "macros.h"

BEGIN_DECL

#include "list.h"
#include <SDL.h>

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

#define KEY_SHIFT (1 << 9) /* 512, SDLK_LAST is 323 */

#define KEY_SHIFT_NORTH (KEY_NORTH|KEY_SHIFT)
#define KEY_SHIFT_SOUTH (KEY_SOUTH|KEY_SHIFT)
#define KEY_SHIFT_EAST  (KEY_EAST|KEY_SHIFT)
#define KEY_SHIFT_WEST  (KEY_WEST|KEY_SHIFT)

#define KEY_SHIFT_NORTHEAST (KEY_NORTHEAST|KEY_SHIFT)
#define KEY_SHIFT_NORTHWEST (KEY_NORTHWEST|KEY_SHIFT)
#define KEY_SHIFT_SOUTHEAST (KEY_SOUTHEAST|KEY_SHIFT)
#define KEY_SHIFT_SOUTHWEST (KEY_SOUTHWEST|KEY_SHIFT)

/* 
 * Give canonical names to the UNICODE versions of the CTRL keypresses 
 * that we care about.
 * CTRL-A is 0x01 and CTRL-Z is 0x1A, others are in-between of course.
 */
#define KEY_CTRL_D      0x04
#define KEY_CTRL_E      0x05
#define KEY_CTRL_O      0x0F
#define KEY_CTRL_Q      0x11
#define KEY_CTRL_R      0x12
#define KEY_CTRL_S      0x13
#define KEY_CTRL_T      0x14
#define KEY_CTRL_Z      0x1A

typedef int (*key_handler_fx_t)(struct KeyHandler * handler, int key, int keymod);

struct TickHandler {
        struct list list;
        bool(*fx) (struct TickHandler * handler);
        void *data;
};


struct KeyHandler {
        struct list list;
        key_handler_fx_t fx;
        void *data;  
        // The data field should always be filled with a struct, 
        // rather than a scalar such as bool or int,
        // to facilitate expansions to two or more subfields
        // For example, the 'struct cursor_movement_keyhandler' above.
};

struct QuitHandler {
        struct list list;
        bool(*fx) (struct QuitHandler * handler);
        void *data;
};

struct MouseMotionHandler {
        struct list list;
        bool(*fx) (struct MouseMotionHandler * handler, SDL_MouseMotionEvent *event);
        void *data;
};

struct MouseButtonHandler {
        struct list list;
        bool(*fx) (struct MouseButtonHandler * handler, SDL_MouseButtonEvent *event);
        void *data;
};

extern int eventInit(void);
extern void eventExit(void);
extern void eventHandle(void);
extern void eventHandlePending(void); /* non-blocking version of eventHandle */
extern void eventPushKeyHandler(struct KeyHandler *keyh);
extern struct KeyHandler * eventPopKeyHandler(void);
extern void eventPushTickHandler(struct TickHandler *keyh);
extern void eventPopTickHandler(void);
extern void eventPushQuitHandler(struct QuitHandler *keyh);
extern void eventPopQuitHandler(void);
extern void eventPushMouseButtonHandler(struct MouseButtonHandler *keyh);
extern void eventPopMouseButtonHandler(void);
extern void eventPushMouseMotionHandler(struct MouseMotionHandler *keyh);
extern void eventPopMouseMotionHandler(void);
extern void eventAddHook(void (*fx) (void));
extern void eventRunKeyHandler(key_handler_fx_t fx, void *data);

END_DECL

#endif
