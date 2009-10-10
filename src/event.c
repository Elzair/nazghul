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
#include "event.h"
#include "cfg.h"

#include <SDL.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <common.h>

#ifndef DEBUG_KEYS
# define DEBUG_KEYS 0
#endif

#define EVENT_NONBLOCK   (1 << 0)
#define EVENT_NOPLAYBACK (1 << 1)

#define getHandler(stack,type) \
  (list_empty((stack)) ? NULL : (type*)(stack)->next)
#define pushHandler(stack,handler) (list_add((stack), &(handler)->list))

typedef struct {
        struct list list;
        SDL_Event event;
} sdl_event_list_t;

static struct list KeyHandlers;
static struct list TickHandlers;
static struct list QuitHandlers;
static struct list MouseButtonHandlers;
static struct list MouseMotionHandlers;
static struct list backlog;

static bool record_events;
static int record_fd;
static bool playback_events;
static int playback_fd;
static int event_playback_speed = 0;

static void (*eventHook) (void);
static int (*wait_event) (SDL_Event * event, int flags);
static int qcount = 0;

static struct list *popHandler(struct list *stack) 
{
        if (list_empty(stack)) {
                return NULL;
        }

        struct list *lptr = stack->next;
        list_remove(lptr);
        return lptr;
}

static void backlog_enqueue(SDL_Event *event)
{
        sdl_event_list_t *elem = (sdl_event_list_t*)malloc(sizeof(*elem));
        elem->event = *event;
        list_add(&backlog, &elem->list);
        qcount++;
}

static int backlog_dequeue(SDL_Event *event)
{
        sdl_event_list_t *elem;
        struct list *ptr;
        if (list_empty(&backlog))
                return -1;
        ptr = backlog.next;
        elem = outcast(ptr, sdl_event_list_t, list);
        list_remove(&elem->list);
        *event = elem->event;
        free(elem);
        qcount--;
        return 0;
}

static int mapKey(SDL_keysym * keysym)
{
	int key = keysym->sym;
        
        if (DEBUG_KEYS) {
                printf("sym='%c'[%d] mod=%02x unicode=%04x\n", 
                       keysym->sym,  
                       keysym->sym,
                       keysym->mod,
                       keysym->unicode);
        }

        /* If the key has a UNICODE representation and its from the default
         * Basic Latin code page then return it as an ASCII character. */
        /* fixme: unicode is messing up ctrl+key sequences */
        if (keysym->unicode) {

                /* Map CR to LF (legacy code expects this) */
                if (keysym->unicode == 0x000d)
                        return '\n';

                /* Map all other Basic Latin codes to ASCII */
                if (keysym->unicode < 0x7f)
                        return keysym->unicode & 0x7f;

                /* Code page not supported... fall through */
        }

        /* Map arrow keys to equivalent numberpad entries */
        if (key >= SDLK_UP && key <= SDLK_LEFT) {
                static int map[] = { KEY_NORTH, KEY_SOUTH, KEY_EAST, 
                                     KEY_WEST };
                key = map[key - SDLK_UP];
        }

        /* Set the "shift" bit */
        if (keysym->mod & KMOD_SHIFT) {
                key |= KEY_SHIFT;
        }

        /* Unsupported? fallback to the SDL sym */
	return key;
}

static int event_get_next_event(SDL_Event *event, int flags)
{
        /* if a key handler exists */
        if (getHandler(&KeyHandlers, struct KeyHandler)) {

                /* if the backlog queue is not empty */
                if (! backlog_dequeue(event)) {

                        /* get the event from the backlog queue */
                        return 1;
                }
        }

        if (flags & EVENT_NONBLOCK)
                return SDL_PollEvent(event);
        else
                return SDL_WaitEvent(event);
}

static int playback_event(SDL_Event * event, int flags)
{
	// For now use the expedient but non-portable technique of reading the
	// binary data straight to the file.
	int n;
	int len = sizeof(SDL_Event);
	char *ptr = (char *) event;

        if (flags & EVENT_NOPLAYBACK)
                return 0;


	while (len) {
		n = read(playback_fd, ptr, len);
		if (n == -1) {
			perror("read");
			return -1;
		}
		ptr += n;
		len -= n;
	}

	SDL_Delay(event_playback_speed);

	return 1;
}

static void record_event(SDL_Event * event)
{
	// For now use the expedient but non-portable technique of writing the
	// binary data straight to the file.
	int n;
	int len = sizeof(SDL_Event);
	char *ptr = (char *) event;
	while (len) {
		n = write(record_fd, ptr, len);
		if (n == -1) {
			perror("write");
			return;
		}
		ptr += n;
		len -= n;
	}
}

static void event_handle_aux(int flags)
{
	bool done = false;
        bool use_hook = false;

	while (!done) {

		SDL_Event event;
                if (!wait_event(&event, flags)) {
                        return;
                }
                if (record_events)
                        record_event(&event);

		switch (event.type) {

		case SDL_USEREVENT:
			{
				struct TickHandler *tickh;
				tickh = getHandler(&TickHandlers,
                                                   struct TickHandler);
				if (tickh) {
                                        use_hook = true;
                                        if (tickh->fx(tickh)) {
                                                done = true;
                                        }
                                }
                                
			}
			break;

		case SDL_KEYDOWN:
			{
				struct KeyHandler *keyh;
				keyh = getHandler(&KeyHandlers, 
                                                  struct KeyHandler);
				if (keyh) {
                                        int mapped_key = 
                                                mapKey(&event.key.keysym);
                                        use_hook = true;
                                        if (keyh->fx(keyh, mapped_key, 
                                                     event.key.keysym.mod)) {
                                                done = true;
                                        }
                                } else {
                                        /* enqueue this event */
                                        backlog_enqueue(&event);
                                }
			}
			break;

		case SDL_QUIT:
			{
				struct QuitHandler *quith;
				quith =
				    getHandler(&QuitHandlers,
					       struct QuitHandler);
				if (quith && quith->fx(quith))
					done = true;
			}
			break;

		case SDL_MOUSEBUTTONDOWN:
			{
				struct MouseButtonHandler *mouseh;
				mouseh = getHandler(&MouseButtonHandlers,
						    struct MouseButtonHandler);
				if (mouseh &&
				    mouseh->fx(mouseh, &event.button)) {
					done = true;
                                }
			}
			break;

		case SDL_MOUSEMOTION:
			{
				struct MouseMotionHandler *mouseh;
				mouseh = getHandler(&MouseMotionHandlers,
						    struct MouseMotionHandler);
				if (mouseh &&
				    mouseh->fx(mouseh, &event.motion)) {
					done = true;
                                }
			}
			break;

		default:
			break;
		}

		if (use_hook && eventHook)
			eventHook();

	}

}

int eventInit(void)
{
        char *record_fname = cfg_get("record-filename");
        char *playback_fname = cfg_get("playback-filename");

	list_init(&KeyHandlers);
	list_init(&TickHandlers);
	list_init(&QuitHandlers);
	list_init(&MouseButtonHandlers);
	list_init(&MouseMotionHandlers);
        list_init(&backlog);
	eventHook = NULL;
	wait_event = event_get_next_event;
        qcount=0;

	if (record_fname != NULL) {
		record_events = true;
		record_fd = open(record_fname, O_WRONLY | O_CREAT, 00666);
		if (record_fd == -1) {
			perror(record_fname);
			return -1;
		}
	}

	if (playback_fname != NULL) {
                char *playback_spd_str = cfg_get("playback-speed");
		playback_events = true;
		playback_fd = open(playback_fname, O_RDONLY, 00666);
		if (playback_fd == -1) {
			perror(playback_fname);
			return -1;
		}
		// Override the normal wait_event routine
		wait_event = playback_event;

                /* Set the play back speed. */
                if (playback_spd_str) {
                        event_playback_speed = atoi(playback_spd_str);
                }
	}

        SDL_EnableUNICODE(1);

	return 0;
}

void eventExit(void)
{
        /* cleanup the backlog queue */
        SDL_Event event;
        while (! backlog_dequeue(&event))
                ;
}

void eventHandle(void)
{
        event_handle_aux(0);
}

void eventHandlePending(void)
{
        event_handle_aux(EVENT_NONBLOCK|EVENT_NOPLAYBACK);
}

void eventPushKeyHandler(struct KeyHandler *keyh)
{
	pushHandler(&KeyHandlers, keyh);
}

struct KeyHandler * eventPopKeyHandler(void)
{
        return (struct KeyHandler*)popHandler(&KeyHandlers);
}

void eventPushTickHandler(struct TickHandler *keyh)
{
	pushHandler(&TickHandlers, keyh);
}

void eventPopTickHandler(void)
{
	popHandler(&TickHandlers);
}

void eventPushQuitHandler(struct QuitHandler *keyh)
{
	pushHandler(&QuitHandlers, keyh);
}

void eventPopQuitHandler(void)
{
	popHandler(&QuitHandlers);
}

void eventPushMouseButtonHandler(struct MouseButtonHandler *keyh)
{
	pushHandler(&MouseButtonHandlers, keyh);
}

void eventPopMouseButtonHandler(void)
{
	popHandler(&MouseButtonHandlers);
}

void eventPushMouseMotionHandler(struct MouseMotionHandler *keyh)
{
	pushHandler(&MouseMotionHandlers, keyh);
}

void eventPopMouseMotionHandler(void)
{
	popHandler(&MouseMotionHandlers);
}

void eventAddHook(void (*fx) (void))
{
	eventHook = fx;
}

void eventRunKeyHandler(key_handler_fx_t fx, void *data)
{
	struct KeyHandler kh;
	kh.fx = fx;
	kh.data = data;
	eventPushKeyHandler(&kh);
	eventHandle();
	eventPopKeyHandler();
}
