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
#define popHandler(stack) if (!list_empty((stack))) list_remove((stack)->next)

typedef struct {
        struct list list;
        SDL_Event event;
} sdl_event_list_t;

// These are in nazghul.c
extern char *RecordFile;
extern char *PlaybackFile;
extern int PlaybackSpeed;

static struct list KeyHandlers;
static struct list TickHandlers;
static struct list QuitHandlers;
static struct list MouseHandlers;
static struct list backlog;

static bool record_events;
static int record_fd;
static bool playback_events;
static int playback_fd;

static void (*eventHook) (void);
static int (*wait_event) (SDL_Event * event, int flags);
static int qcount = 0;

static void backlog_enqueue(SDL_Event *event)
{
        sdl_event_list_t *elem = (sdl_event_list_t*)malloc(sizeof(*elem));
        elem->event = *event;
        list_add(&backlog, &elem->list);
        qcount++;
        printf("enqueue: %d\n", qcount);
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
        printf("dequeue: %d\n", qcount);
        return 0;
}

static int mapKey(SDL_keysym * keysym)
{
	static int map_arrows[] = {
		KEY_NORTH, KEY_SOUTH, KEY_EAST, KEY_WEST,
		KEY_SHIFT_NORTH, KEY_SHIFT_SOUTH, KEY_SHIFT_EAST, 
                KEY_SHIFT_WEST
	};

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
        if (keysym->unicode) {

                /* Map CR to LF (legacy code expects this) */
                if (keysym->unicode == 0x000d)
                        return '\n';

                /* Map all other Basic Latin codes to ASCII */
                if (keysym->unicode < 0x7f)
                        return keysym->unicode & 0x7f;

                /* Code page not supported... fall through */
        }

        /* Map the arrow keys */
	if (key >= SDLK_UP && key <= SDLK_LEFT)
		return map_arrows[key - SDLK_UP +
				  ((keysym->mod & KMOD_SHIFT) ? 4 : 0)];

        /* Unsupported? fallback to the SDL sym */
	return keysym->sym;
}

static int mapButton(Uint8 button)
{
	switch (button) {
	case SDL_BUTTON_RIGHT:
		return BUTTON_RIGHT;
	case SDL_BUTTON_MIDDLE:
		return BUTTON_MIDDLE;
	case SDL_BUTTON_LEFT:
	default:
		return BUTTON_LEFT;
	}
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

	SDL_Delay(PlaybackSpeed);

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
				struct MouseHandler *mouseh;
				mouseh = getHandler(&MouseHandlers,
						    struct MouseHandler);
				if (mouseh &&
				    mouseh->fx(mouseh,
					       mapButton(event.button.button),
					       event.button.x, event.button.y))
					done = true;
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
	list_init(&KeyHandlers);
	list_init(&TickHandlers);
	list_init(&QuitHandlers);
	list_init(&MouseHandlers);
        list_init(&backlog);
	eventHook = NULL;
	wait_event = event_get_next_event;
        qcount=0;

	if (RecordFile != NULL) {
		record_events = true;
		record_fd = open(RecordFile, O_WRONLY | O_CREAT, 00666);
		if (record_fd == -1) {
			perror(RecordFile);
			return -1;
		}
	}

	if (PlaybackFile != NULL) {
		playback_events = true;
		playback_fd = open(PlaybackFile, O_RDONLY, 00666);
		if (playback_fd == -1) {
			perror(PlaybackFile);
			return -1;
		}
		// Override the normal wait_event routine
		wait_event = playback_event;
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

void eventPopKeyHandler(void)
{
	popHandler(&KeyHandlers);
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

void eventPushMouseHandler(struct MouseHandler *keyh)
{
	pushHandler(&MouseHandlers, keyh);
}

void eventPopMouseHandler(void)
{
	popHandler(&MouseHandlers);
}

void eventAddHook(void (*fx) (void))
{
	eventHook = fx;
}
