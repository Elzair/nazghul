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

#define EVENT_NONBLOCK   (1 << 0)
#define EVENT_NOPLAYBACK (1 << 1)

#define getHandler(stack,type) (list_empty((stack)) ? NULL : (type*)(stack)->next)
#define pushHandler(stack,handler) (list_add((stack), &(handler)->list))
#define popHandler(stack) if (!list_empty((stack))) list_remove((stack)->next)

// These are in nazghul.c
extern char *RecordFile;
extern char *PlaybackFile;
extern int PlaybackSpeed;

static struct list KeyHandlers;
static struct list TickHandlers;
static struct list QuitHandlers;
static struct list MouseHandlers;

static bool record_events;
static int record_fd;
static bool playback_events;
static int playback_fd;

static void (*eventHook) (void);
static int (*wait_event) (SDL_Event * event, int flags);

static int mapKey(SDL_keysym * keysym)
{
	static int map_arrows[] = {
		KEY_NORTH, KEY_SOUTH, KEY_EAST, KEY_WEST,
		KEY_SHIFT_NORTH, KEY_SHIFT_SOUTH, KEY_SHIFT_EAST, KEY_SHIFT_WEST
	};

	int key = keysym->sym;

        printf("sym='%c'[%d] mod=%02x\n", keysym->sym,  keysym->sym,
               keysym->mod);

	if (keysym->mod & KMOD_SHIFT) {

		switch (key) {
		case '`':
			return '~';
		case '1':
			return '!';
		case '2':
			return '@';
		case '3':
			return '#';
		case '4':
			return '$';
		case '5':
			return '%';
		case '6':
			return '^';
		case '7':
			return '&';
		case '8':
			return '*';
		case '9':
			return '(';
		case '0':
			return ')';
		case '-':
			return '_';
		case '=':
			return '+';
		case '[':
			return '{';
		case ']':
			return '}';
		case '\\':
			return '|';
		case ';':
			return ':';
		case '\'':
			return '\"';
		case ',':
			return '<';
		case '.':
			return '>';
		case '/':
			return '?';
		default:
			break;
		}

		if ((keysym->sym >= 'a') && (keysym->sym <= 'z'))
			return toupper(keysym->sym);
	}

	if (key >= SDLK_UP && key <= SDLK_LEFT)
		return map_arrows[key - SDLK_UP +
				  ((keysym->mod & KMOD_SHIFT) ? 4 : 0)];

	if (key == '\r')
		return '\n';

	return key;
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

		if (!wait_event(&event, flags))
                        return;

		if (record_events)
			record_event(&event);

		switch (event.type) {

		case SDL_USEREVENT:
			{
				struct TickHandler *tickh;
				tickh = getHandler(&TickHandlers, struct TickHandler);
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
                                int mapped_key;

				keyh = getHandler(&KeyHandlers, struct KeyHandler);
                                mapped_key = mapKey(&event.key.keysym);
				if (keyh) {
                                        use_hook = true;
                                        if (keyh->fx(keyh, mapped_key, event.key.keysym.mod)) {
                                                done = true;
                                        }
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
	eventHook = NULL;
	wait_event = event_get_next_event;

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

	return 0;
}

void eventHandle(void)
{
        event_handle_aux(0);
}

void eventHandlePending(void)
{
        event_handle_aux(EVENT_NONBLOCK|EVENT_NOPLAYBACK);
}

int G_turnaround_start = 0;
int G_turnaround_stop  = 0;

void eventPushKeyHandler(struct KeyHandler *keyh)
{
        if (list_empty(&KeyHandlers))
                G_turnaround_stop = SDL_GetTicks();
	pushHandler(&KeyHandlers, keyh);
}

void eventPopKeyHandler(void)
{
	popHandler(&KeyHandlers);
        if (list_empty(&KeyHandlers))
                G_turnaround_start = SDL_GetTicks();
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
