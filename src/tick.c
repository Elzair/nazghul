/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2002, 2003 Gordon McNutt
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 *
 * Gordon McNutt
 * gmcnutt@users.sourceforge.net
 */

#include "tick.h"

#include <assert.h>
#include <SDL.h>
#include <SDL_thread.h>

#include "common.h" /* TICK_EVENT */

static int tick_paused = 0;
static int tick_killed = 0;
static SDL_Thread *tick_thread = NULL;

static int tick_main(void *data)
{
	long msecs;
	SDL_Event tick_event;

        msecs = (long)data;
	tick_event.type = SDL_USEREVENT;
	tick_event.user.code = TICK_EVENT;

        while (! tick_killed) {
                SDL_Delay(msecs);
                if (! tick_paused)
                        SDL_PushEvent(&tick_event);
	}

        return 0;
}

void tick_start(long msecs)
{
        assert(! tick_thread);
        tick_paused = 0;
        tick_killed = 0;
        if (msecs > 0)
                tick_thread  = SDL_CreateThread(tick_main, (void*)msecs);
}
void tick_kill(void)
{
        tick_killed = 1;
        if (tick_thread)
                SDL_WaitThread(tick_thread, NULL);
}

void tick_pause(void)
{
        tick_paused = 1;
}

void tick_run(void)
{
        tick_paused = 0;
}
