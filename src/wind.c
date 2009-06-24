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
#include "wind.h"
#include "wq.h"
#include "common.h"
#include "screen.h"
#include "session.h"
#include "log.h"

static SDL_Rect windRect;
static int windDirection;
static int windDuration;

void windAdvanceTurns(void)
{
	if (windDuration > 0) {
		windDuration--;
		return;
	}

	if (rand() % 100 < WIND_CHANGE_PROBABILITY) {
            int dir = rand() % NUM_WIND_DIRECTIONS;
            windSetDirection(dir, 10);
	}
}

int windInit(void)
{
	windRect.w = WIND_W;
	windRect.x = WIND_X;
	windRect.y = WIND_Y;
	windRect.h = WIND_H;

	windDirection = NORTH;
        windDuration = 0;

        return 0;
}

void windSetDirection(int dir, int dur)
{
    if (HERE == dir || dir >= NUM_WIND_DIRECTIONS) {
        return;
    }

    if (dir != windDirection) {
        log_msg("The wind shifts to the %s", directionToString(dir));
    }

    windDirection = dir;
    windDuration = dur;
    windRepaint();
}

int windGetDirection(void)
{
	return windDirection;
}

void windRepaint(void)
{
	screenErase(&windRect);
	screenPrint(&windRect, SP_CENTERED, "Wind:%s",
		    directionToString(windDirection));
	screenUpdate(&windRect);
}

void windSave(struct save *save)
{
        save->write(save, "(kern-set-wind %d %d)\n", windDirection, 
                    windDuration);
}
