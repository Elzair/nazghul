/* Copyright (c) 2002 Gordon McNutt */
#include "wind.h"
#include "wq.h"
#include "common.h"
#include "screen.h"

static SDL_Rect windRect;
static int windDirection;
static int windDuration;

void windAdvanceTurns(void)
{
	if (windDuration) {
		windDuration--;
		return;
	}

	if (random() % 100 < WIND_CHANGE_PROBABILITY) {
		windSetDirection(random() % NUM_WIND_DIRECTIONS, 10);
		screenUpdate(&windRect);
	}
}

void windInit(void)
{
	windRect.w = WIND_W;
	windRect.x = WIND_X;
	windRect.y = WIND_Y;
	windRect.h = WIND_H;

	windDirection = NORTH;
}

void windSetDirection(int dir, int dur)
{
	// fixme -- move the similar array in cmd.c to common.h and use that
	windDirection = dir;
	windDuration = dur;
	screenErase(&windRect);
	screenPrint(&windRect, SP_CENTERED, "Wind:%s",
		    directionToString(windDirection));
	screenUpdate(&windRect);
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
