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
#include "common.h"
#include "sky.h"
#include "screen.h"
#include "player.h"
#include "foogod.h"
#include "place.h"
#include "event.h"
#include "console.h"

#include <stdlib.h>
#include <time.h>

struct list OrdnanceTypes;
int Turn;
int Tick;
int AnimationTicks;
int TickMilliseconds;
// int WindDirection;
bool Reveal;
int Quicken;
int TimeStop;
bool TurnChanged;
int MagicNegated;
int ShowAllTerrain = 0;

struct los *LosEngine;

static int CYCLES_PER_MSEC;

static void busywaitInit(void)
{
	int start, diff = 0;
	unsigned long count = 10000;
	volatile unsigned long cycles;

	while (diff < 2) {
		start = SDL_GetTicks();
		for (cycles = 0; cycles < count; cycles++) ;
		diff = SDL_GetTicks() - start;
		if (diff)
			CYCLES_PER_MSEC = count / diff;
		else
			count *= 10;
	}
	// printf("CYCLES_PER_MSEC=%d\n", CYCLES_PER_MSEC);
}				// busywaitInit()

void busywait(int msec)
{
	volatile int cycles;
	cycles = CYCLES_PER_MSEC * msec;
	while (cycles--) ;
}				// busywait()

void commonInit(void)
{
	Turn = 0;
	Tick = 0;
	Reveal = false;
	Quicken = 0;
	TimeStop = 0;
	TurnChanged = false;
	MagicNegated = 0;
	// ShowAllTerrain = 0; set by cmdline option
	// Quit = 0;
	srandom(0);		// fixme: should save/load as part of
	// record/playback
	busywaitInit();
}				// commonInit()

// fixme -- obsolete, use the next one
static char *dir_str[] = {
	"Northwest", "North", "Northeast",
	"West", "Here", "East",
	"Southwest", "South", "Southeast" "Up", "Down",
};

static unsigned char dir_facing[] = {
	WEST, NORTH, EAST,
	WEST, NORTH, EAST,
	WEST, SOUTH, EAST,
};

static int direction_to_rotation_tbl[] = {
	315, 0, 45,
	270, 0, 90,
	225, 180, 135,
};

static int directionToDxTable[] = {
	-1, 0, +1,
	-1, 0, +1,
	-1, 0, +1,
	0, 0,
};

static int directionToDyTable[] = {
	-1, -1, -1,
	0, 0, 0,
	+1, +1, +1,
	0, 0,
};

static int keyToDirectionTable[] = {
	SOUTHWEST, SOUTH, SOUTHEAST,
	WEST, HERE, EAST,
	NORTHWEST, NORTH, NORTHEAST
};

char *directionToString(int dir)
{
	return dir_str[dir];
}

int stringToDirection(char *str)
{
	unsigned int i;
	for (i = 0; i < array_sz(dir_str); i++) {
		if (!strncasecmp(dir_str[i], str, strlen(dir_str[i])))
			return i;
	}
	return -1;
}

int keyToDirection(int key)
{
	return keyToDirectionTable[key - KEY_SOUTHWEST];
}

int directionToDx(int dir)
{
	return directionToDxTable[dir];
}

int directionToDy(int dir)
{
	return directionToDyTable[dir];
}

char *get_dir_str(int dx, int dy)
{
	clamp(dx, -1, 1);
	clamp(dy, -1, 1);
	return dir_str[(dy + 1) * 3 + dx + 1];
}

int vector_to_facing(int dx, int dy)
{
	clamp(dx, -1, 1);
	clamp(dy, -1, 1);
	return dir_facing[(dy + 1) * 3 + dx + 1];
}

int vector_to_rotation(int dx, int dy)
{
	clamp(dx, -1, 1);
	clamp(dy, -1, 1);
	return direction_to_rotation_tbl[(dy + 1) * 3 + dx + 1];
}

#if 0

void windSetDirection(int dir)
{
	static char *dirstr[] = { "North", "East", "South", "West" };
	WindDirection = dir;
	screenErase(&screenWindWindow);
	screenPrint(&screenWindWindow, SP_CENTERED, "Wind:%s", dirstr[dir]);
	screenUpdate(&screenWindWindow);
}

void windAdvance(void)
{
	if (random() % 100 < WIND_CHANGE_PROBABILITY) {
		windSetDirection(random() % NUM_PLANAR_DIRECTIONS);
		screenUpdate(&screenWindWindow);
	}
}
#endif				// 0

void turnAdvance(int turns)
{
	TurnChanged = false;
	if (TimeStop) {
		TimeStop--;
		return;
	}
	if (Quicken) {
		Quicken++;
		if (Quicken % 2)
			return;
	}

	Turn += turns;
	if (turns)
		TurnChanged = true;
}

int vector_to_dir(int dx, int dy)
{
	int adx = abs(dx);
	int ady = abs(dy);

	// Note: north is in the negative x direction, south in the positive

	if (ady > adx) {
		if (dy > 0)
			return SOUTH;
		else
			return NORTH;
	} else {
		if (dx > 0)
			return EAST;
		else
			return WEST;
	}
}

bool isvowel(char c)
{
	return (c == 'a' || c == 'A' ||
		c == 'e' || c == 'E' ||
		c == 'i' || c == 'I' ||
		c == 'o' || c == 'O' ||
		c == 'u' || c == 'U' || c == 'y' || c == 'Y');
}

// eof
