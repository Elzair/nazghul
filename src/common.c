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

int Turn;
int Tick;
int AnimationTicks;
int TickMilliseconds;
// int WindDirection;
int ShowAllTerrain = 0;

struct los *LosEngine;


int commonInit(void)
{
	Turn = 0;
	Tick = 0;
	srand(0);
        return 0;
}

// fixme -- obsolete, use the next one
static const char *dir_str[] = {
	"Northwest", "North", "Northeast",
	"West", "Here", "East",
	"Southwest", "South", "Southeast", 
        "Up", "Down"
};

static unsigned char dir_facing[] = {
	WEST, NORTH, EAST,
	WEST, NORTH, EAST,
	WEST, SOUTH, EAST,
};

static unsigned char dir_8facing[] = {
	NORTHWEST, NORTH, NORTHEAST,
	WEST, NORTH, EAST,
	SOUTHWEST, SOUTH, SOUTHEAST,
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

const char *directionToString(int dir)
{
        if (dir < 0 || dir >= array_sz(dir_str))
                return "*** invalid direction ***";
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

int directionToOpposite(int dir)
{
        static int tbl[] = {
                SOUTHEAST,
                SOUTH,
                SOUTHWEST,
                EAST,
                HERE,
                WEST,
                NORTHEAST,
                NORTH,
                NORTHWEST,
                DOWN,
                UP
        };

        if (dir < 0 || dir >= array_sz(tbl))
                return DIRECTION_NONE;

        return tbl[dir];
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

const char *get_dir_str(int dx, int dy)
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

int vector_to_8facing(int dx, int dy)
{
	if (abs(dx) > 2* (abs(dy)))
	{
		dy = 0;	
	}
	else if (abs(dy) > 2* (abs(dx)))
	{
		dx = 0;	
	}
	clamp(dx, -1, 1);
	clamp(dy, -1, 1);
	return dir_8facing[(dy + 1) * 3 + dx + 1];
}

int vector_to_rotation(int dx, int dy)
{
	clamp(dx, -1, 1);
	clamp(dy, -1, 1);
	return direction_to_rotation_tbl[(dy + 1) * 3 + dx + 1];
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

bool point_in_rect(int x, int y, SDL_Rect *rect)
{
        return (x >= rect->x && 
                y >= rect->y &&
                x < (rect->x + rect->w) &&
                y < (rect->y + rect->h));
}

int logBase2(int val)
{
        int ret = 0;

        if (val<=0)
                return 0; /* incorrect but safe */

        val -= 1;

        while (val) {
                val>>=1;
                ret++;
        }

        return ret;
}


