//
// nazghul - an old-school RPG engine
// Copyright (C) 2002, 2003 Gordon McNutt
//
// Thi program is free software; you can redistribute it and/or modify it
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
#include "sky.h"
#include "screen.h"
#include "common.h"
#include "sprite.h"
#include "place.h"
#include "map.h"
#include "moongate.h"
#include "player.h"
#include "wq.h"

#include <assert.h>

struct moon_info MoonInfo;
struct moon Moons[NUM_MOONS];
struct sun Sun;
struct clock Clock;

static struct sky {
	SDL_Rect screenRect;
} Sky;

static void myAdvanceSun(void)
{
	/* Advance the sun */
	while (Turn > Sun.next_arc_turn) {

		/* Move across the sky */
		Sun.arc = (Sun.arc + 1) % 360;
		Sun.next_arc_turn += SUN_TURNS_PER_DEGREE;

		/* Sunset */
		if (Sun.arc > SUNSET_DEGREE) {
			if (Sun.light > 0) {
				Sun.light -= DELTA_SUNLIGHT;
				if (Sun.light < 0)
					Sun.light = 0;
				player_party->recompute_los();
			}
		}

		/* Sunrise */
		else if (Sun.arc > SUNRISE_DEGREE && Sun.light < MAX_SUNLIGHT) {
			Sun.light += DELTA_SUNLIGHT;
			if (Sun.light > MAX_SUNLIGHT)
				Sun.light = MAX_SUNLIGHT;
			player_party->recompute_los();
		}
	}
}

int sun_is_up (void)
{
  return (Sun.arc >= SUNRISE_DEGREE) && (Sun.arc < SUNSET_DEGREE);
}

int sun_is_down (void)
{
  return (Sun.arc >= SUNSET_DEGREE) || (Sun.arc < SUNRISE_DEGREE);
}

int is_noon (void)
{
  static int noon_arc_begin = NOON_DEGREE - (DEGREES_PER_HOUR / 2);
  static int noon_arc_end   = NOON_DEGREE + (DEGREES_PER_HOUR / 2);
  return ( (Sun.arc >= noon_arc_begin) && 
           (Sun.arc <= noon_arc_end)    );
} // is_noon()

int is_midnight (void)
{
  static int mid_arc_begin = 
    MIDNIGHT_DEGREE_LATE  - (DEGREES_PER_HOUR / 2);
  static int mid_arc_end   = 
    MIDNIGHT_DEGREE_EARLY + (DEGREES_PER_HOUR / 2);
  return ( (Sun.arc >= mid_arc_begin) ||
           (Sun.arc <= mid_arc_end)    );
} // is_midnight()

static void myAdvanceMoons(void)
{
	int i;
	struct moon *moon;

	/* Advance the moons */
	for (i = 0; i < NUM_MOONS; i++) {
		moon = &Moons[i];

		/* Change the moon arc */
		while (Turn > moon->next_arc_turn) {
			moon->arc = (moon->arc + 1) % 360;
			moon->next_arc_turn += MOON_TURNS_PER_DEGREE;

		}

		/* Change the moon phase */
		while (Turn > moon->next_phase_turn) {

			moon->closeMoongate(moon->phase);
			moon->phase = (moon->phase + 1) % MoonInfo.phases;
			moon->openMoongate(moon->phase);
			moon->next_phase_turn += moon->turns_per_phase;
		}
	}
}

void clockSet(void)
{
	// Set the initial time of day from the location of the sun. Only done
	// once at load time.

	int arc;

	arc = Sun.arc;

    // SAM: In future, when GhulScript exists to specify the 
    //      current day, we will also load week, month, year.
    Clock.year  = 0;
    Clock.month = 0;
    Clock.week  = 0;
    Clock.day_w = 0;  // Day of week  (0..6)
	Clock.day   = 0;  // Day of month (0..27)
	Clock.hour  = (arc / DEGREES_PER_HOUR);
	Clock.min   = (arc % DEGREES_PER_HOUR) * 60 / DEGREES_PER_HOUR;

	Clock.baseTurn = 
      (Clock.min   * TURNS_PER_MINUTE) +
      (Clock.hour  * TURNS_PER_HOUR)   + 
      (Clock.day_w * TURNS_PER_DAY)    +
      (Clock.week  * TURNS_PER_WEEK)   +
      (Clock.month * TURNS_PER_MONTH)  +
      (Clock.year  * TURNS_PER_YEAR);
}

void clockUpdate(void)
{
	// Update the time of day clock to match the Turn counter.
	int turn = Turn + Clock.baseTurn;

    Clock.year  =                     turn / TURNS_PER_YEAR;
    Clock.month = (turn % TURNS_PER_YEAR)  / TURNS_PER_MONTH;
    Clock.week  = (turn % TURNS_PER_MONTH) / TURNS_PER_WEEK;
	Clock.day_w = (turn % TURNS_PER_WEEK)  / TURNS_PER_DAY;
	Clock.day   = (turn % TURNS_PER_MONTH) / TURNS_PER_DAY;
	Clock.hour  = (turn % TURNS_PER_DAY)   / TURNS_PER_HOUR;
	Clock.min   = (turn % TURNS_PER_HOUR)  / TURNS_PER_MINUTE;

	mapRepaintClock();
}

char * time_HHMM_as_string (void)
{
  static char str[] = "HH:MMPM";
  static int maxlen = strlen("HH:MMPM") + 1;
  int hr  = Clock.hour;
  int min = Clock.min;
  int n;
  
  hr = (hr > 12) ? (hr - 12) : hr;
  hr = (hr == 0) ? 12        : hr;
  
  n = snprintf(str, maxlen, "%2d:%02d%2s", 
               hr, min, (Clock.hour >= 12) ? "PM" : "AM");
  assert(n != -1);
  return str;
} // time_HHMM_as_string()

char * time_YYYY_MM_DD_as_string (void)
{
  static char str[] = "YYYY/MM/DD";
  static int maxlen = strlen("YYYY/MM/DD") + 1;
  int n = snprintf(str, maxlen, "%04d/%02d/%02d", 
                   Clock.year, Clock.month, Clock.day);
  assert(n != -1);
  return str;
} // time_YYYY_MM_DD_as_string()

#ifdef OTHER_TIME_STRING_FUNCTIONS
char * time_YYYY_as_string (void)
{
  static char str[] = "YYYY";
  static int maxlen = strlen("YYYY") + 1;
  int n = snprintf(str, maxlen, "%4d", Clock.year);
  assert(n != -1);
  return str;
}

char * time_MM_as_string   (void)
{
  static char str[] = "MM";
  static int maxlen = strlen("MM") + 1;
  int n = snprintf(str, maxlen, "%2d", Clock.month);
  assert(n != -1);
  return str;
}

char * time_DD_as_string   (void)
{
  static char str[] = "DD";
  static int maxlen = strlen("DD") + 1;
  int n = snprintf(str, maxlen, "%2d", Clock.day);
  assert(n != -1);
  return str;
}
#endif // OTHER_TIME_STRING_FUNCTIONS

// SAM: 
// A proper implementation of 
// month_name(), week_name(), day_name()
// will wait until we have GhulScript
// for week and month names and such.
// 
char * month_name(void)
{
  int month = Clock.month;
  switch (month)
    {
    case 0:  return "1st Month";
    case 1:  return "2nd Month";
    case 2:  return "3rd Month";
    case 3:  return "4th Month";
    case 4:  return "5th Month";
    case 5:  return "6th Month";
    case 6:  return "7th Month";
    case 7:  return "8th Month";
    case 8:  return "9th Month";
    case 9:  return "10th Month";
    case 10: return "11th Month";
    case 11: return "12th Month";
    default: assert(0);
    }
} // month_name()

char * week_name(void)
{
  int week = Clock.week;
  switch (week)
    {
    case 0:  return "1st Week";
    case 1:  return "2nd Week";
    case 2:  return "3rd Week";
    case 3:  return "4th Week";
    default: assert(0);
    }
} // week_name()

char * day_name(void)
{
  int day = Clock.day;
  switch (day)
    {
    case 0:  return "1st Day";
    case 1:  return "2nd Day";
    case 2:  return "3rd Day";
    case 3:  return "4th Day";
    case 4:  return "5th Day";
    case 5:  return "6th Day";
    case 6:  return "7th Day";
    default: assert(0);
    }
} // day_name()

static void moonPaintSunOrMoon(int arc, struct sprite *sprite)
{
	int x;
	int pixels;

	/* Check if the moon is above the horizon */
	if (arc < SUNRISE_DEGREE || arc > (SUNSET_DEGREE + TILE_W))
		return;

	/* Calculate the moons position in the window */
	pixels = ((arc - SUNRISE_DEGREE) * MOON_WINDOW_PIXELS_PER_DEGREE);
	x = Sky.screenRect.x + Sky.screenRect.w - pixels;

	spritePaint(sprite, 0, x, Sky.screenRect.y);
}

int moon_is_visible (int arc)
{
  // SAM:
  // I tested somewhat before, during, after sunset,
  // and found that the sprite for the moon "left of the sun"
  // became not visible (out of the drawing area?) before
  // the AT command (which calls moon_is_visible() )
  // reported that a moon had set.
  // 
  // Why the difference in apparent "set" time?
  if (arc < SUNRISE_DEGREE || arc > (SUNSET_DEGREE + TILE_W))
    return 0;
  return 1;
} // moon_is_visible()

void skyAdvanceTurns(void)
{
	myAdvanceSun();
	myAdvanceMoons();
	skyRepaint();
}

void skyRepaint(void)
{
	int i;

	/* Erase the moon window */
	screenErase(&Sky.screenRect);

	if (!Place->underground) {

		/* Redraw the sun */
		moonPaintSunOrMoon(Sun.arc, Sun.sprite);

		/* Redraw moons */
		for (i = 0; i < NUM_MOONS; i++) {
			struct moon *moon = &Moons[i];
			moonPaintSunOrMoon(moon->arc,
					   MoonInfo.sprite[moon->phase]);
		}
	}

	screenUpdate(&Sky.screenRect);
}

void skyInit(void)
{
	int i;

	Sky.screenRect.w = SKY_W;
	Sky.screenRect.x = SKY_X;
	Sky.screenRect.y = SKY_Y;
	Sky.screenRect.h = SKY_H;

	Moons[0].openMoongate  = moongateOpenSourceGate;
	Moons[0].closeMoongate = moongateCloseSourceGate;

	Moons[1].openMoongate  = moongateOpenDestinationGate;
	Moons[1].closeMoongate = moongateCloseDestinationGate;

	for (i = 0; i < NUM_MOONS; i++) {
		struct moon *moon;

		moon = &Moons[i];
		moon->turns_per_phase = moon->days_per_cycle * TURNS_PER_DAY /
		    MoonInfo.phases;
		moon->next_phase_turn = Turn + moon->turns_per_phase;
		moon->next_arc_turn   = Turn + MOON_TURNS_PER_DEGREE;
		moon->openMoongate(moon->phase);
	}

	if (Sun.arc > SUNRISE_DEGREE && Sun.arc < SUNSET_DEGREE) {
		Sun.light = MAX_SUNLIGHT;
	}

	skyRepaint();
}
