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
#include <math.h>

struct moon_info MoonInfo;
struct moon Moons[NUM_MOONS];
struct sun Sun;
struct clock Clock;

static struct sky {
	SDL_Rect screenRect;
} Sky;

// Amount by which we horizontally shift the light function to make noon
// produce maximum light
#define SKY_HORZ_SHIFT (NOON_DEGREE - 90)

// Amount by which we vertically shift the light function to make sunrise and
// sunset occur at the specified times. Since this involces taking the sin
// of a value we must do this at runtime.
static double SKY_VERT_SHIFT = 0.0;

// Amount by which we multiple the sine factor of the light function to make it
// ramp up faster. Again, must do this at runtime.
static double SKY_AMPLITUDE = 0.0;

// Conversion factors for deciding where in the sky window an astral body is
//
// W = width of sky window
// t = width of astral body sprite
// R = sunrise
// S = sunset
// P(x) = position in window of arc x
//
//                                     0
// +---+-------------------------------+---+
// | t |<------------ W -------------->| t |
// +---+-------------------------------+---+
// S                                   R
//
// P(R) = 0
// P(S) = W + t
// P(x) = mx + b
//
// And solving we get:
// m = (W + t)/(S - R)
// b = -R * m
//
static double SKY_WIN_SLOPE = ((double)SKY_W + (double)SKY_SPRITE_W) / 
                              ((double)SUNSET_DEGREE - (double)SUNRISE_DEGREE); // 1.06667
static double SKY_WIN_OFFSET = -(double)SUNRISE_DEGREE * 
                                (double)SKY_WIN_SLOPE; // -64.0002

#define SKY_ARC_TO_PIXEL_OFFSET(arc) (int)(SKY_WIN_SLOPE * (double)(arc) + (SKY_WIN_OFFSET))

#define DEGREES_TO_RADIANS(deg) (double)((deg) * 0.0174603)

static int sky_get_light_from_astral_body(int arc, int max_light)
{
        //  light
        // M |        ...................
        //   |       .                   .
        //   |      .                     .
        // 0 |--------|--------|--------|--------| arc
        //   0       90       180      270       0
        //
        // Start ramping up at sunrise, ramp down and hit zero at sunset. I
        // think a simple way to simulate this is to take a horizontal slice
        // out of a sine wave.
        //
        // Here's sin(arc):
        //
        //  |   . .
        //  | .     .
        //  |._______._______.
        //  |         .     .
        //  |           . .
        //  r        s
        //
        // Point 'r' is sunrise, 's' is sunset. In our system the day and night
        // are not equal, so to simulate this we modify the basic sine function
        // like this:
        //
        // Here's sin(arc) + C
        //
        //  |   . .
        //  | .     .
        //  |.       .       .
        //  |_________._____._
        //  |           . .
        //  r         s     r
        //
        // But in our system sunrise is not at zero degrees, so we need to
        // shift the function:
        //
        // sin(arc - theta) + C:
        //
        //  |      . .
        //  |    .     .
        //  |   .       .         
        //  |__._________.____.
        //  |.             . .
        //     r         s    r
        //
        // Since we don't want our function to produce a value greater than
        // 1.0, we cap it, producing a flat top which represents max light.  If
        // we stop here then it takes about four hours for the sun to go from
        // zero to max output. I'd prefer it if this were more like an hour. So
        // for this we need to multiply the result of the sin function to
        // increase the amplitude.
        //
        // A * (sin(arc - theta) + C)
        //
        //  |            
        //  |   .........
        //  |  .         .        
        //  |__._________.____.
        //  |..           ....
        //     r         s    r
        // 
        // 

        int degrees = (arc - SKY_HORZ_SHIFT);
        double radians = DEGREES_TO_RADIANS(degrees);
        double factor = SKY_AMPLITUDE * (sin(radians) + SKY_VERT_SHIFT);
        factor = clamp(factor, 0.0, 1.0);
        int light = (int)(factor * max_light);
        light = (light < 0) ? 0 : light;
        return light;
}

static void sky_compute_factors(void)
{
        // Called once at load time. The light function is derived above as:
        //
        // light = A * (sin(x - theta) + C)
        //
        // Here we solve for C by using the sunrise time 'R', where the
        // function passes through 0. Since the amplitude does not
        //
        // A * (sin(R - theta) + C) = 0
        // C = -sin(R - theta)
        // 
        SKY_VERT_SHIFT = -sin(DEGREES_TO_RADIANS(SUNRISE_DEGREE - 
                                                 SKY_HORZ_SHIFT));

        // Now solve for A, which is the amplitude, using the first value after
        // sunrise for which the function produces 1.0. This value should be
        // one hour after sunrise.
        //
        // A  * (sin(V - theta) + C) = 1
        // A = 1 / (sin(V - theta) + C)

        double inverse = sin(DEGREES_TO_RADIANS(SUNRISE_DEGREE + 
                                                DEGREES_PER_HOUR -  
                                                SKY_HORZ_SHIFT) +
                             SKY_VERT_SHIFT);
        assert(inverse != 0.0);
        SKY_AMPLITUDE = 1 / inverse;
}


static void myAdvanceSun(void)
{
	/* Advance the sun */
	while (Turn > Sun.next_arc_turn) {

		/* Move across the sky */
		Sun.arc = (Sun.arc + 1) % 360;
		Sun.next_arc_turn += SUN_TURNS_PER_DEGREE;

                int new_light = sky_get_light_from_astral_body(Sun.arc, 
                                                               MAX_SUNLIGHT);
                if (new_light != Sun.light) {
                        Sun.light = new_light;
                        player_party->recompute_los();
                }
	}
}

int sun_is_up(void)
{
	return (Sun.arc >= SUNRISE_DEGREE) && (Sun.arc < SUNSET_DEGREE);
}

int sun_is_down(void)
{
	return (Sun.arc >= SUNSET_DEGREE) || (Sun.arc < SUNRISE_DEGREE);
}

int is_noon(void)
{
	static int noon_arc_begin = NOON_DEGREE - (DEGREES_PER_HOUR / 2);
	static int noon_arc_end = NOON_DEGREE + (DEGREES_PER_HOUR / 2);
	return ((Sun.arc >= noon_arc_begin) && (Sun.arc <= noon_arc_end));
}				// is_noon()

int is_midnight(void)
{
	static int mid_arc_begin =
	    MIDNIGHT_DEGREE_LATE - (DEGREES_PER_HOUR / 2);
	static int mid_arc_end = MIDNIGHT_DEGREE_EARLY + (DEGREES_PER_HOUR / 2);
	return ((Sun.arc >= mid_arc_begin) || (Sun.arc <= mid_arc_end));
}				// is_midnight()

static void moon_adjust_light(struct moon *moon)
{
        int max_light;
        int light_per_phase;

        // Light waxes from the new moon, reaches a peak in the middle phase at
        // the full moon, and wanes until the new moon again.
        light_per_phase = MAX_MOONLIGHT/MoonInfo.phases * 2;

        // Max light depends on phase
        if (moon->phase <= MoonInfo.phases/2) {
                max_light = light_per_phase * moon->phase;
        } else {
                max_light = light_per_phase * (MoonInfo.phases - moon->phase);
        }

        // Light depends on arc
        moon->light = sky_get_light_from_astral_body(moon->arc, max_light);

}

static void myAdvanceMoons(void)
{
	int i;
	struct moon *moon;
        int original_light;

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

                // Adjust light coming from moon.
                original_light = moon->light;
                moon_adjust_light(moon);                

                // Update LOS if lighting changed
                if (original_light != moon->light) {
                        player_party->recompute_los();
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
	// current day, we will also load week, month, year.
	Clock.year = 0;
	Clock.month = 0;
	Clock.week = 0;
	Clock.day_w = 0;	// Day of week (0..6)
	Clock.day = 0;		// Day of month (0..27)
	Clock.hour = (arc / DEGREES_PER_HOUR);
	Clock.min = (arc % DEGREES_PER_HOUR) * 60 / DEGREES_PER_HOUR;

	Clock.baseTurn =
	    (Clock.min * TURNS_PER_MINUTE) +
	    (Clock.hour * TURNS_PER_HOUR) +
	    (Clock.day_w * TURNS_PER_DAY) +
	    (Clock.week * TURNS_PER_WEEK) +
	    (Clock.month * TURNS_PER_MONTH) + (Clock.year * TURNS_PER_YEAR);
}

void clockUpdate(void)
{
	// Update the time of day clock to match the Turn counter.
	int turn = Turn + Clock.baseTurn;

	Clock.year = turn / TURNS_PER_YEAR;
	Clock.month = (turn % TURNS_PER_YEAR) / TURNS_PER_MONTH;
	Clock.week = (turn % TURNS_PER_MONTH) / TURNS_PER_WEEK;
	Clock.day_w = (turn % TURNS_PER_WEEK) / TURNS_PER_DAY;
	Clock.day = (turn % TURNS_PER_MONTH) / TURNS_PER_DAY;
	Clock.hour = (turn % TURNS_PER_DAY) / TURNS_PER_HOUR;
	Clock.min = (turn % TURNS_PER_HOUR) / TURNS_PER_MINUTE;

	mapRepaintClock();
}

char *time_HHMM_as_string(void)
{
	static char str[] = "HH:MMPM";
	static int maxlen = strlen("HH:MMPM") + 1;
	int hr = Clock.hour;
	int min = Clock.min;
	int n;

	hr = (hr > 12) ? (hr - 12) : hr;
	hr = (hr == 0) ? 12 : hr;

	n = snprintf(str, maxlen, "%2d:%02d%2s",
		     hr, min, (Clock.hour >= 12) ? "PM" : "AM");
	assert(n != -1);
	return str;
}				// time_HHMM_as_string()

char *time_YYYY_MM_DD_as_string(void)
{
	static char str[] = "YYYY/MM/DD";
	static int maxlen = strlen("YYYY/MM/DD") + 1;
	int n = snprintf(str, maxlen, "%04d/%02d/%02d",
			 Clock.year, Clock.month, Clock.day);
	assert(n != -1);
	return str;
}				// time_YYYY_MM_DD_as_string()

#ifdef OTHER_TIME_STRING_FUNCTIONS
char *time_YYYY_as_string(void)
{
	static char str[] = "YYYY";
	static int maxlen = strlen("YYYY") + 1;
	int n = snprintf(str, maxlen, "%4d", Clock.year);
	assert(n != -1);
	return str;
}

char *time_MM_as_string(void)
{
	static char str[] = "MM";
	static int maxlen = strlen("MM") + 1;
	int n = snprintf(str, maxlen, "%2d", Clock.month);
	assert(n != -1);
	return str;
}

char *time_DD_as_string(void)
{
	static char str[] = "DD";
	static int maxlen = strlen("DD") + 1;
	int n = snprintf(str, maxlen, "%2d", Clock.day);
	assert(n != -1);
	return str;
}
#endif				// OTHER_TIME_STRING_FUNCTIONS

// SAM: 
// A proper implementation of 
// month_name(), week_name(), day_name()
// will wait until we have GhulScript
// for week and month names and such.
// 
char *month_name(void)
{
	int month = Clock.month;
	switch (month) {
	case 0:
		return "1st Month";
	case 1:
		return "2nd Month";
	case 2:
		return "3rd Month";
	case 3:
		return "4th Month";
	case 4:
		return "5th Month";
	case 5:
		return "6th Month";
	case 6:
		return "7th Month";
	case 7:
		return "8th Month";
	case 8:
		return "9th Month";
	case 9:
		return "10th Month";
	case 10:
		return "11th Month";
	case 11:
		return "12th Month";
	default:
		assert(0);
	}
}				// month_name()

char *week_name(void)
{
	int week = Clock.week;
	switch (week) {
	case 0:
		return "1st Week";
	case 1:
		return "2nd Week";
	case 2:
		return "3rd Week";
	case 3:
		return "4th Week";
	default:
		assert(0);
	}
}				// week_name()

char *day_name(void)
{
	int day = Clock.day;
	switch (day) {
	case 0:
		return "1st Day";
	case 1:
		return "2nd Day";
	case 2:
		return "3rd Day";
	case 3:
		return "4th Day";
	case 4:
		return "5th Day";
	case 5:
		return "6th Day";
	case 6:
		return "7th Day";
	default:
		assert(0);
	}
}				// day_name()

static void moonPaintSunOrMoon(int arc, struct sprite *sprite)
{
	int x;
	int pixels;

        pixels = SKY_ARC_TO_PIXEL_OFFSET(arc);
        if (pixels < 0 || pixels > SKY_W + SKY_SPRITE_W)
                return;
	x = Sky.screenRect.x + Sky.screenRect.w - pixels;

	spritePaint(sprite, 0, x, Sky.screenRect.y);
}

int moon_is_visible(int arc)
{
	// SAM:
	// I tested somewhat before, during, after sunset,
	// and found that the sprite for the moon "left of the sun"
	// became not visible (out of the drawing area?) before
	// the AT command (which calls moon_is_visible() )
	// reported that a moon had set.
	// 
	// Why the difference in apparent "set" time?
	if (arc < SUNRISE_DEGREE || arc > (SUNSET_DEGREE + SKY_SPRITE_W))
		return 0;
	return 1;
}				// moon_is_visible()

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

        sky_compute_factors();

	Moons[0].openMoongate = moongateOpenSourceGate;
	Moons[0].closeMoongate = moongateCloseSourceGate;

	Moons[1].openMoongate = moongateOpenDestinationGate;
	Moons[1].closeMoongate = moongateCloseDestinationGate;

	for (i = 0; i < NUM_MOONS; i++) {
		struct moon *moon;

		moon = &Moons[i];
		moon->turns_per_phase = moon->days_per_cycle * TURNS_PER_DAY /
		    MoonInfo.phases;
		moon->next_phase_turn = Turn + moon->turns_per_phase;
		moon->next_arc_turn = Turn + MOON_TURNS_PER_DEGREE;
		moon->openMoongate(moon->phase);
                moon_adjust_light(moon);
	}


        Sun.light = sky_get_light_from_astral_body(Sun.arc, MAX_SUNLIGHT);

	skyRepaint();
}

int sky_get_ambient_light(void)
{
        int i;
        int light = Sun.light;

	for (i = 0; i < NUM_MOONS; i++) {
		light += Moons[i].light;
        }
        
        return clamp(light, 0, MAX_AMBIENT_LIGHT);
}
