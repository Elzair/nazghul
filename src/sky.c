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
#include "Mech.h"
#include "game.h"
#include "clock.h"

#include <assert.h>
#include <math.h>

struct moon_info MoonInfo;
struct moon Moons[NUM_MOONS];
struct sun Sun;

static struct sky {
	SDL_Rect screenRect;
} Sky;

// This is a wart, and should be either in the script or made unnecessary via
// some more general mechanism.
#define MOON_PHASE_FULL 0

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

// 1.06667
static double SKY_WIN_SLOPE = ((double)SKY_W + (double)SKY_SPRITE_W) / 
                              ((double)SUNSET_DEGREE - (double)SUNRISE_DEGREE);

 // -64.0002
static double SKY_WIN_OFFSET = -(double)SUNRISE_DEGREE * (double)SKY_WIN_SLOPE;

#define SKY_ARC_TO_PIXEL_OFFSET(arc) (int)(SKY_WIN_SLOPE * (double)(arc) + \
                                           (SKY_WIN_OFFSET))

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

static void sky_send_signal_to_obj(class Object *obj, void *data)
{
        class Mech *mech;
        int sig;

        if (! obj->isType(MECH_ID))
                return;

        sig = (int)data;
        mech = (class Mech*)obj;

        mech->activate(sig);
}

static bool sky_send_signal_to_place(struct place *place, void *data)
{
        place_for_each_object(place, sky_send_signal_to_obj, data);
        return false;
}

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

static void sky_advance_sun(void)
{
        int new_arc;
        int new_light;


        /* Change the sun's arc */
        new_arc = clock_time_of_day() / MINUTES_PER_DEGREE;
        if (new_arc != Sun.arc) {
                Sun.arc = new_arc;

                /* Change the sun's light */
                new_light = sky_get_light_from_astral_body(Sun.arc, MAX_SUNLIGHT);
                if (new_light != Sun.light) {
                        Sun.light = new_light;
                        player_party->updateView();
                }
        }
}

static void sky_advance_moons(void)
{
	int i;
	struct moon *moon;
        int original_light;
        int new_arc;
        int new_phase;

	/* Advance the moons */
	for (i = 0; i < NUM_MOONS; i++) {

		moon = &Moons[i];

                /* Change the moon's arc */
                new_arc = (clock_time() / MOON_MINUTES_PER_DEGREE);
                new_arc += moon->initial_arc;
                new_arc %= 360;
                if (new_arc != moon->arc) {
                        moon->arc = new_arc;
                        
                        /* Change the moon's phase */
                        new_phase = clock_time() / moon->minutes_per_phase;
                        new_phase += moon->initial_phase;
                        new_phase %= MoonInfo.phases;

                        if (new_phase != moon->phase) {
                                moon->closeMoongate(moon->phase);
                                moon->phase = new_phase;
                                moon->openMoongate(moon->phase);

                                /* Send the full-moon signal */
                                if (moon->phase == MOON_PHASE_FULL) {
                                        game_for_each_place(sky_send_signal_to_place, (void*)MECH_FULL_MOON);
                                }

                                /* Change the moon's light */
                                original_light = moon->light;
                                moon_adjust_light(moon);                
                                if (original_light != moon->light) {
                                        player_party->updateView();
                                }
                        }
                }
        }
}

void sky_advance(void)
{
	sky_advance_sun();
	sky_advance_moons();
	skyRepaint();
}

void skyRepaint(void)
{
	int i;

	/* Erase the moon window */
	screenErase(&Sky.screenRect);

	if (NULL != Place && !Place->underground) {

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
        struct moon *moon;


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


		moon = &Moons[i];

                /* WARNING: bug waiting to happen here. The initial_phase and
                 * initial_arc should be saved separately from the current
                 * phase and arc in the save file, because the clock_time()
                 * call measures from the start of the current session, not the
                 * start of the first session.
                 */
                moon->initial_phase = moon->phase;
                moon->initial_arc = moon->arc;

                moon->minutes_per_phase = (MINUTES_PER_DAY * moon->days_per_cycle) / MoonInfo.phases;
		moon->openMoongate(moon->phase);
                moon_adjust_light(moon);
	}


        Sun.arc = clock_time_of_day() / MINUTES_PER_DEGREE;;
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

