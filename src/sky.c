/* Copyright (c) 2002 Gordon McNutt */
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

	Clock.day = 0;
	Clock.hour = arc / DEGREES_PER_HOUR;
	Clock.min = (arc % DEGREES_PER_HOUR) * 60 / DEGREES_PER_HOUR;

	Clock.baseTurn = Clock.min * TURNS_PER_MINUTE +
	    Clock.hour * TURNS_PER_HOUR + Clock.day * TURNS_PER_DAY;
}

void clockUpdate(void)
{
	// Update the time of day clock to match the Turn counter.
	int turn = Turn + Clock.baseTurn;

	Clock.day = turn / TURNS_PER_DAY;
	Clock.hour = (turn % TURNS_PER_DAY) / TURNS_PER_HOUR;
	Clock.min = ((turn % TURNS_PER_DAY) % TURNS_PER_HOUR) /
	    TURNS_PER_MINUTE;

	mapRepaintClock();
}

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
	}

	if (Sun.arc > SUNRISE_DEGREE && Sun.arc < SUNSET_DEGREE) {
		Sun.light = MAX_SUNLIGHT;
	}

	skyRepaint();
}
