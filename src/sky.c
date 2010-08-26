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
#include "player.h"
#include "wq.h"
#include "clock.h"
#include "session.h"
#include "gob.h"

#include <assert.h>
#include <math.h>

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
static double SKY_WIN_SLOPE;
static double SKY_WIN_OFFSET;

#define SKY_ARC_TO_PIXEL_OFFSET(arc) (int)(SKY_WIN_SLOPE * (double)(arc) + \
                                           (SKY_WIN_OFFSET))

#define DEGREES_TO_RADIANS(deg) (double)((deg) * 0.0174603)

#define ASTRAL_BODY_ARC_WIDTH   ((double)SKY_SPRITE_W/(double)SKY_WIN_SLOPE)
#define ECLIPSE_FACTOR 0.75

//////////////////////////////////////////////////////////////////////////////
//
// Internal helper functions
//
//////////////////////////////////////////////////////////////////////////////

static int sky_get_light_from_astral_body(int arc, int maxlight)
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
        clamp(factor, 0.0, 1.0);
        int light = (int)(factor * maxlight);
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

static void sky_paint_astral_body(struct sky *sky, int arc, 
                                  struct sprite *sprite)
{
	int x;
	int pixels;

        pixels = SKY_ARC_TO_PIXEL_OFFSET(arc);
        if (pixels < 0 || pixels > SKY_W + SKY_SPRITE_W)
                return;
	x = sky->screenRect.x + sky->screenRect.w - pixels;

	sprite_paint(sprite, 0, x, sky->screenRect.y);
}

int astral_body_is_visible(int arc)
{
	// SAM:
	// I tested somewhat before, during, after sunset,
	// and found that the sprite for the moon "left of the sun"
	// became not visible (out of the drawing area?) before
	// the AT command (which calls sky_astral_body_is_visible() )
	// reported that a moon had set.
	// 
	// Why the difference in apparent "set" time?
	if (arc < SUNRISE_DEGREE || arc > (SUNSET_DEGREE + SKY_SPRITE_W))
		return 0;
	return 1;
}

//////////////////////////////////////////////////////////////////////////////
//
// Astral body api
//
//////////////////////////////////////////////////////////////////////////////

struct astral_body *astral_body_new(char *tag, char *name, int n_phases)
{
        struct astral_body *body;

        assert(n_phases);
        assert(name);

        body = (struct astral_body*)calloc(1, sizeof(*body));        
        assert(body);

        list_init(&body->list);
        body->eclipse = 0;
        body->n_phases = n_phases;
        body->phases = (struct phase*)calloc(body->n_phases, 
                                             sizeof(struct phase));
        assert(body->phases);

        body->tag = strdup(tag);
        assert(body->tag);

        body->name = strdup(name);
        assert(body->name);

        return body;
}

void astral_body_del(struct astral_body *body)
{
        int i;

        assert(body);
        assert(body->tag);
        assert(body->name);
        assert(body->phases);

        closure_unref_safe(body->gifc);
        if (body->gob)
                gob_del(body->gob);
        free(body->tag);
        free(body->name);
        for (i = 0; i < body->n_phases; i++)
                if (body->phases[i].name)
                        free(body->phases[i].name);
        free(body->phases);
        free(body);
}

void astral_body_save(struct astral_body *body, struct save *save)
{
        int i;

        if (body->gob)
                save->enter(save, "(bind-astral-body\n");

        save->enter(save, "(kern-mk-astral-body\n");        
        save->write(save, "'%s\t; tag\n", body->tag);
        save->write(save, "\"%s\"\t; name\n", body->name);
        save->write(save, "%d\t; distance\n", body->distance);
        save->write(save, "%d\t; minutes_per_phase\n", 
                    body->minutes_per_phase);
        save->write(save, "%d\t; minutes_per_degress\n", 
                    body->minutes_per_degree);
        save->write(save, "%d\t; initial_arc\n", body->initial_arc);
        save->write(save, "%d\t; initial_phase\n", body->initial_phase);
        //save->write(save, "%d\t; n_phases\n", body->n_phases);
        if (body->gifc)
                closure_save(body->gifc, save);
        else
                save->write(save, "nil\t; gifc\n");
        assert(body->n_phases);
        save->enter(save, "(list\n");
        for (i = 0; i < body->n_phases; i++) {
                save->write(save, "(list %s %d \"%s\")\n", 
                            sprite_get_tag(body->phases[i].sprite),
                            body->phases[i].maxlight,
                            body->phases[i].name);
        }
        save->exit(save, ")\n");
        save->exit(save, ")\n");

        if (body->gob) {
                gob_save(body->gob, save);
                save->exit(save, ") ;; bind-astral-body\n");
        }
}

static void astral_body_advance_phase(struct astral_body *body)
{
        int new_phase;

        assert(body->n_phases);

        // Calculate the new phase
        new_phase = 0;
        if (body->minutes_per_phase) {
                new_phase = clock_time() / body->minutes_per_phase;
        }
        new_phase += body->initial_phase;
        new_phase %= body->n_phases;
        
        if (new_phase == body->phase)
                return;
                
        // Run the phase-change handler in the script
        if (body->gifc)
                closure_exec(body->gifc, "ypdd", "phase-change", 
                             body, body->phase, new_phase);
                
        body->phase = new_phase;
}

static void astral_body_advance_arc(struct astral_body *body)
{
        int new_arc;
        int original_light;

        // Calculate the new arc
        new_arc = (clock_time() / body->minutes_per_degree);
        new_arc += body->initial_arc;
        new_arc %= 360;

        if (new_arc == body->arc)
                return;

        body->arc = new_arc;
                
        if (body->n_phases > 0) {
                astral_body_advance_phase(body);
        }

        // Change the body's light
        original_light = body->light;
        body->light = sky_get_light_from_astral_body(
                body->arc, 
                body->phases[body->phase].maxlight);
        if (original_light != body->light)
                mapSetDirty();
}

typedef struct {
        struct list list;
        int x, w, ref;
} range_t;

static void range_init(range_t *range)
{
        list_init(&range->list);
        range->x=0;
        range->w=0;
        range->ref=0;
}

static range_t *range_new()
{
        range_t *range = (range_t*)malloc(sizeof(*range));
        range_init(range);
        range->ref++;
        return range;
}

static void range_unref(range_t *range)
{
        assert(range->ref);
        range->ref--;
        if (!range->ref)
                free(range);
}

static void range_set_value(range_t *range, int x, int w)
{
        range->x = x;
        range->w = w;
}

static range_t * range_intersect(range_t *r1, range_t *r2)
{
        // Note: this assumes positive values all around, no modulo
        range_t *out = NULL;
        int edge, overlap;

        if (r1->x >= r2->x) {
                edge = r1->x - r1->w;
                if (edge > r2->x) {
                        // no overlap:
                        //   r1---->
                        // --|-----|-----|-----|
                        //              r2---->
                        return NULL;
                }
                // r1 overlaps onto r2:
                //   r1---->
                // --|--|##|-----|
                //     r2------->
                out = range_new();
                overlap = r2->x - edge;
                range_set_value(out, r2->x, overlap);
                return out;
        }

        edge = r2->x - r2->w;
        if (edge > r1->x) {
                // no overlap:
                //                r1---->
                // -----|-----|----|----|
                //     r2---->
                //
                return NULL;
        }

        // r2 overlaps onto r1:
        //   r2---->
        // --|--|##|-----|
        //     r1------->
        out = range_new();
        overlap = r1->x - edge;
        range_set_value(out, r1->x, overlap);
        return out;
}

static range_t *range_merge(range_t *r1, range_t *r2)
{
        // Note: this assumes positive values all around, no modulo
        range_t *out = NULL;
        int edge, overlap;

        if (r1->x >= r2->x) {
                edge = r1->x - r1->w;
                if (edge > r2->x) {
                        // no overlap:
                        //   r1---->
                        // --|-----|-----|-----|
                        //              r2---->
                        return NULL;
                }
                // r1 overlaps onto r2:
                //   r1---->
                // --|##|##|#####|
                //     r2------->
                out = range_new();
                overlap = r2->x - edge;
                range_set_value(out, r1->x, r1->w + r2->w - overlap);
                return out;
        }

        edge = r2->x - r2->w;
        if (edge > r1->x) {
                // no overlap:
                //                r1---->
                // -----|-----|----|----|
                //     r2---->
                //
                return NULL;
        }

        // r2 overlaps onto r1:
        //   r2---->
        // --|##|##|#####|
        //     r1------->
        out = range_new();
        overlap = r1->x - edge;
        range_set_value(out, r2->x, r1->w + r2->w - overlap);
        return out;
}

static void range_set_union(struct list *set, range_t *r1)
{
        struct list *elem;

        if (!r1) {
                //dbg("none\n");
                return;
        }

        //dbg("[%d %d]\n", r1->x-360, r1->w);

        elem = set->next;

        while (elem != set) {
                range_t *ru;
                range_t *r2 = outcast(elem, range_t, list);
                elem = elem->next;

                ru = range_merge(r1, r2);
                if (ru) {
/*                         dbg("   merged: [%d %d] + [%d %d] = [%d %d]\n", */
/*                             r1->x-360, r1->w,   */
/*                            r2->x-360, r2->w,  */
/*                             ru->x-360, ru->w); */
                        range_unref(r1);
                        list_remove(&r2->list);
                        range_unref(r2);
                        r1 = ru;
                }
        }
        
        list_add(set, &r1->list);

}

static int range_set_sum(struct list *set)
{
        struct list *elem;
        int sum = 0;
        list_for_each(set, elem) {
                range_t *rr=outcast(elem, range_t, list);
                sum+=rr->w;
        }
        return sum;
}

static void range_set_unref_elements(struct list *set)
{
        struct list *elem;

        elem = set->next;
        while (elem != set) {
                range_t *range = outcast(elem, range_t, list);
                elem = elem->next;
                range_unref(range);
        }
}

static void sky_get_eclipse(struct sky *sky, 
                            struct astral_body *outer)
{
        struct astral_body *inner = NULL;
        struct list *elem = outer->list.next;
        struct list ranges;
        range_t r1;
        int eclipse_arc;

        //dbg(" sky_get_eclipse %s (%d)\n", outer->name, outer->arc);

        outer->eclipse = 1.0;
        list_init(&ranges);
        range_set_value(&r1, outer->arc+360, (int)ASTRAL_BODY_ARC_WIDTH);

        // Check each body listed after this one to see if it eclipses it.
        while (elem != &sky->bodies) {

                range_t r2;

                inner = outcast(elem, struct astral_body,  list);
                elem = elem->next;
                
                //dbg("  check %s (%d)...", inner->name, inner->arc);

                range_set_value(&r2, inner->arc+360,
                                (int)ASTRAL_BODY_ARC_WIDTH);
                range_set_union(&ranges, range_intersect(&r1, &r2));
        }

        eclipse_arc = range_set_sum(&ranges);
        outer->eclipse = eclipse_arc/ASTRAL_BODY_ARC_WIDTH * ECLIPSE_FACTOR;
        range_set_unref_elements(&ranges);
}

//////////////////////////////////////////////////////////////////////////////
//
// Public sky api
//
//////////////////////////////////////////////////////////////////////////////

void sky_advance(struct sky *sky, int visible)
{
        struct list *elem;
        struct astral_body *body;

	screenErase(&sky->screenRect);

        //dbg("sky_advance\n");
        list_for_each(&sky->bodies, elem) {
                body = outcast(elem, struct astral_body, list);
                astral_body_advance_arc(body);
                if (! visible)
                        continue;
                sky_paint_astral_body(sky, body->arc, 
                                      body->phases[body->phase].sprite);

                // Assume the bodies are listed in order from outermost to
                // innermost; outer bodies will be eclipsed by inner ones
                sky_get_eclipse(sky, body);
        }

	screenUpdate(&sky->screenRect);
}

void sky_init(struct sky *sky)
{
	sky->screenRect.w = SKY_W;
	sky->screenRect.x = SKY_X;
	sky->screenRect.y = SKY_Y;
	sky->screenRect.h = SKY_H;

        SKY_WIN_SLOPE = ((double)SKY_W + (double)SKY_SPRITE_W) / 
                ((double)SUNSET_DEGREE - (double)SUNRISE_DEGREE);
        SKY_WIN_OFFSET = -(double)SUNRISE_DEGREE * (double)SKY_WIN_SLOPE;

        sky_compute_factors();
        list_init(&sky->bodies);
}

void sky_start_session(struct sky *sky, int visible)
{
        sky_advance(sky, visible);
}

void sky_end_session(struct sky *sky)
{
        struct list *elem;
        struct astral_body *body;

        elem = sky->bodies.next;
        while (elem != &sky->bodies) {
                body = outcast(elem, struct astral_body, list);
                elem = elem->next;
                astral_body_del(body);
        }        
}

void sky_add_astral_body(struct sky *sky, struct astral_body *body)
{
        // Need to keep them in order by astronomical distance, furthest to
        // closest, so the sprites are rendered in the right order.
        struct list *elem;
        struct astral_body *other;

        elem = sky->bodies.next;
        while (elem != &sky->bodies) {
                other = outcast(elem, struct astral_body, list);
                // Find the first body closer than this one and insert this one
                // before it.
                if (other->distance < body->distance)
                        break;
                elem = elem->next;
        }

        list_add(elem->prev, &body->list);
}

int sky_get_ambient_light(struct sky *sky)
{
        struct list *elem;
        struct astral_body *body;
        int light = 0;

        list_for_each(&sky->bodies, elem) {
                body = outcast(elem, struct astral_body, list);
                light += (int)((float)body->light * (1.0 - body->eclipse));
        }

        return clamp(light, 0, MAX_AMBIENT_LIGHT);
}

void sky_save(struct sky *sky, struct save *save)
{
        struct list *elem;
        struct astral_body *body;

        save->write(save, ";; ---------\n");
        save->write(save, ";; Astronomy\n");
        save->write(save, ";; ---------\n");

        list_for_each(&sky->bodies, elem) {
                body = outcast(elem, struct astral_body, list);
                astral_body_save(body, save);
        }
}
