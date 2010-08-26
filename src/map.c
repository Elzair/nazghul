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
#include "map.h"
#include "sky.h"  // For time/date functions
#include "screen.h"
#include "place.h"
#include "player.h"
#include "sprite.h"
#include "cursor.h"
#include "terrain.h"
#include "Missile.h"
#include "object.h"
#include "vmask.h"
#include "session.h"
#include "sprite.h"
#include "nazghul.h"  // for DeveloperMode

#include <SDL.h>
#include <math.h>

#define PROFILE_REPAINT 0
#define PROFILE_ANIMATE 0

#define LMAP_W     (VMASK_W)
#define LMAP_H     (VMASK_H)

#define MVIEW_SZ   (sizeof(struct mview))
#define LMAP_SZ    (LMAP_W * LMAP_H)
#define MAX_LIGHTS LMAP_SZ
#define PEER_ZOOM  2

#define LIT        255
#define UNLIT      0

#define mview_x(mview)        ((mview)->vrect.x)
#define mview_y(mview)        ((mview)->vrect.y)
#define mview_w(mview)        ((mview)->vrect.w)
#define mview_h(mview)        ((mview)->vrect.h)
#define mview_center_x(mview) (mview_x(mview) + mview_w(mview) / 2)
#define mview_center_y(mview) (mview_y(mview) + mview_h(mview) / 2)

/**
 *  Convert map coords to screen coords 
 */
#define MX_TO_SX(x) \
    (Map.srect.x+((x)-(Map.aview->vrect.x+Map.aview->subrect.x))*TILE_W)
#define MY_TO_SY(y) \
    (Map.srect.y+((y)-(Map.aview->vrect.y+Map.aview->subrect.y))*TILE_H)

struct light_source {
	int x, y, light;
};

struct mview {
	struct list list;	/* used internally by map lib */
	SDL_Rect vrect;		/* map coords of vrect */
        SDL_Rect subrect;       /* offset into visible subrect of vrect */
	//char *vmask;		/* visibility mask */
	int rad;		/* light radius */
	int zoom;               /* zoom level */
	int dirty:1;		/* needs repaint */
        int blackout:1;         /* erase only on repaint */
};

static struct map {
	SDL_Rect srect;		/* screen coords of viewer */
	SDL_Rect latencyRect;	/* screen coords of latency time */
	SDL_Rect turnaroundRect; /* screen coords of turnaroud time */
	SDL_Rect locRect;	/* screen coords of locater */
	SDL_Rect clkRect;	/* screen coords of clock */
	struct place *place;	/* subject being viewed */
	struct mview *aview;	/* active view */
	struct list views;	/* list of all views */
	struct mview *cam_view;
        class Object *subject;
	int cam_x, cam_y, cam_max_x, cam_max_y, cam_min_x, cam_min_y;
	bool peering;
	char *vmask;	/* final mask used to render */
        SDL_Surface *tile_scratch_surf;
        Uint32 last_repaint;
        class Object *selected; /* selected object -- don't shade the tile it's
                                 * on */

        /* FIXME: why is this dynamically allocated when we're using
         * MAX_LIGHTS? */
        struct light_source *lights;
        unsigned char *lmap;
        unsigned char *tmp_lmap;
        char is_image_mode : 1;
} Map;

/**
 * The callback function prototype for rendering a tile.
 */
typedef void (*map_tile_render_t)(struct place *place, int map_x, int map_y,
                                  int scr_x, int scr_y, int in_los);


static void myRmView(struct mview *view, void *data)
{
	list_remove(&view->list);
}

static void mapMergeRects(SDL_Rect *src_rect, unsigned char *src,
                          SDL_Rect *dst_rect, unsigned char *dst)
{
	int r_src, r_src_start, c_src, c_src_start, i_src, r_end, c_end;
	int r_dst, r_dst_start, c_dst, c_dst_start, i_dst;
	int tmp;

	// skip identical merges (yes, it happens)
	if (src == dst)
		return;

	if (src_rect->x < dst_rect->x) {
		// Source leftmost
		tmp = src_rect->x + src_rect->w - dst_rect->x;
		if (tmp < 0)
			return;
		c_src_start = dst_rect->x - src_rect->x;
		c_end = c_src_start + tmp;
		c_dst_start = 0;
	} else {
		// Destination leftmost
		tmp = dst_rect->x + dst_rect->w - src_rect->x;
		if (tmp < 0)
			return;
		c_src_start = 0;
		c_end = tmp;
		c_dst_start = src_rect->x - dst_rect->x;
	}

	if (src_rect->y < dst_rect->y) {
		// Source topmost
		tmp = src_rect->y + src_rect->h - dst_rect->y;
		if (tmp < 0)
			return;
		r_src_start = dst_rect->y - src_rect->y;
		r_end = r_src_start + tmp;
		r_dst_start = 0;
	} else {
		// Destination topmost
		tmp = dst_rect->y + dst_rect->h - src_rect->y;
		if (tmp < 0)
			return;
		r_src_start = 0;
		r_end = tmp;
		r_dst_start = src_rect->y - dst_rect->y;
	}

	for (r_src = r_src_start, r_dst = r_dst_start; r_src < r_end;
	     r_src++, r_dst++) {
		for (c_src = c_src_start, c_dst = c_dst_start; c_src < c_end;
		     c_src++, c_dst++) {
                        int val;
			i_src = r_src * src_rect->w + c_src;
			i_dst = r_dst * dst_rect->w + c_dst;
                        val = dst[i_dst] + src[i_src];
			dst[i_dst] = (unsigned char)min(val, 255);
		}
	}
}

static void mapMergeView(struct mview *view, void *data)
{
	int r_src, r_src_start, c_src, c_src_start, i_src, r_end, c_end;
	int r_dst, r_dst_start, c_dst, c_dst_start, i_dst;
	int tmp;
        char *vmask;

	/* Skip this view if it is the active view */
	if (view == Map.aview)
		return;

        // ---------------------------------------------------------------------
        // Find the indices to merge from depending on the relationship between
        // the map view rectangle and the mview being merged.
        // ---------------------------------------------------------------------

	if (view->vrect.x < Map.aview->vrect.x) {
		/* This view leftmost (A) */
		tmp = view->vrect.x + view->vrect.w - Map.aview->vrect.x;
		if (tmp < 0)
			return;
		c_src_start = Map.aview->vrect.x - view->vrect.x;
		c_end = c_src_start + tmp;
		c_dst_start = 0;
	} else {
		/* Active view leftmost (A) */
		tmp = Map.aview->vrect.x + Map.aview->vrect.w - view->vrect.x;
		if (tmp < 0)
			return;
		c_src_start = 0;
		c_end = tmp;
		c_dst_start = view->vrect.x - Map.aview->vrect.x;
	}

	if (view->vrect.y < Map.aview->vrect.y) {
		/* This view topmost (A) */
		tmp = view->vrect.y + view->vrect.h - Map.aview->vrect.y;
		if (tmp < 0)
			return;
		r_src_start = Map.aview->vrect.y - view->vrect.y;
		r_end = r_src_start + tmp;
		r_dst_start = 0;
	} else {
		/* Active view topmost (A) */
		tmp = Map.aview->vrect.y + Map.aview->vrect.h - view->vrect.y;
		if (tmp < 0)
			return;
		r_src_start = 0;
		r_end = tmp;
		r_dst_start = view->vrect.y - Map.aview->vrect.y;
	}

        // ---------------------------------------------------------------------
        // From the vmask cache, fetch the vmask corresponding to the tile in
        // the center of the view from the vmask cache. (This will automatically
        // create the vmask if it doesn't already exist).
        // ---------------------------------------------------------------------

        vmask = vmask_get(Map.place, mview_center_x(view), mview_center_y(view));
        assert(vmask);
        if (NULL == vmask)
                return;

        // ---------------------------------------------------------------------
        // Copy the contents of the view's vmask to the master vmask.
        // ---------------------------------------------------------------------

	for (r_src = r_src_start, r_dst = r_dst_start; r_src < r_end;
	     r_src++, r_dst++) {
		for (c_src = c_src_start, c_dst = c_dst_start; c_src < c_end;
		     c_src++, c_dst++) {
			i_src = r_src * VMASK_W + c_src;
			i_dst = r_dst * VMASK_W + c_dst;
			Map.vmask[i_dst] |= vmask[i_src];
		}
	}

}

static void myMarkAsDirty(struct mview *view, void *data)
{
	view->dirty = 1;
}

static void mySetViewLightRadius(struct mview *view, void *data)
{
	int rad = *((int*)data);
	view->rad = rad;
}

static int mapCalcMaxLightRadius(int light)
{
        // until something faster becomes necessary
        return (int)sqrt((double)light);
}

#if 0
// debug
static void mapDumpRect(char *name, SDL_Rect *rect, unsigned char *data)
{
        int x, y, i;

        printf("Rect %s (%d %d %d %d):\n", name, rect->x, rect->y, rect->w,
               rect->h);
        i = 0;
        for (y = 0; y < rect->h; y++) {
                for (x = 0; x < rect->w; x++, i++) {
                        printf(" %03d", data[i]);
                }
                printf("\n");
        }
        printf("\n");
}
#endif

/**
 * Given a light source, add its contribution to the light map (Map.lmap).
 *
 * @param light is the light source to add
 * @param main_view is the view containing the light source
 */
static void mapMergeLightSource(struct light_source *light, struct mview *main_view)
{
        int radius;
        int vmask_i;
        struct mview tmp_view;
        int x;
        int y;
        int map_x;
        int map_y;
        int D;
        char *vmask;

        // Initialize the temporary view to be centered on the light
        // source. (Note: ignore the subrect, it shouldn't matter)
        //
        // REVISIT: not sure I'm calculating vrect.x right: VMASK_W is odd
        memset(&tmp_view,  0, sizeof(tmp_view));
        tmp_view.vrect.x = place_wrap_x(Map.place, light->x - (VMASK_W / 2));
        tmp_view.vrect.y = place_wrap_y(Map.place, light->y - (VMASK_H / 2));
        tmp_view.vrect.w = VMASK_W;
        tmp_view.vrect.h = VMASK_H;
        tmp_view.zoom    = 1;

        radius = min(mapCalcMaxLightRadius(light->light), VMASK_W / 2);

        // Fetch the vmask from the cache.
        vmask = vmask_get(Map.place, light->x, light->y);

        // For each visible tile in the vmask, calculate how much light is
        // hitting that tile from the light source. The loop optimizes by only
        // checking those tiles that are within the radius of the light source.
        // This optimization makes no difference on my fast box, haven't tested
        // it yet on my slow one.
        int min_y = 0;
        int max_y = VMASK_H;
        int min_x = 0;
        int max_x = VMASK_W;

        //dbg("lightmap %d:%d:%s\n", light->x, light->y, Map.place->name);

        for (y = min_y; y < max_y; y++) {

                map_y = place_wrap_y(Map.place, tmp_view.vrect.y + y);
                vmask_i = y * VMASK_W + min_x;
                
                for (x = min_x; x < max_x; x++, vmask_i++) {

                        // skip non-visible tiles
                        if (vmask[vmask_i] == 0) {
                                Map.tmp_lmap[vmask_i] = 0;
                                continue;
                        }
                                
                        map_x = place_wrap_x(Map.place, tmp_view.vrect.x + x);

                        D = place_flying_distance(Map.place, light->x, 
                                                  light->y, map_x, map_y);
                        D = D * D + 1;
                        Map.tmp_lmap[vmask_i] = min(light->light / D, 255);
                }
        }

        // Merge this source's lightmap (contained in the vmask we just built)
        // with the main lightmap.
        //
        // Note: try to optimize this by merging only the portion of the vmask
        // which is within the light radius. In fact, why don't I just limit
        // the vrect to the radius? Would that work?
        mapMergeRects(&tmp_view.vrect, Map.tmp_lmap, &main_view->vrect, 
                      Map.lmap);

}

/**
 * This clears and rebuilds Map.lmap, which is a grid of values indicating how
 * much light is hitting each tile. The results are used in mapShadeScene to
 * darken the scene.
 *
 * @param view specifies which part of the map to use
 */
static void mapBuildLightMap(struct mview *view)
{
        int x;
        int y;
        int lt_i;
        int map_x;
        int map_y;
        int ambient_light;


        /* Initialize the main lightmap to ambient light levels. */
        ambient_light = sky_get_ambient_light(&Session->sky);
        memset(Map.lmap, 
               (Map.place->underground ? UNLIT : ambient_light),
               LMAP_SZ);

        /* Optimization: if we're already getting max light everywhere from the
         * sun then skip further processing. Building a lightmap usually takes
         * about 1/3 of the time devoted to rendering. */
        if (! Map.place->underground 
            && ambient_light == MAX_AMBIENT_LIGHT) {
                return;
        }

        /* Build the list of light sources visible in the current map viewer
         * window. This actually searches outside of the current view to the
         * entire mview rectangle, so light sources that are just out-of-view
         * may cast light into the view. */
        lt_i = 0;
	for (y = 0; y < LMAP_H; y++) {
		map_y = place_wrap_y(Map.place, view->vrect.y + y);
		for (x = 0; x < LMAP_W; x++) {
			int light;

			map_x = place_wrap_x(Map.place, view->vrect.x + x);
			light = place_get_light(Map.place, map_x, map_y);
			if (!light)
				continue;

			Map.lights[lt_i].x = map_x;
			Map.lights[lt_i].y = map_y;
			Map.lights[lt_i].light = light;
			lt_i++;
		}
	}
        
	/* Skip further processing if there are no light sources */
        if (!lt_i) {
                return;
        }


        /* For each light source build a lightmap centered on that source and
         * merge it into the main lightmap. */
        while (lt_i--) {
                mapMergeLightSource(&Map.lights[lt_i], view);
        }

}

static void myShadeScene(SDL_Rect *subrect)
{
	int x, y;
	SDL_Rect rect;
        int lmap_i;

	rect.x = Map.srect.x;
	rect.y = Map.srect.y;
	rect.w = TILE_W;
	rect.h = TILE_H;

        lmap_i = subrect->y * VMASK_W + subrect->x;
        //lmap_i = 0;

	// Iterate over the tiles in the map window and the corresponding
	// values in the lightmap simultaneously */
	for (y = 0; y < MAP_TILE_H; y++, rect.y += TILE_H, 
                     lmap_i += LMAP_W /*lmap_i += VMASK_W*/) {
		for (x = 0, rect.x = Map.srect.x;
		     x < MAP_TILE_W; x++, rect.x += TILE_W) {

			/* Set the shading based on the lightmap value. The
			 * lightmap values must be converted to opacity values
			 * for a black square, so I reverse them by subtracting
			 * them from LIT. */
			screenShade(&rect, LIT - Map.lmap[lmap_i + x]);
		}
	}
}

static inline void myAdjustCameraInBounds(void)
{
	if (Map.place->wraps)
		return;

	Map.cam_x = min(Map.cam_x, Map.cam_max_x);
	Map.cam_x = max(Map.cam_x, Map.cam_min_x);
	Map.cam_y = min(Map.cam_y, Map.cam_max_y);
	Map.cam_y = max(Map.cam_y, Map.cam_min_y);
}

void mapForEachView(void (*fx) (struct mview *, void *), void *data)
{
	struct list *list;
	list = Map.views.next;
	while (list != &Map.views) {
		struct list *tmp;
		struct mview *view;
		view = outcast(list, struct mview, list);
		tmp = list->next;
		fx(view, data);
		list = tmp;
	}
}

void mapSetLosStyle(const char *los)
{
        if (LosEngine) {
                los_destroy(LosEngine);
        }
	LosEngine = los_create(los, VMASK_W, VMASK_H, -1);
        assert(LosEngine);
}

static void mapExit(void)
{
        if (Map.lights) {
                free(Map.lights);
                Map.lights = 0;
        }

        if (Map.lmap) {
                free(Map.lmap);
                Map.lmap = 0;
        }

        if (Map.tmp_lmap) {
                free(Map.tmp_lmap);
                Map.tmp_lmap = 0;
        }

        if (Map.cam_view) {
                mapDestroyView(Map.cam_view);
                Map.cam_view = 0;
        }

        if (Map.vmask) {
                free(Map.vmask);
                Map.vmask = 0;
        }
}

int mapInit(void)
{

	memset(&Map, 0, sizeof(Map));

        Map.lights = (struct light_source*)calloc(MAX_LIGHTS, 
                                                  sizeof(Map.lights[0]));
        if (!Map.lights)
                goto abort;

        /* The lightmap only needs to be as big as the map viewer
           window. Making it larger does allow for lights outside the field of
           view to be processed, but this makes dungeons appear too bright - I
           like them dark and gloomy. */
        Map.lmap = (unsigned char*)calloc(LMAP_SZ, sizeof(Map.lmap[0]));
        if (!Map.lmap)
                goto abort;

        /* This one is used during mapMergeLightSource */
        Map.tmp_lmap = (unsigned char*)calloc(LMAP_SZ, 
                                              sizeof(Map.tmp_lmap[0]));
        if (!Map.tmp_lmap)
                goto abort;

	if (!(Map.cam_view = mapCreateView()))
                goto abort;

        Map.vmask = (char*)calloc(VMASK_SZ, sizeof(Map.vmask[0]));
        if (!Map.vmask)
                goto abort;

	list_init(&Map.views);

	Map.srect.x   = MAP_X;
	Map.srect.y   = MAP_Y;
	Map.srect.w   = MAP_W;
	Map.srect.h   = MAP_H;

	Map.latencyRect.x = MAP_X;
	Map.latencyRect.y = MAP_Y;
	Map.latencyRect.w = ASCII_W * 10;
	Map.latencyRect.h = ASCII_H;

	Map.turnaroundRect.x = MAP_X;
	Map.turnaroundRect.y = MAP_Y + ASCII_H;
	Map.turnaroundRect.w = ASCII_W * 10;
	Map.turnaroundRect.h = ASCII_H;

	Map.locRect.x = MAP_X;
	Map.locRect.y = MAP_Y + MAP_H - ASCII_H;
	Map.locRect.w = ASCII_W * 9;
	Map.locRect.h = ASCII_H;

	Map.clkRect.w = ASCII_W * 7;
	Map.clkRect.h = ASCII_H;
	Map.clkRect.x = MAP_X + MAP_W - Map.clkRect.w;
	Map.clkRect.y = MAP_Y;

	Map.peering   = false;
        LosEngine     = NULL;

        Map.tile_scratch_surf = screenCreateSurface(TILE_W, TILE_H);
        assert(Map.tile_scratch_surf);

	return 0;

abort:
        mapExit();
        return -1;
}

void mapFlash(int mdelay)
{
	screenFlash(&Map.srect, mdelay, White);
}

void mapSetPlace(struct place *place)
{
	Map.place = place;

	if (place->wraps)
		return;

	if (place_w(place) > MAP_TILE_W) {
		Map.cam_max_x = place_w(place) - (MAP_TILE_W - 1) / 2 - 1;
		Map.cam_min_x = MAP_TILE_W / 2;
	} else {
		Map.cam_min_x = Map.cam_max_x = (place_w(place) + 1)/ 2 - 1;
	}

	if (place_h(place) > MAP_TILE_W) {
		Map.cam_max_y = place_h(place) - (MAP_TILE_H - 1) / 2 - 1;
		Map.cam_min_y = MAP_TILE_H / 2;
	} else {
		Map.cam_min_y = Map.cam_max_y = (place_h(place) + 1) / 2 - 1;
	}
}

struct mview *mapCreateView(void)
{
	struct mview *v;

	/* Allocate a new view */
	if (!(v = (struct mview *) malloc(MVIEW_SZ)))
		return 0;

	/* Initialize the new view */
	memset(v, 0, MVIEW_SZ);
	list_init(&v->list);
	v->vrect.w   = VMASK_W;
	v->vrect.h   = VMASK_H;
        v->zoom      = 1;
        v->subrect.w = MAP_TILE_W * v->zoom;
        v->subrect.h = MAP_TILE_H * v->zoom;
        v->subrect.x = (v->vrect.w - v->subrect.w) / 2;
        v->subrect.y = (v->vrect.h - v->subrect.h) / 2;

        //dbg("mapCreateView: return %08lx\n", v);

	return v;

}

void mapDestroyView(struct mview *view)
{
        //dbg("mapDestroyView(%08lx)\n", view);
	free(view);
}

void mapAddView(struct mview *view)
{
        //dbg("mapAddView(%08lx)\n", view);
	list_add(&Map.views, &view->list);
}

void mapRmView(struct mview *view)
{
        //dbg("mapRmView(%08lx)\n", view);
	if (view == ALL_VIEWS)
		mapForEachView(myRmView, 0);
	else
		myRmView(view, 0);
}

void mapCenterView(struct mview *view, int x, int y)
{
        x -= view->vrect.w / 2; // back up to corner of vrect
        y -= view->vrect.h / 2; // back up to corner of vrect
        view->vrect.x = place_wrap_x(Map.place, x);
        view->vrect.y = place_wrap_y(Map.place, y);
}

void mapRepaintClock(void)
{
  char * date_time_str = time_HHMM_as_string();
 
  if (! DeveloperMode)
          return;
 
  // Show the clock time:
  screenErase(&Map.clkRect);
  screenPrint(&Map.clkRect, 0, "%s", date_time_str);
  screenUpdate(&Map.clkRect);
} // mapRepaintClock()

/**
 *  converts points so they appear correctly on wrapping maps 
 */
static void map_convert_point_to_vrect(int *x, int *y)
{
        SDL_Rect *vrect = &Map.aview->vrect;
  
        // If the view rect extends past the right side of the map, and x is
        // left of the view rect, then convert x to be right of the view rect.
        if ((vrect->x + vrect->w) > place_w(Map.place) && 
            *x < vrect->x) {
                *x += place_w(Map.place);
        }
        
        // Likewise if the view rect extends beyond the southern edge of the
        // map, and y is less than the top of the view rect, then convert y to
        // be south of the view rect.
        if ((vrect->y + vrect->h) > place_h(Map.place) && 
            *y < vrect->y) {
                *y += place_h(Map.place);
        }
}

static void map_paint_cursor(void)
{
        int x, y;

        if (!Session->crosshair->is_active())
                return;

        /* Convert to view rect offset */
        x = Session->crosshair->getX();
        y = Session->crosshair->getY();
        map_convert_point_to_vrect(&x, &y);
        if (!point_in_rect(x, y, &Map.aview->vrect)) {
                return;
        }

        /* Paint it */
        sprite_paint(Session->crosshair->getSprite(), 0, 
                    MX_TO_SX(x), MY_TO_SY(y));

}

/**
 * Paint the terrain sprite for a tile.
 *
 * @param place The place to use as the tile source.
 * @param map_x The tile coordinate in the place.
 * @param map_y The tile coordinate in the place.
 * @param scr_x The screen pixel coordinate to blit to.
 * @param scr_y The screen pixel coordinate to blit to.
 * @param in_los Zero iff the tile is not in player LOS.
 */
static void map_paint_tile_terrain(struct place *place, int map_x, int map_y,
                                   int scr_x, int scr_y, int in_los)
{
        if (in_los) {

                /* This tile is in player LOS, so paint normally. */
                struct terrain *terrain = place_get_terrain(place, map_x, map_y);
                struct sprite *sprite = terrain->sprite;
                sprite_paint(sprite, 0, scr_x, scr_y);

        } else if (ShowAllTerrain || XrayVision) {

                /* This tile is not in player LOS, but the command-line option
                 * to show all terrain or the special XrayVision flag is in
                 * effect, so paint the terrain but then shade it. */
                struct terrain *terrain = place_get_terrain(place,map_x,map_y);
                struct sprite *sprite = terrain->sprite;
                sprite_paint(sprite, 0, scr_x, scr_y);
                                
                SDL_Rect shade_rect;
                shade_rect.x = scr_x;
                shade_rect.y = scr_y;
                shade_rect.w = TILE_W;
                shade_rect.h = TILE_H;
                screenShade(&shade_rect, 128);
        }

}

/**
 * Paint the object sprites for a tile.
 *
 * @param place The place to use as the tile source.
 * @param map_x The tile coordinate in the place.
 * @param map_y The tile coordinate in the place.
 * @param scr_x The screen pixel coordinate to blit to.
 * @param scr_y The screen pixel coordinate to blit to.
 * @param in_los Zero iff the tile is not in player LOS.
 */
static void map_paint_tile_objects(struct place *place, int map_x, int map_y,
                                   int scr_x, int scr_y, int in_los)
{
        if (in_los) {

                /* The tile is visible, so paint the objects normally. */
                place_paint_objects(place, map_x, map_y, scr_x, scr_y);

                /* If the crosshair is active but this tile is not in range
                 * then shade the tile. */
                if (Session->crosshair->is_active() &&
                    Session->crosshair->isRangeShaded() &&
                    ! Session->crosshair->inRange(map_x, map_y)) {
                        SDL_Rect shade_rect;
                        shade_rect.x = scr_x;
                        shade_rect.y = scr_y;
                        shade_rect.w = TILE_W;
                        shade_rect.h = TILE_H;
                        screenShade(&shade_rect, 128);
                }
        }
}

/**
 * Loop over the map tiles shown in the map viewer, invoking a callback
 * function for each tile.
 *
 * @param place The place viewed.
 * @param region The part of the place covered by the visibility mask. Units
 * are tiles.
 * @param dest The screen rectangle of the viewer. Units are pixels.
 * @param mask The visibility mask for the region. Each entry in the mask
 * covers one tile.
 * @param subrect The part of the place under the viewer (this is contained
 * within the region). Units are tiles.
 * @param tile_w Tile dimension in pixels.
 * @param tile_h Tile dimension in pixels.
 */
static void map_render_loop(struct place *place, 
                            SDL_Rect * region,
                            SDL_Rect * dest,
                            unsigned char *mask,
                            SDL_Rect * subrect,
                            int tile_h, 
                            int tile_w,
                            map_tile_render_t tile_render)
{
	int row;
	int col;
	int map_y; /* in rows */
	int map_x; /* in cols */
	int scr_x; /* in pixels */
	int scr_y; /* in pixels */
        int mask_i;
	bool use_mask;

	if (place->wraps) {
		region->x = place_wrap_x(place, region->x);
		region->y = place_wrap_y(place, region->y);
	}

        /* 
           +-----------------------------------------------------------------+
           | region/mask                                                     |
           |                                                                 |
           |                    +-------------------------+                  |
           |                    | subrect                 |                  |
           |                    |                         |                  |
           |                    |                         |                  |
           |                    |                         |                  |
           |                    |                         |                  |
           |                    |                         |                  |
           |                    |                         |                  |
           |                    |                         |                  |
           |                    |                         |                  |
           |                    |                         |                  |
           |                    |                         |                  |
           |                    |                         |                  |
           |                    +-------------------------+                  |
           |                                                                 |
           |                                                                 |
           +-----------------------------------------------------------------+
        */

	use_mask = (mask != NULL);
	map_y = region->y + subrect->y;
        mask_i = (subrect->y * region->w) + subrect->x;

	for (row = 0; row < subrect->h; row++, map_y++, mask_i += region->w) {

                /* Test if the row is off-map */
		if (place->wraps) {
			map_y = place_wrap_y(place, map_y);
		} else if (map_y < 0) {
			continue;
		} else if (map_y >= place->terrain_map->h) {
			break;
		}
                
                /* Set the screen pixel row */
		scr_y = row * tile_h + dest->y;

                /* Set the initial map column for this row */
		map_x = region->x + subrect->x;

		for (col = 0; col < subrect->w; col++, map_x++) {

                        int in_los;

                        /* Test if the column is off-map */
			if (place->wraps) {
				map_x = place_wrap_x(place, map_x);
			} else if (map_x < 0) {
				continue;
			} else if (map_x >= place->terrain_map->w) {
				break;
			}

                        /* Set the screen pixel column */
			scr_x = col * tile_w + dest->x;

                        /* Set the LOS flag. */
                        in_los = (!use_mask || mask[mask_i + col]);

                        /* Invoke the callback function that does the rendering
                         * for the tile. */
                        tile_render(place, map_x, map_y, scr_x, scr_y, in_los);
		}
	}

}

static void mapPaintPlace(struct place *place, 
                          SDL_Rect * region,   /* portion of place covered by
                                                * the vmask */
                          SDL_Rect * dest,     /* screen rectangle */
                          unsigned char *mask, /* visibility mask for entire
                                                * region */
                          SDL_Rect * subrect,  /* sub-rectangle within region
                                                * that the map viewer sees */
                          int tile_h, 
                          int tile_w)
{
        /* In order to render giant characters properly over the terrain of
         * neighboring tiles, rendering must be done in two passes. The first
         * pass renders the terrain, the second the objects. */
        map_render_loop(place, region, dest, mask,  subrect, tile_h, tile_w,
                        map_paint_tile_terrain);
        map_render_loop(place, region, dest, mask,  subrect, tile_h, tile_w,
                        map_paint_tile_objects);
	place->dirty = 0;
}


static void mapRepaintCoordinates(void)
{
        if (! DeveloperMode) {
                return;
        }

        if (player_party->isOnMap()) {
                screenPrint(&Map.locRect, 0, "[%d,%d]", player_party->getX(), 
                            player_party->getY());
                return;
        }
        
        if (NULL != Map.subject)
                screenPrint(&Map.locRect, 0, "[%d,%d]", Map.subject->getX(), 
                            Map.subject->getY());
}

static void mapRepaintTurnaround(void)
{
        extern int G_turnaround;

        if (! DeveloperMode)
                return;

	screenPrint(&Map.turnaroundRect, 0, "TA: %d", G_turnaround);
}

extern int G_latency_start;
static void mapRepaintLatency(void)
{
        static int latency = 0;

        if (! DeveloperMode)
                return;

        latency = SDL_GetTicks() - G_latency_start;

        //printf("repaint: %d\n", latency);
        screenPrint(&Map.latencyRect, 0, "LAT: %d", latency);
        screenUpdate(&Map.latencyRect);
}

/**
 * This is the main paint routine. 
 *
 * @param view defines which part of the map to show
 * @param flags controls controls policies of whether and what to paint
 */
static void mapRepaintView(struct mview *view, int flags)
{
	int t1, t2, t3, t4, t5, t6, t7, t8;

	Map.aview = view;

	if (flags & REPAINT_IF_DIRTY && !view->dirty)
		return;

        if (flags & REPAINT_IF_OLD
            && (SDL_GetTicks() - Map.last_repaint) < (Uint32)TickMilliseconds
            && (Map.last_repaint < SDL_GetTicks()))
                return;

        Map.last_repaint = SDL_GetTicks();
	view->dirty = 0;

        G_latency_start = SDL_GetTicks();

	t1 = SDL_GetTicks();

	screenErase(&Map.srect);

	t2 = SDL_GetTicks();

        if (Map.aview->blackout) {
                // In blackout mode leave the screen erased
                goto done_painting_place;
        }

	if (Map.aview->zoom > 1) {
                sprite_zoom_out(Map.aview->zoom);
		screenZoomOut(Map.aview->zoom);
		t5 = SDL_GetTicks();
		mapPaintPlace(Map.place, &view->vrect, &Map.srect, 
                              0/* vmask */, &view->subrect, 
                              TILE_W / Map.aview->zoom, 
                              TILE_H / Map.aview->zoom);
		t6 = SDL_GetTicks();
		screenZoomIn(Map.aview->zoom);
                sprite_zoom_in(Map.aview->zoom);
	} else if (flags & REPAINT_NO_LOS) {
                t5 = SDL_GetTicks();
		mapPaintPlace(Map.place, &view->vrect, &Map.srect, 0, 
                              &view->subrect, TILE_W, TILE_H);
		t6 = SDL_GetTicks();
	} else {

                // ------------------------------------------------------------
                // Map.vmask serves as the "master" vmask. Start by zeroing it
                // out so that by default nothing is in line-of-sight. Then
                // iterate over all the active views (each player party member
                // has an active view, spells may add others), and for each
                // view merge it's vmask onto the master. The result is the
                // line-of-sight for all party members is always visible to the
                // player.
                // ------------------------------------------------------------

                memset(Map.vmask, 0, VMASK_SZ);
		t3 = SDL_GetTicks();
		mapForEachView(mapMergeView, 0);
                //vmask_dump(Map.vmask);
		t4 = SDL_GetTicks();
		mapBuildLightMap(view);
		t5 = SDL_GetTicks();
		mapPaintPlace(Map.place, &view->vrect, &Map.srect,
                              (unsigned char *) Map.vmask, &view->subrect,
                              TILE_W, TILE_H);
		t6 = SDL_GetTicks();
                if (! XrayVision) {
                        myShadeScene(&view->subrect);
                }
		t7 = SDL_GetTicks();
                map_paint_cursor();

                // After shading, repaint the tile with the selected object so
                // that it shows up brightly even in darkness.
                if (Map.selected 
                    && Map.selected->getPlace() == Map.place)
                        mapUpdateTile(Map.place,
                                      Map.selected->getX(),
                                      Map.selected->getY());
	}

 done_painting_place:

        mapRepaintCoordinates();
	mapRepaintClock();
        mapRepaintTurnaround();
	screenUpdate(&Map.srect);

        // ---------------------------------------------------------------------
        // Repaint the latency AFTER the screenUpdate because we want that to
        // be part of the time measurement.
        // ---------------------------------------------------------------------

        mapRepaintLatency();

        t8 = SDL_GetTicks();

	if (PROFILE_REPAINT) {
	  printf("Total time=%d\n", t8 - t1);
	  printf("    erase screen=%d\n", t2 - t1);
	  printf("          memcpy=%d\n", t3 - t2);
	  printf("    merge vmasks=%d\n", t4 - t3);
	  printf("  build lightmap=%d\n", t5 - t4);
	  printf("     paint place=%d\n", t6 - t5);
	  printf("           shade=%d\n", t7 - t6);
	  printf("   update screen=%d\n", t8 - t7);
	}
}

static int mapXToViewX(int x)
{
        SDL_Rect *vrect = &Map.aview->vrect;
        int x2 = x - vrect->x;
        if (x2 < 0
            && place_is_wrapping(Map.place)
            && (vrect->x + vrect->w) > place_w(Map.place))
                x2 += place_w(Map.place);
        return x2;
}

static int mapYToViewY(int y)
{
        SDL_Rect *vrect = &Map.aview->vrect;
        int y2 = y - vrect->y;
        if (y2 < 0
            && place_is_wrapping(Map.place)
            && (vrect->y + vrect->h) > place_h(Map.place))
                y2 += place_h(Map.place);
        return y2;
}

int mapTileIsWithinViewport(int x, int y)
{
        SDL_Rect *vrect = &Map.aview->vrect;
        int vx = mapXToViewX(x);
        int vy = mapYToViewY(y);
        if (vx < 0
            || vx >= vrect->w
            || vy < 0
            || vy >= vrect->h)
                return 0;
        return 1;
}

unsigned char mapTileLightLevel(int x, int y)
{
        int vx = mapXToViewX(x);
        int vy = mapYToViewY(y);
        return Map.lmap[vy * LMAP_W + vx];
}

int mapTileIsVisible(int x, int y)
{       
        SDL_Rect *vrect = &Map.aview->vrect;
        int vx = mapXToViewX(x);
        int vy = mapYToViewY(y);

        // check if coords in vrect
        if (vx < 0
            || vx >= vrect->w
            || vy < 0
            || vy >= vrect->h)
                return 0;

        // If zoomed out then don't bother checking the vmask.
        if (Map.aview->zoom > 1)
                return 1;

        // Return if the tile is marked as visible
        return Map.vmask[vy * vrect->w + vx];
            
}
void mapMarkAsDirty(struct mview *view)
{
	if (view == ALL_VIEWS)
		mapForEachView(myMarkAsDirty, 0);
	else
		myMarkAsDirty(view, 0);
}

void mapSetRadius(struct mview *view, int rad)
{
	if (view == ALL_VIEWS)
		mapForEachView(mySetViewLightRadius, (void *) &rad);
	else
		mySetViewLightRadius(view, (void *) &rad);
}

int mapGetRadius(struct mview *view)
{
	return view->rad;
}

void mapGetMapOrigin(int *x, int *y)
{
	assert(Map.aview);
	*x = Map.aview->vrect.x + Map.aview->subrect.x;
	*y = Map.aview->vrect.y + Map.aview->subrect.y;
}

void mapGetScreenOrigin(int *x, int *y)
{
	*x = Map.srect.x;
	*y = Map.srect.y;
}

void mapGetTileDimensions(int *w, int *h)
{
        *w = TILE_W / Map.aview->zoom;
        *h = TILE_H / Map.aview->zoom;
}

void mapSetActiveView(struct mview *view)
{
	Map.aview = view;
        //dbg("mapSetActiveView: aview=[%d %d]\n", Map.aview->vrect.x, Map.aview->vrect.y);
}

void mapCenterCamera(int x, int y)
{
	Map.cam_x = x;
	Map.cam_y = y;

	Map.cam_x = place_wrap_x(Map.place, Map.cam_x);
	Map.cam_y = place_wrap_y(Map.place, Map.cam_y);

	myAdjustCameraInBounds();
	mapCenterView(Map.cam_view, Map.cam_x, Map.cam_y);
}

void mapMoveCamera(int dx, int dy)
{
	mapCenterCamera(Map.cam_x + dx, Map.cam_y + dy);
}

void mapUpdate(int flags)
{
        if (Map.is_image_mode)
                return;

	mapRepaintView(Map.cam_view, flags);
}

void mapSetDirty(void)
{
        if (Map.cam_view != NULL)
                Map.cam_view->dirty = 1;
}

void mapJitter(bool val)
{
	Map.srect.x = MAP_X;
	Map.srect.y = MAP_Y;
	Map.srect.w = MAP_W;
	Map.srect.h = MAP_H;

	if (val) {
		Map.srect.x += (rand() % 5) - 2;
		Map.srect.y += (rand() % 5) - 2;
	}
}

void mapPeer(bool val)
{
	int dx, dy;
	// Peering will apply to the camera view. Set the scale factor and
	// adjust the pertinent rectangle dimensions.
        Map.peering = val;
	if (val) {
		Map.cam_view->zoom = PEER_ZOOM;
		dx = (Map.cam_view->vrect.w / 2) * (PEER_ZOOM - 1);
		dy = (Map.cam_view->vrect.h / 2) * (PEER_ZOOM - 1);
		Map.cam_view->vrect.x -= dx;
		Map.cam_view->vrect.y -= dy;
		Map.cam_view->vrect.w *= PEER_ZOOM;
		Map.cam_view->vrect.h *= PEER_ZOOM;
	} else {
		Map.cam_view->zoom = 1;
		Map.cam_view->vrect.w /= PEER_ZOOM;
		Map.cam_view->vrect.h /= PEER_ZOOM;
		dx = (Map.cam_view->vrect.w / 2) * (PEER_ZOOM - 1);
		dy = (Map.cam_view->vrect.h / 2) * (PEER_ZOOM - 1);
		Map.cam_view->vrect.x += dx;
		Map.cam_view->vrect.y += dy;
	}

        Map.cam_view->subrect.w = MAP_TILE_W * Map.cam_view->zoom;
        Map.cam_view->subrect.h = MAP_TILE_H * Map.cam_view->zoom;
        Map.cam_view->subrect.x = (Map.cam_view->vrect.w - 
                                   Map.cam_view->subrect.w) / 2;
        Map.cam_view->subrect.y = (Map.cam_view->vrect.h - 
                                   Map.cam_view->subrect.h) / 2;

        mapCenterCamera(Map.cam_x, Map.cam_y); // recenter
}

void mapTogglePeering(void)
{
        mapPeer(!Map.peering);
        mapCenterCamera(Map.cam_x, Map.cam_y); // recenter
        mapUpdate(0);
}

void mapGetCameraFocus(struct place **place, int *x, int *y)
{
        *place = Map.place;
        *x = Map.cam_x;
        *y = Map.cam_y;
}

int mapIsInCameraView(struct place *place, int x, int y)
{
        int min;
        int max;

        if (place != Map.place)
                return 0;

        min = Map.cam_view->subrect.x + Map.cam_view->vrect.x;
        max = min + Map.cam_view->subrect.w;

        if (x < min || x >= max)
                return 0;

        min = Map.cam_view->subrect.y + Map.cam_view->vrect.y;
        max = min + Map.cam_view->subrect.h;

        if (y < min || y >= max)
                return 0;
        
        return 1;
}

void mapBlackout(int val)
{
        Map.cam_view->blackout = !!val;
}

static void mapPaintProjectile(SDL_Rect *rect, struct sprite *sprite,
                               SDL_Surface *surf, int dur, int currentframe, bool beam)
{
	// The rect coordinates are in SCREEN coordinates (not map) so I need
	// to do some clipping here to make sure we don't paint off the map
	// viewer.
	if (rect->x < MAP_X || rect->y < MAP_Y ||
	    ((rect->x + rect->w) > (MAP_X + MAP_W)) ||
	    ((rect->y + rect->h) > (MAP_Y + MAP_H)))
		return;

	// Save the backdrop of the new location
	if (!beam)
		screenCopy(rect, NULL, surf);

	// Paint the missile at the new location
        sprite_zoom_out(Map.aview->zoom);
        screenZoomOut(Map.aview->zoom);
	sprite_paint_frame(sprite, currentframe, rect->x, rect->y);
        sprite_zoom_in(Map.aview->zoom);
        screenZoomIn(Map.aview->zoom);

	screenUpdate(rect);

	SDL_Delay(dur);

	// Erase the missile by blitting the background
	if (!beam)
	{
		screenBlit(surf, NULL, rect);
		screenUpdate(rect);
	}
}

void mapPaintDamage(int x, int y)
{
        int tile_w, tile_h;
        SDL_Rect rect;

        if (!mapTileIsVisible(x, y))
                return;

        mapGetTileDimensions(&tile_w, &tile_h);
        rect.w = tile_w;
        rect.h = tile_h;
        rect.x = MX_TO_SX(x);
        rect.y = MY_TO_SY(y);

        /* Sometimes a damage flash doesn't make sense to the player unless the
         * map view is updated first. For example, a character that gets 2x the
         * normal action points per turn might move and then attack. The move
         * won't be shown until the map is updated. */
        mapUpdate(REPAINT_IF_DIRTY);

        mapPaintProjectile(&rect, Session->damage_sprite, 
                           Map.tile_scratch_surf, 100, 0, false);
}

//paint damage, but with custom sprite
void mapFlashSprite(int x, int y, struct sprite *sprite)
{
        int tile_w, tile_h;
        SDL_Rect rect;

        if (!mapTileIsVisible(x, y))
                return;

        mapGetTileDimensions(&tile_w, &tile_h);
        rect.w = tile_w;
        rect.h = tile_h;
        rect.x = MX_TO_SX(x);
        rect.y = MY_TO_SY(y);

        /* Sometimes a damage flash doesn't make sense to the player unless the
         * map view is updated first. For example, a character that gets 2x the
         * normal action points per turn might move and then attack. The move
         * won't be shown until the map is updated. */
        mapUpdate(REPAINT_IF_DIRTY);

        mapPaintProjectile(&rect, sprite, 
                           Map.tile_scratch_surf, 100, 0, false);
}

void mapAnimateProjectile(int Ax, int Ay, int *Bx, int *By, 
                          struct sprite *sprite, struct place *place,
                          class Missile *missile, float range)
{
	// 
	// Derived from Kenny Hoff's Bresenhaum impl at
	// http://www.cs.unc.edu/~hoff/projects/comp235/bresline/breslin1.txt
	// (no license or copyright noted)
	// 
	int framecount = sprite_num_frames(sprite);
	int currentframe = 0;
	
	int t1, t2;
	SDL_Surface * surf;	// for saving/restoring the background

	t1 = SDL_GetTicks();

	// Get tile dimensions
	int tile_w;
	int tile_h;
	mapGetTileDimensions(&tile_w, &tile_h);
	
	// Half tile offset- missiles fly from and to the middle of a tile
	int tile_w_half = tile_w/2;
	int tile_h_half = tile_h/2;
	
	MissileType *mistype = missile->getObjectType();
	bool canEnter = missile->canEnter();
	
	// Create a scratch surface for saving/restoring the background
	surf = Map.tile_scratch_surf;
	assert(surf);
	
	// Get the map coordinates of the view origin (upper left corner)
	int Ox, Oy;
	mapGetMapOrigin(&Ox, &Oy);

	// Get the screen coordinates of the map viewer origin
	int Sx, Sy;
	mapGetScreenOrigin(&Sx, &Sy);

	// Copy the place coordinates of the origin of flight. I'll walk these
	// along as the missile flies and check for obstructions.
	int Px, Py, oPx, oPy, orx, ory;
	Px = Ax;
	Py = Ay;
	orx = Ax;
	ory = Ay;

	// Convert to screen coordinates. (I need to keep the original
	// B-coordinates for field effects at the bottom of this routine).
	int sBx;
	int sBy;
	
	if (place_is_wrapping(place))
	{
		if (Ax > Ox)
		      Ax = (Ax - Ox) * tile_w + Sx;
		else
		      Ax = (place_w(place) - Ox + Ax)  * tile_w + Sx;
		if (Ay >= Oy)
		      Ay = (Ay - Oy) * tile_h + Sy;
		else
		      Ay = (place_h(place) - Oy + Ay)  * tile_h + Sy;
		
		if (*Bx >= Ox)
		      sBx = (*Bx - Ox) * tile_w + Sx;
		else
		      sBx = (place_w(place) - Ox + *Bx) * tile_w + Sx;
		if (*By >= Oy)
		      sBy = (*By - Oy) * tile_h + Sy;
		else
		      sBy = (place_h(place) - Oy + *By)  * tile_h + Sy;
	} 
	else
	{
		Ax = (Ax - Ox) * tile_w + Sx;
		Ay = (Ay - Oy) * tile_h + Sy;
		sBx = (*Bx - Ox) * tile_w + Sx;
		sBy = (*By - Oy) * tile_h + Sy;
	}
	
	// Create the rect which bounds the missile's sprite (used to update
	// that portion of the screen after blitting the sprite).
	SDL_Rect rect;
	rect.x = Ax;
	rect.y = Ay;
	rect.w = TILE_W;
	rect.h = TILE_H;
 
	// Get the distance components
	int dX = sBx - rect.x;
	int dY = sBy - rect.y;
	int AdX = abs(dX);
	int AdY = abs(dY);

	// Select the sprite orientation based on direction of travel
	if (sprite)
	{
		if ((sprite_facings_list(sprite) & 495) == 495) //nsew + diagonals
		{
			sprite_set_facing(sprite, vector_to_8facing(dX, dY));
		}
		else if ((sprite_facings_list(sprite) & 170) == 170) //nsew only
		{
			sprite_set_facing(sprite, vector_to_dir(dX, dY));
		}
	}

	// Moving left?
	int Xincr = (rect.x > sBx) ? -1 : 1;
	// adjust for rounding errors
	if (rect.x < sBx)
	{
		tile_w_half--;
	}

	// Moving down?
	int Yincr = (rect.y > sBy) ? -1 : 1;
	// adjust for rounding errors
	if (rect.y < sBy)
	{
		tile_h_half--;
	}
	
	int dPr, dPru, P, i , Xsubincr, Ysubincr;
	int oldx, oldy, tempx, tempy;
		
	//number of steps between missile repaints
	int paintloopsize = 20;	
	
	// Walk the x-axis?
	if (AdX >= AdY)
	{
		dPr = AdY << 1;
		dPru = dPr - (AdX << 1);
		P = dPr - AdX;
		Xsubincr = Xincr;
		Ysubincr = 0;
		if (range > 0.5) // floating point hence error margins
		{
			i = TILE_W * (place_w(place) + 2); // == "enough": its actually checked in the loop instead
		}
		else
		{ 
			i = AdX;	
		}
                if (AdX==0 && AdY==0) {
                    // Fix for 2364311: avoid divide-by-zero
                    paintloopsize = 0;
                } else {
                    paintloopsize = paintloopsize * (AdX*AdX)/((AdX*AdX)+(AdY*AdY));
                }
	}
	else
	{
		dPr = AdX << 1;
		dPru = dPr - (AdY << 1);
		P = dPr - AdY;	
		Xsubincr = 0;
		Ysubincr = Yincr;

		if (range > 0.5) // floating point hence error margins
		{
			i = TILE_H * (place_h(place) + 2); // == "enough": its actually checked in the loop instead
		}
		else
		{ 
			i = AdY;	
		}
		paintloopsize = paintloopsize * (AdY*AdY)/((AdX*AdX)+(AdY*AdY));
	}
	
	// firing past selected range, so work out the map edges if need be.
	bool checkEdge = ((range >= 1) && !(place_is_wrapping(place)));
	
	oldx = rect.x;
	oldy = rect.y;
	
	int paintloop = paintloopsize;	

	bool beam = missile->getObjectType()->isBeam();
	
	// For each step
	for (; i >= 0; i--)
	{	
		oPx = Px;
		oPy = Py;
		Px = place_wrap_x(place, ((tile_w_half + rect.x - Sx) / tile_w + Ox));
		Py = place_wrap_y(place, ((tile_h_half + rect.y - Sy) / tile_h + Oy));
		
		if (oPx != Px || oPy != Py)
		{			
			// check edge if required
			if (checkEdge)
			{
				if (Px < 0 || Py < 0 || Px >= place_w(place) || Py >= place_h(place))
					goto done;		
			}
				
			// check range if required
			if (range>1)
			{
				if (range < place_flying_distance(place, orx, ory, Px, Py))
				{
					//need to back up one square, since we the missile shouldnt have gotten this far
					Px = oPx;
					Py = oPy;
					goto done;
				}
			}
		
			// check if blocked by terrain
			if (!missile->enterTile(place, Px, Py))
				goto done;
			
			
			//check if callback indicates blocked
			if (canEnter & !mistype->fireEnterTile(missile, place, Px, Py))
				goto done;
		}
		
		if (paintloop == 0)
		{
			// if have done paintloop steps, redraw sprite
			paintloop = paintloopsize;
			if (mapTileIsVisible(Px, Py) && sprite)			
			{
				tempx = rect.x;
				tempy = rect.y;
				rect.x = (rect.x + oldx)/2;
				rect.y = (rect.y + oldy)/2;
				
				mapPaintProjectile(&rect, sprite, surf, 15, currentframe, beam);		
				if (framecount > 1)  
					currentframe=(currentframe+1)%framecount;
				
				rect.x = oldx = tempx;
				rect.y = oldy = tempy;
					
				mapPaintProjectile(&rect, sprite, surf, 15, currentframe, beam);		
				if (framecount > 1)  
					currentframe=(currentframe+1)%framecount;		
			}
			else if (framecount > 1)
			{
				currentframe=(currentframe+2)%framecount;
			}
		}
		paintloop--;
		
		if (P > 0)
		{
			rect.x += Xincr;
			rect.y += Yincr;
			P += dPru;
		}
		else
		{
			rect.x += Xsubincr;
			rect.y += Ysubincr;
			P += dPr;
		}
	}
      done:
	// erase the missile
	// mapRepaintView(NULL, REPAINT_ACTIVE);
	if (beam)
		SDL_Delay(100);
	
	mapUpdate(0);
	
	// restore the missile sprite to the default facing
	if (sprite)
		sprite_set_facing(sprite, SPRITE_DEF_FACING);

  *Bx = Px;
  *By = Py;

	t2 = SDL_GetTicks();

	if (PROFILE_ANIMATE) {
	  printf("mapAnimateProjectile: %d msec\n", t2 - t1);
	}
}

void mapAttachCamera(class Object *subject)
{
        Map.subject = subject;
}

void mapDetachCamera(class Object *subject)
{
        Map.subject = NULL;
}

void mapUpdateTile(struct place *place, int x, int y)
{
        struct terrain *terrain;
        int index;
        char *vmask;
        SDL_Rect rect;

        //dbg("mapUpdateTile %d:%d:%s\n", x, y, place->name);

        if (NULL == Map.aview)
                return;
        

        // ---------------------------------------------------------------------
        // Assume we want the active view as it was last rendered. Calculate
        // the screen coordinates of the given map location. Check if the
        // coordinates are in the map viewer and abort if not.
        // ---------------------------------------------------------------------

        if (place != Map.place)
                return;

        rect.x = (x - (Map.aview->vrect.x + Map.aview->subrect.x)) 
                * TILE_W/Map.aview->zoom + Map.srect.x;
        if (rect.x < Map.srect.x || rect.x > (Map.srect.x + Map.srect.w 
                                              - TILE_W/Map.aview->zoom))
                return;

        rect.y = (y - (Map.aview->vrect.y + Map.aview->subrect.y)) 
                * TILE_H/Map.aview->zoom + Map.srect.y;
        if (rect.y < Map.srect.y || rect.y > (Map.srect.y + Map.srect.h 
                                              - TILE_H/Map.aview->zoom))
                return;

        // ---------------------------------------------------------------------
        // Erase the tile.
        // ---------------------------------------------------------------------

        rect.w = TILE_W/Map.aview->zoom;
        rect.h = TILE_H/Map.aview->zoom;
        screenErase(&rect);

        terrain = place_get_terrain(place, x, y);

        if (Map.aview->zoom > 1) {

                // ------------------------------------------------------------
                // When zoomed ignore LOS. The vmasks aren't big enough to
                // cover the area viewed.
                // ------------------------------------------------------------
                
                sprite_zoom_out(Map.aview->zoom);
		screenZoomOut(Map.aview->zoom);
                sprite_paint(terrain->sprite, 0, rect.x, rect.y);
                place_paint_objects(place, x, y, rect.x, rect.y);
                sprite_zoom_in(Map.aview->zoom);
		screenZoomIn(Map.aview->zoom);

        } else {

                // ------------------------------------------------------------
                // If the place is not in line-of-sight then don't paint the
                // object(s) there. Paint the terrain iff ShowAllTerrain or
                // XrayVision are in effect.
                // ------------------------------------------------------------

                vmask = Map.vmask;
                index = ((y - Map.aview->vrect.y) * Map.aview->vrect.w) 
                        + (x - Map.aview->vrect.x);

                if (vmask[index] || ShowAllTerrain || XrayVision) {
                        sprite_paint(terrain->sprite, 0, rect.x, rect.y);
                }

                if (vmask[index]) {
                        place_paint_objects(place, x, y, rect.x, rect.y);
                }

                // If the selected object is not on this tile then shade it
                if (! Map.selected
                    || Map.selected->getPlace() != Map.place
                    || Map.selected->getX() != x
                    || Map.selected->getY() != y)                
                        screenShade(&rect, LIT - Map.lmap[index]);

        }

        if (x == Session->crosshair->getX() && y == Session->crosshair->getY())
                map_paint_cursor();

        
        

        screenUpdate(&rect);

}

void mapSetSelected(class Object *obj)
{
        if (Map.selected == obj)
                return;

        if (Map.selected) {
                obj_dec_ref(Map.selected);
                Map.selected = NULL;
        }

        Map.selected = obj;
        if (obj)
                obj_inc_ref(obj);
}

int mapScreenToPlaceCoords(int *x, int *y)
{
    if (! point_in_rect(*x, *y, &Map.srect)) {
        return -1;
    }
    int px = ((*x - Map.srect.x) * Map.aview->zoom) / TILE_W + Map.aview->vrect.x + Map.aview->subrect.x;
    int py = ((*y - Map.srect.y) * Map.aview->zoom) / TILE_H + Map.aview->vrect.y + Map.aview->subrect.y;
    
    if (place_off_map(Map.place, px, py)) {
        return -1;
    }

    *x = place_wrap_x(Map.place, px);
    *y = place_wrap_y(Map.place, py);
    return 0;
}

void mapSetImage(SDL_Surface *image)
{
        Map.is_image_mode = 1;
        if (image) {
                /* center the image over the map */
                SDL_Rect rect = Map.srect;
                if (image->w < Map.srect.w) {
                        rect.x = Map.srect.x + (Map.srect.w - image->w) / 2;
                        rect.w = image->w;
                }
                if (image->h < Map.srect.h) {
                        rect.y = Map.srect.y + (Map.srect.h - image->h) / 2;
                        rect.h = image->h;
                }
                screenErase(&Map.srect);
                screenBlit(image, NULL, &rect);
        } else {
                screenErase(&Map.srect);
        }
        screenUpdate(&Map.srect);
}

void mapClearImage(void)
{
        Map.is_image_mode = 0;
}

void mapBlitImage(SDL_Surface *image, Uint32 x, Uint32 y)
{
        SDL_Rect rect;

        rect.x = Map.srect.x + x;
        rect.y = Map.srect.y + y;
        rect.w = Map.srect.w - x;
        rect.h = Map.srect.h - y;

        screenBlit(image, NULL, &rect);
        screenUpdate(&Map.srect);
}
