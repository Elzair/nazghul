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
#include "screen.h"
#include "place.h"
#include "player.h"

#include <SDL/SDL.h>

#define MVIEW_W  MAP_TILE_W
#define MVIEW_H  MAP_TILE_H
#define VMASK_SZ (MVIEW_W * MVIEW_H)
#define MVIEW_SZ (sizeof(struct mview) + VMASK_SZ)
#define MAX_LIGHTS (MVIEW_W * MVIEW_H)

#define LIT 255
#define UNLIT 0

/* Global flag changeble by command-line parameters. */
int map_use_circular_vision_radius = 0;

static struct light_source {
	int x, y, light;
} lights[MAX_LIGHTS];

struct mview {
	struct list list;	/* used internally by map lib */
	SDL_Rect vrect;		/* map coords of vrect */
	char *vmask;		/* visibility mask */
	int rad;		/* light radius */
	int zoom;
	int dirty:1;		/* needs repaint */
};

static struct map {
	SDL_Rect srect;		/* screen coords of viewer */
	SDL_Rect fpsRect;	/* screen coords of FPS counter */
	SDL_Rect locRect;	/* screen coords of locater */
	SDL_Rect clkRect;	/* screen coords of clock */
	struct place *place;	/* subject being viewed */
	struct mview *aview;	/* active view */
	struct list views;	/* list of all views */
	struct mview *cam_view;
	int cam_x, cam_y, cam_max_x, cam_max_y, cam_min_x, cam_min_y;
	bool peering;
	char vmask[VMASK_SZ];	/* final mask used to render */
} Map;

static unsigned char lmap[MVIEW_W * MVIEW_H];	/* the light map */

static void myUpdateAlphaMask(struct mview *view)
{
	int vx;
	int vy;
	int mx;
	int my;
	int index = 0;

	for (vy = 0; vy < view->vrect.h; vy++) {
		my = view->vrect.y + vy;
		for (vx = 0; vx < view->vrect.w; vx++) {
			mx = view->vrect.x + vx;
			LosEngine->alpha[index] =
			    place_visibility(Map.place, mx, my);
			index++;
		}
	}
}

static void myRmView(struct mview *view, void *data)
{
	list_remove(&view->list);
}

static void myRecomputeLos(struct mview *view, void *data)
{
	myUpdateAlphaMask(view);
	if (LosEngine->r > 0)
		LosEngine->r = view->rad;
	LosEngine->compute(LosEngine);
	memcpy(view->vmask, LosEngine->vmask, VMASK_SZ);
}

static void myMergeVmask(struct mview *view, void *data)
{
	int r_src, r_src_start, c_src, c_src_start, i_src, r_end, c_end;
	int r_dst, r_dst_start, c_dst, c_dst_start, i_dst;
	int tmp;

	/* Skip this view if it is the active view */
	if (view == Map.aview)
		return;

	/* 
	 * x |<-- w -->| +--------------------+ | A | | +-------------------+-y
	 * | |/////////.  |^ | |/////////.  || | |/////////.  || | |/////////.
	 * || | |/////////.  || | |/////////.  || | |/////////.  || |
	 * |/////////.  || | |/////////.  |h | |/////////.  || | |/////////.  ||
	 * | |/////////.  || | |/////////.  || | |/////////.  || | |/////////.
	 * || | |/////////.  |v +----------|..........  |- | B |
	 * +-------------------+ */
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

	for (r_src = r_src_start, r_dst = r_dst_start; r_src < r_end;
	     r_src++, r_dst++) {
		for (c_src = c_src_start, c_dst = c_dst_start; c_src < c_end;
		     c_src++, c_dst++) {
			i_src = r_src * MVIEW_W + c_src;
			i_dst = r_dst * MVIEW_W + c_dst;
			Map.vmask[i_dst] |= view->vmask[i_src];
		}
	}
}

static void myMarkAsDirty(struct mview *view, void *data)
{
	view->dirty = 1;
}

static void mySetViewLightRadius(struct mview *view, void *data)
{
	int rad = (int) data;
	view->rad = rad;
}

static void myBuildLightMap(struct mview *view)
{
	int x, y, i = 0, j, k, mx, my;

	/* Initialize the lightmap to ambient light levels. */
	if (Map.place->underground) {
		memset(lmap, UNLIT, sizeof(lmap));
	} else {
		memset(lmap, Sun.light, sizeof(lmap));
	}

	/* Pass 1: fill out all the light sources */
	for (y = 0; y < MVIEW_H; y++) {
		my = view->vrect.y + y;
		for (x = 0; x < MVIEW_W; x++) {
			int light;

			mx = view->vrect.x + x;
			light = place_get_light(Map.place /* Place */ , mx, my);
			if (!light)
				continue;

			/* Remember the light source */
			lights[i].x = x;
			lights[i].y = y;
			lights[i].light = light;
			i++;
		}
	}

	/* Don't forget the player if this is not a small-scale map. Even if
	 * the player has light zero, by simply adding a light source on his
	 * location we make sure that the tile he is standing on will be lit. */
	if (player_party->context != CONTEXT_COMBAT) {
		lights[i].x = place_wrap_x(Map.place,
					   player_party->getX() -
					   view->vrect.x);
		lights[i].y =
		    place_wrap_y(Map.place,
				 player_party->getY() - view->vrect.y);
		lights[i].light = player_party->light;
		i++;
	}

	/* Skip further processing if there are no light sources */
	if (!i)
		return;

	/* Pass 2: distribute the light to all the squares */
	for (y = 0, k = 0; y < MVIEW_H; y++) {

		for (x = 0; x < MVIEW_W; x++, k++) {

			/* For each light source... */
			for (j = 0; j < i; j++) {

				int dx, dy, D, tmp;

				/* Calculate the distance squared. */
				dx = lights[j].x - x;
				dy = lights[j].y - y;
				D = (dx * dx) + (dy * dy) + 1;

				/* Calculate the light as source luminence over
				 * distance squared. Clamp to 255. */
				tmp = lmap[k] + lights[j].light / D;
				if (tmp > 255)
					tmp = 255;
				lmap[k] = tmp;
			}
		}
	}
}

static void myShadeScene(void)
{
	int i, x, y;
	SDL_Rect rect;

	rect.x = Map.srect.x;
	rect.y = Map.srect.y;
	rect.w = TILE_W;
	rect.h = TILE_H;

	/* Iterate over the tiles in the map window and the corresponding
	 * values in the lightmap simultaneously */
	for (y = 0, i = 0; y < MAP_TILE_H; y++, rect.y += TILE_H) {
		for (x = 0, rect.x = Map.srect.x;
		     x < MAP_TILE_W; x++, i++, rect.x += TILE_W) {

			/* Set the shading based on the lightmap value. The
			 * lightmap values must be converted to opacity values
			 * for a black square, so I reverse them by subtracting
			 * them from LIT. */
			screenShade(&rect, LIT - lmap[i]);
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

void mapForEach(void (*fx) (struct mview *, void *), void *data)
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

int mapInit(char *los)
{

	memset(&Map, 0, sizeof(Map));

	if (!(Map.cam_view = mapCreateView()))
		return -1;

	Map.srect.x = MAP_X;
	Map.srect.y = MAP_Y;
	Map.srect.w = MAP_W;
	Map.srect.h = MAP_H;

	Map.fpsRect.x = MAP_X;
	Map.fpsRect.y = MAP_Y;
	Map.fpsRect.w = ASCII_W * 10;
	Map.fpsRect.h = ASCII_H;

	Map.locRect.x = MAP_X;
	Map.locRect.y = MAP_Y + MAP_H - ASCII_H;
	Map.locRect.w = ASCII_W * 9;
	Map.locRect.h = ASCII_H;

	Map.clkRect.w = ASCII_W * 7;
	Map.clkRect.h = ASCII_H;
	Map.clkRect.x = MAP_X + MAP_W - Map.clkRect.w;
	Map.clkRect.y = MAP_Y;

	list_init(&Map.views);
	Map.peering = false;

	/* fixme -- why create this here? why not just pass it in? */
	LosEngine = los_create(los, MAP_TILE_W, MAP_TILE_H,
			       map_use_circular_vision_radius ?
			       MAX_VISION_RADIUS : -1);
	if (!LosEngine) {
		mapDestroyView(Map.cam_view);
		return -1;
	}

	return 0;
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
		Map.cam_max_x = place_w(place) - MAP_TILE_W / 2 - 1;
		Map.cam_min_x = MAP_TILE_W / 2;
	} else {
		Map.cam_min_x = Map.cam_max_x = place_w(place) / 2 - 1;
	}

	if (place_h(place) > MAP_TILE_W) {
		Map.cam_max_y = place_h(place) - MAP_TILE_H / 2 - 1;
		Map.cam_min_y = MAP_TILE_H / 2;
	} else {
		Map.cam_min_y = Map.cam_max_y = place_h(place) / 2 - 1;
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
	v->vrect.w = MVIEW_W;
	v->vrect.h = MVIEW_H;
	v->vmask = (char *) v + sizeof(*v);

	return v;

}

void mapDestroyView(struct mview *view)
{
	free(view);
}

void mapAddView(struct mview *view)
{
	list_add(&Map.views, &view->list);
}

void mapRmView(struct mview *view)
{
	if (view == ALL_VIEWS)
		mapForEach(myRmView, 0);
	else
		myRmView(view, 0);
}

void mapCenterView(struct mview *view, int x, int y)
{
	view->vrect.x = place_wrap_x(Map.place, x - view->vrect.w / 2);
	view->vrect.y = place_wrap_y(Map.place, y - view->vrect.h / 2);
}

void mapRecomputeLos(struct mview *view)
{
	if (view == ALL_VIEWS)
		mapForEach(myRecomputeLos, 0);
	else
		myRecomputeLos(view, 0);
}

void mapRepaintClock(void)
{
	int hr = Clock.hour;

	// Show the clock time.
	screenErase(&Map.clkRect);
	hr = hr > 12 ? hr - 12 : hr;
	hr = hr == 0 ? 12 : hr;
	screenPrint(&Map.clkRect, 0, "%2d:%02d%s", hr, Clock.min,
		    Clock.hour >= 12 ? "PM" : "AM");

	screenUpdate(&Map.clkRect);
}

void mapRepaintView(struct mview *view, int flags)
{
	int t2, t3, t4, t5, t6, t7, t8;

	if (Map.peering)
		return;

	Map.aview = view;

	if (flags & REPAINT_IF_DIRTY && !view->dirty)
		return;
	view->dirty = 0;

	int start = SDL_GetTicks();

	screenErase(&Map.srect);

	t2 = SDL_GetTicks();

	if (Map.aview->zoom > 1) {
		screenZoomOut(Map.aview->zoom);
		placePaint(Map.place, &view->vrect, &Map.srect, 0/* vmask */ ,
			   TILE_W / Map.aview->zoom, TILE_H / Map.aview->zoom);
		screenZoomIn(Map.aview->zoom);
	} else if (flags & REPAINT_NO_LOS) {
		placePaint(Map.place, &view->vrect, &Map.srect, 0, TILE_W,
			   TILE_H);
	} else {
		memcpy(Map.vmask, Map.aview->vmask, VMASK_SZ);
		t3 = SDL_GetTicks();
		mapForEach(myMergeVmask, 0);
		t4 = SDL_GetTicks();
		myBuildLightMap(view);
		t5 = SDL_GetTicks();
		placePaint(Map.place, &view->vrect, &Map.srect,
			   (unsigned char *) Map.vmask, TILE_W, TILE_H);
		t6 = SDL_GetTicks();
		myShadeScene();
		t7 = SDL_GetTicks();
	}

	// Show the player's location. In combat mode use the leader, else use
	// the party.
	class Character *leader = player_party->get_leader();
	if (leader)
		screenPrint(&Map.locRect, 0, "[%d,%d]", leader->getX(),
			    leader->getY());
	else
		screenPrint(&Map.locRect, 0, "[%d,%d]", player_party->getX(),
			    player_party->getY());

	mapRepaintClock();	// since we erased it above

	screenUpdate(&Map.srect);
	t8 = SDL_GetTicks();

	// Show the frame rate (do this after the update above to get a better
	// measure of the time it takes to paint the screen).
	screenPrint(&Map.fpsRect, 0, "FPS: %d",
		    1000 / (SDL_GetTicks() - start + 1));
	screenUpdate(&Map.fpsRect);

#ifdef PROFILE
	printf("Total time=%d\n", t8 - start);
	printf("  Erase screen=%d\n", t2 - start);
	printf("  memcpy=%d\n", t3 - t2);
	printf("  merge vmasks=%d\n", t4 - t3);
	printf("  build lightmap=%d\n", t5 - t4);
	printf("  paint place=%d\n", t6 - t5);
	printf("  shade=%d\n", t7 - t6);
	printf("  update screen=%d\n", t8 - t7);
#endif
}

void mapMarkAsDirty(struct mview *view)
{
	if (view == ALL_VIEWS)
		mapForEach(myMarkAsDirty, 0);
	else
		myMarkAsDirty(view, 0);
}

void mapSetRadius(struct mview *view, int rad)
{
	if (view == ALL_VIEWS)
		mapForEach(mySetViewLightRadius, (void *) rad);
	else
		mySetViewLightRadius(view, (void *) rad);
}

int mapGetRadius(struct mview *view)
{
	return view->rad;
}

void mapGetMapOrigin(int *x, int *y)
{
	assert(Map.aview);
	*x = Map.aview->vrect.x;
	*y = Map.aview->vrect.y;
}

void mapGetScreenOrigin(int *x, int *y)
{
	*x = Map.srect.x;
	*y = Map.srect.y;
}

void mapSetActiveView(struct mview *view)
{
	Map.aview = view;
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
	mapRepaintView(Map.cam_view, flags);
}

void mapSetDirty(void)
{
	Map.cam_view->dirty = 1;
}

void mapJitter(bool val)
{
	Map.srect.x = MAP_X;
	Map.srect.y = MAP_Y;
	Map.srect.w = MAP_W;
	Map.srect.h = MAP_H;

	if (val) {
		Map.srect.x += (random() % 5) - 2;
		Map.srect.y += (random() % 5) - 2;
	}
}

void mapPeer(bool val)
{
#define PEER_ZOOM 2
	int dx, dy;
	// Peering will apply to the camera view. Set the scale factor and
	// adjust the pertinent rectangle dimensions.
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
#if 0

	// This is the original peering code which used one pixel per
	// tile. That makes the map WAY too small. But it could be useful for a
	// minimap so I'm keeping the code for now.

	int sx, sy, mx, my, max_sx, max_sy;
	int start_mx, start_my;
	int center_mx, center_my;

	Map.peering = val;

	if (!val)
		return;

	if (screenLock() < 0)
		return;

	// Find the map coordinates of the center of the viewed area.
	class Character *leader = playerGetLeader();
	if (leader) {
		center_mx = leader->getX();
		center_my = leader->getY();
	} else {
		center_mx = player_party->getX();
		center_my = player_party->getY();
	}

	// Find the edges in map coordinates. Ok if negative.
	start_my = center_my - MVIEW_H / 2;
	start_mx = center_mx - MVIEW_W / 2;

	// Iterate over the visible region. Paint one pixel for each tile
	// colored according to its terrain type.
	max_sx = Map.srect.x + Map.srect.w;
	max_sy = Map.srect.y + Map.srect.h;
	my = start_my;
	for (sy = Map.srect.y; sy < max_sx; my++, sy++) {
		mx = start_mx;
		for (sx = Map.srect.x; sx < max_sx; mx++, sx++) {
			screenSetPixel(sx, sy, place_get_color(Place, mx, my));
		}
	}

	screenUnlock();

	screenUpdate(&Map.srect);
#endif
}

void mapGetCameraFocus(struct place **place, int *x, int *y)
{
        *place = Map.place;
        *x = Map.cam_x;
        *y = Map.cam_y;
}
