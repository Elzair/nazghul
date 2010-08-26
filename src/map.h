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
#ifndef map_h
#define map_h

#include "macros.h"
#include <SDL.h>

BEGIN_DECL

/* In general, this is valid for functions which take only a 'view' arg */
#define ALL_VIEWS ((struct mview *)-1)

/* Flags for mapRepaintView() */
#define REPAINT_IF_DIRTY 1	/* repaint iff the view is dirty */
#define REPAINT_NO_LOS   2	/* don't apply LOS */
#define REPAINT_IF_OLD   4      /* repaint iff last repaint < tick ms ago */

extern int map_use_circular_vision_radius;

struct mview;
struct place;

extern int mapInit(void);
extern void mapFlash(int mdelay);
extern void mapSetPlace(struct place *place);

extern struct mview *mapCreateView(void);
extern void mapDestroyView(struct mview *view);
extern void mapAddView(struct mview *view);
extern void mapRmView(struct mview *mview);
extern void mapCenterView(struct mview *view, int x, int y);
extern void mapRecomputeLos(struct mview *view);
extern void mapSetRadius(struct mview *view, int rad);
extern int mapGetRadius(struct mview *view);

// Hacked in to support missile animation:
extern void mapGetMapOrigin(int *x, int *y);
extern void mapGetScreenOrigin(int *x, int *y);

extern void mapCenterCamera(int x, int y);
extern void mapMoveCamera(int dx, int dy);
extern void mapUpdate(int flags);
extern void mapSetDirty(void);

extern void mapRepaintClock(void);
extern void mapJitter(bool val);	// added for tremor
extern void mapPeer(bool val);
extern void mapTogglePeering(void);

extern void mapGetCameraFocus(struct place **place, int *x, int *y);
extern int mapIsInCameraView(struct place *place, int x, int y);

extern int mapTileIsWithinViewport(int x, int y);
extern int mapTileIsVisible(int x, int y);
extern unsigned char mapTileLightLevel(int x, int y);

extern void mapBlackout(int val); // for sleeping in town

// The destination (dx, dy) point is an in-out parm, upon return it
// points to the location where the missile actually landed (in case it
// was blocked in its path)
extern void mapAnimateProjectile(int ox, int oy, int *dx, int *dy, 
                                 struct sprite *sprite, 
                                 struct place *place,
                                 class Missile *missile,
                                 float range);

extern void mapAttachCamera(class Object *subject);
extern void mapDetachCamera(class Object *subject);
extern void mapSetLosStyle(const char *los);
extern void mapUpdateTile(struct place *place, int x, int y);
extern void mapPaintDamage(int x, int y);
extern void mapSetSelected(class Object *obj);
extern void mapFlashSprite(int x, int y, struct sprite *sprite);

extern int mapScreenToPlaceCoords(int *x, int *y);

/**
 * Instead of showing the current place, show an image. This lasts until
 * mapClearImage() is called.
 *
 * @param image The image to show. If null, the map is erased instead (ie,
 * blacked-out).
 */
extern void mapSetImage(SDL_Surface *image);

/**
 * Turn off the image set via mapSetImage() and resume showing the current
 * place.
 */
extern void mapClearImage(void);

/**
 * Blit an image onto the map window. This lasts until something else is
 * painted over it. The image will be clipped to fit within the map
 * window. This function will update the screen before returning.
 *
 * @param image is the image to show. It should not be NULL.
 * @param x is the pixel offset within the map window to blit it.
 * @param y is the pixel offset within the map window to blit it.
 */
extern void mapBlitImage(SDL_Surface *image, Uint32 x, Uint32 y);

END_DECL

#endif
