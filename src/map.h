/* Copyright (c) 2002 Gordon McNutt */
#ifndef map_h
#define map_h

#ifdef __cplusplus
extern "C" {
#endif

/* In general, this is valid for functions which take only a 'view' arg */
#define ALL_VIEWS ((struct mview *)-1)

/* Flags for mapRepaintView() */
#define REPAINT_IF_DIRTY 1	/* repaint iff the view is dirty */
#define REPAINT_NO_LOS   2	/* don't apply LOS */

	extern int map_use_circular_vision_radius;

	struct mview;
	struct place;

	extern int mapInit(char *los);
	extern void mapFlash(int mdelay);
	extern void mapSetPlace(struct place *place);

	extern struct mview *mapCreateView(void);
	extern void mapDestroyView(struct mview *view);
	extern void mapAddView(struct mview *view);
	extern void mapRmView(struct mview *mview);
	extern void mapCenterView(struct mview *view, int x, int y);
	extern void mapRecomputeLos(struct mview *view);
	// extern void mapRepaintView(struct mview *view, int flags);
	// extern void mapMarkAsDirty(struct mview *view);
	extern void mapSetRadius(struct mview *view, int rad);
	extern int mapGetRadius(struct mview *view);
	// extern void mapSetActiveView(struct mview *view);

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

#ifdef __cplusplus
}
#endif
#endif
