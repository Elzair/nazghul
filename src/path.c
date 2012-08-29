#include "astar.h"
#include "kern_intvar.h"
#include "place.h"
#include "terrain.h"
#include "Object.h"


#define WRAP_DISTANCE(a,b,max) (min((a) + (max) - (b), (b) - (a)))


struct path_context {
	struct place *place;
	int target_x;
	int target_y;
	int pflags;
	class Object *requestor;
};


static int path_location_is_valid(struct path_context *context, int from_x, int from_y, int x, int y);
static void path_heuristic(struct astar_search_info *info, int *goodness, int *cost, int from_x, int from_y);

static int path_is_impossible(struct path_context *context)
{
	/* Check final destination */
	if ((context->pflags & PFLAG_ADJACENTNOTOK)
	    && ! place_is_passable(context->place, context->target_x,
				   context->target_y, context->requestor,
				   context->pflags)) {
		return 0;
	}

	/* check four neighbors */
	if (path_location_is_valid(context,
				   context->target_x,
				   context->target_y,
				   context->target_x-1,
				   context->target_y)
	    || path_location_is_valid(context,
				      context->target_x,
				      context->target_y,
				      context->target_x+1,
				      context->target_y)
	    || path_location_is_valid(context,
				      context->target_x,
				      context->target_y,
				      context->target_x,
				      context->target_y-1)
	    || path_location_is_valid(context,
				      context->target_x,
				      context->target_y,
				      context->target_x,
				      context->target_y+1))
		return 0;

	/* all 4 neighbors impassable so forget it */
	return 1;

}


static void path_heuristic(struct astar_search_info *info, int *goodness, int *cost, int from_x, int from_y)
{
	struct terrain *terrain;
	struct path_context *context;

	context = (struct path_context *) info->context;

	/* The basic goodness is walking distance. Duplicate that algorithm
	 * except pay attention to the info->flags. */

	if ((info->flags & ASTAR_HORZ) == 0) {
		// Yes, we are interested in the x coordinate of the
		// destination.
		if (context->place->wraps) {
			*goodness -= WRAP_DISTANCE(min(info->x0, info->x1), max(info->x0, info->x1), context->place->terrain_map->w);
		} else {
			*goodness -= max(info->x0, info->x1) - min(info->x0, info->x1);
		}
	}

	if ((info->flags & ASTAR_VERT) == 0) {
		// Yes, we are interested in the y coordinate of the
		// destination.
		if (context->place->wraps) {
			*goodness -= WRAP_DISTANCE(min(info->y0, info->y1),
						  max(info->y0, info->y1),
					       context->place->terrain_map->h);
		} else {
			*goodness -= max(info->y0, info->y1) -
				min(info->y0, info->y1);
		}
	}

	/* Add the terrain cost. */
	*cost += place_get_diagonal_movement_cost(context->place, from_x, from_y, info->x0, info->y0, context->requestor, PFLAG_IGNOREMECHS);

	/* And penalize tiles with hazards on them. I really should assign
	 * different penalties to different hazerds. */
	terrain = place_get_terrain(context->place, info->x0, info->y0);
	if (terrain->effect) {
		*cost += kern_intvar_get("AP_TOTAL:normal_human") * 9;
	}

	if (place_get_object(context->place, info->x0, info->y0,
			     field_layer) != NULL) {
		*cost += kern_intvar_get("AP_TOTAL:normal_human") * 9;
	}
}


static int path_location_is_valid(struct path_context *context, int from_x, int from_y, int x, int y)
{
	class Object *portal;

	//dbg("[%d %d]...", x, y);

	/* I used to check this after passability, but it really belongs first.
	 * In several cases the target location may not be passable but if the
	 * seeker can get adjacent to it that will be good enough. If this is
	 * not what the caller wants, they need to se the PFLAG_ADJACENTNOTOK
	 * flag. */
	if ((!(context->pflags & PFLAG_ADJACENTNOTOK))
	    && x == context->target_x
	    && y == context->target_y) {
		//dbg("ok\n");
		return 1;
	}

	if (!place_move_is_passable(context->place, from_x, from_y, x, y, context->requestor, context->pflags)) {
		return 0;
	}

	// --------------------------------------------------------------------
	// Check if the caller is blocked by an occupant on this tile.
	// --------------------------------------------------------------------

	if (0 == (context->pflags & PFLAG_IGNOREBEINGS)) {
		class Object *occupant;
		occupant = place_get_object(context->place, x, y, being_layer);
		if (occupant != NULL) {
			if (! (context->pflags & PFLAG_IGNORECOMPANIONS) || ! context->requestor->isCompanionOf(occupant)) {
				return 0;
			}
		}
	}

	// --------------------------------------------------------------------
	// I used to penalize portals in the heuristic routine, but that was
	// back in the day when I would pathfind for the player on a
	// right-click. Any more pathfinding is used exclusively for NPCs (or
	// PC's in follow mode) and I NEVER want them to enter a portal unless
	// they explicitly want to (and currently they never do). Likewise for
	// open moongates.
	//
	// Addendum: portals are now mechs with "step" signal handlers. The
	// code below avoids any mechanism which responds to the "step" signal,
	// including non-portals. That's fine, because anything which responds
	// to "step" is probably something I want to avoid.
	//
	// Addendum 2: fix for SF Bug #[ 1523230 ] "pathfinding across
	// mechs". Added the new IGNORESTEPTRIG flag and wrapped the following
	// with a check. Currently the only time this flag should be set is in
	// pathfinding done by player party members in Follow Mode.
	// --------------------------------------------------------------------

	if (!(context->pflags & PFLAG_IGNORESTEPTRIG)) {
		if ((portal = place_get_object(context->place, x, y, mech_layer)) && portal->canStep()) {
			return 0;
		}
	}

	return 1;
}


struct astar_node *path_find(struct place *place, struct astar_search_info *info, class Object *requestor)
{
	struct astar_node *path;
	struct path_context context;

	/* Store the target location as the context */
	context.place = place;
	context.target_x = info->x1;
	context.target_y = info->y1;
	context.pflags = info->flags;
	context.requestor = requestor;

	if (path_is_impossible(&context)) {
		return NULL;
	}

	/* Fill out the search information */
	info->is_valid_location =
	    (int (*)(void *, int, int, int, int))
		path_location_is_valid;
	info->heuristic = path_heuristic;
	info->width = place_w(place);
	info->height = place_h(place);
	info->wraps = place->wraps;
	info->context = &context;

	/* Run the pathfinding alg */
	path = astar_search(info);

	return path;

}

