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
#include "combat.h"
#include "NpcParty.h"
#include "place.h"
#include "player.h"
#include "object.h"
#include "common.h"
#include "map.h"
#include "hash.h"
#include "wq.h"
#include "console.h"
#include "sound.h"
#include "status.h"
#include "cursor.h"
#include "Container.h"
#include "terrain.h"
#include "Field.h"
#include "event.h"
#include "play.h"
#include "Spell.h"
#include "Trap.h"
#include "portal.h"
#include "foogod.h"
#include "Mech.h"
#include "wind.h"
#include "dup_constants.h"
#include "cmdwin.h"
#include "terrain_map.h"
#include "vehicle.h"
#include "formation.h"
#include "pinfo.h"
#include "Loader.h"

#include <assert.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <math.h>

#define FORMATION_H     (formation[array_sz(formation) - 1].y)
#define N_MAX_NPCS      32	/* arbitrary limit */
#define MAX_DEPTH       64
#define MAX_PLACEMENT_RECTANGLE_W 32
#define MAX_PLACEMENT_RECTANGLE_H 16
#define SEARCH_QUEUE_SZ 100

#define VMAP_VISIT(x,y) (Combat.vmap[place_w(Place) * (y) + (x)] = 0)
#define VMAP_VISITED(x,y) (Combat.vmap[place_w(Place) * (y) + (x)])

#define LOC_IS_SAFE(x,y,pmask) \
    (!place_off_map(Place, x, y) && \
     place_is_passable(Place, x, y, pmask, 0) && \
     !place_is_occupied(Place, x, y))

/* Formation pattern -- party facing north (dx=0,dy=-1), origin at the leader's
 * position */
static struct formation_entry default_formation_entries[] = {
	{0, 0},
	{1, 1},
	{-1, 1},
	{2, 2},
	{0, 2},
	{-2, 2},
	{3, 3},
	{1, 3},
	{-1, 3},
	{-3, 3},
	{4, 4},
	{2, 4},
	{0, 4},
	{-2, 4},
	{-4, 4},
	{5, 5},
	{3, 5},
	{1, 5},
	{-1, 5},
	{-3, 5},
	{-5, 5},
	{6, 6},
	{4, 6},
	{2, 6},
	{0, 6},
	{-2, 6},
	{-4, 6},
	{-6, 6},		/* 28 */
};

static struct formation default_formation;

// The combat state machine:
//
//          a-> VICTORY d-> LOOTING c-> 
// FIGHTING b-> DEFEAT e--------------> DONE
//          c-> RETREAT f------------->
//
// a - Last NPC killed or fled the map
// b - Last PC killed
// c - Last PC fled the map
// d - Victory declared
// e - Defeat declared
// f - Retreat declared
enum CombatState {
	FIGHTING,
	DEFEAT,			// All PCs dead or fled
	VICTORY,		// All NPCs dead or fled
	RETREAT,		// Last PC fled
	LOOTING,		// VICTORY was declared
	CAMPING,
	DONE,			// Last PC exited after LOOTING
};

struct retreat_info {
	enum RetreatType {
		NO_RETREAT = 0,
		EDGE_RETREAT,
		PORTAL_RETREAT
	} type;
	int x, y, dx, dy;
};

static struct {
	struct place place;	// The temporary combat place used for
	// wilderness combat
	enum CombatState state;
	class Character *pm;	// The currently active player party membet
	int n_pcs;		/* number of PCs on the map */
	int n_npcs;		/* number of NPCs on the map */
	int max_n_npcs;		// number of NPCs originally allocated
	class Character *npcs[N_MAX_NPCS];
	char vmap[7 * 7];	// visited map (used to search for positions)
	struct retreat_info rinfo;
	bool followMode;
	bool defend;		/* 1 iff the player was attacked */

	// The list of npc parties in combat.
	struct list parties;

	int turns_per_round;
	int end_of_camping_turn;
	class Character *guard;
	bool rested;
	struct list summoned;
	bool tmp_terrain_map;
	class Vehicle *enemy_vehicle;
        char *sound_enter;
        char *sound_defeat;
        char *sound_victory;
} Combat;

struct v2 {
	int dx, dy;
};

// Search alg data
static char rmap[MAX_PLACEMENT_RECTANGLE_W * MAX_PLACEMENT_RECTANGLE_H];
static int x_q[SEARCH_QUEUE_SZ];
static int y_q[SEARCH_QUEUE_SZ];
static int q_head;
static int q_tail;

static void Defeat(void)
{
	// The NPCs have won.
	soundPlay(Combat.sound_defeat, SOUND_MAX_VOLUME);
	consolePrint("\n*** Battle is Lost! ***\n\n");
	Combat.state = DONE;
}

static void Victory(void)
{
	// The player has won. Play the triumphant music and advance to the
	// looting phase.
	soundPlay(Combat.sound_victory, SOUND_MAX_VOLUME);
	consolePrint("\n*** VICTORY! ***\n\n");
	Combat.state = LOOTING;
}

static void Retreat(void)
{
	// The player has escaped by fleeing.  
	soundPlay(Combat.sound_defeat, SOUND_MAX_VOLUME);
	consolePrint("\n*** Run Away! ***\n\n");
	Combat.state = DONE;
}

static void myForEachNpc(bool(*fx) (class Character * npc, void *data),
			 void *data)
{
	for (int i = 0; i < Combat.max_n_npcs; i++) {
		class Character *npc = Combat.npcs[i];
		if (npc == NULL)
			continue;
		if (fx(npc, data))
			return;
	}
}

static bool myCheckIfHostile(class Character * npc, void *data)
{
	if (npc->isOnMap() && !npc->isDead() &&
	    npc->party->isHostile(player_party->alignment)) {
		*((bool *) data) = false;
		return true;
	}
	return false;
}

static int location_is_safe(struct position_info *info)
{
	struct astar_node *path;
	int flags = PFLAG_IGNOREBEINGS;
	int edge_x = 0, edge_y = 0;
	struct astar_search_info as_info;

	// Is it passable?
	if (!place_is_passable(Place, info->px, info->py, info->pmask, 0)) {
		printf("impassable\n");
		return -1;
	}
	// Is it occupied?
	if (place_is_occupied(Place, info->px, info->py)) {
		printf("occupied\n");
		return -1;
	}

	memset(&as_info, 0, sizeof(as_info));

	if (info->find_edge) {

		assert(info->dx || info->dy);
		assert(!info->dx || !info->dy);	// assume no diagonals for now

		printf("searching for path to ");

		if (info->dx < 0) {
			// facing west, find path back to east edge
			edge_x = place_w(Place) - 1;
			flags |= PFLAG_VERT;
			printf("east ");
		} else if (info->dx > 0) {
			// facing east, find path to back west edge
			edge_x = 0;
			flags |= PFLAG_VERT;
			printf("west ");
		} else if (info->dy < 0) {
			// facing north, find path back to south edge
			edge_y = place_h(Place) - 1;
			flags |= PFLAG_HORZ;
			printf("north ");
		} else {
			// facing south, find path back to north edge
			edge_y = 0;
			flags |= PFLAG_HORZ;
			printf("south ");
		}

		printf("edge...");

		as_info.x0 = info->px;
		as_info.y0 = info->py;
		as_info.x1 = edge_x;
		as_info.y1 = edge_y;
		as_info.flags = flags;

		path = place_find_path(Place, &as_info, info->pmask);

		if (!path)
			printf("no path back to edge\n");

	} else if (info->find_party) {
		// Each member should be able to find a path back to the
		// party's originating location on the map.
		printf("searching for path to party [%d %d]...",
		       info->x, info->y);

		as_info.x0 = info->px;
		as_info.y0 = info->py;
		as_info.x1 = info->x;
		as_info.y1 = info->y;
		as_info.flags = flags;
		as_info.limit_depth = true;
		as_info.max_depth = 5;

		path = place_find_path(Place, &as_info, info->pmask);

		if (!path)
			printf("no path back to party\n");
	} else {
		// skip the pathfinding check
		return 0;
	}

	if (path) {
		astar_path_destroy(path);
		return 0;
	}

	return -1;
}

static int search_for_safe_position(struct position_info *info)
{
	unsigned int i;
	int index;
	static int x_offsets[] = { -1, 1, 0, 0 };
	static int y_offsets[] = { 0, 0, -1, 1 };

	printf("checking [%d %d]...", info->px, info->py);

	// translate the map coords into an rmap index
	index = info->py - info->ry;
	index *= info->rw;
	index += (info->px - info->rx);

	// If the current location is off-map, outside of the placement
	// rectangle or already visited then discontinue the search.  
	if (rmap[index]) {
		printf("already visited [%d]\n", index);
		return -1;	// already visited
	}
	if (info->px < info->rx || info->px >= info->rx + info->rw ||
	    info->py < info->ry || info->py >= info->ry + info->rh) {
		printf("outside the placement area\n");
		return -1;	// outside the placement rect
	}
	if (place_off_map(Place, info->px, info->py)) {
		printf("off-map\n");
		// return -1; // off map
		goto enqueue_neighbors;
	}
	// Mark this location as visited.
	rmap[index] = 1;

	// If the current location is safe then the search succeeded.
	if (location_is_safe(info) == 0) {
		printf("OK!\n");
		return 0;
	}

      enqueue_neighbors:
	// Enqueue the adjacent neighbors onto the search queue.
	for (i = 0; i < array_sz(x_offsets) && q_tail < SEARCH_QUEUE_SZ; i++) {
		assert(q_tail < SEARCH_QUEUE_SZ);
		x_q[q_tail] = info->px + x_offsets[i];
		y_q[q_tail] = info->py + y_offsets[i];
		q_tail++;
	}

	// Return still not found.
	return -1;
}

static int myFindSafePosition(struct position_info *info)
{
	// Here's my new definition of a safe place: a safe place is a tile
	// within the placement rectangle which is passable to the character in
	// question and from which the character in question can pathfind to
	// the edge from which the party entered.

	// Clear the search queue.
	q_head = q_tail = 0;

	// Push the preferred position onto the search queue.
	x_q[q_tail] = info->px;
	y_q[q_tail] = info->py;
	q_tail++;

	// Run through the search queue until it is exhausted or a safe
	// position has been found.
	while (q_head != q_tail) {

		// Dequeue the next location to check.
		info->px = x_q[q_head];
		info->py = y_q[q_head];
		q_head++;

		// If it is ok then we're done.
		if (search_for_safe_position(info) == 0)
			return 0;
	}

	return -1;
}

static bool myPutNpc(class Character * pm, void *data)
{
	int tmp;
	struct position_info *info;

	info = (struct position_info *) data;

	if (pm->isDead())
		return false;

	if (Combat.n_npcs == N_MAX_NPCS) {
		// Filled up the npc buffer so abort.
		printf("Can't fit any more NPCs on the map!\n");
		return true;
	}
	// In the case where there is more than one NPC party entering combat
	// this might be called more than once for an NPC. I want to ignore all
	// but the first call, so check if the NPC is already on the map.
	if (pm->isOnMap())
		return false;

	// Make sure we don't index beyond the end of the formation array.
	if (pm->getOrder() >= info->formation->n)
		return false;

	pm->setCombat(true);

	pm->setX(info->formation->entry[pm->getOrder()].x);
	pm->setY(info->formation->entry[pm->getOrder()].y);

	/* Counterclockwise rotations: x = x * cos - y * sin y = x * sin + y *
	 * cos */
	if (info->dx < 0) {
		/* Rotate +90 degrees */
		tmp = pm->getX();
		pm->setX(pm->getY());
		pm->setY(tmp);
	} else if (info->dx > 0) {
		/* Rotate -90 degrees */
		tmp = pm->getX();
		pm->setX(-pm->getY());
		pm->setY(tmp);
	} else if (info->dy > 0) {
		/* Rotate 180 degrees */
		pm->setX(-pm->getX());
		pm->setY(-pm->getY());
	}

	/* If dy > 1 then the formation is ok as-is. */
	pm->changeX(info->x);
	pm->changeY(info->y);

	// Check if that location will really work. If not then do a DFS
	// starting from the desired location and see if we can find someplace
	// that WILL work.

	// initialize the position info for a new search
	info->pmask = pm->getPmask();
	memset(rmap, 0, sizeof(rmap));

	// set the preferred location
	info->px = pm->getX();
	info->py = pm->getY();

	printf("Placing %s\n", pm->getName());

	if (myFindSafePosition(info) == -1) {
		// If I can't place a member then I can't place it.
		printf("*** Can't place %s ***\n", pm->getName());
		return false;
	}

	pm->setX(info->px);
	pm->setY(info->py);
	printf("Put '%s' at [%d %d]\n", pm->getName(), info->px, info->py);

	pm->setPlace(Place);
	place_add_object(Place, pm);
	Combat.npcs[Combat.n_npcs++] = (class Character *) pm;
	info->placed++;

	return false;
}

static void set_party_initial_position(struct position_info *pinfo, int x,
				       int y)
{
	pinfo->x = x;
	pinfo->y = y;

	// Set the bounds of the placement rectangle. For now I don't care if
	// it overlaps the edge of the map because the search algorithm will
	// check for off-map locations.
	pinfo->rw = MAX_PLACEMENT_RECTANGLE_W;
	pinfo->rh = MAX_PLACEMENT_RECTANGLE_H;
	pinfo->rx = pinfo->x - pinfo->rw / 2;
	pinfo->ry = pinfo->y - pinfo->rh / 2;

	printf("Moved party start position to [%d %d]\n", pinfo->x, pinfo->y);;

}

static void myFillPositionInfo(struct position_info *info, int x, int y,
			       int dx, int dy, bool defend)
{
	// 
	// This function will:
	// * determine the party's coordinates upon entry to combat
	// * turn the defending party to face the attacker
	// * specify the placement rectangle for the party members
	// * set the flags for the placement algorithm
	// 

	info->dx = dx;
	info->dy = dy;

	if (Place != &Combat.place) {
		// Occupy the same location and face the same way
		info->x = x;
		info->y = y;

	} else {

		if (defend) {
			// Reverse facing
			info->dx = -dx;
			info->dy = -dy;
			dx = -dx;
			dy = -dy;
		}
		// Occupy an edge facing the opponent
		if (dx < 0) {
			// facing west, occupy east half
			info->x = place_w(Place) - place_w(Place) / 4;
		} else if (dx > 0) {
			// facing east, occupy west half
			info->x = place_w(Place) / 4;
		} else {
			// facing north or south, center on east-west
			info->x = place_w(Place) / 2;
		}

		if (dy < 0) {
			// facing north, occupy south
			info->y = place_h(Place) - place_h(Place) / 4;
		} else if (dy > 0) {
			// facing south, occupy north
			info->y = place_h(Place) / 4;
		} else {
			// facing east or west, center on north-south
			info->y = place_h(Place) / 2;
		}
	}

	set_party_initial_position(info, info->x, info->y);

	// clear the pmask and search map before first use
	info->pmask = 0;
	memset(rmap, 0, sizeof(rmap));

	// If this is the special combat map then the attackers must be able to
	// flee back the way they came. Defenders are placed in the center, so
	// this requirement does not apply. If this is NOT the special combat
	// map then all combatants must be able to rendezvous at their original
	// party location. It isn't that I'm concerned so much with
	// rendezvous. What I'm worried about is placing a combatant in a
	// position which he cannot pathfind to from the party's starting
	// location. Such combatants could be stranded by impassable terrain,
	// preventing the player from exiting the map.
	// 
	// Note: vehicles make this an exception. Rather than check for a
	// vehicle I'm going to loosen the above requirement by generalizing
	// this to all wilderness combat. Which completely nullifies the
	// requirement since that's the only time we use the special combat
	// map.
#define FIND_EDGE_REQUIREMENT false
	info->find_edge = false;
	info->find_party = false;
	if (Place == &Combat.place && FIND_EDGE_REQUIREMENT) {
		if (!defend)
			info->find_edge = true;
	} else {
		info->find_party = true;
	}
	info->placed = 0;
}

static bool myPutPC(class Character * pm, void *data)
{
	// Put a party member on the combat map

	int tmp;
	struct position_info *info;

	info = (struct position_info *) data;

	if (pm->isDead())
		return false;

	if (pm->getOrder() >= info->formation->n)
		return false;

	pm->setX(info->formation->entry[pm->getOrder()].x);
	pm->setY(info->formation->entry[pm->getOrder()].y);

	/* Counterclockwise rotations: x = x * cos - y * sin y = x * sin + y *
	 * cos */
	if (info->dx < 0) {
		/* Rotate +90 degrees */
		tmp = pm->getX();
		pm->setX(pm->getY());
		pm->setY(tmp);
	} else if (info->dx > 0) {
		/* Rotate -90 degrees */
		tmp = pm->getX();
		pm->setX(-pm->getY());
		pm->setY(tmp);
	} else if (info->dy > 0) {
		/* Rotate 180 degrees */
		pm->setX(-pm->getX());
		pm->setY(-pm->getY());
	}

	/* If dy > 1 then the formation is ok as-is. */
	pm->changeX(info->x);
	pm->changeY(info->y);

	// Check if that location will really work. If not then do a DFS
	// starting from the desired location and see if we can find someplace
	// that WILL work.

	// init the position info for a new search
	info->pmask = pm->getPmask();
	memset(rmap, 0, sizeof(rmap));
	info->px = pm->getX();
	info->py = pm->getY();
	printf("Placing %s\n", pm->getName());

	if (myFindSafePosition(info) == -1) {

		// Ok, so that didn't work. This can happen when the party
		// leader is right on the map border facing towards the map
		// center. In this case the find-safe-place alg won't handle
		// followers that are too deep off the map. If this IS the
		// party leader, or if upon retry we STILL can't find a safe
		// place, then screw it - we'll place this character on the
		// start location (even if we have to end up stacking the whole
		// party there!).

                // Corner case: a portal leads from one lake to another. A
                // hostile npc party is sitting right on the destination. The
                // party enters the portal on foot, upon arrival the npc party
                // attacks. Party members which are on foot get stacked on the
                // entry point and cannot move or flee. This is an unfriendly
                // situation, but something of a corner case. Map hackers can
                // skirt the issue and player's can expect that entering
                // portals involves an element of danger :). The engine won't
                // crash and the player can usually get out of the fix by
                // defeating the npc's.

		class Character *leader = player_party->get_leader();

		if (!leader) {
#ifdef POSITION_CAN_FAIL
			// No party leader => nobody can be placed
			consolePrint("No place to put %s on the combat map!\n",
				     pm->getName());
			return false;
#else // ! POSITION_CAN_FAIL
                        printf("Putting %s on start location [%d %d]\n",
                               pm->getName(), info->x, info->y);
                        info->px = info->x;
                        info->py = info->y;
#endif // ! POSITION_CAN_FAIL
		} else {
                        // init the position info to search again
                        memset(rmap, 0, sizeof(rmap));
                        info->px = leader->getX();
                        info->py = leader->getY();
                        printf("Retrying %s\n", pm->getName());

                        if (myFindSafePosition(info) == -1) {
#ifdef POSITION_CAN_FAIL
                                consolePrint("No place to put %s on the "
                                             "combat map!\n",
                                             pm->getName());
                                return false;
#else // ! POSITION_CAN_FAIL
                                printf("Putting %s on start location "
                                       "[%d %d]\n",
                                       pm->getName(), info->x, info->y);
                                info->px = info->x;
                                info->py = info->y;
#endif // ! POSITION_CAN_FAIL
                        }
                }
        }

	pm->setX(info->px);
	pm->setY(info->py);

	pm->setPlace(Place);
	mapAddView(pm->getView());

	place_add_object(Place, pm);

	/* Initialize the pc's map view */
	mapCenterView(pm->getView(), pm->getX(), pm->getY());

	// Set the PC's light radius based on ambient light and personal light
	// sources. Fixme: should not assume the sun is visible.
	// int lrad = (int)sqrt(pm->getLight() + Sun.light);
	mapSetRadius(pm->getView(), min(pm->getVisionRadius(),
					MAX_VISION_RADIUS));;
	mapRecomputeLos(pm->getView());

	/* Do some one-time init */
	pm->setPlace(Place);
	// pm->setKilledNotifier(myCharacterKilledNotifier);
	pm->setAlignment(player_party->alignment);
	pm->setCombat(true);

	Combat.n_pcs++;

	return false;
}

        static bool mySetInitialCameraPosition(class Character * pm, void *data)
{
	if (pm->isOnMap()) {
		mapCenterCamera(pm->getX(), pm->getY());
		return true;
	}
	return false;
}

static void mySelectPC(class Character * c)
{
	if (Combat.pm != NULL)
		Combat.pm->select(false);
	Combat.pm = c;
	c->select(true);

	if (Combat.n_pcs == 1 || Combat.pm->isSolo() || Combat.followMode)
		mapCenterCamera(Combat.pm->getX(), Combat.pm->getY());

	mapUpdate(0);
}

static void myExitMap(class Character * c, int dx, int dy)
{
	c->remove();
	if (c->isPlayerControlled()) {

		Combat.n_pcs--;

		// If combat is still raging than the PC has fled; if combat is
		// over then it has simply left the map.
		if (Combat.state == FIGHTING) {
			consolePrint("%s escapes!\n", c->getName());
			if (Combat.n_pcs == 0)
				// Handle the case where the last PC has left
				// the map.
				Retreat();
		} else {
			consolePrint("%s exits\n", c->getName());
			if (Combat.n_pcs == 0)
				Combat.state = DONE;
		}

		return;
	} else {
		consolePrint("%s escapes!\n", c->getName());
		Combat.n_npcs--;
		bool victory = true;
		myForEachNpc(myCheckIfHostile, &victory);
		if (victory)
			Combat.state = VICTORY;
		c->party->setFleeVector(dx, dy);
	}
}

static void myMoveNPC(class Character * c, int dx, int dy)
{
	switch (c->move(dx, dy)) {

	case Character::ExitedMap:
		myExitMap(c, dx, dy);
		break;

	case Character::EngagedEnemy:
		statusRepaint();
		break;
	case Character::WasOccupied:
	case Character::WasImpassable:
	case Character::SlowProgress:
	case Character::MovedOk:
	case Character::SwitchedOccupants:
	case Character::CouldNotSwitchOccupants:
		break;
	}

}

static bool myCanRetreat(int x, int y, int dx, int dy)
{
#ifdef NO_RETREAT
	if (Place == &Combat.place)
		return true;
#endif
	if (Combat.rinfo.type == retreat_info::NO_RETREAT)
		return true;

	if (Combat.rinfo.type == retreat_info::EDGE_RETREAT &&
	    Combat.rinfo.dx == dx && Combat.rinfo.dy == dy)
		return true;

	return false;
}

static bool myExitSoloMode(class Character * pc)
{
	if (!pc->isSolo())
		return false;
	consolePrint("Revert to party mode\n");
	pc->setSolo(false);
	return true;
}

static void myMovePC(class Character * c, int dx, int dy)
{
	if (place_off_map(Place, c->getX() + dx, c->getY() + dy) &&
	    !myCanRetreat(c->getX(), c->getY(), dx, dy)) {
		consolePrint("All party members must exit the same way!\n");
		return;
	}

	switch (c->move(dx, dy)) {
	case Character::MovedOk:
		break;
	case Character::ExitedMap:
		myExitMap(c, dx, dy);

		// The first member to exit from a town map sets the exit: the
		// other members must all leave the same way.
		if (
#ifdef NO_RETREAT
			   Place != &Combat.place &&
#endif
			   Combat.rinfo.type == retreat_info::NO_RETREAT) {
			Combat.rinfo.type = retreat_info::EDGE_RETREAT;
			Combat.rinfo.dx = dx;
			Combat.rinfo.dy = dy;
		}

		break;
	case Character::EngagedEnemy:
		break;
	case Character::WasOccupied:
		// Normally I don't print this message in follow mode unless
		// this is the leader because otherwise I see it all the time
		// as the other party members bump into each other.
		if (!Combat.followMode || c == player_party->get_leader())
			consolePrint("Occupied!\n");
		break;
	case Character::WasImpassable:
		consolePrint("Impassable\n");
		break;
	case Character::SlowProgress:
		consolePrint("Slow progress!\n");
		break;
	case Character::SwitchedOccupants:
		break;
	}

	mapCenterView(c->getView(), c->getX(), c->getY());
	mapRecomputeLos(c->getView());

	if (Combat.n_pcs == 1 || c->isSolo() ||
	    (Combat.followMode && c == player_party->get_leader()))
		mapCenterCamera(c->getX(), c->getY());
}

static bool los_blocked(int Ax, int Ay, int Bx, int By)
{
	// Apply the bresenhaum algorithm to walk the line from (x0, y0) to
	// (x1, y1) and check for visibility at each step. Note that the real
	// intention here is to see if I can fire an arrow from one point to
	// another. The missile flight code in Missile:animate() uses a test
	// for visibility on each tile to determine if a missile is blocked in
	// its flight path (missiles don't have a pmask...).

	int Px = Ax;
	int Py = Ay;

	// Get the distance components
	int dX = Bx - Ax;
	int dY = By - Ay;
	int AdX = abs(dX);
	int AdY = abs(dY);

	// Moving left?
	int Xincr = (Ax > Bx) ? -1 : 1;

	// Moving down?
	int Yincr = (Ay > By) ? -1 : 1;

	// Walk the x-axis
	if (AdX >= AdY) {

		int dPr = AdY << 1;
		int dPru = dPr - (AdX << 1);
		int P = dPr - AdX;

		// For each x
		for (int i = AdX; i >= 0; i--) {

			if (!place_visibility(Place, Px, Py))
				return true;

			if (P > 0) {
				Px += Xincr;
				Py += Yincr;
				P += dPru;
			} else {
				Px += Xincr;
				P += dPr;
			}
		}
	}
	// Walk the y-axis
	else {
		int dPr = AdX << 1;
		int dPru = dPr - (AdY << 1);
		int P = dPr - AdY;

		// For each y
		for (int i = AdY; i >= 0; i--) {

			if (!place_visibility(Place, Px, Py))
				return true;

			if (P > 0) {
				Px += Xincr;
				Py += Yincr;
				P += dPru;
			} else {
				Py += Yincr;
				P += dPr;
			}
		}
	}

	return false;
}

#define MSV_IGNORESLEEPERS (1 << 0)
#define MSV_IGNORELOS      (1 << 1)

static class Character *mySelectVictim(class Character * from,
				       unsigned int *min, int flags)
{
	*min = (unsigned int) -1;	// initialize to max possible value
	class Character *victim = NULL, *remember = NULL;
	unsigned int d;

	// *** Find Player Combatant ***

	if (from->isHostile(player_party->alignment)) {

		// By far the most common case. Iterate over the player array
		// and search for the minimum distance to an active party
		// member.

		for (int j = 0; j < player_party->n_pc; j++) {

			class Character *pm;

			pm = player_party->pc[j];
			if (!pm || !pm->isOnMap() || pm->isDead() ||
			    pm == from || !pm->isVisible())
				continue;

			// If LOS is blocked then this victim cannot be
			// attacked by melee, missile or most spells. So skip
			// it unless the caller does not care.
			if ((flags & MSV_IGNORELOS) == 0 &&
			    los_blocked(from->getX(), from->getY(),
					pm->getX(), pm->getY()))
				continue;

			if (!pm->isHostile(from->getAlignment()))
				// Normally we don't want to attack charmed PCs
				// because they're helping us. But if we can't
				// find another target then remember this one
				// and kill him/her off when all the other
				// targets are gone.
				remember = pm;

			d = place_flying_distance(Place,
						  from->getX(), from->getY(),
						  pm->getX(), pm->getY());

			if (d < *min) {
				*min = d;
				victim = pm;
			}
		}

		if (!victim && remember) {
			// Handle cases where there is no uncharmed target and
			// resort to attcking a charmed target that we remember
			// from the search. Need to recompute the distance.
			victim = remember;
			*min = place_flying_distance(Place,
						     from->getX(), from->getY(),
						     victim->getX(),
						     victim->getY());
		}
		// Now fall through and check if there's an even closer target
		// among the NPC combatants. This way a charmed npc is not
		// igored by its former companions.
	}
	// *** Find NPC Combatant ***

	for (int i = 0; i < Combat.max_n_npcs; i++) {

		class Character *pm = Combat.npcs[i];

		if (pm == NULL ||
		    pm == from ||
		    pm->isDead() ||
		    !pm->isOnMap() ||
		    ((flags & MSV_IGNORESLEEPERS) && pm->isAsleep()))
			continue;

		// If LOS is blocked then this victim cannot be
		// attacked by melee, missile or most spells. So skip
		// it unless the caller does not care.
		if ((flags & MSV_IGNORELOS) == 0 &&
		    los_blocked(from->getX(), from->getY(),
				pm->getX(), pm->getY()))
			continue;

		if (!pm->isHostile(from->getAlignment())) {
			if (!pm->party->isHostile(from->getAlignment()))
				continue;
			// This particular PC is not currently hostile (perhaps
			// due to a charm spell) but his party is, so remember
			// him as a victim of last resort if we can't find
			// someone better.
			remember = pm;
		}

		d = place_flying_distance(Place,
					  from->getX(), from->getY(),
					  pm->getX(), pm->getY());

		if (d < *min) {
			*min = d;
			victim = pm;
		}
	}

	if (!victim && remember) {
		// Handle cases where there is no uncharmed target and
		// resort to attcking a charmed target that we remember
		// from the search. Need to recompute the distance.
		victim = remember;
		*min = place_flying_distance(Place,
					     from->getX(), from->getY(),
					     victim->getX(), victim->getY());
	}

	return victim;
}

static bool myAttack(class Character * pc)
{
	bool committed = false;
	int x, y;

	// If in follow mode, when the leader attacks automatically switch to
	// turn-based mode.
	Combat.followMode = false;

	// Loop over all readied weapons
	for (class ArmsType * weapon = pc->enumerateWeapons();
	     weapon != NULL; weapon = pc->getNextWeapon()) {

		cmdwin_clear();
		cmdwin_print("%s:", pc->getName());

		// Check ammo
		if (!pc->hasAmmo(weapon)) {
			consolePrint("%s has no ammo for %s\n", pc->getName(),
				     weapon->getName());
			continue;
		}
		// Get the target. It's important to do this every time through
		// the loop because the last iteration may have killed the
		// previous target. The getAttackTarget routine will reevaluate
		// the current target.
		class Character *target = pc->getAttackTarget();

		if (weapon->isMissileWeapon()) {
			// Check for interference from any nearby
			// hostiles.
			unsigned int d;
			class Character *near;

			// I really only need to check adjacent squares here...
			near = mySelectVictim(pc, &d, MSV_IGNORESLEEPERS);
			if (near != NULL && d <= 1) {
				consolePrint("%s interferes with %s's %s!\n",
					     near->getName(), pc->getName(),
					     weapon->getName());
				committed = true;
				continue;
			}
		}
		// prompt the user
		cmdwin_clear();
		cmdwin_print("Attack[%s]-", weapon->getName());

		// select the target location
		x = target->getX();
		y = target->getY();
		if (select_target(pc->getX(), pc->getY(), &x, &y,
				  weapon->getRange()) == -1)
			continue;

		// Find the new target under the cursor
		target = (class Character *) place_get_object(Place, x, y,
							      being_layer);
		if (target == NULL) {
			// Use the terrain as the target.
			struct terrain *t;
			t = placeGetTerrain(x, y);
			pc->attackTerrain(x, y);
			cmdwin_print("%s", t->name);

			/* Check for a mech */
			class Mech *mech;
			mech = (class Mech *) place_get_object(Place, x, y,
							       mech_layer);
			if (mech)
				mech->activate(MECH_ATTACK);

		} else {

			// in combat all npc parties and the player party
			// should be removed, so only characters reside at the
			// being layer
			assert(target->isType(CHARACTER_ID));

			cmdwin_print("%s", target->getName());
			consolePrint("%s attacks %s with %s: ",
				     pc->getName(), target->getName(),
				     weapon->getName());

			// Strike the target
			if (pc->attackTarget(target)) {
				consolePrint("%s\n",
					     target->getWoundDescription());
			} else {
				consolePrint("missed!\n");
			}

			// If we hit a party member then show their new hit
			// points in the status window
			if (target->isPlayerControlled())
				statusRepaint();
		}

		// Warn the user if out of ammo
		if (!pc->hasAmmo(pc->getCurrentWeapon()))
			consolePrint("%s out of ammo for %s!\n",
				     pc->getCurrentWeapon()->getName(),
				     weapon->getName());

		// Once the player uses a weapon he can't cancel out of the
		// attack and continue his round with a different command.
		committed = true;
		pc->addExperience(XP_PER_ATTACK);
	}

	return committed;
}

static bool myNPCAttackNearest(class Character * npc, class Character * pc,
			       int d)
{
	bool ret = false;

	for (class ArmsType * weapon = npc->enumerateWeapons(); weapon != NULL;
	     weapon = npc->getNextWeapon()) {

		if (d > weapon->getRange() || !npc->hasAmmo(weapon)) {
			printf("%s's %s out of range [%d>%d]\n",
			       npc->getName(), weapon->getName(),
			       d, weapon->getRange());
			continue;
		}

		if (d <= 1 && weapon->isMissileWeapon()) {
			// Handle missile weapon interference
			printf("%s's %s interfered\n", npc->getName(),
			       weapon->getName());
			continue;
		}

		if (npc->attackTarget(pc)) {
			consolePrint("%s %s\n", pc->getName(),
				     pc->getWoundDescription());
		} else {
			consolePrint("%s missed!\n", pc->getName());
		}

		if (!npc->hasAmmo(weapon))
			consolePrint("%s out of ammo!\n", weapon->getName());
		statusRepaint();
		ret = true;

		if (pc->isDead())
			break;
	}

	if (ret && ((class Character *) npc)->needToRearm())
		((class Character *) npc)->armThyself();

	return ret;
}

static bool myNPCEnchantNearest(class Character * npc, class Character * pc,
				int d)
{
	class Spell *spell;
	int i;

	printf("%s considering spells...\n", npc->getName());

	// Enumerate all the known spells for this npc
	for (i = 0; i < npc->species->n_spells; i++) {

		spell = npc->species->spells[i];

		printf("\t%s...", spell->getName());

		// Check if the NPC has enough mana
		if (spell->cost > npc->getMana()) {
			printf("not enough mana [%d > %d]\n", spell->cost,
			       npc->getMana());
			continue;
		}
		// Check if the nearest is in range
		if (d > spell->range) {
			printf("out-of-range\n");
			continue;
		}
		// Cast the spell
		printf("ok!\n");
		spell->cast(npc, pc, 0, 0, 0);
		return true;
	}

	return false;
}

static void myApplyGenericEffects(class Character * c, int effects)
{
	if (effects & TERRAIN_BURN) {
		c->changeHp(-DAMAGE_FIRE * Combat.turns_per_round);
		consolePrint("%s burning-%s!\n", c->getName(),
			     c->getWoundDescription());
		consoleRepaint();
	}
	if (effects & TERRAIN_POISON && !c->isPoisoned()) {
		c->setPoison(true);
		c->changeHp(-DAMAGE_POISON * Combat.turns_per_round);
		consolePrint("%s poisoned-%s!\n", c->getName(),
			     c->getWoundDescription());
		consoleRepaint();
	}
}

static void myApplyExistingEffects(class Character * c)
{
	if (c->isPoisoned()) {
		c->changeHp(-DAMAGE_POISON * Combat.turns_per_round);
		consolePrint("%s poisoned-%s\n", c->getName(),
			     c->getWoundDescription());
	}
	if (c->isAsleep()) {

		if (Combat.state != CAMPING) {
			consolePrint("%s sleeping...", c->getName());
			if ((random() % 100) < PROB_AWAKEN) {
				c->awaken();
				consolePrint("awakes!");
			}
			consolePrint("\n");
		} else {
			// Hack: set the "rested" flag if anybody actually gets
			// any rest...
			if (!Combat.rested && c->getRestCredits())
				Combat.rested = true;
			c->rest(Combat.turns_per_round / TURNS_PER_HOUR);
		}
	}
}

static bool doApplyCombatEffects(class Character * c, void *data)
{
	if (!c->isOnMap())
		// Handle the case where the character is a PC which has fled
		return false;

	// Apply lingering effects like poison, sleep, etc.
	myApplyExistingEffects(c);

	if (c->isDead())
		// Handle cases where the character dies as a result of
		// existing effects.
		return false;

	int effects = 0;

	// Get the terrain effects.
	struct terrain *terrain = placeGetTerrain(c->getX(), c->getY());
	effects |= terrain->effects;

	// Get any field effects. Note that only the topmost field at this
	// tile (if any) is used.
	class Field *field;
	field = (class Field *) place_get_object(c->getPlace(), c->getX(),
						 c->getY(), field_layer);
	if (field != NULL)
		effects |= field->getObjectType()->getEffects();

	// Apply the combined effects once per character per round.
	myApplyGenericEffects(c, effects);
	return false;
}

static bool myRunNpc(class Character * npc, void *data)
{
	unsigned int min = (unsigned int) -1;	/* max possible */
	class Character *nearest = 0;
	struct astar_node *path;
	struct astar_search_info as_info;

	// fixme: this logic probably belongs in the NPC class. But as long as
	// I'm allowing the NPC to directly access the PC array in order to
	// hunt for targets I'll leave things here.

	if (Combat.state == DEFEAT) {
		// Handle the case where a previous NPC ended combat by killing
		// the last PC.
		Defeat();
		return false;	// fixme -- return true 
	}

	if (npc->isIncapacitated())
		// Handle the case where the NPC fled, is sleeping, etc.
		return false;

	if (npc->isFleeing()) {
		// Handle the case where the NPC is still fleeing.
		printf("%s fleeing\n", npc->getName());
		if (npc->flee() == Character::ExitedMap) {
			myExitMap(npc, npc->getFleeDx(), npc->getFleeDy());
		}
		return false;
	}
	// First check for a victim within LOS.
	nearest = mySelectVictim(npc, &min, 0);
	if (nearest) {
		// printf("%s selected %s for victim\n", npc->getName(),
		// nearest->getName());
	} else {
		printf("%s cannot find a victim in los\n", npc->getName());
		// 
		nearest = npc->quarry;
		if (nearest && nearest != npc && nearest->isOnMap() &&
		    !nearest->isDead() && nearest->isVisible()) {
			// printf("Aha! %s remembers previous victim %s and "
			// "gives chase!\n", npc->getName(), 
			// nearest->getName());
			min = place_flying_distance(Place,
						    npc->getX(), npc->getY(),
						    nearest->getX(),
						    nearest->getY());
		} else {
			npc->quarry = 0;
			return false;
		}
	}

	npc->quarry = nearest;

	// Don't allow NPCs to attack characters they can't see (this gives
	// their bowmen an unfair advantage).
	if (min > (unsigned int) npc->getVisionRadius()) {
		// printf("%s can't see %s at range %d\n", npc->getName(),
		// nearest->getName(), min);
	} else {
		// Check if there are any spells this NPC can cast on the
		// nearest PC.
		if (myNPCEnchantNearest(npc, nearest, min)) {
			return false;
		}
		// Check if the nearest PC is already in weapon range and, if
		// so, attack. If the nearest was attacked then this NPCs turn
		// is over.
		if (myNPCAttackNearest(npc, nearest, min)) {
			// printf("%s attacked %s\n", npc->getName(), 
			// nearest->getName());
			return false;
		}
	}

	// printf("%s searching for path to %s\n", npc->getName(), 
	// nearest->getName());

	/* Find a path to the nearest member */
	memset(&as_info, 0, sizeof(as_info));
	as_info.x0 = npc->getX();
	as_info.y0 = npc->getY();
	as_info.x1 = nearest->getX();
	as_info.y1 = nearest->getY();
	path = place_find_path(Place, &as_info, npc->getPmask());

	if (!path)
		// Handle the case where no path is found by doing nothing this
		// turn.
		return false;

	if (path->next) {
		// The path is more than zero steps long (should always be the
		// case, really). Take one step along that path.
		myMoveNPC(npc,
			  path->next->x - npc->getX(),
			  path->next->y - npc->getY());

	}
	// Destroy the path. This is not very efficient since often we
	// could reuse this path on the next round. FIXME.
	astar_path_destroy(path);

	return false;
}

static void doNpcRound(void)
{
	printf("--- NPCs ---\n");

	myForEachNpc((bool(*)(class Character *, void *)) doApplyCombatEffects,
		     NULL);

	if (Combat.state == VICTORY) {
		// Handle the case where combat effects killed off the last of
		// the NPCs.
		Victory();
		return;
	}

	myForEachNpc(myRunNpc, NULL);

	if (Combat.state == DEFEAT) {
		// Handle the case where the last NPC killed the last PC.
		Defeat();
	} else if (Combat.state == VICTORY) {
		// Handle the case where the last NPC just escaped
		Victory();
	}

}

static bool myEnterSoloMode(class Character * pc, int index)
{
	if (index >= player_party->n_pc)
		return false;

	class Character *newpc = player_party->pc[index];
	if (newpc == NULL || !newpc->setSolo(true))
		return false;

	if (newpc != pc)
		// Switching solo mode from one pc to another.
		pc->setSolo(false);

	Combat.followMode = false;
	consolePrint("Switch to %s\n", newpc->getName());
	mySelectPC(newpc);
	return true;
}

static bool pc_end_combat(class Character * pc, void *data)
{
	pc->remove();
	pc->setCombat(false);
	if (pc->isOnMap() && !pc->isDead())
		pc->addExperience(XP_PER_COMBAT);
	if (pc->wasElevated()) {
		consolePrint("%s was elevated to level %d!\n", pc->getName(),
			     pc->getLevel());
		pc->setElevated(false);
	}

	/* Bugfix: if ambushed while camping without a guard - or even if just
	 * enchanted by a sleep spell - a character might be asleep on exit
	 * from combat. I don't really want to allow this since in non-combat
	 * mode I don't periodically roll to wake up sleepers like I do every
	 * turn in combat. */
	if (pc->isAsleep()) {
		pc->awaken();
		statusRepaint();
	}

	return false;
}

// Automatically pathfind and move all the party members to the party leader's
// position.
//
// max_path_len - max number of steps to allow for a path, or -1 for no limit
//
// returns true if everyony can find a path in max_path_len steps, false
// otherwise.
static bool combat_rendezvous_party(int max_path_len)
{
	int i, rx, ry;
	bool abort = false;
	bool done;
	class Character *leader;

	// Use the party leader's position as the rendezvous point.
	leader = player_party->get_leader();
	assert(leader);
	rx = leader->getX();
	ry = leader->getY();

	printf("rendezvous at [%d %d] on %s\n", rx, ry, leader->getName());

	// Center the camera on the rendezvous point.
	mapCenterCamera(rx, ry);
	mapUpdate(0);
	consolePrint("Rendezvous...");
	consoleRepaint();

	// Have each party member find and store a path to the rendezvous
	// point.
	for (i = 0; i < player_party->n_pc; i++) {

		struct astar_search_info as_info;
		class Character *pc = player_party->pc[i];

		if (!pc || pc->isDead() || !pc->isOnMap() || pc == leader ||
                    (pc->getX() == rx && pc->getY() == ry))
			continue;

		memset(&as_info, 0, sizeof(as_info));
		as_info.x0 = pc->getX();
		as_info.y0 = pc->getY();
		as_info.x1 = rx;
		as_info.y1 = ry;
		as_info.flags = PFLAG_IGNOREBEINGS;
		pc->path = place_find_path(Place, &as_info, pc->getPmask());

		if (!pc->path) {
			consolePrint("%s cannot make the rendezvous!\n",
				     pc->getName());
			mapCenterCamera(pc->getX(), pc->getY());
			mapUpdate(0);
			abort = true;
		} else if (max_path_len > 0 && 
                           pc->path->len > max_path_len) {
                        consolePrint("%s is too far away!\n", pc->getName());
			mapCenterCamera(pc->getX(), pc->getY());
			mapUpdate(0);
			abort = true;
                }
	}

	// If anyone could not find a path then abort.
	if (abort) {
		for (i = 0; i < player_party->n_pc; i++) {

			class Character *pc = player_party->pc[i];

			if (pc->path) {
				astar_path_destroy(pc->path);
				pc->path = 0;
			}
		}
		return false;
	}
	// Otherwise everyone has a path, so have them follow it to the
	// rendezvous point.
	done = false;
	while (!done) {
		done = true;
		consolePrint(".");
		for (i = 0; i < player_party->n_pc; i++) {

			struct astar_node *tmp;
			class Character *pc = player_party->pc[i];

			// already arrived in an earlier iteration
			if (!pc->path)
				continue;

			// should always be at least two nodes
			assert(pc->path->next);

			// arrived
			if (pc->path->next->x == rx && pc->path->next->y == ry) {
				astar_node_destroy(pc->path);
				pc->path = 0;
				pc->remove();	// try this... seems ok
				continue;
			}

			done = false;

			// move one step
			pc->move(pc->path->next->x - pc->getX(),
				 pc->path->next->y - pc->getY());

			// clean up used path node
			tmp = pc->path;
			pc->path = pc->path->next;
			astar_node_destroy(tmp);

			mapUpdate(0);
			// SDL_Delay(100);
		}
	}

	consolePrint("Ok!\n");
	Combat.state = DONE;
	return true;

}

static bool combat_enter_portal(class Character * c)
{
        // First of all, note that this is the only way - other than casting a
        // teleportal spell - to exit a dungeon. I have some special rules to
        // apply here. These rules enforce the principle that all the members
        // of the player party must be in the same place. This means that if
        // one party member exits a dungeon, all others must exit the same
        // way. And since party members may have different passability masks,
        // and map terrains can be changed at runtime, exiting a dungeon must
        // be "atomic". Everybody has to do it at the same time, or nobody does
        // it all.
        //
        // So, here are the rules.
        //
        // #1. The party must be in follow mode
        // #2. All party members must be able to rendezvous in n steps
        //
        // If entry succeeds then the members all rendezvous and the party
        // exits the dungeon via the portal. Otherwise we complain to the user
        // and reject the entry.


	class Portal *portal;

	cmdwin_clear();
	cmdwin_print("Enter-");

	// Check for a portal
	portal = place_get_portal(Place, c->getX(), c->getY());
	if (!portal) {
		cmdwin_print("Nothing!");
		return false;
	}

	// fixme: instead of asserting here we should add code to the loader to
	// remove any portals to terrain combat maps.
	assert(Place != &Combat.place);

        // Check if in follow mode
        if (! Combat.followMode) {
                cmdwin_print("Must be in follow mode!");
                return false;
        }

        // Ok, check for rendezvous. Not too sure if this next provides enough
        // steps or not. Feel free to tune it.
        if (! combat_rendezvous_party(Combat.n_pcs * 2)) {
                cmdwin_print("Party must be together!");
                return false;
        }

	// Great, it worked. Setup to exit combat.
	cmdwin_print("Ok");
        assert(Combat.rinfo.type == retreat_info::NO_RETREAT);
        Combat.rinfo.type = retreat_info::PORTAL_RETREAT;
        Combat.rinfo.x = c->getX();
        Combat.rinfo.y = c->getY();


	// This all we need to do. Upon exit from combat we'll move the player
	// party to the place at the end of the portal.
	myExitMap(c, 0, 0);
	return true;
}


static bool myExitCombat(void)
{
	// The player has hit the ESC key.

	if (Combat.state != LOOTING) {
		// Combat must be over.
		consolePrint("Not while foes remain!\n");
		return false;
	}

        if (Place != &Combat.place) {
                // Can't escape from towns or dungeons -- have to use
                // an exit.  return false;
                return combat_rendezvous_party(-1);
        }

        consolePrint("Exit!\n");
        Combat.state = DONE;
        return true;
}

static bool myFollow(class Character * pc)
{
	if (Combat.followMode) {
		Combat.followMode = false;
		return true;
	}

	Combat.followMode = true;
	if (pc)
		myExitSoloMode(pc);
	return true;
}

static bool myPcCommandHandler(struct KeyHandler *kh, int key)
{
	int dir;
	bool ret;
	class Character *pc = (class Character *) kh->data;
	static int unshift[] = { KEY_NORTH, KEY_SOUTH, KEY_EAST, KEY_WEST };

	switch (key) {
	case KEY_NORTH:
	case KEY_EAST:
	case KEY_SOUTH:
	case KEY_WEST:
		dir = keyToDirection(key);
		myMovePC(pc, directionToDx(dir), directionToDy(dir));
		ret = true;
		break;
	case KEY_SHIFT_NORTH:
	case KEY_SHIFT_EAST:
	case KEY_SHIFT_SOUTH:
	case KEY_SHIFT_WEST:
		key = unshift[(key - KEY_SHIFT_NORTH)];
		dir = keyToDirection(key);
		mapMoveCamera(directionToDx(dir), directionToDy(dir));
		mapUpdate(0);
		ret = false;
		break;
	case 'a':
		ret = myAttack(pc);
		break;
	case 'c':
		ret = cmdCastSpell(pc);	// myCastSpell(pc);
		break;
	case 'e':
		ret = combat_enter_portal(pc);
		break;
	case 'f':
		ret = myFollow(pc);
		break;
	case 'g':
		cmdGet(pc->getX(), pc->getY(), Combat.state == LOOTING);
		ret = true;
		break;
	case 'h':
		ret = cmdHandle(pc);
		break;
	case 'l':
                // SAM: Changing (L)ook command 
                // from "look at 1 tile" to a "Look Mode"
		ret = cmdLook(pc->getX(), pc->getY());
		break;
	case 'o':
		ret = cmdOpen(pc);
		break;
	case 'q':
		ret = cmdQuit();
		break;
	case 'r':
		ret = cmdReady(pc);
		break;
	case 'u':
		ret = cmdUse(pc);
		break;
        case 'x':
                ret = cmdXamine(pc); // SAM: 1st step towards new (L)ook cmd...
                break;
	case 'z':
		ret = cmdZtats(pc);
		break;
	case ' ':
		cmdwin_print("Pass");
		ret = true;
		break;
	case SDLK_1:
	case SDLK_2:
	case SDLK_3:
	case SDLK_4:
	case SDLK_5:
	case SDLK_6:
	case SDLK_7:
	case SDLK_8:
	case SDLK_9:
		ret = myEnterSoloMode(pc, key - SDLK_1);
		break;
	case SDLK_0:
		ret = myExitSoloMode(pc);
		break;
	case SDLK_ESCAPE:
		ret = myExitCombat();
		break;
	default:
		ret = false;
		break;
	}

	return ret;
}

static bool myRunPc(class Character * pc, void *data)
{
	if (Quit || !pc->isOnMap() || pc->isDead() || pc->isAsleep() ||
	    Combat.state == DONE)
		return false;

	if (Combat.pm != NULL && Combat.pm->isSolo() && Combat.pm != pc)
		// Handle the case where a previous PC selected solo
		// mode in this round
		return false;

	// *** Charmed ***

	if (pc->isHostile(player_party->alignment)) {
		if (pc->isSolo()) {
			// Make sure we switch back to party mode!
			myExitSoloMode((class Character *) pc);
		}
		myRunNpc(pc, data);
		return false;
	}
	// *** Follow Mode ***

	class Character *leader;
	if (Combat.followMode && ((leader = player_party->get_leader()) != pc)) {

		struct astar_node *path;
		struct astar_search_info as_info;
		int d;

		d = place_flying_distance(Place, pc->getX(), pc->getY(),
					  leader->getX(), leader->getY());
		if (d < 2)
			return false;

		memset(&as_info, 0, sizeof(as_info));
		as_info.x0 = pc->getX();
		as_info.y0 = pc->getY();
		as_info.x1 = leader->getX();
		as_info.y1 = leader->getY();
		as_info.flags = PFLAG_IGNOREBEINGS;
		path = place_find_path(Place, &as_info, pc->getPmask());
		if (!path)
			return false;

		if (path->next) {
			myMovePC(pc,
				 path->next->x - pc->getX(),
				 path->next->y - pc->getY());
		}

		astar_path_destroy(path);

		return false;
	}
	// *** Normal Turn ***

	// Highlight the selected PC and shows its name and weapons
	mySelectPC((class Character *) pc);
	cmdwin_clear();
	cmdwin_print("%s:", pc->getName());

#ifdef SHOW_WEAPONS_ON_PROMPT
	consolePrint("%s[", pc->getName());
	for (class ArmsType * weapon = pc->enumerateWeapons(); weapon != NULL;
	     weapon = pc->getNextWeapon()) {
		consolePrint("%s ", weapon->getName());
	}
	cmdwin_backspace(1);
	cmdwin_print("]:");
#endif

	struct KeyHandler kh;
	kh.fx = myPcCommandHandler;
	kh.data = pc;
	eventPushKeyHandler(&kh);
	eventHandle();
	eventPopKeyHandler();

	// *** Analyze Outcome ***

	if (Combat.state == VICTORY) {
		// This PC just killed the last NPC.
		Victory();
	} else if (Combat.state == RETREAT) {
		// This PC was the last PC and it just left the map.
		Retreat();
	}

	return false;
}

static void doPlayerRound(void)
{
	if (Combat.state == CAMPING) {
		// Show "Zzz..."
		consolePrint(".");
		consoleRepaint();
	}
	// At the start of the player's round I apply all "passive" effects to
	// each PC.
	player_party->for_each_member(doApplyCombatEffects, NULL);

	if (Combat.state == DEFEAT) {
		// The last PC was killed by combat effects.
		Defeat();
		return;
	}

	if (Combat.state == CAMPING) {
		return;
	}

	if (Combat.pm != NULL && Combat.pm->isSolo()) {
		// Handle the case where the player has selected a single PC to
		// run in "solo" mode. In this case we don't iterate over the
		// other PCs.

		if (Combat.pm->isIncapacitated()) {
			// Automatically exit solo mode if something happens to
			// the solo mode PC.
			myExitSoloMode(Combat.pm);
		} else {
			myRunPc(Combat.pm, NULL);
			return;
		}
	}

	player_party->for_each_member(myRunPc, NULL);
}

static void overlay_map(struct terrain_map *map, struct position_info *pinfo)
{
	int x = 0, y = 0;

	assert(pinfo->dx || pinfo->dy);
	assert(!pinfo->dx || !pinfo->dy);

	// Clone the map so we can make a rotated copy.
	map = terrain_map_clone(map);
	if (!map) {
		err("Failed to allocate temporary terrain map");
		return;
	}
	// Rotate the map so that north faces the opponent.
	terrain_map_rotate(map, vector_to_rotation(pinfo->dx, pinfo->dy));

	// Position the map against the boundary dividing the map.
	if (pinfo->dx < 0) {
		// facing west, shift map west toward edge
		x = place_w(Place) / 2;
		y = (place_h(Place) - map->h) / 2;
	} else if (pinfo->dx > 0) {
		// facing east, shift map east toward edge
		x = place_w(Place) / 2 - map->w;
		y = (place_h(Place) - map->h) / 2;
	} else if (pinfo->dy < 0) {
		// facing north, shift map north toward edge
		x = (place_w(Place) - map->w) / 2;
		y = place_h(Place) / 2;
	} else if (pinfo->dy > 0) {
		// facing south, shift map south toward edge
		x = (place_w(Place) - map->w) / 2;
		y = place_h(Place) / 2 - map->h;
	}
	// Adjust the party's starting position to be centered on the overlap
	// map.
	set_party_initial_position(pinfo, x + map->w / 2, y + map->h / 2);

	// Blit the rotated map centered on the given coordinates.
	terrain_map_blit(Place->terrain_map, x, y, map, 0, 0, map->w, map->h);

	// Cleanup.
	terrain_map_destroy(map);
}

static void myPutEnemy(class NpcParty * foe, struct position_info *pinfo)
{
	foe->forEachMember(myPutNpc, pinfo);
	Combat.max_n_npcs = Combat.n_npcs;
	list_add(&Combat.parties, &foe->container_link.list);
}

static bool myPositionEnemy(class NpcParty * foe, int dx, int dy, bool defend)
{
	assert(foe->getSize());

	myFillPositionInfo(&foe->pinfo, foe->getX(), foe->getY(), dx, dy,
			   defend);
	foe->pinfo.formation = foe->get_formation();
	if (!foe->pinfo.formation)
		foe->pinfo.formation = &default_formation;

	// Check for a map overlay.
	if (foe->vehicle && foe->vehicle->getObjectType()->map &&
	    Place == &Combat.place)
		overlay_map(foe->vehicle->getObjectType()->map, &foe->pinfo);

	Combat.enemy_vehicle = foe->vehicle;
	foe->disembark();
	foe->remove();
	myPutEnemy(foe, &foe->pinfo);
	return (foe->pinfo.placed != 0);
}

static bool pc_wakeup_after_camping(class Character * pc, void *data)
{
	pc->awaken();
	return false;
}

static bool pc_wakeup_for_ambush(class Character * pc, void *data)
{
	class Character *guard = (class Character *) data;
	if (pc->isAsleep()) {
		if (guard ||
		    pc->getRestCredits() == 0 ||
		    (random() % 100) < PROB_AWAKEN) {
			pc->awaken();
			if (guard)
				consolePrint("%s wakes up %s\n",
					     guard->getName(), pc->getName());
			else
				consolePrint("%s wakes up!\n", pc->getName());
		}
	}

	return false;
}

static void random_ambush(void)
{
	class NpcParty *foe;
	int dir;

	foe = place_random_encounter(Place->location.place);
	if (!foe)
		return;

	// Roll to pick a direction
	dir = ((random() % 4) + 1) * 2 - 1;

        // Try to place the enemy party. This can fail. For example, the place
        // might randomly generate a party of nixies as the npc party while the
        // player is camped on dry land.
	if (!myPositionEnemy(foe, directionToDx(dir), directionToDy(dir), 
                             false)) {
                printf("ambush failed for %s\n", foe->getName());
                // I think the party will get cleaned up on exit...
                return;
        }

	Combat.state = FIGHTING;
	Combat.turns_per_round = 1;

	consolePrint("\n*** Ambush ***\n\n");

	// Roll to wakeup party members
	player_party->for_each_member(pc_wakeup_for_ambush, Combat.guard);

}

static void myEventLoop(void)
{
	while (!Quit && Combat.state != DONE) {
		// Loop until the player opts to quit or combat is over

		doPlayerRound();

		turnAdvance(Combat.turns_per_round);

		if (!TurnChanged)
			// This handles Quicken and Time Stop...
			continue;

		if (!Quit && Combat.state != DONE)
			// Handle the case where combat did not finish during
			// the player's round.
			doNpcRound();

		if (Combat.state == CAMPING) {

			SDL_Event event;

			SDL_Delay(500);

			// Hack: drain the backlog of tick events. The tick
			// thread just posted a bunch of them while we were in
			// the delay. By handling them now I can make animation
			// appear somewhat normal. If I don't then they pile up
			// on the event queue and the next time I go to check
			// it I end up doing all the pent-up sprite animations
			// at once, which makes the animated sprites dance like
			// electrocuted puppets.
			while (SDL_PollEvent(&event)) {
				if (event.type != SDL_USEREVENT)
					continue;
				Tick++;
				wqRunToTick(&TickWorkQueue, Tick);
				mapUpdate(REPAINT_IF_DIRTY);
			}

			// If the player is camping while aboard a vehicle and
			// a watch is on guard then the vehicle will regain 10%
			// of its hit points per hour.
			if (player_party->vehicle && Combat.guard) {
				player_party->vehicle->repair();
			}
			// Check for the end of camping.
			if (Turn >= Combat.end_of_camping_turn) {
				consoleNewline();
				Combat.state = DONE;
				player_party->
				    for_each_member(pc_wakeup_after_camping, 0);
				if (Combat.rested)
					consolePrint("Party rested!\n");
				else
					consolePrint("No effect...\n");
			} else {
				random_ambush();
			}
		}
		// Note: always update the clock before the turn wq. For
		// example, when entering a place all the NPC parties use the
		// wall clock time to synchronize their schedules, so it needs
		// to be set BEFORE calling them.
		clockUpdate();

		if (!Quit && Combat.state != DONE)
			placeAdvanceCombatTurn();

		foogodAdvanceTurns();
		// playerAdvanceTurns();
		skyAdvanceTurns();
		windAdvanceTurns();

		wqRunToTick(&TurnWorkQueue, Turn);

		mapUpdate(REPAINT_IF_DIRTY);
	}
}

static void myCleanupObject(class Object * object, void *data)
{
	object->remove();
	delete object;
}

static void myFindAndPositionEnemy(class Object * obj, void *data)
{
	struct v2 *info;

	if (!obj->isType(NPCPARTY_ID))
		return;

	info = (struct v2 *) data;
	if (((class NpcParty *) obj)->isHostile(player_party->alignment))
		Combat.state = FIGHTING;
	myPositionEnemy((class NpcParty *) obj, info->dx, info->dy, false);
}

static bool pc_camp(class Character * pc, void *data)
{
	struct combat_info *info;

	info = (struct combat_info *) data;
	if (pc == info->guard)
		return false;
	pc->changeSleep(info->hours * TURNS_PER_HOUR);
	return false;
}

int combatInit(void)
{
	// This is called once at the beginning of the game
	memset(&Combat, 0, sizeof(Combat));

	// srandom(0/*time(0)*/); done in common.c

	/* Initialize the place to safe defaults */
	list_init(&Combat.place.list);
	list_init(&Combat.place.vehicles);
	Combat.place.type = combat_place;
	Combat.place.name = "Combat map";
	Combat.place.objects = hash_create(11);
	Combat.state = DONE;
	list_init(&Combat.parties);

	default_formation.n = sizeof(default_formation_entries) /
	    sizeof(default_formation_entries[0]);
	default_formation.entry = default_formation_entries;

	return 0;
}

int combatLoad(class Loader *loader)
{        
        if (!loader->matchToken('{'))
                return -1;

        while (!loader->matchToken('}')) {
                if (loader->matchWord("enter")) {
                        if (!loader->getString(&Combat.sound_enter))
                                goto fail;
                } else if (loader->matchWord("defeat")) {
                        if (!loader->getString(&Combat.sound_defeat))
                                goto fail;
                } else if (loader->matchWord("victory")) {
                        if (!loader->getString(&Combat.sound_victory))
                                goto fail;
                } else {
                        loader->setError("unknown field '%s'",
                                         loader->getLexeme());
                        goto fail;
                }
        }

        return 0;

 fail:
        return -1;
}

#ifdef USE_OLD_MAP_FILL

static void fill_map_half(struct terrain_map *map, int dx, int dy,
			  struct terrain *terrain)
{
	assert(dx || dy);

	if (dx < 0) {
		// facing west, fill east half
		terrain_map_fill(map, map->w / 2, 0, map->w / 2, map->h,
				 terrain);
	} else if (dx > 0) {
		// facing east, fill west half
		terrain_map_fill(map, 0, 0, map->w / 2, map->h, terrain);
	} else if (dy < 0) {
		// facing north, fill south half
		terrain_map_fill(map, 0, map->h / 2, map->w, map->h / 2,
				 terrain);
	} else if (dy > 0) {
		// facing south, fill north half
		terrain_map_fill(map, 0, 0, map->w, map->h / 2, terrain);
	}
}

#else // ! USE_OLD_MAP_FILL

static void fill_temporary_terrain_map(struct terrain_map *map, 
                                       struct place *place, int x, int y, 
                                       int dx, int dy)
{
        struct terrain_map *tile_map;
        struct terrain *terrain;
        int dst_x = 0, dst_y = 0, src_x = 0, src_y = 0, src_w = 0, src_h = 0;

        assert(dx || dy);
        assert(!(dx && dy));

        if (dx < 0) {

		// facing west, fill east half
                dst_x = map->w/2;
                dst_y = 0;
                src_x = 0;
                src_y = 0;
                src_w = map->w/2;
                src_h = map->h;

        } else if (dx > 0) {

		// facing east, fill west half
                dst_x = 0;
                dst_y = 0;
                src_x = map->w/2;
                src_y = 0;
                src_w = map->w/2;
                src_h = map->h;

        } else if (dy < 0) {

		// facing north, fill south half
                dst_x = 0;
                dst_y = map->h/2;
                src_x = 0;
                src_y = 0;
                src_w = map->w;
                src_h = map->h/2;

        } else if (dy > 0) {

		// facing south, fill north half
                dst_x = 0;
                dst_y = 0;
                src_x = 0;
                src_y = map->h/2;
                src_w = map->w;
                src_h = map->h/2;
        }

        tile_map = place_get_combat_terrain_map(place, x, y);
        
        if (tile_map) {

                // fixme -- instead of crashing at runtime, check for properly
                // sized combat maps at load time (this will require the combat
                // map dimensions to be also specified at load time or at least
                // well-documented for map developers)
                assert(tile_map->w >= (src_x + src_w));
                assert(tile_map->h >= (src_y + src_h));

                // Use the combat map associated with the terrain type.
                terrain_map_blit(map, dst_x, dst_y, tile_map, src_x, src_y,
                                 src_w, src_h);
        } else {
                // Fill with the terrain type.
                terrain = place_get_terrain(place, x, y);
                terrain_map_fill(map, dst_x, dst_y, src_w, src_h, terrain);
        }
}

#endif  // ! USE_OLD_MAP_FILL

static struct terrain_map *create_camping_map(struct place *place, int x, 
                                              int y)
{
        struct terrain_map *map;

        map = place_get_combat_terrain_map(place, x, y);
        if (map)
                return terrain_map_clone(map);

        map = terrain_map_create("tmp_combat_map", COMBAT_MAP_W, COMBAT_MAP_H);
        if (map) {
                terrain_map_fill(map, 0, 0, COMBAT_MAP_W, COMBAT_MAP_H, 
                                 place_get_terrain(place, x, y));
        }

        return map;
}

static struct terrain_map *create_temporary_terrain_map(struct combat_info
							*info)
{
	struct terrain_map *map;
        int player_dx, player_dy, npc_dx, npc_dy;

        // If there is no enemy then create a map derived entirely from the
        // player party's tile. This is the case for camping and zoom-in.

        if (!info->move->npc_party) {
                return create_camping_map(info->move->place, info->move->x, 
                                          info->move->y);
        }

        // Otherwise create a map derived partially from the enemy's tile and
        // partially from the player's tile.

	map = terrain_map_create("tmp_combat_map", COMBAT_MAP_W, COMBAT_MAP_H);
	if (!map)
		return 0;

        // Determine orientation for both parties.

        if (info->defend) {
                player_dx = -info->move->dx;
                player_dy = -info->move->dy;
                npc_dx    = info->move->dx;
                npc_dy    = info->move->dy;
        } else {
                player_dx = info->move->dx;
                player_dy = info->move->dy;
                npc_dx    = -info->move->dx;
                npc_dy    = -info->move->dy;
        }

        // Fill the player's half of the combat map

        fill_temporary_terrain_map(map, player_party->getPlace(), 
                                   player_party->getX(), player_party->getY(), 
                                   player_dx, player_dy);

        // Fill the npc party's half of the combat map

	fill_temporary_terrain_map(map, info->move->npc_party->getPlace(),
                                   info->move->npc_party->getX(),
                                   info->move->npc_party->getY(), npc_dx, 
                                   npc_dy);

	return map;
}

static bool position_player_party(struct combat_info *cinfo)
{
	class Vehicle *vehicle;

	myFillPositionInfo(&player_party->pinfo, cinfo->move->x,
			   cinfo->move->y,
			   cinfo->move->dx, cinfo->move->dy, cinfo->defend);
	player_party->pinfo.formation = player_party->get_formation();
	if (!player_party->pinfo.formation)
		player_party->pinfo.formation = &default_formation;

	// Check for map overlays. First check if the player is in a vehicle
	// with a map.
	if (player_party->vehicle &&
	    player_party->vehicle->getObjectType()->map &&
	    Place == &Combat.place) {
		overlay_map(player_party->vehicle->getObjectType()->map,
			    &player_party->pinfo);

	}
	// Next check if the player is OVER (on the map) but not in a vehicle
	// on the map. Note: this only applies to non-dungeon combat, and in a
	// series of dungeon combats the player party may not have a place
	// (because we remove it just below and the calling code does not
	// relocate the player party until it returns to a town or wilderness.
	else if (player_party->getPlace() &&
                 (vehicle = place_get_vehicle(player_party->getPlace(),
					      player_party->getX(),
					      player_party->getY())) &&
		 vehicle->getObjectType()->map) {
		overlay_map(vehicle->getObjectType()->map,
			    &player_party->pinfo);
	}
	// Finally, since there is no vehicle map check for a camping map.
	else if (cinfo->camping && player_party->campsite_map) {
		overlay_map(player_party->campsite_map, &player_party->pinfo);
	}

	player_party->remove();
	player_party->for_each_member(myPutPC, &player_party->pinfo);

	// Check if entrance was blocked. If so then briefly show the part of
	// the combat map where the player party would have been placed so that
	// the user can see why combat failed.
	if (!player_party->get_leader() ||
	    !player_party->get_leader()->isOnMap()) {
		consolePrint("\n*** ABORT ***\n\n");
		consolePrint("Combat aborted (battlefield impassable)\n");
		mapCenterCamera(player_party->pinfo.x, player_party->pinfo.y);
		mapRmView(ALL_VIEWS);
		mapUpdate(REPAINT_NO_LOS);
		SDL_Delay(500);
		mapAddView(player_party->view);
		return false;
	}

	player_party->context = CONTEXT_COMBAT;
	return true;
}

bool combat_enter(struct combat_info * info)
{
	class Character *leader;
	int rx, ry, original_context;
	struct list *pelem;

	if (player_party->all_dead())
		// Yes, this can happen in some rare circumstances...
		return false;

	// *** Memorize Entry State ***

	original_context = player_party->context;

	// *** Initialize Combat Globals ***

	Combat.defend = info->defend;
	Combat.n_pcs = 0;
	Combat.n_npcs = 0;
	Combat.max_n_npcs = 0;
	Combat.followMode = false;
	Combat.turns_per_round = info->camping ? TURNS_PER_HOUR : 1;
	Combat.tmp_terrain_map = false;
	Combat.enemy_vehicle = NULL;
	Cursor->remove();
	memset(&Combat.rinfo, 0, sizeof(Combat.rinfo));
	list_init(&Combat.parties);

	// *** Position parties (not members - just the parties) ***

	// *** Initialize the Temporary Combat Place ***

	if (info->move->place->type != wilderness_place &&
	    info->move->place->type != town_place) {
		// When not in a town or the wilderness (i.e., dungeon) use the
		// current map for combat.
		Place = info->move->place;
	} else {

                // Town or wilderness combat uses a temporary terrain map which
                // we create on the fly.
		Combat.place.terrain_map = create_temporary_terrain_map(info);
                Combat.tmp_terrain_map = true;

		// fixme -- instead of asserting I should use a last-resort map
		// that does not require allocation.
		assert(Combat.place.terrain_map);

                // fixme -- are these obsolete now?
		Combat.place.location.place = info->move->place;
		Combat.place.location.x = info->move->x;
		Combat.place.location.y = info->move->y;

		Place = &Combat.place;
	}

	mapSetPlace(Place);

	// *** Position the Player Companions ***

	// This is where the map overlays on the player side of the map get
	// placed, if any.
	if (!position_player_party(info))
		return false;

	// *** Position the Enemy Party Members ***

	if (info->move->npc_party) {
		Combat.state = FIGHTING;
		myPositionEnemy(info->move->npc_party, info->move->dx,
				info->move->dy, !info->defend);
		if (Combat.n_npcs == 0) {
			consolePrint("\n*** FORFEIT ***\n\n");
			consolePrint("Your opponent slips away!\n");
			Combat.state = LOOTING;
		}
	} else if (info->camping) {
		player_party->for_each_member(pc_camp, info);
		Combat.end_of_camping_turn =
		    Turn + info->hours * TURNS_PER_HOUR;
		Combat.state = CAMPING;
		Combat.guard = info->guard;
		Combat.rested = false;
		consolePrint("Zzzz...");
		consoleRepaint();
	} else {
		struct v2 v2;
		v2.dx = info->move->dx;
		v2.dy = info->move->dy;
		Combat.state = LOOTING;
		placeForEachObject(myFindAndPositionEnemy, &v2);
	}

	// *** Setup the Map Viewer ***

	mapRmView(player_party->view);
	player_party->for_each_member(mySetInitialCameraPosition, 0);
	mapUpdate(0);
	foogodRepaint();

	if (Combat.state == FIGHTING) {
		consolePrint("\n*** Combat ***\n\n");
		consoleRepaint();
		soundPlay(Combat.sound_enter, SOUND_MAX_VOLUME);
	} else if (Combat.state != CAMPING) {
		myFollow(0);
	}
	// *** Main Combat Loop ***

	myEventLoop();

	// *** Process Player Exit Conditions ***

	leader = player_party->get_leader();
	if (leader) {
		rx = leader->getX();
		ry = leader->getY();
	} else {
		rx = info->move->x;
		ry = info->move->y;
	}

	player_party->for_each_member(pc_end_combat, NULL);
	player_party->onMap = true;

	switch (Combat.rinfo.type) {
	case retreat_info::NO_RETREAT:
		printf("no retreat\n");
		if (Place != &Combat.place) {
			info->move->x = rx;
			info->move->y = ry;
		}
		break;
	case retreat_info::EDGE_RETREAT:
		printf("edge retreat\n");
		info->move->place = Place->location.place;
		// Edge retreats which result in relocating the player are just
		// too problematic. If the tile is impassable in that direction
		// I have no good way of dealing with it. So just forget it.
		info->move->x = Place->location.x;	// + Combat.rinfo.dx;
		info->move->y = Place->location.y;	// + Combat.rinfo.dy;
		info->move->dx = Combat.rinfo.dx;
		info->move->dy = Combat.rinfo.dy;
		break;
	case retreat_info::PORTAL_RETREAT:
		printf("portal retreat\n");
		class Portal *portal = place_get_portal(Place, Combat.rinfo.x,
							Combat.rinfo.y);
		info->move->place = portal->getToPlace();
		info->move->x = portal->getToX();
		info->move->y = portal->getToY();
		break;
	}

	// If the player retreats or dies then destroy the enemy npc party's
	// vehicle
	if (Combat.enemy_vehicle &&
	    (Combat.rinfo.type != retreat_info::NO_RETREAT || !leader)) {
		Combat.enemy_vehicle->remove();
		Combat.enemy_vehicle->destroy();
		delete Combat.enemy_vehicle;
	}
	// Special case: if the party was in follow mode and the leader left
	// then the next-in-command was made leader. This should only be a
	// temporary assignment until the party is all together again, at which
	// point the "real" leader should resume his/her role. By setting the
	// leader to NULL on exit I'll force a reevalution of the leader the
	// next time somebody wants to know who it is.
	player_party->leader = NULL;

	// *** Process NPC Party Exit Conditions ***

	pelem = Combat.parties.next;
	while (pelem != &Combat.parties) {

		class NpcParty *party;
		struct list *melem, *tmp;
		class Character *member;
		bool keep, put, onmap;

		tmp = pelem->next;
		party = outcast(pelem, class NpcParty, container_link.list);
                printf("processing %s\n", party->getName());
		pelem = tmp;

		// For wilderness combat automatically destroy the npc parties
		// after combat.
		if (Place == &Combat.place) {
                        printf("cleaning up %s\n", party->getName());
			party->cleanupAfterCombat();
			party->destroy();
                        
                        // If the party was summoned or was brought in as an
                        // ambush then we need to delete it now. Parties which
                        // were originally on the map are deleted after combat.
                        if (party->destroy_on_combat_exit)
                                delete party;

			continue;
		}
		// If the party contains any living members still on the map
		// then keep it here.
		keep = false;
		put = false;
		onmap = false;
		list_for_each(&party->members, melem) {
			member = outcast(melem, class Character, plist);

			if (member->isDead() || member->is_clone)
				continue;

			// Parties who 'belong' in a place are allowed to
			// return after combat if all of their surviving
			// members fled. 'Invaders' that flee do not return.
			if (member->isOnMap()) {
				onmap = true;
				keep = true;
			} else {
				if (party->isHome(Place)) {
					keep = true;
				}
				continue;
			}


		}

		if (!keep) {
			// Destroying the party will automatically remove all
			// of its members and destroy them.
			printf("Destroying party %s\n", party->getName());
			party->cleanupAfterCombat();
			party->destroy();
			continue;
		}

		// Put the party back where it was at the beginning of combat.
		party->cleanupAfterCombat();
		party->relocate(Place, party->pinfo.x, party->pinfo.y);

	}

	memset(Combat.npcs, 0, sizeof(Combat.npcs));
	list_init(&Combat.parties);
	if (Place == &Combat.place)
		place_for_each_object(Place, myCleanupObject, 0);

	if (Combat.tmp_terrain_map)
		terrain_map_destroy(Combat.place.terrain_map);

	// *** Restore Map Viewer **

	mapRmView(ALL_VIEWS);
	mapAddView(player_party->view);

	player_party->context = original_context;

	printf("moving party to %s [%d %d]\n", info->move->place->name,
	       info->move->x, info->move->y);

	return true;
}

char combatGetState(void)
{
	switch (Combat.state) {
	case FIGHTING:
		return 'Y';
		break;
	case DONE:
		return 'N';
		break;
	case RETREAT:
	case DEFEAT:
		return 'D';
		break;
	case VICTORY:
	case LOOTING:
		return 'V';
		break;
	case CAMPING:
		return 'K';
		break;
	}

	return 'N';
}

void combatKIA(class Character * c)
{
	assert(Combat.state != DONE);

	if (c->isPlayerControlled()) {
		Combat.n_pcs--;
		if (Combat.n_pcs == 0) {
			// Handle the case where the last PC is killed by an
			// NPC.
			Combat.state = DEFEAT;
		}
	} else {
		bool victory = true;
		Combat.n_npcs--;
		myForEachNpc(myCheckIfHostile, &victory);
		if (victory)
			Combat.state = VICTORY;
	}
}

bool combatAddNpcParty(class NpcParty * party, int dx, int dy, bool located,
		       int x, int y)
{
	if (!located) {
		// Caller has not specified a location so use the normal
		// procedure.
		return myPositionEnemy(party, dx, dy, false);
	}
	// Special case: caller wants to put the party at (x, y). Duplicate the
	// code in myPositionEnemy except fill out the position info based on
	// caller's request.
	party->disembark();
	party->remove();        

	memset(&party->pinfo, 0, sizeof(party->pinfo));
	party->pinfo.x = x;
	party->pinfo.y = y;
	party->pinfo.dx = dx;
	party->pinfo.dy = dy;
	party->pinfo.formation = party->get_formation();
	if (!party->pinfo.formation)
		party->pinfo.formation = &default_formation;
	set_party_initial_position(&party->pinfo, x, y);
	myPutEnemy(party, &party->pinfo);        
	return (party->pinfo.placed != 0);
}
