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
#include "dice.h"
#include "Party.h"
#include "place.h"
#include "player.h"
#include "object.h"
#include "common.h"
#include "map.h"
#include "hash.h"
#include "wq.h"
#include "sound.h"
#include "status.h"
#include "cursor.h"
#include "Container.h"
#include "terrain.h"
#include "Field.h"
#include "event.h"
#include "play.h"
#include "foogod.h"
#include "wind.h"
#include "dup_constants.h"
#include "cmdwin.h"
#include "terrain_map.h"
#include "vehicle.h"
#include "formation.h"
#include "pinfo.h"
#include "cmd.h"
#include "formation.h"
#include "session.h"
#include "log.h"
#include "vmask.h"
#include "factions.h"

#include <assert.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <math.h>

#define FORMATION_H     (formation[array_sz(formation) - 1].y)
#define N_MAX_NPCS      256      /* arbitrary limit */
#define MAX_DEPTH       64
#define MAX_PLACEMENT_RECTANGLE_W 32
#define MAX_PLACEMENT_RECTANGLE_H 16
#define SEARCH_QUEUE_SZ 100

/* Formation pattern -- party facing north (dx=0,dy=-1), origin at the leader's
 * position */

enum combat_faction_status {
        COMBAT_FACTION_EXISTS,
        COMBAT_FACTION_GONE,
        COMBAT_FACTION_CHARMED,
        COMBAT_FACTION_CAMPING
};

static struct {
        struct place *place;
        void *session_handle;
        enum combat_state state;
        char vmap[7 * 7];       // visited map (used to search for positions)
        struct list parties;
        class Vehicle *enemy_vehicle;
        char *sound_enter;
        char *sound_defeat;
        char *sound_victory;
        int round;
} Combat;

struct v2 {
        struct place *place;
        int dx, dy;
};

// Search alg data
static char rmap[MAX_PLACEMENT_RECTANGLE_W * MAX_PLACEMENT_RECTANGLE_H];
static int x_q[SEARCH_QUEUE_SZ];
static int y_q[SEARCH_QUEUE_SZ];
static int q_head;
static int q_tail;

static void combat_print_banner(char *msg)
{
        log_msg("*** %s ***", msg);
}

enum combat_state combat_get_state(void)
{
        return Combat.state;
}

void combat_set_state(enum combat_state new_state)
{
        // --------------------------------------------------------------------
        // Interesting state transitions:
        //
        // ====================================================================
        // old state              | new state              | result
        // ====================================================================
        // COMBAT_STATE_DONE      | COMBAT_STATE_FIGHTING  | entry to combat
        // COMBAT_STATE_DONE      | COMBAT_STATE_LOOTING   | non-hostile
        // COMBAT_STATE_DONE      | COMBAT_STATE_CAMPING   | entry to camping
        // COMBAT_STATE_FIGHTING  | COMBAT_STATE_DONE      | defeat
        // COMBAT_STATE_FIGHTING  | COMBAT_STATE_LOOTING   | victory
        // COMBAT_STATE_LOOTING   | COMBAT_STATE_FIGHTING  | hostiles entered
        // COMBAT_STATE_LOOTING   | COMBAT_STATE_DONE      | exit normally
        // COMBAT_STATE_CAMPING   | COMBAT_STATE_FIGHTING  | ambush
        // COMBAT_STATE_CAMPING   | COMBAT_STATE_DONE      | exit camping
        // ====================================================================
        //
        // --------------------------------------------------------------------

        if (Combat.state == new_state)
                return;


        switch (Combat.state) {

        case COMBAT_STATE_DONE:
                switch (new_state) {
                case COMBAT_STATE_FIGHTING:
                        combat_print_banner("COMBAT");
                        soundPlay(Combat.sound_enter, SOUND_MAX_VOLUME);
                        break;
                case COMBAT_STATE_LOOTING:
                        break;
                case COMBAT_STATE_CAMPING:
                        combat_print_banner("CAMPING");
                        break;
                default:
                        assert(false);
                        break;
                }
                break;

        case COMBAT_STATE_FIGHTING:
                switch (new_state) {
                case COMBAT_STATE_LOOTING:
                        combat_print_banner("VICTORY");
                        soundPlay(Combat.sound_victory, SOUND_MAX_VOLUME);
                        break;
                case COMBAT_STATE_DONE:
                        combat_print_banner("DEFEAT");
                        soundPlay(Combat.sound_defeat, SOUND_MAX_VOLUME);
                        break;
                default:
                        assert(false);
                        break;
                }
                break;

        case COMBAT_STATE_LOOTING:
                switch (new_state) {
                case COMBAT_STATE_FIGHTING:
                        combat_print_banner("COMBAT");
                        soundPlay(Combat.sound_enter, SOUND_MAX_VOLUME);
                        break;
                case COMBAT_STATE_DONE:
                        break;
                default:
                        assert(false);
                        break;
                }
                break;

        case COMBAT_STATE_CAMPING:
                switch (new_state) {
                case COMBAT_STATE_FIGHTING:
                        combat_print_banner("COMBAT");
                        soundPlay(Combat.sound_enter, SOUND_MAX_VOLUME);
                        break;
                case COMBAT_STATE_LOOTING:
                case COMBAT_STATE_DONE:
                        break;
                default:
                        assert(false);
                        break;
                }
                break;

        default:
                assert(false);
                break;
        }

        Combat.state = new_state;
}

static int location_is_safe(struct position_info *info)
{
        struct astar_node *path;
        int flags = PFLAG_IGNOREBEINGS;
        int edge_x = 0, edge_y = 0;
        struct astar_search_info as_info;
        struct terrain *terrain;

        // Is it passable?
        if (!place_is_passable(info->place, info->px, info->py, 
                               info->subject, 0)) {
                dbg("impassable\n");
                return -1;
        }
        // Is it occupied?
        if (place_is_occupied(info->place, info->px, info->py)) {
                dbg("occupied\n");
                return -1;
        }

        // I added the next two checks because a character was getting
        // positioned over the firepit while camping, and I thought it was
        // damaging him. Turns out firepits weren't setup to cause fire damage
        // (oddly), and the character was just starving. I'll leave this here
        // for now anyway.

        // Is it dangerous? Hack: check for a field and dangerous terrain
        if (place_get_object(info->place, info->px, info->py, field_layer)) {
                dbg("possibly dangerous field\n");
                return -1;
        }
        terrain = place_get_terrain(info->place, info->px, info->py);
        if (terrain->effect) {
                dbg("possibly dangerous terrain\n");
                return -1;
        }
        

        memset(&as_info, 0, sizeof (as_info));

        if (info->find_edge) {

                assert(info->dx || info->dy);
                assert(!info->dx || !info->dy); // assume no diagonals for now

                dbg("searching for path to ");

                if (info->dx < 0) {
                        // facing west, find path back to east edge
                        edge_x = place_w(info->place) - 1;
                        flags |= PFLAG_VERT;
                        dbg("east ");
                }
                else if (info->dx > 0) {
                        // facing east, find path to back west edge
                        edge_x = 0;
                        flags |= PFLAG_VERT;
                        dbg("west ");
                }
                else if (info->dy < 0) {
                        // facing north, find path back to south edge
                        edge_y = place_h(info->place) - 1;
                        flags |= PFLAG_HORZ;
                        dbg("north ");
                }
                else {
                        // facing south, find path back to north edge
                        edge_y = 0;
                        flags |= PFLAG_HORZ;
                        dbg("south ");
                }

                dbg("edge...");

                as_info.x0 = info->px;
                as_info.y0 = info->py;
                as_info.x1 = edge_x;
                as_info.y1 = edge_y;
                as_info.flags = flags;

                path = place_find_path(info->place, &as_info, info->subject);

                if (!path)
                        dbg("no path back to edge\n");

        }
        else if (info->find_party) {
                // Each member should be able to find a path back to the
                // party's originating location on the map.
                dbg("searching for path to party [%d %d]...",
                       info->x, info->y);

                as_info.x0 = info->px;
                as_info.y0 = info->py;
                as_info.x1 = info->x;
                as_info.y1 = info->y;
                as_info.flags = flags;
                as_info.limit_depth = true;
                as_info.max_depth = 5;

                path = place_find_path(info->place, &as_info, info->subject);

                if (!path)
                        dbg("no path back to party\n");
        }
        else {
                // skip the pathfinding check
                return 0;
        }

        if (path) {
                astar_path_destroy(path);
                return 0;
        }

        return -1;
}

static int combat_search_for_safe_position(struct position_info *info)
{
        unsigned int i;
        int index;
        static int x_offsets[] = { -1, 1, 0, 0 };
        static int y_offsets[] = { 0, 0, -1, 1 };

        dbg("checking [%d %d]...", info->px, info->py);

        // translate the map coords into an rmap index
        index = info->py - info->ry;
        index *= info->rw;
        index += (info->px - info->rx);

        // If the current location is off-map, outside of the placement
        // rectangle or already visited then discontinue the search.  
        if (rmap[index]) {
                dbg("already visited [%d]\n", index);
                return -1;      // already visited
        }
        if (info->px < info->rx || info->px >= info->rx + info->rw ||
            info->py < info->ry || info->py >= info->ry + info->rh) {
                dbg("outside the placement area\n");
                return -1;      // outside the placement rect
        }
        if (place_off_map(info->place, info->px, info->py)) {
                dbg("off-map\n");
                // return -1; // off map
                goto enqueue_neighbors;
        }
        // Mark this location as visited.
        rmap[index] = 1;

        // If the current location is safe then the search succeeded.
        if (location_is_safe(info) == 0) {
                dbg("OK!\n");
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

static int combat_find_safe_position(struct position_info *info)
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
                if (combat_search_for_safe_position(info) == 0)
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
        }
        else if (info->dx > 0) {
                /* Rotate -90 degrees */
                tmp = pm->getX();
                pm->setX(-pm->getY());
                pm->setY(tmp);
        }
        else if (info->dy > 0) {
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
        info->subject = pm;
        memset(rmap, 0, sizeof (rmap));

        // set the preferred location
        info->px = pm->getX();
        info->py = pm->getY();

        dbg("Placing %s\n", pm->getName());

        if (combat_find_safe_position(info) == -1) {
                // If I can't place a member then I can't place it.
                dbg("*** Can't place %s ***\n", pm->getName());
                return false;
        }

        pm->setX(info->px);
        pm->setY(info->py);
        dbg("Put '%s' at [%d %d]\n", pm->getName(), info->px, info->py);
        pm->setPlace(Place);
        place_add_object(Place, pm);
        pm->setOnMap(true);
        info->placed++;

        /* Check if we need to go back to fighting */
        if (combat_get_state() != COMBAT_STATE_FIGHTING &&
            are_hostile(pm, player_party)) {
                combat_set_state(COMBAT_STATE_FIGHTING);
        }

        return false;
}

static void set_party_initial_position(struct position_info *pinfo, int x, int y)
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

        dbg("Moved party start position to [%d %d]\n", pinfo->x, pinfo->y);;

}

void combat_fill_position_info(struct position_info *info, struct place *place, int x, int y, int dx, int dy, bool defend)
{
        // 
        // This function will:
        // * determine the party's coordinates upon entry to combat
        // * turn the defending party to face the attacker
        // * specify the placement rectangle for the party members
        // * set the flags for the placement algorithm
        // 

        info->place = place;
        info->dx = dx;
        info->dy = dy;

        if (info->place != Combat.place) {
                // Occupy the same location and face the same way
                info->x = x;
                info->y = y;

        }
        else {

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
                        info->x = place_w(info->place) - place_w(info->place) / 4;
                }
                else if (dx > 0) {
                        // facing east, occupy west half
                        info->x = place_w(info->place) / 4;
                }
                else {
                        // facing north or south, center on east-west
                        info->x = place_w(info->place) / 2;
                }

                if (dy < 0) {
                        // facing north, occupy south
                        info->y = place_h(info->place) - place_h(info->place) / 4;
                }
                else if (dy > 0) {
                        // facing south, occupy north
                        info->y = place_h(info->place) / 4;
                }
                else {
                        // facing east or west, center on north-south
                        info->y = place_h(info->place) / 2;
                }
        }

        set_party_initial_position(info, info->x, info->y);

        // clear the pmask and search map before first use
        info->subject = NULL;
        memset(rmap, 0, sizeof (rmap));

        info->placed = 0;
}

bool combat_place_character(class Character * pm, void *data)
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
        }
        else if (info->dx > 0) {
                /* Rotate -90 degrees */
                tmp = pm->getX();
                pm->setX(-pm->getY());
                pm->setY(tmp);
        }
        else if (info->dy > 0) {
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
        info->subject = pm;
        memset(rmap, 0, sizeof (rmap));
        info->px = pm->getX();
        info->py = pm->getY();
        dbg("Placing %s\n", pm->getName());

        if (combat_find_safe_position(info) == -1) {

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
                        dbg("Putting %s on start location [%d %d]\n",
                               pm->getName(), info->x, info->y);
                        info->px = info->x;
                        info->py = info->y;
                }
                else {
                        // init the position info to search again
                        memset(rmap, 0, sizeof (rmap));
                        info->px = leader->getX();
                        info->py = leader->getY();
                        dbg("Retrying %s\n", pm->getName());

                        if (combat_find_safe_position(info) == -1) {
                                dbg("Putting %s on start location "
                                       "[%d %d]\n",
                                       pm->getName(), info->x, info->y);
                                info->px = info->x;
                                info->py = info->y;
                        }
                }
        }

#if 0
        pm->setX(info->px);
        pm->setY(info->py);

        pm->setPlace(info->place);

        place_add_object(info->place, pm);

        if (pm->isPlayerControlled()) {
                mapAddView(pm->getView());

                /* Initialize the pc's map view */
                mapCenterView(pm->getView(), pm->getX(), pm->getY());
                
                // Set the PC's light radius based on ambient light and personal light
                // sources. Fixme: should not assume the sun is visible.
                // int lrad = (int)sqrt(pm->getLight() + Sun.light);
                mapSetRadius(pm->getView(), min(pm->getVisionRadius(), MAX_VISION_RADIUS));;
                mapRecomputeLos(pm->getView());
        }
                
        /* Do some one-time init */
        pm->setCombat(true);
#else
        pm->relocate(info->place, info->px, info->py);
#endif
        
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


// Automatically pathfind and move all the party members to the party leader's
// position.
//
// max_path_len - max number of steps to allow for a path, or -1 for no limit
//
// returns true if everyony can find a path in max_path_len steps, false
// otherwise.

static void combat_overlay_map(struct terrain_map *map, 
                               struct position_info *pinfo, int broadside)
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
        if (broadside) {
                terrain_map_rotate(map, vector_to_rotation(pinfo->dy, pinfo->dx));

                // Position the map against the boundary dividing the map.
                if (pinfo->dx < 0) {
                        // facing west, shift map west toward edge
                        x = (place_w(Place)) / 2;
                        y = (place_h(Place) - map->h) / 2;
                }
                else if (pinfo->dx > 0) {
                        // facing east, shift map east toward edge
                        x = (place_w(Place)) / 2 - map->w;
                        y = (place_h(Place) - map->h) / 2;
                }
                else if (pinfo->dy < 0) {
                        // facing north, shift map north toward edge
                        x = (place_w(Place) - map->w) / 2;
                        y = (place_h(Place)) / 2;
                }
                else if (pinfo->dy > 0) {
                        // facing south, shift map south toward edge
                        x = (place_w(Place) - map->w) / 2;
                        y = (place_h(Place)) / 2 - map->h;
                }
        }
        else {
                terrain_map_rotate(map, vector_to_rotation(pinfo->dx, 
                                                           pinfo->dy));
                // center the overlayed map
                x = (place_w(Place) - map->w) / 2;
                y = (place_h(Place) - map->h) / 2;
        }

        assert(x >= 0);
        assert(y >= 0);

        // Adjust the party's starting position to be centered on the overlap
        // map.
        set_party_initial_position(pinfo, 
                                   x + (map->w /*+ 1*/) / 2, 
                                   y + (map->h /*+ 1*/) / 2);

        // Blit the rotated map centered on the given coordinates.
        terrain_map_blit(Place->terrain_map, x, y, map, 0, 0, map->w, map->h);
        // terrain_map_print(stdout, INITIAL_INDENTATION, Place->terrain_map);

        // Cleanup.
        terrain_map_del(map);
}

static void myPutEnemy(class Party * foe, struct position_info *pinfo)
{
        foe->forEachMember(myPutNpc, pinfo);
        if (foe->pinfo.placed)
                list_add(&Combat.parties, &foe->container_link.list);
}

static bool myPositionEnemy(class Party * foe, int dx, int dy, bool defend, struct place *place)
{
        assert(foe->getSize());

        combat_fill_position_info(&foe->pinfo, place, foe->getX(), foe->getY(), dx, dy, defend);
        foe->pinfo.formation = foe->get_formation();
        if (!foe->pinfo.formation)
                foe->pinfo.formation = formation_get_default();

        // Check for a map overlay.
        if (foe->vehicle && foe->vehicle->getObjectType()->map &&
            Place == Combat.place) {
                combat_overlay_map(foe->vehicle->getObjectType()->map, &foe->pinfo, 1);
          }

        Combat.enemy_vehicle = foe->vehicle;
        foe->disembark();
        foe->remove();
        myPutEnemy(foe, &foe->pinfo);
        return (foe->pinfo.placed != 0);
}

#if 0
static void random_ambush(void)
{
        class Party *foe;
        int dir;

        foe = place_random_encounter(Place->location.place);
        if (!foe)
                return;

        // Roll to pick a direction
        dir = ((random() % 4) + 1) * 2 - 1;

        // Try to place the enemy party. This can fail. For example, the place
        // might randomly generate a party of nixies as the npc party while the
        // player is camped on dry land.
        if (!myPositionEnemy(foe, directionToDx(dir), directionToDy(dir), false)) {
                dbg("ambush failed for %s\n", foe->getName());
                // I think the party will get cleaned up on exit...
                return;
        }

        combat_set_state(COMBAT_STATE_FIGHTING);

        log_msg("*** AMBUSH ***");

        player_party->ambushWhileCamping();
}
#endif

#if 0
struct node {
        struct node *next;
        struct node *prev;
        void *ptr;
};


/*****************************************************************************
 * combat_npc_status_visitor - determine the status of the npc faction(s)
 *
 * This is a "visitor" function applied to each object in a list of nodes. It's
 * meant to be applied to every object in a place. It checks for any npc
 * party members and whether or not they're charmed, and sets the combat status
 * of the npc faction.
 *
 * This routine does not distinguish between different NPC factions, it just
 * looks for anybody hostile to the player.
 *
 *****************************************************************************/
static void combat_hostile_status_visitor(struct node *node, void *data)
{
        enum combat_faction_status *stat;
        class Object *obj;

        /* Extract the typed variables from the generic parms */
        stat = (struct combat_faction_status *)data;
        obj = (class Object*)node->ptr;

        /* If we already know a hostile faction exists then skip the rest */
        if (*stat == COMBAT_FACTION_EXISTS)
                return;

        /* Skip non-beings */
        if (! obj_is_being(obj))
                return;

        /* Skip player party members */
        if (obj->isPlayerPartyMember())
                return;

        /* A hostile npc means a hostile faction still exists */
        if (are_hostile((Being*)obj, player_party)) {
                *stat = COMBAT_FACTION_EXISTS;
                return;
        }

        /* Check for a charmed hostile */
        if (are_natively_hostile((Being*)obj, player_party)) {
                *stat = COMBAT_FACTION_CHARMED;
        }
}

/*****************************************************************************
 * combat_get_hostile_faction_status - check hostile combat status
 *
 * This is literally a dupe of combat_get_player_faction_status() below.
 *
 *****************************************************************************/
static enum combat_faction_status combat_get_hostile_faction_status(void)
{
        enum combat_faction_status stat;

        /* Assume until proven otherwise that the player faction is gone. */
        stat = COMBAT_FACTION_GONE;

        /* Check each object in the current place to determine the status of
         * the player faction. */
        node_for_each(place_get_all_objects(Place),
                      combat_hostile_status_visitor,
                      &stat);

        /* Return the discovered status. */
        return stat;
}

/*****************************************************************************
 * combat_player_status_visitor - determine the status of the player faction
 *
 * This is a "visitor" function applied to each object in a list of nodes. It's
 * meant to be applied to every object in a place. It checks for any player
 * party members and whether or not they're charmed, and sets the combat status
 * of the player party based on the cumulative results.
 *
 *****************************************************************************/
static void combat_player_status_visitor(struct node *node, void *data)
{
        enum combat_faction_status *stat;
        class Object *obj;

        /* Extract the typed variables from the generic parms */
        stat = (struct combat_faction_status *)data;
        obj = (class Object*)node->ptr;

        /* If we already know the player is still fighting then skip the rest
         * of this. */
        if (info->status == COMBAT_FACTION_EXISTS)
                return;

        /* Skip non-beings */
        if (! obj_is_being(obj))
                return;

        /* Skip non-party-members */
        if (! obj->isPlayerPartyMember())
                return;

        /* A non-hostile party member means the player is still fighting. */
        if (! are_hostile((Being*)obj, player_party)) {
                info->status = COMBAT_FACTION_EXISTS;
                return;
        }

        /* Check if a player party members has been charmed */
        if (are_natively_hostile((Being*)obj, player_party)) {
                info->status = COMBAT_FACTION_CHARMED;
        }
}

/*****************************************************************************
 * combat_get_player_faction_status - check player combat status
 *
 * Loops over all objects to check if any player party members are still around
 * and whether or not they're charmed.
 *
 * FIXME: why not just check the player party directly? Or at least merge this
 * with the combat_get_hostile_faction_status() function above?
 *
 *****************************************************************************/
static enum combat_faction_status combat_get_player_faction_status(void)
{
        enum combat_faction_status stat;

        /* Assume until proven otherwise that the player faction is gone. */
        stat = COMBAT_FACTION_GONE;

        /* Check each object in the current place to determine the status of
         * the player faction. */
        node_for_each(place_get_all_objects(Place),
                      combat_player_status_visitor,
                      &stat);

        /* Return the discovered status. */
        return stat;
}

#else /* ! TURN_LIST_NODES */

enum combat_faction_status combat_get_hostile_faction_status(void)
{
        // ---------------------------------------------------------------------
        // Search the list of all objects in the current place. For each object
        // that is not a member of the player party check its native and
        // charmed faction.
        //
        // ==================================================
        // Native   | Charmed  | Result
        // ==================================================
        // hostile  | n/a      | [1] hostile
        // hostile  | friendly | [2] friendly
        // hostile  | hostile  | [1] hostile
        // friendly | n/a      | [3] friendly
        // friendly | friendly | [3] friendly
        // friendly | hostile  | [1] hostile
        // ==================================================
        //
        // o If any cases of [1] exist, then a hostile faction exists.
        //
        // o Otherwise if any cases of [2] exist then only a charmed faction
        //   exists.
        //
        // o Otherwise no hostile faction exists.
        //
        // ---------------------------------------------------------------------

        struct list *head;
        struct list *elem;
        class Object *obj;
        bool found_charmed_hostile;


        found_charmed_hostile = false;

        head = place_get_all_objects(Place);
        list_for_each(head, elem) {

                obj = outcast(elem, class Object, turn_list);

                if (! obj_is_being(obj))
                        continue;

                // -------------------------------------------------------------
                // Player-controlled objects (objects in the player party) are
                // handled by combat_get_player_faction_status(), not here.
                // -------------------------------------------------------------

                if (obj->isPlayerPartyMember())
                        continue;

                // -------------------------------------------------------------
                // Among the non-player controlled objects anything that is
                // simply hostile implies a hostile faction exists.
                // -------------------------------------------------------------

                if (are_hostile((Being*)obj, player_party)) {
                        return COMBAT_FACTION_EXISTS;
                }

                // -------------------------------------------------------------
                // Among non-player controlled, non-hostile objects, check for
                // any that are charmed to side with the player.
                // -------------------------------------------------------------

                if (!found_charmed_hostile &&
                    are_natively_hostile((Being*)obj, player_party)) {
                        found_charmed_hostile = true;
                }
        }

        if (found_charmed_hostile)
                return COMBAT_FACTION_CHARMED;

        return COMBAT_FACTION_GONE;
}


enum combat_faction_status combat_get_player_faction_status(void)
{
        struct list *head;
        struct list *elem;
        class Object *obj;
        bool found_charmed_member;

        found_charmed_member = false;
        head                 = place_get_all_objects(Place);

        list_for_each(head, elem) {

                obj = outcast(elem, class Object, turn_list);

                if (! obj_is_being(obj))
                        continue;

                // ------------------------------------------------------------
                // Non-player-controlled objects (objects not in the player
                // party) are handled by combat_get_hostile_faction_status(),
                // not here.
                // ------------------------------------------------------------

                if (! obj->isPlayerPartyMember())
                        continue;

                // ------------------------------------------------------------
                // Among the player-controlled objects anything that is not
                // hostile implies a player faction exists.
                // ------------------------------------------------------------

                if (! are_hostile((Being*)obj, player_party)) {
                        return COMBAT_FACTION_EXISTS;
                }

                // ------------------------------------------------------------
                // Among player-controlled, hostile objects, check for any that
                // are charmed to against the player.
                // ------------------------------------------------------------

                if (!found_charmed_member &&
                    are_natively_hostile((Being*)obj, player_party)) {
                        found_charmed_member = true;
                }
        }

        if (found_charmed_member)
                return COMBAT_FACTION_CHARMED;

        return COMBAT_FACTION_GONE;
        
}

#endif /* ! TURN_LIST_NODES */

void combat_analyze_results_of_last_turn()
{

        enum combat_faction_status hostile_faction_status;
        enum combat_faction_status player_faction_status;

        // ---------------------------------------------------------------------
        // Now check for changes in the combat state as a result of the last
        // turn. Check the status of the hostile party or parties and the
        // player party. The following table shows the outcome with all
        // possible combinations of status:
        //
        // =====================================================
        // hostiles | player party | result
        // ===================================================== 
        // exist    | exist        | continue combat
        // exist    | gone         | exit combat
        // exist    | charmed      | uncharm, continue combat
        // gone     | exist        | looting
        // gone     | gone         | exit combat
        // gone     | charmed      | uncharm, looting
        // charmed  | exist        | continue combat
        // charmed  | gone         | exit combat
        // charmed  | charmed      | uncharm, continue combat
        // =====================================================
        //
        // ---------------------------------------------------------------------
        
        hostile_faction_status = combat_get_hostile_faction_status();
        player_faction_status  = combat_get_player_faction_status();
                        
        switch (player_faction_status) {
                
        case COMBAT_FACTION_EXISTS:
                
                switch (hostile_faction_status) {
                        
                case COMBAT_FACTION_EXISTS:
                        // -----------------------------------------------------
                        // Both factions exist. Continue or restart fighting.
                        // -----------------------------------------------------                        
                        combat_set_state(COMBAT_STATE_FIGHTING);
                        break;

                case COMBAT_FACTION_CHARMED:
                        // -----------------------------------------------------
                        // The hostile faction are all charmed. Tough luck for
                        // them. Make sure we are fighting.
                        // -----------------------------------------------------
                        combat_set_state(COMBAT_STATE_FIGHTING);
                        break;

                case COMBAT_FACTION_GONE:
                        // -----------------------------------------------------
                        // No hostiles around. Loot at will.
                        // -----------------------------------------------------
                        combat_set_state(COMBAT_STATE_LOOTING);
                        break;

                default:
                        assert(false);
                        break;
                }
                
                break;

        case COMBAT_FACTION_CHARMED:

                // -------------------------------------------------------------
                // In all of these cases I uncharm the party members. If I
                // don't, then I'm risking a deadlock situation where the
                // hostiles can't or won't finish off the charmed members, in
                // which case the game gets stuck running all the npc's forever
                // while the player helplessly watches.
                // -------------------------------------------------------------

                player_party->unCharmMembers();

                switch (hostile_faction_status) {
                        
                case COMBAT_FACTION_EXISTS:
                        combat_set_state(COMBAT_STATE_FIGHTING);
                        break;

                case COMBAT_FACTION_GONE:
                        // ----------------------------------------------------
                        // No hostiles around. Loot at will. Uncharm or we'll
                        // definitely deadlock.
                        // ----------------------------------------------------
                        combat_set_state(COMBAT_STATE_LOOTING);
                        break;

                default:
                        assert(false);
                        break;
                }

                break;

        case COMBAT_FACTION_GONE:

                // ------------------------------------------------------------
                // In all of these cases combat is over. Simple. If combat is
                // ocurring in the special combat place then we need to clean
                // it up by calling combat_exit().
                // ------------------------------------------------------------
                
                combat_set_state(COMBAT_STATE_DONE);

//                if (Place == Combat.place)
                combat_exit();

                break;
                
        case COMBAT_FACTION_CAMPING:

                switch (hostile_faction_status) {
                        
                case COMBAT_FACTION_EXISTS:
                        // ----------------------------------------------------
                        // Ambush!
                        // ----------------------------------------------------                        
                        combat_set_state(COMBAT_STATE_FIGHTING);
                        break;

                case COMBAT_FACTION_GONE:
                        // ----------------------------------------------------
                        // No hostiles around. Change nothing.
                        // ----------------------------------------------------
                        break;

                default:
                        assert(false);
                        break;
                }

                break;

        }
        

}

static void myFindAndPositionEnemy(class Object * obj, void *data)
{
        struct v2 *info;

        if (!obj->isType(PARTY_ID))
                return;

        info = (struct v2 *) data;
        assert(obj_is_being(obj));
        if (are_hostile((Being*)obj, player_party))
                combat_set_state(COMBAT_STATE_FIGHTING);
        myPositionEnemy((class Party *) obj, info->dx, info->dy, false, info->place);
}

int combatInit(void)
{
        // This is called once at the beginning of the game
        memset(&Combat, 0, sizeof (Combat));

        /* Initialize the place to safe defaults */
        Combat.state = COMBAT_STATE_DONE;
        list_init(&Combat.parties);

        return 0;
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
        }
        else if (dx > 0) {
                // facing east, fill west half
                terrain_map_fill(map, 0, 0, map->w / 2, map->h, terrain);
        }
        else if (dy < 0) {
                // facing north, fill south half
                terrain_map_fill(map, 0, map->h / 2, map->w, map->h / 2,
                                 terrain);
        }
        else if (dy > 0) {
                // facing south, fill north half
                terrain_map_fill(map, 0, 0, map->w, map->h / 2, terrain);
        }
}

#else                           // ! USE_OLD_MAP_FILL

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
                dst_x = map->w / 2;
                dst_y = 0;
                src_x = 0;
                src_y = 0;
                src_w = (map->w + 1) / 2;
                src_h = map->h;

        }
        else if (dx > 0) {

                // facing east, fill west half
                dst_x = 0;
                dst_y = 0;
                src_x = map->w / 2;
                src_y = 0;
                src_w = (map->w) / 2;
                src_h = map->h;

        }
        else if (dy < 0) {

                // facing north, fill south half
                dst_x = 0;
                dst_y = map->h / 2;
                src_x = 0;
                src_y = 0;
                src_w = map->w;
                src_h = (map->h + 1) / 2;

        }
        else if (dy > 0) {

                // facing south, fill north half
                dst_x = 0;
                dst_y = 0;
                src_x = 0;
                src_y = map->h / 2;
                src_w = map->w;
                src_h = (map->h)/ 2;
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
        }
        else {
                // Fill with the terrain type.
                terrain = place_get_terrain(place, x, y);
                terrain_map_fill(map, dst_x, dst_y, src_w, src_h, terrain);
        }
}

#endif                          // ! USE_OLD_MAP_FILL

static struct terrain_map *create_camping_map(struct place *place, int x, int y)
{
        struct terrain_map *map;
        struct terrain *terrain;
        struct list *elem;

        map = place_get_combat_terrain_map(place, x, y);
        if (map) {
                // terrain_map_print(stdout, INITIAL_INDENTATION, map);
                return terrain_map_clone(map);
        }

        map = terrain_map_new("tmp_combat_map", COMBAT_MAP_W, COMBAT_MAP_H, Session->palette);
        terrain = place_get_terrain(place, x, y);
        terrain_map_fill(map, 0, 0, COMBAT_MAP_W, COMBAT_MAP_H, terrain);

#if 0
        // gmcnutt: with the new loader we no longer have a global list of
        // terrain palettes. I don't think it matters unless we want to save
        // the temporary combat map to a script, and if that's the case we
        // should just create a palette on-the-fly.

        // Find the first palette in the global list 
        // which has an entry for this terrain:
        // (Not ideal, but the camping map is temporary anyways.)
        list_for_each(&Terrain_Palettes, elem) {
                struct terrain_palette *pp =
                    list_entry(elem, struct terrain_palette, list);

                if (palette_contains_terrain(pp, terrain)) {
                        map->palette = pp;
                        break;
                }
        }
        if (!map->palette) {
                // We did not find 'terrain' defined in any palette!
                // For 'terrain' to have been passed to this function,
                // it had to appear on a map, and that map should have
                // had a palette.  Therefore, this error case should
                // be impossible, and is at the least unlikely or 
                // a sign that things are badly screwed up.
                dbg("create_camping_map() IMPOSSIBLE (OK, improbable...)\n"
                       "  strange terrain %p (tag '%s' name '%s')\n"
                       "  was in no palette!\n",
                       terrain, terrain->tag, terrain->name);

                assert(0);
        }
#endif

        // terrain_map_print(stdout, INITIAL_INDENTATION, map);
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

        map = terrain_map_new("tmp_combat_map", COMBAT_MAP_W, COMBAT_MAP_H, 0);
        assert(map);

        // Determine orientation for both parties.

        if (info->defend) {
                player_dx = -info->move->dx;
                player_dy = -info->move->dy;
                npc_dx = info->move->dx;
                npc_dy = info->move->dy;
        }
        else {
                player_dx = info->move->dx;
                player_dy = info->move->dy;
                npc_dx = -info->move->dx;
                npc_dy = -info->move->dy;
        }

        // Fill the player's half of the combat map
        fill_temporary_terrain_map(map,
                                   player_party->getPlace(),
                                   player_party->getX(),
                                   player_party->getY(), player_dx, player_dy);

        // Fill the npc party's half of the combat map
        fill_temporary_terrain_map(map,
                                   info->move->npc_party->getPlace(),
                                   info->move->npc_party->getX(),
                                   info->move->npc_party->getY(),
                                   npc_dx, npc_dy);

        struct terrain_map *party_map =
            place_get_combat_terrain_map(player_party->getPlace(),
                                         player_party->getX(),
                                         player_party->getY());
        struct terrain_map *npc_party_map =
            place_get_combat_terrain_map(info->move->npc_party->getPlace(),
                                         info->move->npc_party->getX(),
                                         info->move->npc_party->getY());

        // SAM: It seems that, until we can think of something 
        //      more clever, (palette merging code?  yech...)
        //      that all combat maps will have to have a palette
        //      in common.  It may be more restrictive than I have
        //      stated, but I would need to research the question...
        // 
        // SAM:
        // Hmmm...ship-to-shore blits a map with a different palette 
        // onto a map with 'pal_standard', and this code has no way of seeing that.
        // Possibly the map blitting should merge the palettes after all...
        if (party_map && npc_party_map) {
                dbg("maps '%s' '%s', palettes '%s' '%s'\n",
                       party_map->tag, npc_party_map->tag,
                       party_map->palette->tag, npc_party_map->palette->tag);
                int palette_tags_match = !strcmp(party_map->palette->tag,
                                                 npc_party_map->palette->tag);
                if (!palette_tags_match) {
                        dbg("create_temporary_terrain_map() warning: \n"
                               "  Two combat maps (tags '%s' and '%s') \n"
                               "  merging with dissimilar palettes (tags '%s' and '%s').\n"
                               "  (This should work OK now, but be aware...)\n",
                               party_map->tag, npc_party_map->tag,
                               party_map->palette->tag,
                               npc_party_map->palette->tag);
                        // SAM: New code in palette_print() should enable us to carry on.
                        // assert(0);
                }
        }
        struct terrain_map *the_map;
        if (party_map)
                the_map = party_map;
        else if (npc_party_map)
                the_map = npc_party_map;
        else {
                // No combat map for either location?
                // Use the parent map (wilderness, or whatever) to get a palette.
                // Since the terrain fill for each half is based on 
                // some terrain in the parent map, that palette should be appropriate.
                assert(player_party->getPlace()->terrain_map);
                the_map = player_party->getPlace()->terrain_map;
        }
        map->palette = the_map->palette;
        terrain_map_print(stdout, INITIAL_INDENTATION, map);

        return map;
}

static bool position_player_party(struct combat_info *cinfo)
{
        class Vehicle *vehicle;

        combat_fill_position_info(&player_party->pinfo, Place,
                                  cinfo->move->x, cinfo->move->y,
                                  cinfo->move->dx, cinfo->move->dy, 
                                  cinfo->defend);

        player_party->pinfo.formation = player_party->get_formation();
        if (!player_party->pinfo.formation)
                player_party->pinfo.formation = formation_get_default();

        // Check for map overlays. First check if the player is in a vehicle
        // with a map.
        if (player_party->vehicle &&
            player_party->vehicle->getObjectType()->map &&
            Place == Combat.place) {
                combat_overlay_map(player_party->vehicle->getObjectType()->map,
                                   &player_party->pinfo,
                                   cinfo->move->npc_party != NULL);
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
                // dbg("party overlay, party over vehicle\n");
                combat_overlay_map(vehicle->getObjectType()->map,
                                   &player_party->pinfo, 0);
        }
        // Finally, since there is no vehicle map check for a camping map.
        else if (cinfo->camping && player_party->campsite_map) {
                // dbg("party overlay, party is camping\n");
                combat_overlay_map(player_party->campsite_map, 
                                   &player_party->pinfo, 0);
        }

        player_party->remove();
        player_party->forEachMember(combat_place_character, &player_party->pinfo);
        return true;
}

bool combat_enter(struct combat_info * info)
{
        struct location loc;

        if (player_party->allDead())
                // Yes, this can happen in some rare circumstances...
                return false;

        // --------------------------------------------------------------------
        // Default to the entry point as the combat exit location for the
        // player party.
        // --------------------------------------------------------------------

        loc.place = info->move->place;
        loc.x     = info->move->x;
        loc.y     = info->move->y;
        player_party->setCombatExitDestination(&loc);


        // *** Initialize Combat Globals ***

        Combat.enemy_vehicle = NULL;
        Combat.round = 0;
        Session->crosshair->remove();
        list_init(&Combat.parties);


        if (! info->move->place->wilderness) {

                // ------------------------------------------------------------
                // When not in the wilderness use the current place for combat.
                // ------------------------------------------------------------
                Place = info->move->place;
        }
        else {

                // ------------------------------------------------------------
                // Create a temporary place for combat in the wilderness. It's
                // parent will be the wilderness. We have to set a special flag
                // to indicate that it's wilderness combat (used for things
                // like exit policy).
                // ------------------------------------------------------------

                Combat.place = place_new("p_wilderness_combat", 
                                         "Wilderness Combat",
                                         0, // sprite
                                         create_temporary_terrain_map(info),
                                         0, // ! wrapping
                                         info->move->place->underground, 
                                         0, // ! wilderness
                                         1  // wilderness combat
                                         );

                Combat.place->is_wilderness_combat = 1;
                Combat.place->location.place = info->move->place;
                Combat.place->location.x = info->move->px;
                Combat.place->location.y = info->move->py;

                place_add_subplace(info->move->place, Combat.place, 
                                   info->move->px, info->move->py);

                Place = Combat.place;
        }

        mapSetPlace(Place);

        // *** Position the Player Companions ***

        // This is where the map overlays on the player side of the map get
        // placed, if any.
        if (!position_player_party(info))
                return false;

        // *** Position the Enemy Party Members ***

        if (info->move->npc_party) {
                combat_set_state(COMBAT_STATE_FIGHTING);
                if (!myPositionEnemy(info->move->npc_party, info->move->dx, info->move->dy, 
                                     !info->defend, Place)) {

                        log_begin("*** FORFEIT ***");
                        log_msg("Your opponent slips away!");
                        log_end(NULL);
                        combat_set_state(COMBAT_STATE_LOOTING);

                        // The npc party did not get added to the list because
                        // myPositionEnemy() failed. Add it to the list so that
                        // it gets cleaned up in the normal way.
                        list_add(&Combat.parties, 
                                 &info->move->npc_party->container_link.list);
                }
        }
        else if (info->camping) {

                combat_set_state(COMBAT_STATE_CAMPING);
                log_msg("Zzzz...");
        }
        else {
                struct v2 v2;
                v2.dx = info->move->dx;
                v2.dy = info->move->dy;
                v2.place = Place;
                combat_set_state(COMBAT_STATE_LOOTING);
                place_for_each_object(Place, myFindAndPositionEnemy, &v2);
        }

        player_party->forEachMember(mySetInitialCameraPosition, 0);

        if (combat_get_state() == COMBAT_STATE_FIGHTING) {
                player_party->enableRoundRobinMode();
                soundPlay(Combat.sound_enter, SOUND_MAX_VOLUME);
        }
        else if (combat_get_state() != COMBAT_STATE_CAMPING) {
                player_party->enableFollowMode();
        }

        // ---------------------------------------------------------------------
        // Force a map update. If an npc initiates combat then the event
        // handler will not run and do a repaint until the next event.
        // ---------------------------------------------------------------------

        mapUpdate(0);
        foogodRepaint();


        // ---------------------------------------------------------------------
        // Return to the main loop. Combat will continue from there.
        // ---------------------------------------------------------------------

        return true;
}

char combatGetState(void)
{
        switch (Combat.state) {
        case COMBAT_STATE_FIGHTING:
                return 'Y';
                break;
        case COMBAT_STATE_DONE:
                return 'N';
                break;
        case COMBAT_STATE_LOOTING:
                return 'V';
                break;
        case COMBAT_STATE_CAMPING:
                return 'K';
                break;
        }

        return 'N';
}

bool combatAddParty(class Party * party, int dx, int dy, bool located,
                       struct place *place, int x, int y)
{
        if (!located) {
                // Caller has not specified a location so use the normal
                // procedure.
                return myPositionEnemy(party, dx, dy, false, place);
        }
        // Special case: caller wants to put the party at (x, y). Duplicate the
        // code in myPositionEnemy except fill out the position info based on
        // caller's request.
        party->disembark();
        party->remove();

        memset(&party->pinfo, 0, sizeof (party->pinfo));
        party->pinfo.place = place;
        party->pinfo.x = x;
        party->pinfo.y = y;
        party->pinfo.dx = dx;
        party->pinfo.dy = dy;
        party->pinfo.formation = party->get_formation();
        party->pinfo.find_party = true;
        if (!party->pinfo.formation)
                party->pinfo.formation = formation_get_default();
        set_party_initial_position(&party->pinfo, x, y);
        myPutEnemy(party, &party->pinfo);
        return (party->pinfo.placed != 0);
}

void combat_exit(void)
{
        struct place *parent = 0;
        int x, y;

        // ------------------------------------------------------------
        // Ensure all party members are removed from the current place
        // so they don't get destroyed below.
        // ------------------------------------------------------------
        
        //player_party->removeMembers();

        
        // --------------------------------------------------------------------
        // Clean up the temporary combat place, if we used one. If we started
        // off in combat when we loaded this session then we need to remove
        // the temp place and temp map from the session's list of objects
        // to save. We can tell that we need to remove them if they have
        // non-null handles (those handles are only set by the loader, if
        // we create the tmp place during normal game play we don't set them).
        //
        // Before destroying the place I have to memorize it's location for the
        // next step.
        // --------------------------------------------------------------------

        if (Place->is_wilderness_combat) {

                assert(! Place->handle); // should not be top-level
                assert(place_get_parent(Place));

                if (Place->terrain_map->handle)
                        session_rm(Session, Place->terrain_map->handle);

                place_remove_and_destroy_all_objects(Place);
                parent = place_get_parent(Place);
                x = place_get_x(Place);
                y = place_get_y(Place);
                place_remove_subplace(parent, Place);

                // ------------------------------------------------------------
                // Bugfix: Invalidate the entire map from the vmask cache. If
                // you don't do this, then the next time the player enters
                // combat and we start looking up vmasks we will find old,
                // stale ones from this place, resulting in LOS bugs. That's
                // because the keys used by the vmask are built from the name
                // of the place, and for the combat map it is always the same
                // name.
                // ------------------------------------------------------------

                vmask_invalidate(Place, 0, 0, place_w(Place), place_h(Place));

                place_del(Place); // map deleted in here
                Combat.place = 0;

                // ------------------------------------------------------------
                // Relocate the player party back to the wilderness. This will
                // handle the place switch implicitly by setting the global
                // 'Place' pointer to the wilderness.
                // ------------------------------------------------------------

                player_party->relocate(parent, x, y);                

        }

        assert(NULL != player_party->getPlace());

        // --------------------------------------------------------------------
        // Force a map update. Although the map has been marked dirty by now,
        // we will not see a repaint until the next event if we do not act
        // now. This routine is called from the context of the main play loop,
        // not the event loop.
        // --------------------------------------------------------------------

        //mapUpdate(0);
        
}
