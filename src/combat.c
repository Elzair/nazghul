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
#include "cmd.h"
#include "formation.h"

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

#define VMAP_VISIT(x,y) (Combat.vmap[place_w(Place) * (y) + (x)] = 0)
#define VMAP_VISITED(x,y) (Combat.vmap[place_w(Place) * (y) + (x)])

#define LOC_IS_SAFE(x,y,pmask) \
    (!place_off_map(Place, x, y) && \
     place_is_passable(Place, x, y, pmask, 0) && \
     !place_is_occupied(Place, x, y))

/* Formation pattern -- party facing north (dx=0,dy=-1), origin at the leader's
 * position */

enum combat_faction_status {
        COMBAT_FACTION_EXISTS,
        COMBAT_FACTION_GONE,
        COMBAT_FACTION_CHARMED,
        COMBAT_FACTION_CAMPING
};

static struct {
        struct place place;
        enum combat_state state;
        char vmap[7 * 7];       // visited map (used to search for positions)
        struct list parties;

        bool tmp_terrain_map;
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
        consolePrint("\n\n*** %s ***\n\n", msg);
}

enum combat_state combat_get_state(void)
{
        return Combat.state;
}

void combat_set_state(enum combat_state new_state)
{
        // ---------------------------------------------------------------------
        // Interesting state transitions:
        //
        // =====================================================================
        // old state              | new state              | result
        // =====================================================================
        // COMBAT_STATE_DONE      | COMBAT_STATE_FIGHTING  | entry to combat
        // COMBAT_STATE_DONE      | COMBAT_STATE_LOOTING   | entry to non-hostile
        // COMBAT_STATE_DONE      | COMBAT_STATE_CAMPING   | entry to camping
        // COMBAT_STATE_FIGHTING  | COMBAT_STATE_DONE      | defeat
        // COMBAT_STATE_FIGHTING  | COMBAT_STATE_LOOTING   | victory
        // COMBAT_STATE_LOOTING   | COMBAT_STATE_FIGHTING  | hostiles entered
        // COMBAT_STATE_LOOTING   | COMBAT_STATE_DONE      | exit normally
        // COMBAT_STATE_CAMPING   | COMBAT_STATE_FIGHTING  | ambush
        // COMBAT_STATE_CAMPING   | COMBAT_STATE_DONE      | exit camping
        // =====================================================================
        //
        // ---------------------------------------------------------------------

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

void combat_attack(class Character *attacker, class ArmsType *weapon, class Character *defender)
{
        int hit;
        int def;
        int damage;
        int armor;
        bool miss;

        miss = ! weapon->fire(defender, attacker->getX(), attacker->getY());
        attacker->useAmmo();
        attacker->setAttackTarget(defender);

        if (miss) {
                consolePrint("miss!\n");
                return;
        }

        // Roll to hit.
        hit = dice_roll(2, 6) + weapon->getHit();
        def = dice_roll(2, 6) + defender->getDefend();
        if (hit < def) {
                consolePrint("miss!\n");
                return;
        } else {
                consolePrint("hit! ");
        }

        // roll for damage
        damage = weapon->getDamage();
        armor = defender->getArmor();
        consolePrint("Rolled %d damage, %d armor ", damage, armor);
        damage -= armor;
        damage = max(damage, 0);
        consolePrint("for %d total damage, ", damage);
        defender->damage(damage);

        consolePrint("%s!\n", defender->getWoundDescription());
}


static int location_is_safe(struct position_info *info)
{
        struct astar_node *path;
        int flags = PFLAG_IGNOREBEINGS;
        int edge_x = 0, edge_y = 0;
        struct astar_search_info as_info;

        // Is it passable?
        if (!place_is_passable(info->place, info->px, info->py, info->pmask, 0)) {
                printf("impassable\n");
                return -1;
        }
        // Is it occupied?
        if (place_is_occupied(info->place, info->px, info->py)) {
                printf("occupied\n");
                return -1;
        }

        memset(&as_info, 0, sizeof (as_info));

        if (info->find_edge) {

                assert(info->dx || info->dy);
                assert(!info->dx || !info->dy); // assume no diagonals for now

                printf("searching for path to ");

                if (info->dx < 0) {
                        // facing west, find path back to east edge
                        edge_x = place_w(info->place) - 1;
                        flags |= PFLAG_VERT;
                        printf("east ");
                }
                else if (info->dx > 0) {
                        // facing east, find path to back west edge
                        edge_x = 0;
                        flags |= PFLAG_VERT;
                        printf("west ");
                }
                else if (info->dy < 0) {
                        // facing north, find path back to south edge
                        edge_y = place_h(info->place) - 1;
                        flags |= PFLAG_HORZ;
                        printf("north ");
                }
                else {
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

                path = place_find_path(info->place, &as_info, info->pmask, NULL);

                if (!path)
                        printf("no path back to edge\n");

        }
        else if (info->find_party) {
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

                path = place_find_path(info->place, &as_info, info->pmask, NULL);

                if (!path)
                        printf("no path back to party\n");
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

        printf("checking [%d %d]...", info->px, info->py);

        // translate the map coords into an rmap index
        index = info->py - info->ry;
        index *= info->rw;
        index += (info->px - info->rx);

        // If the current location is off-map, outside of the placement
        // rectangle or already visited then discontinue the search.  
        if (rmap[index]) {
                printf("already visited [%d]\n", index);
                return -1;      // already visited
        }
        if (info->px < info->rx || info->px >= info->rx + info->rw ||
            info->py < info->ry || info->py >= info->ry + info->rh) {
                printf("outside the placement area\n");
                return -1;      // outside the placement rect
        }
        if (place_off_map(info->place, info->px, info->py)) {
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
        info->pmask = pm->getPmask();
        memset(rmap, 0, sizeof (rmap));

        // set the preferred location
        info->px = pm->getX();
        info->py = pm->getY();

        printf("Placing %s\n", pm->getName());

        if (combat_find_safe_position(info) == -1) {
                // If I can't place a member then I can't place it.
                printf("*** Can't place %s ***\n", pm->getName());
                return false;
        }

        pm->setX(info->px);
        pm->setY(info->py);
        printf("Put '%s' at [%d %d]\n", pm->getName(), info->px, info->py);
        pm->setPlace(Place);
        place_add_object(Place, pm);
        info->placed++;

        /* Check if we need to go back to fighting */
        if (combat_get_state() != COMBAT_STATE_FIGHTING &&
            pm->party->isHostile(player_party->alignment)) {
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

        printf("Moved party start position to [%d %d]\n", pinfo->x, pinfo->y);;

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

        if (info->place != &Combat.place) {
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
        info->pmask = 0;
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
        info->pmask = pm->getPmask();
        memset(rmap, 0, sizeof (rmap));
        info->px = pm->getX();
        info->py = pm->getY();
        printf("Placing %s\n", pm->getName());

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
                        printf("Putting %s on start location [%d %d]\n",
                               pm->getName(), info->x, info->y);
                        info->px = info->x;
                        info->py = info->y;
                }
                else {
                        // init the position info to search again
                        memset(rmap, 0, sizeof (rmap));
                        info->px = leader->getX();
                        info->py = leader->getY();
                        printf("Retrying %s\n", pm->getName());

                        if (combat_find_safe_position(info) == -1) {
                                printf("Putting %s on start location "
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
        //pm->setAlignment(player_party->alignment);
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
bool combat_rendezvous_party(int max_path_len)
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

                if (NULL == pc             ||
                    pc->isDead()           ||
                    false == pc->isOnMap() ||
                    pc->isAsleep()         ||
                    (pc->getX() == rx && pc->getY() == ry))
                        continue;

                memset(&as_info, 0, sizeof (as_info));
                as_info.x0 = pc->getX();
                as_info.y0 = pc->getY();
                as_info.x1 = rx;
                as_info.y1 = ry;
                as_info.flags = PFLAG_IGNOREBEINGS;
                pc->path = place_find_path(Place, &as_info, pc->getPmask(), NULL);

                if (!pc->path) {
                        consolePrint("%s cannot make the rendezvous!\n",
                                     pc->getName());
                        abort = true;
                }
                else if (max_path_len > 0 && pc->path->len > max_path_len) {
                        consolePrint("%s is too far away!\n", pc->getName());
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
                                pc->remove();   // try this... seems ok
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
        combat_set_state(COMBAT_STATE_DONE);
        return true;

}

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
        terrain_map_destroy(map);
}

static void myPutEnemy(class NpcParty * foe, struct position_info *pinfo)
{
        foe->forEachMember(myPutNpc, pinfo);
        if (foe->pinfo.placed)
                list_add(&Combat.parties, &foe->container_link.list);
}

static bool myPositionEnemy(class NpcParty * foe, int dx, int dy, bool defend, struct place *place)
{
        assert(foe->getSize());

        combat_fill_position_info(&foe->pinfo, place, foe->getX(), foe->getY(), dx, dy, defend);
        foe->pinfo.formation = foe->get_formation();
        if (!foe->pinfo.formation)
                foe->pinfo.formation = formation_get_default();

        // Check for a map overlay.
        if (foe->vehicle && foe->vehicle->getObjectType()->map &&
            Place == &Combat.place) {
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
        if (!myPositionEnemy(foe, directionToDx(dir), directionToDy(dir), false)) {
                printf("ambush failed for %s\n", foe->getName());
                // I think the party will get cleaned up on exit...
                return;
        }

        combat_set_state(COMBAT_STATE_FIGHTING);

        consolePrint("\n*** Ambush ***\n\n");

        player_party->ambushWhileCamping();
}
#endif

enum combat_faction_status combat_get_hostile_faction_status(void)
{
        // ---------------------------------------------------------------------
        // Search the list of all objects in the current place. For each object
        // that is not a member of the player party check its native and
        // charmed alignment.
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

                if (obj->isHostile(player_party->getAlignment())) {
                        return COMBAT_FACTION_EXISTS;
                }

                // -------------------------------------------------------------
                // Among non-player controlled, non-hostile objects, check for
                // any that are charmed to side with the player.
                // -------------------------------------------------------------

                if (!found_charmed_hostile &&
                    obj->isNativelyHostile(player_party->getAlignment())) {
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

                // -------------------------------------------------------------
                // Non-player-controlled objects (objects not in the player
                // party) are handled by combat_get_hostile_faction_status(),
                // not here.
                // -------------------------------------------------------------

                if (! obj->isPlayerPartyMember())
                        continue;

                // -------------------------------------------------------------
                // Among the player-controlled objects anything that is not
                // hostile implies a player faction exists.
                // -------------------------------------------------------------

                if (! obj->isHostile(player_party->getAlignment())) {
                        return COMBAT_FACTION_EXISTS;
                }

                // -------------------------------------------------------------
                // Among player-controlled, hostile objects, check for any that
                // are charmed to against the player.
                // -------------------------------------------------------------

                if (!found_charmed_member &&
                    !obj->isNativelyHostile(player_party->getAlignment())) {
                        found_charmed_member = true;
                }
        }

        if (found_charmed_member)
                return COMBAT_FACTION_CHARMED;

        return COMBAT_FACTION_GONE;
        
}

static void combat_uncharm_all_hostiles()
{
        struct list *head;
        struct list *elem;
        class Object *obj;

        head = place_get_all_objects(Place);
        list_for_each(head, elem) {

                obj = outcast(elem, class Object, turn_list);

                if (obj->isNativelyHostile(player_party->getAlignment()) &&
                    !obj->isHostile(player_party->getAlignment())) {
                        obj->unCharm();
                }
        }
}

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
                        // -----------------------------------------------------
                        // All party members charmed and hostiles are closing
                        // in. Uncharm the party members to avoid deadlocks
                        // where the hostiles can't or won't finish off the
                        // charmed party members.
                        // -----------------------------------------------------
                        combat_set_state(COMBAT_STATE_FIGHTING);
                        break;

                        // -----------------------------------------------------
                        // Well, both sides are all charmed. An interesting
                        // case. Again, to avoid deadlocks, uncharm the party
                        // members. To be fair I'll uncharm the hostiles, too.
                        // -----------------------------------------------------
                        combat_uncharm_all_hostiles();
                        combat_set_state(COMBAT_STATE_FIGHTING);
                        break;

                case COMBAT_FACTION_GONE:
                        // -----------------------------------------------------
                        // No hostiles around. Loot at will. Uncharm or we'll
                        // definitely deadlock.
                        // -----------------------------------------------------
                        combat_set_state(COMBAT_STATE_LOOTING);
                        break;

                default:
                        assert(false);
                        break;
                }

                break;

        case COMBAT_FACTION_GONE:

                // -------------------------------------------------------------
                // In all of these cases combat is over. Simple. If combat is
                // ocurring in the special combat place then we need to clean it
                // up by calling combat_exit().
                // -------------------------------------------------------------
                
                combat_set_state(COMBAT_STATE_DONE);

//                if (Place == &Combat.place)
                combat_exit();

                break;
                
        case COMBAT_FACTION_CAMPING:

                switch (hostile_faction_status) {
                        
                case COMBAT_FACTION_EXISTS:
                        // -----------------------------------------------------
                        // Ambush!
                        // -----------------------------------------------------                        
                        combat_set_state(COMBAT_STATE_FIGHTING);
                        break;

                case COMBAT_FACTION_GONE:
                        // -----------------------------------------------------
                        // No hostiles around. Change nothing.
                        // -----------------------------------------------------
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

        if (!obj->isType(NPCPARTY_ID))
                return;

        info = (struct v2 *) data;
        if (((class NpcParty *) obj)->isHostile(player_party->alignment))
                combat_set_state(COMBAT_STATE_FIGHTING);
        myPositionEnemy((class NpcParty *) obj, info->dx, info->dy, false, info->place);
}

int combatInit(void)
{
        // This is called once at the beginning of the game
        memset(&Combat, 0, sizeof (Combat));

        /* Initialize the place to safe defaults */
        list_init(&Combat.place.list);
        list_init(&Combat.place.vehicles);
        list_init(&Combat.place.turn_list);
        Combat.place.type = combat_place;
        Combat.place.name = "Combat map";
        Combat.place.objects = hash_create(11);
        Combat.state = COMBAT_STATE_DONE;
        Combat.place.is_wilderness_combat = true;
        Combat.place.scale = NON_WILDERNESS_SCALE;
        list_init(&Combat.parties);

        return 0;
}

int combatLoad(class Loader * loader)
{
        if (!loader->matchToken('{'))
                return -1;

        while (!loader->matchToken('}')) {
                if (loader->matchWord("enter")) {
                        if (!loader->getString(&Combat.sound_enter))
                                goto fail;
                }
                else if (loader->matchWord("defeat")) {
                        if (!loader->getString(&Combat.sound_defeat))
                                goto fail;
                }
                else if (loader->matchWord("victory")) {
                        if (!loader->getString(&Combat.sound_victory))
                                goto fail;
                }
                else {
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

        map = terrain_map_create("tmp_combat_map", COMBAT_MAP_W, COMBAT_MAP_H);
        assert(map);            // Fails on failed malloc(), whereupon we are hosed

        terrain = place_get_terrain(place, x, y);
        terrain_map_fill(map, 0, 0, COMBAT_MAP_W, COMBAT_MAP_H, terrain);

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
                printf("create_camping_map() IMPOSSIBLE (OK, improbable...)\n"
                       "  strange terrain %p (tag '%s' name '%s')\n"
                       "  was in no palette!\n",
                       terrain, terrain->tag, terrain->name);
                assert(0);
        }

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

        map = terrain_map_create("tmp_combat_map", COMBAT_MAP_W, COMBAT_MAP_H);
        if (!map)
                return 0;

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
                printf("maps '%s' '%s', palettes '%s' '%s'\n",
                       party_map->tag, npc_party_map->tag,
                       party_map->palette->tag, npc_party_map->palette->tag);
                int palette_tags_match = !strcmp(party_map->palette->tag,
                                                 npc_party_map->palette->tag);
                if (!palette_tags_match) {
                        printf("create_temporary_terrain_map() warning: \n"
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
            Place == &Combat.place) {
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
                // printf("party overlay, party over vehicle\n");
                combat_overlay_map(vehicle->getObjectType()->map,
                                   &player_party->pinfo, 0);
        }
        // Finally, since there is no vehicle map check for a camping map.
        else if (cinfo->camping && player_party->campsite_map) {
                // printf("party overlay, party is camping\n");
                combat_overlay_map(player_party->campsite_map, 
                                   &player_party->pinfo, 0);
        }

        player_party->remove();
        player_party->for_each_member(combat_place_character, &player_party->pinfo);
        return true;
}

bool combat_enter(struct combat_info * info)
{
        struct location loc;

        if (player_party->allDead())
                // Yes, this can happen in some rare circumstances...
                return false;

        // *** Memorize Entry State ***

        // ---------------------------------------------------------------------
        // Default to the entry point as the combat exit location for the
        // player party.
        // ---------------------------------------------------------------------

        loc.place = info->move->place;
        loc.x     = info->move->x;
        loc.y     = info->move->y;
        player_party->setCombatExitDestination(&loc);


        // *** Initialize Combat Globals ***

        Combat.tmp_terrain_map = false;
        Combat.enemy_vehicle = NULL;
        Combat.round = 0;
        Cursor->remove();
        list_init(&Combat.parties);

        // *** Initialize the Temporary Combat Place ***

        if (info->move->place->type != wilderness_place &&
            info->move->place->type != town_place) {
                // When not in a town or the wilderness (i.e., dungeon) use the
                // current map for combat.
                Place = info->move->place;
        }
        else {

                // Town or wilderness combat uses a temporary terrain map which
                // we create on the fly.
                Combat.place.terrain_map = create_temporary_terrain_map(info);
                Combat.tmp_terrain_map = true;

                // fixme -- instead of asserting I should use a last-resort map
                // that does not require allocation.
                // 
                // SAM: On the other hand, this failure only occurs due to 
                //      malloc() failure, whereupon we are hosed anyways..
                assert(Combat.place.terrain_map);

                // fixme -- are these obsolete now?
                Combat.place.location.place = info->move->place;
                Combat.place.location.x = info->move->x;
                Combat.place.location.y = info->move->y;

                Combat.place.underground = info->move->place->underground;

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
                combat_set_state(COMBAT_STATE_FIGHTING);
                if (!myPositionEnemy(info->move->npc_party, info->move->dx, info->move->dy, !info->defend, Place)) {

                        consolePrint("\n*** FORFEIT ***\n\n");
                        consolePrint("Your opponent slips away!\n");
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
                consolePrint("Zzzz...");
                consoleRepaint();
        }
        else {
                struct v2 v2;
                v2.dx = info->move->dx;
                v2.dy = info->move->dy;
                v2.place = Place;
                combat_set_state(COMBAT_STATE_LOOTING);
                place_for_each_object(Place, myFindAndPositionEnemy, &v2);
        }

        player_party->for_each_member(mySetInitialCameraPosition, 0);

        if (combat_get_state() == COMBAT_STATE_FIGHTING) {
                player_party->enableRoundRobinMode();
                soundPlay(Combat.sound_enter, SOUND_MAX_VOLUME);
        }
        else if (combat_get_state() != COMBAT_STATE_CAMPING) {
                player_party->enableFollowMode();
        }

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

bool combatAddNpcParty(class NpcParty * party, int dx, int dy, bool located,
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
        // ---------------------------------------------------------------------
        // Ensure all party members are removed from the current place so they
        // don't get destroyed below.
        // ---------------------------------------------------------------------
        
        player_party->removeMembers();
        
        // ---------------------------------------------------------------------
        // Clean up the temporary combat place.
        // ---------------------------------------------------------------------

        if (Place == &Combat.place)
                place_remove_and_destroy_all_objects(Place);

        // ---------------------------------------------------------------------
        // Relocate the player party back to the wilderness. This will handle
        // the place switch implicitly by setting the global 'Place' pointer to
        // the wilderness.
        //
        // Addendum: in some corner cases the player party has already been
        // relocated. Specifically, if a party member casts a Gate travel spell
        // and the moongate empties onto the wilderness, then the gate code
        // already relocated the party.
        // ---------------------------------------------------------------------

        if (NULL != place_get_parent(Place)) {
                player_party->relocate(place_get_parent(Place), 
                                       place_get_x(Place), 
                                       place_get_y(Place));
        }
        
        assert(NULL != player_party->getPlace());
}
