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

#include "vmask.h"
#include "tree.h"
#include "list.h"
#include "place.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>


#define VMASK_MAX_KEY_LEN 64
#define VMASK_MAX_ENTRIES 128
#define VMASK_HI_WATER    VMASK_MAX_ENTRIES
#define VMASK_LO_WATER    (VMASK_HI_WATER - VMASK_HI_WATER / 4)

#define vmask_key(V)      ((V)->tree.key.s_key)

struct vmask {
        struct tree tree;  // used by lookup-table
        struct list list;  // used by lru-q
        char *data;        // visibility mask data
};


static struct tree *vmask_root;
static struct list vmask_q;
static int vmask_n_entries;

void vmask_dump(struct vmask *vmask)
{
        int x, y, i = 0;

        //dbg("vmask_dump: %s\n", vmask_key(vmask));

        for (x = 0; x < VMASK_W; x++) {
                printf("%d", x % 10);
        }
        printf("\n");

        for (y = 0; y < VMASK_H; y++) {
                for (x = 0; x < VMASK_W; x++) {
                        printf("%c", vmask->data[i] ? '#' : '.');
                        i++;
                }
                printf(" %d\n", y % 10);
        }
}

static void vmask_error(const char *msg)
{
        fprintf(stderr, "vmask.c: %s\n", msg);
        exit(1);
}

static void vmask_make_key(char *key, struct place *place, int x, int y)
{
        snprintf(key, VMASK_MAX_KEY_LEN, "%d:%d:%p", x, y, place);
}


static struct vmask *vmask_lookup(const char *key)
{
        struct vmask *vmask;
        struct tree *tree;

        //dbg("vmask_lookup: %s\n", key);

        tree = tree_s_search(vmask_root, key);
        if (NULL == tree)
                return NULL;

        vmask = tree_entry(tree, struct vmask, tree);
        return vmask;
}

static void vmask_delete(struct vmask *vmask)
{
        //dbg("vmask_delete: %s\n", vmask_key(vmask));
        list_remove(&vmask->list);
        tree_delete(&vmask_root, &vmask->tree);
        free(vmask->tree.key.s_key);
        free(vmask);
        vmask_n_entries--;
}

static void vmask_insert(struct vmask *vmask)
{
        //printf("vmask_insert: %s\n", vmask_key(vmask));
        list_add(&vmask_q, &vmask->list);
        tree_insert(&vmask_root, &vmask->tree);
        vmask_n_entries++;
}

static void vmask_purge(void)
{
        struct vmask *vmask;
        struct list *tail;

        //printf("vmask_purge\n");

        // --------------------------------------------------------------------
        // Remove and destroy the least-recently-used vmasks until we are down
        // to the low-water mark.
        // --------------------------------------------------------------------

        while(vmask_n_entries > VMASK_LO_WATER) {
                tail = vmask_q.prev;
                vmask = list_entry(tail, struct vmask, list);
                vmask_delete(vmask);
        }
}

static void vmask_los(struct vmask *vmask, struct place *place, 
                      int center_x, int center_y)
{
        /* First, build an "alpha mask", which is a grid corresponding to the
         * surrounding tiles. Each cell in the grid indicates if a tile blocks
         * line-of-sight or lets it pass. */
	int x;
        int y;
        int start_x = center_x - VMASK_W / 2;
        int start_y = center_y - VMASK_H / 2;
        int end_x   = start_x + VMASK_W;
        int end_y   = start_y + VMASK_H;
	int index   = 0;

        for (y = start_y; y < end_y; y++) {
                for (x = start_x; x < end_x; x++) {
                        LosEngine->alpha[index] = place_visibility(place, x, 
                                                                   y);
			index++;
		}
	}
        
        /* Next invoke the los engine and copy the results into the new
         * vmask */
        LosEngine->r = max(VMASK_W, VMASK_H);
        LosEngine->compute(LosEngine);
        memcpy(vmask->data, LosEngine->vmask, VMASK_SZ);
}

static struct vmask *vmask_create(const char *key, struct place *place, int x, int y)
{
        struct vmask *vmask;

        //dbg("vmask_create: %s\n", key);

        // --------------------------------------------------------------------
        // Remove least-recently-used vmasks if we need to make room for
        // a new one.
        // --------------------------------------------------------------------

        if (vmask_n_entries >= VMASK_HI_WATER) {
                vmask_purge();
        }

        // --------------------------------------------------------------------
        // Allocate and initialize a new vmask structure. Setup the tree to
        // use the given string key.
        // --------------------------------------------------------------------

        vmask = (struct vmask*)calloc(1, sizeof(struct vmask) + VMASK_SZ);
        if (NULL == vmask)
                return vmask;

        list_init(&vmask->list);
        vmask->data = (char *)vmask + sizeof(struct vmask);
        vmask->tree.key_type  = tree_s_key;
        vmask->tree.key.s_key = strdup(key);
        if (NULL == vmask->tree.key.s_key) {
                free(vmask);
                return NULL;
        }

        // --------------------------------------------------------------------
        // Finally, fill out the vmask based on the line-of-sight
        // characteristics of the given location, and then insert it into the
        // tree and the priority list.
        // --------------------------------------------------------------------

        vmask_los(vmask, place, x, y);
        vmask_insert(vmask);
        /*vmask_dump(vmask);*/

        return vmask;
}

static void vmask_prioritize(struct vmask *vmask)
{
        // --------------------------------------------------------------------
        // Move the vmask from wherever it was in the least-recently-used queue
        // up to the front.
        // --------------------------------------------------------------------

        list_remove(&vmask->list);
        list_add(&vmask_q, &vmask->list);
}

// ----------------------------------------------------------------------------
//
//                                Public API
//
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
// Call this once to initialize the library on startup.
// ----------------------------------------------------------------------------

int vmask_init(void)
{
        vmask_root = NULL;
        list_init(&vmask_q);
        vmask_n_entries = 0;

        return 0;
}


// ----------------------------------------------------------------------------
// Fetch the vmask corresponding to the given location. The vmask is gauranteed
// to be valid until the next call to vmask_get(), at which point all bets are
// off. I expect the typical usage will be for callers to only use one at a
// time, and to make their own copy in the rare cases where they need to deal
// with more than one at a time.
// ----------------------------------------------------------------------------

char *vmask_get(struct place *place, int x, int y)
{
        struct vmask *vmask;
        char key[VMASK_MAX_KEY_LEN + 1];

        x = place_wrap_x(place, x);
        y = place_wrap_y(place, y);

        vmask_make_key(key, place, x, y);
        vmask = vmask_lookup(key);
        if (NULL == vmask) {
                vmask = vmask_create(key, place, x, y);
        }
        if (NULL == vmask)
                vmask_error("Can't create vmask");
        vmask_prioritize(vmask);

        //vmask_dump(vmask);

        return vmask->data;
}


// ----------------------------------------------------------------------------
// Invalidate all vmasks in the area surrounding the given location. You should
// call this whenever you do something that will change the line-of-sight
// properties of a tile. It will force the surrounding vmasks to recompute
// their line-of-sight the next time somebody tries to fetch them.
//
// Note that the 'w' and 'h' indicate a rectangle of tiles whose line-of-sight
// property have changed. For a single tile they would each be 1. Don't worry
// about trying to evaluate the extent of the damage: the function will
// automatically figure out which vmasks are affected.
// ----------------------------------------------------------------------------

void vmask_invalidate(struct place *place, int x, int y, int w, int h)
{
        char key[VMASK_MAX_KEY_LEN + 1];
        struct vmask *vmask;

        int start_x = x - MAP_TILE_W / 2;
        int start_y = y - MAP_TILE_H / 2;
        int end_x   = start_x + w + MAP_TILE_W;
        int end_y   = start_y + h + MAP_TILE_H;

       //dbg("vmask_invalidate: %s [%d %d %d %d]\n", place->name, x,  y, w, h);

        for (y = start_y; y < end_y; y++) {
                int wrap_y =  place_wrap_y(place, y);
                for (x = start_x; x < end_x; x++) {
                        vmask_make_key(key, place, place_wrap_x(place, x), 
                                       wrap_y);
                        vmask = vmask_lookup(key);
                        if (NULL != vmask) {
                                vmask_delete(vmask);
                        }
                }
        }
}

void vmask_flush_all(void)
{
        struct vmask *vmask;
        struct list *tail;

        while(vmask_n_entries) {
                tail = vmask_q.prev;
                vmask = list_entry(tail, struct vmask, list);
                vmask_delete(vmask);
        }
}
