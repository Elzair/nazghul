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

#include "dtable.h"
#include "debug.h"

#include <assert.h>
#include <stdlib.h>

#define dtable_index(ptab,f1,f2) ((f1) * (ptab)->n_factions + (f2))

#define dtable_is_valid_faction(dtable,fac) \
        ((fac) >= 0 && (fac) < (dtable)->n_factions)

struct dtable *dtable_new(int n_factions)
{
        struct dtable *dtable;
        int row, col, index;

        /* allocate the "main" struct */
        dtable = (struct dtable*)calloc(1, sizeof(*dtable));
        assert(dtable);
        
        dtable->n_factions = n_factions;

        /* allocate the table of stack pointers */
        dtable->table = (hstack_t**)calloc(n_factions * n_factions, 
                                           sizeof(dtable->table[0]));
        assert(dtable->table);

        /* initialize each stack ptr in the table */
        index = 0;
        for (row = 0; row < n_factions; row++) {
                for (col = 0; col < n_factions; col++) {
                        dtable->table[index] = hstack_new();
                        index++;
                }
        }

        return dtable;        
}

static int dtable_check(struct dtable *dtable, int faction)
{
        if (! dtable_is_valid_faction(dtable, faction)) {
                warn("dtable_check: invalid faction=%d\n", faction);
                return -1;
        }

        return 0;
}

void dtable_set(struct dtable *dtable, int f1, int f2, int level)
{
        int index;

        if (dtable_check(dtable, f1))
                return;

        if (dtable_check(dtable, f2))
                return;

        if (level < dtable->lower_bound) {
                level = dtable->lower_bound;
        } else if (level > dtable->upper_bound) {
                level = dtable->upper_bound;
        }

        index = dtable_index(dtable, f1, f2);

        /* clear the stack */
        while (hstack_top(dtable->table[index]))
                hstack_pop(dtable->table[index]);

        /* push the new value */
        hstack_push(dtable->table[index], (void*)level);
}

int dtable_get(struct dtable *dtable, int f1, int f2)
{
        int index;

        if (dtable_check(dtable, f1)) {
                warn("dtable_get: defaulting to neutral\n");
                return 0;
        }

        if (dtable_check(dtable, f2)) {
                warn("dtable_get: defaulting to neutral\n");
                return 0;
        }

        index = dtable_index(dtable, f1, f2);
        return (int)hstack_top(dtable->table[index]);
}

void dtable_change(struct dtable *dtable, int f1, int f2, int delta)
{
        int level;

        level = dtable_get(dtable, f1, f2);
        level += delta;
        dtable_set(dtable, f1, f2, level);
}

static void dtable_del_entry(hstack_t *entry)
{
        if (! entry)
                return;

        while (hstack_top(entry))
                hstack_pop(entry);

        hstack_del(entry);
}

void dtable_del(struct dtable *dtable)
{
        int row, col, index;

        assert(dtable);

        if (dtable->table) {

                index = 0;
                for (row = 0; row < dtable->n_factions; row++) {
                        for (col = 0; col < dtable->n_factions; col++) {
                                dtable_del_entry(dtable->table[index]);
                                index++;
                        }
                }

                free(dtable->table);
        }
        free(dtable);
}
