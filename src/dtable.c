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

        dtable = (struct dtable*)calloc(1, sizeof(*dtable));
        assert(dtable);

        dtable->n_factions = n_factions;
        dtable->table = (int*)calloc(n_factions * n_factions, sizeof(int));
        assert(dtable->table);

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

        if (level < dtable->lower_bound || 
            level > dtable->upper_bound) {
                warn("dtable_set: invalid level=%d\n", level);
                return;
        }

        index = dtable_index(dtable, f1, f2);
        dtable->table[index] = level;
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
        return dtable->table[index];
}

void dtable_del(struct dtable *dtable)
{
        assert(dtable);

        if (dtable->table)
                free(dtable->table);
        free(dtable);
}
