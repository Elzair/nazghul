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

#include "ptable.h"

#include "debug.h"

#include <assert.h>
#include <stdlib.h>

#define ptable_index(ptab,mmode,pclass) \
    ((pclass) * (ptab)->n_mmode + (mmode))
#define ptable_is_valid_mmode(ptable,mmode) \
        ((mmode) >= 0 && (mmode) < (ptable)->n_mmode)
#define ptable_is_valid_pclass(ptable,pclass) \
        ((pclass) >= 0 && (pclass) < (ptable)->n_pclass)

struct ptable *ptable_new(int n_mmodes, int n_pclass)
{
        struct ptable *ptable;

        ptable = (struct ptable*)calloc(1, sizeof(*ptable));
        assert(ptable);

        ptable->n_mmode = n_mmodes;
        ptable->n_pclass = n_pclass;
        ptable->table = (int*)calloc(n_mmodes * n_pclass, sizeof(int));
        assert(ptable->table);

        return ptable;
        
}

static int ptable_check(struct ptable *ptable, int mmode, int pclass)
{
        if (! ptable_is_valid_mmode(ptable, mmode)) {
                warn("ptable_check: invalid mmode=%d\n", mmode);
                return -1;
        }

        if (! ptable_is_valid_pclass(ptable, pclass)) {
                warn("ptable_check: invalid pclass=%d\n", pclass);
                return -1;
        }

        return 0;
}

void ptable_set(struct ptable *ptable, int mmode, int pclass, 
                int cost)
{
        int index;

        if (ptable_check(ptable, mmode, pclass))
                return;

        if (cost < 0 || cost > PTABLE_IMPASSABLE) {
                warn("ptable_set: invalid cost=%d\n", cost);
                return;
        }

        index = ptable_index(ptable, mmode, pclass);
        ptable->table[index] = cost;
}

int ptable_get(struct ptable *ptable, int mmode, int pclass)
{
        int index;

        if (ptable_check(ptable, mmode, pclass)) {
                warn("ptable_get: defaulting to impassable\n");
                return PTABLE_IMPASSABLE;
        }

        index = ptable_index(ptable, mmode, pclass);
        return ptable->table[index];
}

void ptable_del(struct ptable *ptable)
{
        assert(ptable);

        if (ptable->table)
                free(ptable->table);
        free(ptable);
}
