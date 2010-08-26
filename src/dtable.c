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
#include "session.h" /* for save */

#include <assert.h>
#include <stdlib.h>

#define dtable_is_valid_faction(dtable,fac) \
        ((fac) >= 0 && (fac) < (dtable)->n_factions)

#define dtable_clamp_level(dtable,lvl) \
        clamp((lvl), (dtable)->lower_bound, (dtable)->upper_bound)

struct dtable *dtable_new(int n_factions)
{
        struct dtable *dtable;

        /* allocate the "main" struct */
        dtable = (struct dtable*)calloc(1, sizeof(*dtable));
        assert(dtable);
        
        dtable->n_factions = n_factions;

        /* allocate the table of stack pointers */
        dtable->table = (int*)calloc(n_factions * n_factions, 
                                     sizeof(dtable->table[0]));
        assert(dtable->table);

        /* For now, hardcode the table limits and settings. Note that we must
         * set the upper and lower bounds before poking any entries into the
         * table. */
        dtable_set_hostile(dtable, DTABLE_DEFAULT_HOSTILE);
        dtable_set_allies(dtable, DTABLE_DEFAULT_ALLIES);
        dtable_set_upper_bound(dtable, DTABLE_DEFAULT_UPPER_BOUND);
        dtable_set_lower_bound(dtable, DTABLE_DEFAULT_LOWER_BOUND);
        
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

static int dtable_index(struct dtable *dtable, int f1, int f2)
{
        if (dtable_check(dtable, f1))
                return -1;

        if (dtable_check(dtable, f2))
                return -1;

        return f1 * dtable->n_factions + f2;
        
}

void dtable_set(struct dtable *dtable, int f1, int f2, int level)
{
        int index;

        dtable_clamp_level(dtable, level);

        if ((index = dtable_index(dtable, f1, f2)) < 0)
                return;

        dtable->table[index] = level;
        
        /* mirror the change on the other half of the table */
        index =  dtable_index(dtable, f2, f1);
        dtable->table[index] = level;
}

int dtable_get(struct dtable *dtable, int f1, int f2)
{
        int index;

        if ((index = dtable_index(dtable, f1, f2)) < 0) {
                warn("dtable_get: defaulting to neutral\n");
                return 0;
        }

        return dtable->table[index];
}

void dtable_del(struct dtable *dtable)
{
        assert(dtable);

        if (dtable->table) {
                free(dtable->table);
        }
        free(dtable);
}


void dtable_save(struct dtable *dtable, struct save *save)
{
        int rows;
        int cols;
        int index;

        save->enter(save, "(kern-mk-dtable\n");

        index = 0;
        for (rows = 0; rows < dtable->n_factions; rows++) {
                save->write(save, "(list ");
                for (cols = 0; cols < dtable->n_factions; cols++) {
                        save->write(save, "%2d ", dtable->table[index]);
                        index++;
                }
                save->append(save, ")\n");
        }
        
        save->exit(save, ")\n");
}

void dtable_inc(struct dtable *dtable, int f1, int f2)
{
        int level;

        level = dtable_get(dtable, f1, f2);
        dtable_set(dtable, f1, f2, level+1);
}

void dtable_dec(struct dtable *dtable, int f1, int f2)
{
        int level;

        level = dtable_get(dtable, f1, f2);
        dtable_set(dtable, f1, f2, level-1);

}

extern const char *dtable_describe(struct dtable *dtable, int f1, int f2)
{
        int level = dtable_get(dtable, f1, f2);
        
        if (level <= dtable->hostile)
                return "hostile";
        else if (level >= dtable->allies)
                return "allied";
        else
                return "neutral";
}
