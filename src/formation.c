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

#include "formation.h"
#include "common.h"

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
        {-6, 6},                /* 28 */
};

static struct formation default_formation;

int formation_init(void)
{
        default_formation.n = array_sz(default_formation_entries);
        default_formation.entry = default_formation_entries;
        return 0;
}

struct formation *formation_get_default(void)
{
        return &default_formation;
}
