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

#ifndef dtable_h
#define dtable_h

#include "macros.h"

BEGIN_DECL

#include "hstack.h"

#define FACTION_NONE               0
#define DTABLE_DEFAULT_LOWER_BOUND -2
#define DTABLE_DEFAULT_UPPER_BOUND 2
#define DTABLE_DEFAULT_HOSTILE     -2
#define DTABLE_DEFAULT_ALLIES      2

#define dtable_are_hostile(dtable,f1, f2) \
        (dtable_get((dtable),(f1),(f2)) <= dtable_hostile)

#define dtable_are_allies(dtable,f1, f2) \
        (dtable_get((dtable),(f1),(f2)) >= dtable_allies)

#define dtable_set_hostile(tab,lvl)     ((tab)->hostile     = (lvl))
#define dtable_set_allies(tab,lvl)      ((tab)->allies      = (lvl))
#define dtable_set_lower_bound(tab,lvl) ((tab)->lower_bound = (lvl))
#define dtable_set_upper_bound(tab,lvl) ((tab)->upper_bound = (lvl))

struct dtable {
        int n_factions;
        int hostile;
        int allies;
        int lower_bound;
        int upper_bound;
        hstack_t **table;
};

extern struct dtable *dtable_new(int n_factions);
extern void dtable_set(struct dtable *dtable, int f1, int f2, int level);
extern int dtable_get(struct dtable *dtable, int f1, int f2);
extern void dtable_change(struct dtable *dtable, int f1, int f2, int level);
extern void dtable_del(struct dtable *dtable);
extern void dtable_push(struct dtable *dtable, int f1, int f2, int level);
extern int dtable_pop(struct dtable *dtable, int f1, int f2);
extern int dtable_bottom(struct dtable *dtable, int f1, int f2);

END_DECL

#endif
