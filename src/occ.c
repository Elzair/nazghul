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

#include "occ.h"
#include "object.h"
#include "sprite.h"
#include "common.h"

#include <string.h>
#include <stdlib.h>

static void occ_del(struct occ *occ)
{
	if (!occ)
		return;
	if (occ->tag)
		free(occ->tag);
	if (occ->name)
		free(occ->name);
	if (occ->items)
		free(occ->items);
        if (occ->traps)
                free(occ->traps);
        free(occ);
}

extern struct occ *occ_new(char *tag,
                           char *name,
                           float magic,
                           int hp_mod,
                           int hp_mult,
                           int mp_mod,
                           int mp_mult,
                           int hit_mod,
                           int def_mod,
                           int dam_mod,
                           int arm_mod,
                           int n_arms, int n_items, int n_traps)
{
	struct occ *occ = 0;

	occ = new struct occ;
        assert(occ);
	memset(occ, 0, sizeof(struct occ));        

        occ->tag = strdup(tag);
        assert(occ->tag);

        occ->name = strdup(name);
        assert(occ->name);

/*         if (n_arms > 0) { */
/*                 occ->arms = (class ArmsType**)calloc(n_arms,  */
/*                                                      sizeof(class ArmsType*)); */
/*                 assert(occ->arms); */
/*         } */

        if (n_items > 0) {
                occ->items = (struct typical_items*)calloc(
                        n_items, 
                        sizeof(struct typical_items));
                assert(occ->items);
        }

        if (n_traps > 0) {
                occ->traps = (closure_t*)calloc(n_traps, 
                                                sizeof(occ->traps[0]));
                assert(occ->traps);
        }

        occ->magic = magic;
        occ->hp_mod = hp_mod;
        occ->hp_mult = hp_mult;
        occ->mp_mod = mp_mod;
        occ->mp_mult = mp_mult;
        occ->hit_mod = hit_mod;
        occ->def_mod = def_mod;
        occ->dam_mod = dam_mod;
        occ->arm_mod = arm_mod;
/*         occ->n_arms = n_arms; */
        occ->n_items = n_items;
        occ->n_traps = n_traps;

        assert(occ->tag);
        assert(occ->name);

        return occ;
}

extern void occ_unref(struct occ* occ)
{
        assert(occ->refcount > 0);
        occ->refcount--;
        if (! occ->refcount)
                occ_del(occ);
}
