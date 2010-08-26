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
#ifndef occ_h
#define occ_h

#include "list.h"
#include "closure.h"

#define occ_ref(occ) ((occ)->refcount++)

struct typical_items {
	int prob;
	class ObjectType *type;
	int n_max;
};

struct occ {
	struct list list;
	char *tag;
	char *name;
	float magic;

        int hp_mod;   /* part of base hp contributed by occupation */
        int hp_mult;  /* additional hp per-level contributed by occupation  */
        int mp_mod;   /* similar, for mana */
        int mp_mult;  /* similar, for mana */

        int hit_mod;  /* unused */
        int def_mod;  /* unused */
        int dam_mod;  /* unused */
        int arm_mod;  /* unused */
        
        int xpval; /* reward for killing this type */
        int refcount;
		
        struct gob *gob;
        struct skill_set *skills;
};

extern struct occ *occ_new(const char *tag,
                           const char *name,
                           float magic,
                           int hp_mod,
                           int hp_mult,
                           int mp_mod,
                           int mp_mult,
                           int hit_mod,
                           int def_mod,
                           int dam_mod,
                           int arm_mod);

extern void occ_unref(struct occ *occ);
extern void occ_set_skills(struct occ *occ, struct skill_set *skills);

#endif
