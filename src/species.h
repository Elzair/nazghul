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
#ifndef species_h
#define species_h

#include "macros.h"
#include "list.h"

BEGIN_DECL

struct species {
        char *tag;
        struct list list;
        char *name;
        int str;
        int intl;
        int dex;
        int spd;
        int vr;
        int pmask;

        int hp_mod;   /* part of base hp contributed by species */
        int hp_mult;  /* additional hp per-level contributed by species */
        int mp_mod;   /* similar, for mana */
        int mp_mult;  /* similar, for mana */

        int hit_mod;  /* unused */
        int def_mod;  /* unused */
        int dam_mod;  /* unused */
        int arm_mod;  /* unused */

        struct sprite *sleep_sprite;
        int n_slots;
        int *slots;
        int n_spells;
        char **spells;
        class ArmsType *weapon;
        bool visible;
        char *damage_sound;
        char *movement_sound;
};

extern struct species *species_new(char *tag,
                                   char *name,
                                   char *damage_sound,
                                   char *movement_sound,
                                   int str,
                                   int intl,
                                   int dex,
                                   int spd,
                                   int vr,
                                   int pmask,
                                   int hp_mod,
                                   int hp_mult,
                                   int mp_mod,
                                   int mp_mult,
                                   int hit_mod,
                                   int def_mod,
                                   int dam_mod,
                                   int arm_mod,
                                   bool visible,
                                   int n_slots,
                                   int n_spells
                                   );
extern void species_del(struct species *species);

END_DECL

#endif
