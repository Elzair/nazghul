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
#include "sound.h"

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
        struct mmode *mmode;

        int hp_mod;   /* part of base hp contributed by species */
        int hp_mult;  /* additional hp per-level contributed by species */
        int mp_mod;   /* similar, for mana */
        int mp_mult;  /* similar, for mana */

        struct sprite *sleep_sprite;
        int n_slots;
        int *slots;
        int n_spells;
        char **spells;
        class ArmsType *weapon;
        bool visible;
        sound_t *damage_sound;
        sound_t *movement_sound;
        int xpval;           /* reward for killing this type */
        char *armor_dice;    /* for scaly or chitinous types */
        struct skill_set *skills;
        int stationary : 1;  /* doesn't move?                */
};

extern struct species *species_new(const char *tag,
                                   const char *name,
                                   sound_t *damage_sound,
                                   sound_t *movement_sound,
                                   int str,
                                   int intl,
                                   int dex,
                                   int spd,
                                   int vr,
                                   int hp_mod,
                                   int hp_mult,
                                   int mp_mod,
                                   int mp_mult,
                                   bool visible,
                                   int n_slots,
                                   int n_spells
                                   );
extern void species_del(struct species *species);

END_DECL

#endif
