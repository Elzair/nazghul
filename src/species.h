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

#ifdef __cplusplus
extern "C" {
#endif

#include "list.h"

        struct species {
                char *tag;
                struct list list;
                char *name;
                int str, intl, dex, spd, vr, pmask, ac;
                struct sprite *sleep_sprite;
                int n_occs;
                struct occ **occs;
                int n_slots;
                int *slots;
                int n_spells;
                class Spell **spells;
                class ArmsType *weapon;
                bool visible;
                char *damage_sound;
        };

        extern struct species *speciesLoad(class Loader *);
        extern void speciesDestroy(struct species *species);


#ifdef __cplusplus
}
#endif

#endif
