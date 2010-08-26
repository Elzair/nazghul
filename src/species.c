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

#include "closure.h"
#include "species.h"
#include "sprite.h"

#include <assert.h>
#include <string.h>
#include <stdlib.h>

struct species_slot_elem {
        struct list list;
        int slot;
};

struct species_spell_elem {
        struct list list;
        class Spell *spell;
};

struct species *species_new(const char *tag,
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
        )
{
        struct species *species = (struct species *)malloc(sizeof(*species));
        assert(species);
        memset(species, 0, sizeof(*species));

        species->tag = strdup(tag);
        assert(species->tag);

        species->name = strdup(name);
        assert(species->name);

        species->damage_sound = damage_sound;
        species->movement_sound = movement_sound;

        if (n_slots > 0) {
                species->slots = (int*)calloc(n_slots, sizeof(int));
                assert(species->slots);
        }

        if (n_spells > 0) {
                species->spells = (char**)calloc(n_spells, 
                                                 sizeof(char*));
                assert(species->spells);
        }

        species->str                = str;
        species->intl               = intl;
        species->dex                = dex;
        species->spd                = spd;  // AP_per_round
        species->vr                 = vr;
        species->hp_mod             = hp_mod;
        species->hp_mult            = hp_mult;
        species->mp_mod             = mp_mod;
        species->mp_mult            = mp_mult;
        species->visible            = visible;
        species->n_slots            = n_slots;
        species->n_spells           = n_spells;

        return species;
}

void species_del(struct species *species)
{
	if (!species)
		return;
	if (species->tag)
		free(species->tag);
	if (species->name)
		free(species->name);
	if (species->slots)
		free(species->slots);
	if (species->spells) {
                int i;
                for (i = 0; i < species->n_spells; i++)
                        free(species->spells[i]);
		free(species->spells);
        }
        if (species->armor_dice) {
                free(species->armor_dice);
        }
        free(species);
}
