/* Copyright (c) 2002 Gordon McNutt */
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
        };

        extern struct species *speciesLoad(class Loader *);
        extern void speciesDestroy(struct species *species);


#ifdef __cplusplus
}
#endif

#endif
