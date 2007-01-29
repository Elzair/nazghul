/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2007 Gordon McNutt
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 *
 * Gordon McNutt
 * gmcnutt@users.sourceforge.net
 */
#ifndef skill_h
#define skill_h

#include "list.h"
#include "node.h"

/* Description of a material item needed by a skill */
struct skill_material {
        struct list list;  /* list of materials in skill */
        void *objtype;     /* ObjectType consumed */
        int quantity;      /* quantity consumed */
};

/* Description of a skill */
struct skill {
        struct list list;         /* list of all skills in the session */
        char *name;               /* name shown in UI */
        char *desc;               /* flavor text for UI; should also describe
                                   * special requirements checked for in the
                                   * can_yuse closure. */
        int ap;                   /* action points consumed */
        int mp;                   /* mana points consumed */
        struct node tools;        /* ObjectTypes needed to y)use */
        struct list materials;    /* skill_materials consumed with y)use */
        struct closure *yuse;     /* proc that executes the y)use */
        struct closure *can_yuse; /* check special requirements to y)use */
        int refcount;             /* memory management */
        char wilderness_ok : 1;   /* can y)use in wilderness */
        char passive : 1;         /* y)use is n/a */
};

extern struct skill *skill_new();
extern void skill_ref(struct skill *);
extern void skill_unref(struct skill *);
extern void skill_set_name(struct skill *skill, char *name);
extern void skill_set_desc(struct skill *skill, char *name);
extern void skill_add_tool(struct skill *skill, void *objtype);
extern void skill_add_material(struct skill *skill, void *objtype, int quan);

#endif
