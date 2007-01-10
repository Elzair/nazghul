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
#ifndef skill_set_h
#define skill_set_h

#include "list.h"

/**
 * Skills are grouped into skill sets. Within a skill set, each skill has a
 * minimum level at which it may be used. Multiple skill sets may reference the
 * same skill, and each may have its own min level. So whereas skill structs
 * are shared across skill sets, skill_set_entry structs are unique to a skill
 * set. For example, a detect trap skill may be part of both a rogue skill set
 * and a ranger skill set, but the rogue can use the skill at level 2 whereas
 * the ranger must be level 4.
 */
struct skill_set {
        struct list list;         /* list of all skill_sets in the session */
        char *name;               /* name of the skill set, eg "Ranger" */
        struct list skills;       /* list of skill_set_entry structs */
        int refcount;             /* memory management */
};

extern struct skill_set *skill_set_new();
extern void skill_set_ref(struct skill_set *skset);
extern void skill_set_unref(struct skill_set *skset);
extern void skill_set_set_name(struct skill_set *skset, char *name);
extern void skill_set_add_skill(struct skill_set *skset, struct skill *skill,
                                int level);


#endif
