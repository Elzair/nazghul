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
#ifndef skill_set_entry_h
#define skill_set_entry_h

#include "list.h"

/* These are used to list the skills in a skill set, and to associate a minimum
 * required level to use each skill. */
struct skill_set_entry {
        struct list list;         /* list of other skills in the skill set */
        struct skill *skill;      /* the skill                             */
        int level;                /* min skill level to use this skill     */
        int refcount;             /* memory management                     */
};

extern struct skill_set_entry *skill_set_entry_new(struct skill *skill, int level);
extern void skill_set_entry_ref(struct skill_set_entry *ssent);
extern void skill_set_entry_unref(struct skill_set_entry *ssent);

#endif
