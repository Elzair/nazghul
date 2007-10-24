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

#include "skill_set_entry.h"
#include "skill.h"

#include <assert.h>
#include <config.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <string.h>

static void skill_set_entry_del(struct skill_set_entry *ssent)
{
        assert(!ssent->refcount);
        if (ssent->skill) {
                skill_unref(ssent->skill);
        }
        free(ssent);
}

struct skill_set_entry *skill_set_entry_new(struct skill *skill, int lvl)
{
        struct skill_set_entry *ssent;
        ssent = (struct skill_set_entry*)calloc(1, sizeof(*ssent));
        assert(ssent);
        list_init(&ssent->list);
        ssent->refcount = 1;
        ssent->skill = skill;
        skill_ref(skill);
        ssent->level = lvl;
        return ssent;
}

void skill_set_entry_ref(struct skill_set_entry *ssent)
{
        ssent->refcount++;
}

void skill_set_entry_unref(struct skill_set_entry *ssent)
{
        assert(ssent->refcount > 0);
        ssent->refcount--;
        if (!ssent->refcount) {
                skill_set_entry_del(ssent);
        }
}
