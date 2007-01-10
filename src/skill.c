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

#include "skill.h"

#include <assert.h>
#include <malloc.h>
#include <string.h>

static void skill_del(struct skill *skill)
{
        assert(!skill->refcount);
        if (skill->name) {
                free(skill->name);
        }
        if (skill->desc) {
                free(skill->desc);
        }
        free(skill);
}

struct skill *skill_new(void)
{
        struct skill *skill = (struct skill*)calloc(1, sizeof(*skill));
        assert(skill);
        list_init(&skill->list);
        skill->refcount++;
        return skill;
}

void skill_set_name(struct skill *skill, char *val)
{
        assert(val);
        if (skill->name) {
                free(skill->name);
        }
        skill->name = strdup(val);
        assert(skill->name);
}

void skill_set_desc(struct skill *skill, char *val)
{
        assert(val);
        if (skill->desc) {
                free(skill->desc);
        }
        skill->desc = strdup(val);
        assert(skill->name);
}

void skill_ref(struct skill *skill)
{
        skill->refcount++;
}

void skill_unref(struct skill *skill)
{
        assert(skill->refcount > 0);
        skill->refcount--;
        if (!skill->refcount) {
                skill_del(skill);
        }
}
