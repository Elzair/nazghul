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

#include "skill_set.h"
#include "skill_set_entry.h"

#include <assert.h>
#include <config.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <string.h>


static void skill_set_del(struct skill_set *skset)
{
        struct list *list;

        assert(!skset->refcount);

        if (skset->name) {
                free(skset->name);
        }
        
        list = skset->skills.next;
        while (list != &skset->skills) {
                struct skill_set_entry *ssent;

                ssent = list_entry(list, struct skill_set_entry, list);
                list = list->next;
                list_remove(&ssent->list);
                skill_set_entry_unref(ssent);
        }

        free(skset);
}

struct skill_set *skill_set_new(void)
{
        struct skill_set *skset = (struct skill_set*)calloc(1, sizeof(*skset));
        assert(skset);
        list_init(&skset->list);
        list_init(&skset->skills);
        skset->refcount = 1;
        return skset;
}

void skill_set_set_name(struct skill_set *skset, char *val)
{
        assert(val);
        if (skset->name) {
                free(skset->name);
        }
        skset->name = strdup(val);
        assert(skset->name);
}

void skill_set_ref(struct skill_set *skset)
{
        skset->refcount++;
}

void skill_set_unref(struct skill_set *skset)
{
        assert(skset->refcount > 0);
        skset->refcount--;
        if (!skset->refcount) {
                skill_set_del(skset);
        }
}

void skill_set_add_skill(struct skill_set *skset, struct skill *skill,
                         int level)
{
        struct skill_set_entry *ssent = skill_set_entry_new(skill, level);
        assert(ssent);
        list_add_tail(&skset->skills, &ssent->list);
}
