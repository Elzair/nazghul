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
#include "closure.h"

#include <assert.h>
#include <config.h>
//#ifdef HAVE_MALLOC_H
#include <malloc.h>
//#endif
#include <string.h>

static void skill_del(struct skill *skill)
{
        struct list *elem;

        assert(!skill->refcount);
        if (skill->name) {
                free(skill->name);
        }
        if (skill->desc) {
                free(skill->desc);
        }
        if (skill->yuse) {
                closure_unref(skill->yuse);
        }
        if (skill->can_yuse) {
                closure_unref(skill->can_yuse);
        }

        /* delete the list of tools */
        node_list_unlink_and_unref(&skill->tools);

        /* delete the list of materials */
        elem = skill->materials.next;
        while (elem != &skill->materials) {
                struct list *tmp = elem->next;
                free(elem);
                elem = tmp;
        }

        free(skill);
}

struct skill *skill_new(void)
{
        struct skill *skill = (struct skill*)calloc(1, sizeof(*skill));
        assert(skill);
        list_init(&skill->list);
        node_init(&skill->tools);
        list_init(&skill->materials);
        skill->refcount = 1;
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

void skill_add_tool(struct skill *skill, void *objtype)
{
        struct node *node = node_new(objtype);
        node_add(&skill->tools, node);
}

void skill_add_material(struct skill *skill, void *objtype, int quan)
{
        struct skill_material *mat;
        mat = (struct skill_material*)calloc(1, sizeof(*mat));
        assert(mat);
        mat->objtype = objtype;
        mat->quantity = quan;
        list_add(&skill->materials, &mat->list);
}
