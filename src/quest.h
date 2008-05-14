/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2008 Gordon McNutt
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

#ifndef quest_h
#define quest_h

#include "closure.h"
#include "gob.h"
#include "list.h"
#include "object.h"

struct quest {
        struct list list;
        char *title;
        char *description;
        class Object *assignee;
        struct closure *assign;
        struct closure *status;
        struct gob *gob;
        int refcount;
        int complete : 1;
};

extern struct quest *quest_new(void);
extern void quest_ref(struct quest *quest);
extern void quest_unref(struct quest *quest);

#endif
