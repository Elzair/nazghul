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
/* 
 * From-scratch impl inspired by the Linux kernel source's list.h 
 */

#ifndef hstack_h
#define hstack_h

#include "macros.h"

BEGIN_DECL

typedef struct hstack hstack_t;

extern hstack_t *hstack_new();
extern void hstack_del(hstack_t *stack);
extern int hstack_push(hstack_t *stack, void *data);
extern void hstack_pop(hstack_t *stack);
extern void * hstack_top(hstack_t *stack);
extern void hstack_rm(hstack_t *stack, int handle);

END_DECL

#endif
