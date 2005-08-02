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
#ifndef heap_h
#define heap_h

#include "macros.h"

BEGIN_DECL

#define heap_entry(ptr,type,field) \
        ((type*)((char*)(ptr)-(unsigned long)(&((type *)0)->field)))

#define heap_empty(h) (!(h)->num_entries)

struct heap {
        unsigned int max_entries;
        unsigned int num_entries;
        int **entries;
};

extern struct heap *heap_create(unsigned int max_entries);
extern void heap_destroy(struct heap *heap);
extern void heapify(struct heap *heap, int i);
extern int heap_expand(struct heap *heap);
extern int heap_insert(struct heap *heap, int *entry);
extern int *heap_extract(struct heap *heap);
extern void heap_clean(struct heap *heap);

END_DECL

#endif
