/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2011 Gordon McNutt
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

#ifndef mem_h
#define mem_h

/**
 * A generic reference-counting memory allocator with support for
 * destructors.
 */

#define MEM_ALLOC_NTYPE(type, n, fin) \
        ((type*)mem_alloc(sizeof(type) * (n), (fin)))
#define MEM_ALLOC_TYPE(type, fin) MEM_ALLOC_NTYPE(type, 1, fin)

void *mem_alloc(int size, void (*fin)(void *));
void mem_ref(void *ptr);
void mem_deref(void *ptr);
void mem_set_err_handler(void (*handler)(int size));

#endif
