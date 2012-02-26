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

#include <assert.h>
#include <stdlib.h>

typedef struct {
        unsigned int refcount;
        void (*finalize)(void *ptr);
} mem_t;

static void (*mem_err_handler)(int size) = NULL;

void *mem_alloc(int size, void (*finalize)(void *))
{
        mem_t *chunk = 
                (mem_t*)calloc(1, sizeof(*chunk)+size);
        if (!chunk) {
                if (mem_err_handler) {
                        mem_err_handler(size);
                }
                return NULL;
        }
        chunk->refcount = 1;
        chunk->finalize = finalize;
        return (char *)chunk + sizeof(*chunk);
}

void mem_ref(void *ptr)
{
        mem_t *chunk = (mem_t*)ptr - 1;
        chunk->refcount++;
}

void mem_deref(void *ptr)
{
        mem_t *chunk = (mem_t*)ptr - 1;
        assert(chunk->refcount);
        chunk->refcount--;
        if (!chunk->refcount) {
                if (chunk->finalize) {
                        chunk->finalize(ptr);
                }
                free(chunk);
        }
}

void mem_set_err_handler(void (*handler)(int size))
{
        mem_err_handler = handler;
}

unsigned int mem_get_refs(void *ptr)
{
        mem_t *chunk = (mem_t*)ptr - 1;
        return chunk->refcount;
}
