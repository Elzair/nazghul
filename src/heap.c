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
#include "heap.h"

#include <stdlib.h>
#include <string.h>

struct heap *heap_create(unsigned int max_entries)
{
	struct heap *heap;

	heap = (struct heap *) malloc(sizeof(struct heap));
	if (!heap)
		return 0;
	memset(heap, 0, sizeof(struct heap));

	heap->max_entries = max_entries;
	heap->num_entries = 0;

	heap->entries = (int **) malloc(sizeof(int *) * max_entries);
	if (!heap->entries) {
		free(heap);
		return 0;
	}
	memset(heap->entries, 0, sizeof(void *) * max_entries);

	return heap;
}

void heap_destroy(struct heap *heap)
{
	if (heap->entries)
		free(heap->entries);
	free(heap);
}

void heapify(struct heap *heap, int i)
{
	unsigned int left;
	unsigned int right;
	int largest;

	left = 2 * i;
	right = left + 1;

	if (left < heap->num_entries &&
	    *heap->entries[left] > *heap->entries[i])
		largest = left;
	else
		largest = i;

	if (right < heap->num_entries &&
	    *heap->entries[right] > *heap->entries[largest])
		largest = right;

	if (largest != i) {
		int *tmp = heap->entries[i];
		heap->entries[i] = heap->entries[largest];
		heap->entries[largest] = tmp;
		heapify(heap, largest);
	}
}

int heap_expand(struct heap *heap)
{
	int **tmp;
	tmp =
	    (int **) realloc(heap->entries,
			     heap->max_entries * 2 * sizeof(int *));
	if (!tmp)
		return -1;
	heap->entries = tmp;
	heap->max_entries *= 2;
	return 0;
}

int heap_insert(struct heap *heap, int *entry)
{
	int i;

	/* Expand the heap if necessary */
	if (heap->num_entries == heap->max_entries) {
		if (heap_expand(heap) < 0)
			return -1;
	}

	/* Put the new entry at the bottom of the heap */
	i = heap->num_entries++;

	/* Percolate the new entry up to where it belongs */
	while (i > 0 && *heap->entries[i / 2] < *entry) {
		heap->entries[i] = heap->entries[i / 2];
		i /= 2;
	}

	heap->entries[i] = entry;

	return 0;
}

int *heap_extract(struct heap *heap)
{
	int *max;

	if (!heap->num_entries)
		return 0;

	max = heap->entries[0];
	heap->entries[0] = heap->entries[heap->num_entries - 1];
	heap->num_entries--;
	heapify(heap, 0);

	return max;
}

void heap_clean(struct heap *heap)
{
	memset(heap->entries, 0, heap->num_entries * sizeof(heap->entries[0]));
	heap->num_entries = 0;
}
