/* Copyright (c) 2002 Gordon McNutt */
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
