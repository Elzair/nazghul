/* Copyright (c) 2002 Gordon McNutt */
#ifndef heap_h
#define heap_h

#ifdef __cplusplus
extern "C" {
#endif

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

#ifdef __cplusplus
}
#endif
#endif
