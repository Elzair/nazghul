/* Copyright (c) 2002 Gordon McNutt */
/* 
 * From-scratch impl inspired by the Linux kernel source's list.h 
 */

#ifndef list_h
#define list_h

#ifdef __cplusplus
extern "C" {
#endif

#define list_for_each(head,ptr) \
        for ((ptr) = (head)->next; (ptr) != (head); (ptr) = (ptr)->next)
#define list_init(list) { (list)->next = (list); (list)->prev = (list); }
#define list_empty(list) ((list)->next == (list) && (list)->prev == (list))
#define list_entry(ptr,type,field) \
        ((type*)((char*)(ptr)-(unsigned long)(&((type *)0)->field)))

	struct list {
		struct list *next;
		struct list *prev;
	};

	static inline void list_add_aux(struct list *before, struct list *after,
					struct list *between) {
		between->next = after;
		between->prev = before;
		before->next = between;
		after->prev = between;
	} static inline void list_add(struct list *head, struct list *ptr) {
		list_add_aux(head, head->next, ptr);
	}

	static inline void list_add_tail(struct list *head, struct list *ptr) {
		list_add_aux(head->prev, head, ptr);
	}

	static inline void list_remove(struct list *list) {
		list->prev->next = list->next;
		list->next->prev = list->prev;
		list_init(list);
	}

#ifdef __cplusplus
}
#endif

#endif
