#include "hstack.h"
#include "list.h"
#include "debug.h"

#include <assert.h>
#include <stdlib.h>

struct hstack {
        struct list list;
        int handle;
        void *data;
};

hstack_t *hstack_new()
{
        hstack_t *stack;

        stack = (hstack_t*)calloc(1, sizeof(*stack));
        assert(stack);
        list_init(&stack->list);

        return stack;
}

void hstack_del(hstack_t *stack)
{
        free(stack);
}

int hstack_push(hstack_t *stack, void *data)
{
        hstack_t *entry;

        entry = hstack_new();

        /* assign a new handle; the current stack handle is guaranteed to be
         * unique */
        entry->handle = stack->handle;
        stack->handle++;
        entry->data = data;

        /* insert the value next after the head */
        list_add(&stack->list, &entry->list);
        
        /* caller gets the handle */
        return entry->handle;
}

static void hstack_entry_del(hstack_t *stack, hstack_t *entry)
{
        /* remove the entry */
        list_remove(&entry->list);

        /* reuse its handle */
        stack->handle = entry->handle;

        /* free its memory */
        hstack_del(entry);
}

void hstack_pop(hstack_t *stack)
{
        hstack_t *entry;

        entry = (hstack_t *)stack->list.next;

        /* check for the empty stack case */
        if (entry == stack)
                return;

        hstack_entry_del(stack, entry);
}

void * hstack_top(hstack_t *stack)
{
        hstack_t *entry;

        entry = (hstack_t *)stack->list.next;

        /* check for the empty stack case */
        if (entry == stack)
                return NULL;
        
        return entry->data;
}

void hstack_rm(hstack_t *stack, int handle)
{
        hstack_t *entry;

        entry = (hstack_t*)stack->list.next;

        /* linear search for the handle */
        while (entry != stack) {

                /* found it? */
                if (entry->handle == handle) {
                        hstack_entry_del(stack, entry);
                        return;
                }

                entry = (hstack_t*)entry->list.next;
        }

        /* not found */
        warn("handle not found in stack\n");
}

