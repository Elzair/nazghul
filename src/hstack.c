#include "hstack.h"
#include "list.h"
#include "debug.h"
#include "session.h" /* for save */

#include <assert.h>
#include <stdlib.h>

struct hstack {
        struct list list;
        struct list order;
        int handle;
        void *data;
};

static int hstack_find_free_handle(hstack_t *hstack)
{
        struct list *elem;
        hstack_t *node;
        int handle = 0;

        list_for_each(&hstack->order, elem) {
                node = outcast(elem, hstack_t, order);
                if (handle < node->handle)
                        return handle;
                handle = node->handle + 1;
        }
        return handle;
}

static void hstack_insert(hstack_t *hstack, hstack_t *inode)
{
        struct list *elem;
        hstack_t *enode;

        /* Insert it in the order list, which is kept sorted lowest to
         * highest by handle. */

        list_for_each(&hstack->order, elem) {
                enode = outcast(elem, hstack_t, order);
                if (inode->handle < enode->handle) {
                        /* add it before the existing node */
                        list_add_tail(&enode->order, &inode->order);
                        return;
                }
        }
        /* add it to the end of the list */
        list_add_tail(&hstack->order, &inode->order);
}

static void hstack_entry_del(hstack_t *stack, hstack_t *entry)
{
        list_remove(&entry->list);
        list_remove(&entry->order);
        hstack_del(entry);
}

hstack_t *hstack_new()
{
        hstack_t *stack;

        stack = (hstack_t*)calloc(1, sizeof(*stack));
        assert(stack);
        hstack_init(stack);
        return stack;
}

void hstack_del(hstack_t *stack)
{
        free(stack);
}

void hstack_init(hstack_t *stack)
{
        list_init(&stack->list);
        list_init(&stack->order);
}

int hstack_push(hstack_t *stack, void *data)
{
        hstack_t *entry;

        entry = hstack_new();
        entry->handle = hstack_find_free_handle(stack);
        entry->data = data;

        list_add(&stack->list, &entry->list);
        hstack_insert(stack, entry);

        return entry->handle;
}


void hstack_restore(hstack_t *stack, void *data, int handle)
{
        hstack_t *entry;

        entry = hstack_new();
        entry->handle = handle;
        entry->data = data;
        list_add(&stack->list, &entry->list);        
        hstack_insert(stack, entry);
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

int hstack_depth(hstack_t *hstack)
{
        hstack_t *entry;
        int count = 0;

        entry = (hstack_t*)hstack->list.next;

        while (entry != hstack) {
                count++;
                entry = (hstack_t*)entry->list.next;
        }

        return count;
}

int hstack_empty(hstack_t *hstack)
{
        return (hstack->list.next == &hstack->list);
}

void hstack_save(hstack_t *hstack, struct save *save,
                 void (*save_data)(struct save *save, void *data))
{
        hstack_t *entry;

        save->append(save, "(list ");

        entry = (hstack_t*)hstack->list.prev;
        while (entry != hstack) {
                save->append(save, "(list %d ", entry->handle);
                save_data(save, entry->data);
                save->append(save, ") ");
                entry = (hstack_t*)entry->list.prev;
        }

        save->append(save, ") ");
}
