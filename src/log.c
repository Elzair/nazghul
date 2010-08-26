#include "log.h"
#include "list.h"
#include "common.h"
#include "console.h"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#define LOG_ENTRY_BUF_SZ 512

static struct list log_q;
static struct list log_stk;
static int log_group;
static int log_disabled;

struct log_entry {
        struct list q_hook;
        struct list stk_hook;
        char buf[LOG_ENTRY_BUF_SZ];
        char *ptr;
        int room;
};

struct log_entry *log_entry_new()
{
        struct log_entry *entry = (struct log_entry*)calloc(1, sizeof(*entry));
        assert(entry);
        entry->ptr = entry->buf;
        entry->room = sizeof(entry->buf);
        return entry;
}

static void log_entry_del(struct log_entry *entry)
{
        free(entry);
}

static void log_entry_print(struct log_entry *entry, const char *fmt, va_list args)
{
        /* If fmt is NULL experimentation shows that vsnprintf returns -1,
         * which is also the indication that we attempted to overflow the
         * buffer. To prevent the ambiguity (we want to handle 'nothing
         * written' differently than 'overflow') check for NULL here. */
        if (!fmt) {
                return;
        }

        int wrote = vsnprintf(entry->ptr, entry->room, fmt, args);
        if (wrote >= 0) {
                entry->room -= wrote;
                entry->ptr += wrote;
        } else {
                entry->room = 0;
        }
}

void log_flush()
{
        struct log_entry *entry;

        if (list_empty(&log_q))
                return;

        entry = outcast(log_q.next, struct log_entry, q_hook);

        if (entry->ptr != entry->buf) {
                consolePrint(entry->buf);
                consoleRepaint();
                memset(entry->buf, 0, sizeof(entry->buf));
                entry->ptr = entry->buf;
                entry->room = sizeof(entry->buf);
        }
}

static inline void log_print_queued_msgs()
{
        struct list *elem;

        elem = log_q.next;

        while (elem != &log_q) {

                struct log_entry *entry;
                
                entry = outcast(elem, struct log_entry, q_hook);
                elem = elem->next;
                consolePrint("%s\n", entry->buf);
                list_remove(&entry->q_hook);
                log_entry_del(entry);
        }
}

static inline void log_flush_queued_msgs(struct log_entry *entry)
{
        struct list *elem;

        elem = &entry->q_hook;

        while (elem != &log_q) {
                entry = outcast(elem, struct log_entry, q_hook);
                elem = elem->next;
                list_remove(&entry->q_hook);
                log_entry_del(entry);
        }
}

static inline void log_push(struct log_entry *entry)
{
        list_add_tail(&log_q, &entry->q_hook);
        list_add(&log_stk, &entry->stk_hook);
        log_begin_group();
}

static inline void log_pop()
{
        assert(! list_empty(&log_stk));
        
        /* Pop the topmost entry from the msg stack */
        list_remove(log_stk.next);
        
        /* If the msg stack is now empty then log all msgs on the queue */
        if (list_empty(&log_stk))
                log_print_queued_msgs();
        log_end_group();
}

void log_abort(void)
{
        struct log_entry *entry;

        if (log_disabled)
                return;

        assert(! list_empty(&log_stk));
        entry = outcast(log_stk.next, struct log_entry, stk_hook);

        list_remove(log_stk.next);
        log_flush_queued_msgs(entry);
        log_end_group();
}

void log_init(void)
{
        list_init(&log_q);
        list_init(&log_stk);
        log_group = 0;
        log_disabled = 0;
}

void log_begin(const char *fmt, ...)
{
        va_list args;

        if (log_disabled)
                return;

        struct log_entry *entry = log_entry_new();
        log_push(entry);
        va_start(args, fmt);
        log_entry_print(entry, fmt, args);
        va_end(args);
}

void log_continue(const char *fmt, ...)
{
        va_list args;
        struct log_entry *entry;

        if (log_disabled)
                return;

        assert(! list_empty(&log_stk));
        entry = outcast(log_stk.next, struct log_entry, stk_hook);
        va_start(args, fmt);
        log_entry_print(entry, fmt, args);
        va_end(args);
}

void log_end(const char *fmt, ...)
{
        va_list args;
        struct log_entry *entry;

        if (log_disabled)
                return;

        assert(! list_empty(&log_stk));
        entry = outcast(log_stk.next, struct log_entry, stk_hook);
        va_start(args, fmt);
        log_entry_print(entry, fmt, args);
        va_end(args);
        log_pop();
}

void log_msg(const char *fmt, ...)
{
        va_list args;
        struct log_entry *entry = log_entry_new();

        if (log_disabled)
                return;

        log_push(entry);
        va_start(args, fmt);
        log_entry_print(entry, fmt, args);
        va_end(args);
        log_pop();        
}

void log_banner(const char *fmt, ...)
{
        struct log_entry *entry;
        va_list args;

        if (log_disabled) {
                return;
        }

        log_begin_group();
        log_msg("^c+y*********************************^c-");
        log_begin("^c+y*^c- ");
        
        
        entry = outcast(log_stk.next, struct log_entry, stk_hook);
        va_start(args, fmt);
        log_entry_print(entry, fmt, args);
        va_end(args);

        log_end("");
        log_msg("^c+y*********************************^c-");
        log_end_group();
}

void log_begin_group()
{
        if (log_disabled)
                return;

        log_group++;
}

void log_end_group()
{
        if (log_disabled)
                return;

        log_group--;
        if (log_group == 0)
                consolePrint("\n");
}

void log_disable()
{
        log_disabled++;
}

void log_enable()
{
        log_disabled--;
}
