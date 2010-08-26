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

#include "cmdwin.h"

#include "cfg.h"
#include "common.h"
#include "console.h"
#include "dimensions.h"
#include "images.h"
#include "log.h"
#include "screen.h"
#include "sprite.h"

#include <assert.h>
#include <errno.h>
#include <stdarg.h>

#define CMDWIN_FRAG_MAX_LEN 64
#define CMDWIN_BUF_SZ       256

/* Fragment flags */
#define CMDWIN_FRAG_SEP     (1<<0)
#define CMDWIN_FRAG_MARK    (1<<1)

struct cmdwin_frag {
        struct list list;
        int flags;
        char buf[CMDWIN_FRAG_MAX_LEN];
};

static struct {
	SDL_Rect srect; /* screen rectangle (pixels) */
	char *buf;      /* string buffer */
	char *ptr;      /* next empty spot in buffer */
	int blen;       /* buffer length, this should be bigger than slen and
                         * is the max expected total size of any prompt (the
                         * longest prompts may be too big for the window) */
	int room;       /* empty space in buffer */
        int slen;       /* printable string length (blen >= slen), this is
                         * limited by the cmdwin UI size */
        struct sprite *cursor_sprite;
        struct list frags;
} cmdwin;

#ifdef DEBUG
static FILE *log = NULL;
#endif

static inline void cmdwin_clear_no_repaint()
{
	memset(cmdwin.buf, 0, cmdwin.blen);
	cmdwin.ptr = cmdwin.buf;
	cmdwin.room = cmdwin.blen;
}

static void cmdwin_cursor_sprite_init()
{
        char *fname = cfg_get("cursor-image-filename");
        struct images *ss_cursor = 0;

        assert(fname);
        ss_cursor = images_new(0, 8, 16, 1, 4, 0, 0, fname);
        assert(ss_cursor);
        cmdwin.cursor_sprite = sprite_new(0, 4, 0, 0, 0, ss_cursor);
        assert(cmdwin.cursor_sprite);
}

static void cmdwin_clear_frag_stack(void)
{
        struct list *entry;
        
        entry = cmdwin.frags.next;
        while (entry != &cmdwin.frags) {
                struct cmdwin_frag *frag = (struct cmdwin_frag*)entry;
                entry = entry->next;
                list_remove(&frag->list);
                free(frag);
        }
}

static struct cmdwin_frag *cmdwin_top()
{
        if (list_empty(&cmdwin.frags))
                return 0;
        return (struct cmdwin_frag*)cmdwin.frags.prev;
}

static void cmdwin_reprint_buffer(void)
{
        struct list *entry;

        /* Erase the buffer */
        cmdwin_clear_no_repaint();

        /* Loop over the fragments until out of room or out of fragments */
        list_for_each(&cmdwin.frags, entry) {
                struct cmdwin_frag *frag = (struct cmdwin_frag*)entry;
                int n = 0;

                /* Append the fragment to the buffer. */
                if ((frag->flags & CMDWIN_FRAG_SEP)
                    && (entry->next != &cmdwin.frags)) {
                        /* Print a '-' after this fragment. */
                        n = snprintf(cmdwin.ptr, cmdwin.room, "%s-", frag->buf);
                } else {
                        /* No '-' afterwards. */
                        n = snprintf(cmdwin.ptr, cmdwin.room, "%s", frag->buf);
                }
                n = min(n, cmdwin.room);
                cmdwin.room -= n;
                cmdwin.ptr += n;

                /* If out of room then stop, and backup the ptr to the last
                 * entry in the buffer */
                if (!cmdwin.room) {
                        cmdwin.ptr--;
                        break;
                }

        }
}

int cmdwin_init(void)
{
        cmdwin_cursor_sprite_init();

        list_init(&cmdwin.frags);

	cmdwin.srect.x = CMD_X;
	cmdwin.srect.y = CMD_Y;
	cmdwin.srect.w = CMD_W;
	cmdwin.srect.h = CMD_H;
	cmdwin.slen = (CMD_W / ASCII_W) - 1; /* leave one space for the
                                              * cursor */
        cmdwin.blen = CMDWIN_BUF_SZ;
        assert(cmdwin.blen >= cmdwin.slen);

	cmdwin.buf = (char *) malloc(cmdwin.blen);
	if (!cmdwin.buf)
		return -1;

#ifdef DEBUG
	log = fopen(".cmdwin", "w+");
	if (!log) {
		err(strerror(errno));
		return -1;
	}
#endif

        cmdwin_clear_no_repaint();
	return 0;
}

static void cmdwin_vpush(int flags, const char *fmt, va_list args)
{
        /* Allocate a new fragment */
        struct cmdwin_frag *frag = (struct cmdwin_frag*)malloc(sizeof(*frag));
        if (!frag) {
                warn("allocation failed");
                return;
        }

        frag->flags = flags;

        /* default to empty string */
        frag->buf[0] = 0;

        /* Store the string in the fragment */
        if (fmt != NULL) {
                vsnprintf(frag->buf, sizeof(frag->buf), fmt, args);
        }

        /* Push the fragment onto the stack */
        list_add_tail(&cmdwin.frags, &frag->list);

        /* Reprint the buffer with the new fragment */
        cmdwin_reprint_buffer();

        /* Update the display */
	cmdwin_repaint();        
}

void cmdwin_spush(const char *fmt, ...)
{
	va_list args;

	va_start(args, fmt);
        cmdwin_vpush(CMDWIN_FRAG_SEP, fmt, args);
	va_end(args);
        
}

void cmdwin_push(const char *fmt, ...)
{
	va_list args;

	va_start(args, fmt);
        cmdwin_vpush(0, fmt, args);
	va_end(args);
}

void cmdwin_push_mark()
{
        cmdwin_vpush(CMDWIN_FRAG_MARK, 0, 0);
}

void cmdwin_pop(void)
{
        struct cmdwin_frag *frag;

        /* Fragment stack should not be empty. */
        assert(! list_empty(&cmdwin.frags));

        /* Remove the last fragment and free it. */
        frag = (struct cmdwin_frag*)cmdwin.frags.prev;
        list_remove(&frag->list);
        free(frag);

        /* Reprint the buffer without the fragment */
        cmdwin_reprint_buffer();

        /* Update the display */
	cmdwin_repaint();
}

void cmdwin_pop_to_mark()
{
        struct cmdwin_frag *frag = cmdwin_top();
        while (frag && frag->flags != CMDWIN_FRAG_MARK) {
                cmdwin_pop();
                frag = cmdwin_top();
        }

        /* DON'T pop the mark itself */
}

void cmdwin_clear(void)
{
        cmdwin_clear_frag_stack();
        cmdwin_clear_no_repaint();
        cmdwin_repaint();
}

void cmdwin_repaint_cursor(void)
{
	SDL_Rect rect;

	rect.x = cmdwin.srect.x;
	rect.y = cmdwin.srect.y;
	rect.w = ASCII_W;
	rect.h = ASCII_H;

        /* If the string is too big, show the last part of it (in other words,
         * right-justify it) */
        char *start = max(cmdwin.buf, cmdwin.ptr - cmdwin.slen);
	rect.x += (cmdwin.ptr - start) * ASCII_W;

	sprite_paint(cmdwin.cursor_sprite, 0, rect.x, rect.y);
	screenUpdate(&rect);
}

void cmdwin_repaint(void)
{
        /* If the string is too big, show the last part of it (in other words,
         * right-justify it) */
        char *start = max(cmdwin.buf, cmdwin.ptr - cmdwin.slen);
	screenErase(&cmdwin.srect);
	screenPrint(&cmdwin.srect, 0, start);
	screenUpdate(&cmdwin.srect);
	cmdwin_repaint_cursor();
}

void cmdwin_flush(void)
{
        if (!strlen(cmdwin.buf))
                return;

        log_msg("%s\n", cmdwin.buf);
        cmdwin_clear();
}
