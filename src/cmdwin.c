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
#include "screen.h"
#include "sprite.h"

#include <assert.h>
#include <errno.h>
#include <stdarg.h>

static struct {
	SDL_Rect srect;
	char *buf;
	char *ptr;
	int blen;
	int room;
	char *mark;
        struct sprite *cursor_sprite;
} cmdwin;

#ifdef DEBUG
static FILE *log = NULL;
#endif

static inline void cmdwin_clear_no_repaint()
{
	memset(cmdwin.buf, 0, cmdwin.blen);
	cmdwin.ptr = cmdwin.buf;
	cmdwin.room = cmdwin.blen;
	cmdwin.mark = cmdwin.buf;
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

int cmdwin_init(void)
{
        cmdwin_cursor_sprite_init();

	cmdwin.srect.x = CMD_X;
	cmdwin.srect.y = CMD_Y;
	cmdwin.srect.w = CMD_W;
	cmdwin.srect.h = CMD_H;

        /* The actual string buffer length is one less than the width of the
         * cmdwin, because the last space must always be reserved for the
         * cursor. */
	cmdwin.blen = CMD_W / ASCII_W;
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

void cmdwin_print(char *fmt, ...)
{
	va_list args;
	int n;

	va_start(args, fmt);
	n = vsnprintf(cmdwin.ptr, cmdwin.room, fmt, args);
	va_end(args);

	if (n == -1) {
		cmdwin.room = 0;
	} else {
		cmdwin.ptr += n;
		cmdwin.room -= n;
	}

	cmdwin_repaint();
}

void cmdwin_backspace(int n)
{
	int len;

	/* Don't allow backspace beyond end of prompt */
	len = cmdwin.blen - cmdwin.room;
	if (len > n)
		len = n;

	/* Backup */
	cmdwin.ptr -= len;
	cmdwin.room += len;

	/* Erase everything beyond the Console.cursor */
	memset(cmdwin.ptr, 0, cmdwin.room);

	cmdwin_repaint();
}

void cmdwin_clear(void)
{
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

	rect.x += (cmdwin.ptr - cmdwin.buf) * ASCII_W;

	spritePaint(cmdwin.cursor_sprite, 0, rect.x, rect.y);
	screenUpdate(&rect);
}

void cmdwin_repaint(void)
{
	screenErase(&cmdwin.srect);
	screenPrint(&cmdwin.srect, 0, cmdwin.buf);
	screenUpdate(&cmdwin.srect);
	cmdwin_repaint_cursor();
}

void cmdwin_mark(void)
{
	cmdwin.mark = cmdwin.ptr;
}

void cmdwin_erase_back_to_mark(void)
{
	int n;
	n = cmdwin.ptr - cmdwin.mark;
	if (n > 0)
		cmdwin_backspace(n);
}

void cmdwin_flush_to_console(void)
{
        if (!strlen(cmdwin.buf))
                return;

        consolePrint("%s\n", cmdwin.buf);
#ifdef DEBUG
	if (log)
	        fprintf(log, "%s\n", cmdwin.buf);
#endif
        //cmdwin_clear();
        //cmdwin_repaint();
}

void cmdwin_flush(void)
{
        if (!strlen(cmdwin.buf))
                return;

        consolePrint("%s\n", cmdwin.buf);
        consoleRepaint();
#ifdef DEBUG
	if (log)
	        fprintf(log, "%s\n", cmdwin.buf);
#endif
        cmdwin_clear();
}
