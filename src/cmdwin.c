/* Copyright (c) 2002 Gordon McNutt */

#include "cmdwin.h"
#include "screen.h"
#include "common.h"
#include "sprite.h"

#include <stdarg.h>

static struct {
	SDL_Rect srect;
	char *buf;
	char *ptr;
	int blen;
	int room;
	char *mark;
} cmdwin;

struct sprite *CursorSprite = NULL;

int cmdwin_init(void)
{
	cmdwin.srect.x = CMD_X;
	cmdwin.srect.y = CMD_Y;
	cmdwin.srect.w = CMD_W;
	cmdwin.srect.h = CMD_H;

	cmdwin.blen = CMD_W / ASCII_W + 1;
	cmdwin.buf = (char *) malloc(cmdwin.blen);
	if (!cmdwin.buf)
		return -1;
	cmdwin_clear();
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
	memset(cmdwin.buf, 0, cmdwin.blen);
	cmdwin.ptr = cmdwin.buf;
	cmdwin.room = cmdwin.blen;
	cmdwin.mark = cmdwin.buf;
}

void cmdwin_repaint_cursor(void)
{
	SDL_Rect rect;

	rect.x = cmdwin.srect.x;
	rect.y = cmdwin.srect.y;
	rect.w = ASCII_W;
	rect.h = ASCII_H;

	rect.x += (cmdwin.ptr - cmdwin.buf) * ASCII_W;

	spritePaint(CursorSprite, 0, rect.x, rect.y);
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
