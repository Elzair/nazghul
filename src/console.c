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
#include "cmd.h" // for getkey()
#include "common.h"
#include "console.h"
#include "event.h"
#include "foogod.h"  // for foogod_get_y()
#include "glyph.h"
#include "play.h"
#include "screen.h"
#include "sprite.h"

#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>
#include <stdio.h>

#define MAX_MSG_SZ 4096
#define CONS_BUF_SIZE (CONS_LINES * (CONSOLE_MAX_MSG_SZ + 1))

static char console_scratch_buf[MAX_MSG_SZ + 1];

static struct console {
        SDL_Rect screenRect;
        glyph_formatter_t *gf;   /* for converting strings to glyphs */
        glyph_doc_t *gdoc;       /* buffered line history */
        glyph_doc_iter_t *gdi;   /* iterator over line buffer */
        int line;                /* current line index */
} Console;

int console_init(void)
{
        memset(console_scratch_buf, 0, sizeof(console_scratch_buf));
        Console.gf = glyph_formatter_alloc();
        Console.gdoc = glyph_doc_alloc(CONS_W / ASCII_W);
        Console.gdi = glyph_doc_iter_alloc(Console.gdoc);
        Console.screenRect.x = CONS_X;
        Console.screenRect.w = CONS_W;
        Console.line = 0;
        return 0;
}

void console_print(const char *fmt, ...)
{
        va_list args;

       /* Print message to the scratch buffer */
        va_start(args, fmt);
        vsnprintf(console_scratch_buf, MAX_MSG_SZ, fmt, args);
        va_end(args);

        /* Convert to glyphs. */
        glyph_buf_t *gbuf = glyph_buf_alloc_and_format(Console.gf, console_scratch_buf);

        /* Append to doc */
        glyph_doc_layout_and_append(Console.gdoc, gbuf);
        glyph_buf_deref(gbuf);

        /* Always scroll to the bottom after printing new stuff. */
        Console.line = glyph_doc_get_num_lines(Console.gdoc);

        /* Update the view. */
        console_repaint();
}

void console_repaint(void)
{
        /* Erase */
        SDL_Rect rectall;
        rectall.x = Console.screenRect.x;
        rectall.w = Console.screenRect.w;
        rectall.y = console_get_y();
        rectall.h = console_get_h();
        screen_erase(&rectall);

        /* Slice the lines we want to print into a temp glyph doc. */
        SDL_Rect rectline = rectall;
        rectline.h = ASCII_H;
        int maxlines = console_get_h() / ASCII_H;
        int startline, numlines;
        if (Console.line > maxlines) {
                startline = Console.line - maxlines;
                numlines = maxlines;
        } else {
                startline = 0;
                numlines = Console.line;
        }

        glyph_doc_iter_goto(Console.gdi, startline);

        /* Render the lines. */
        while (numlines--) {
                glyph_buf_t *gline = glyph_doc_iter_next(Console.gdi);
                screen_print_glyph_buf(&rectline, 0, gline);
                rectline.y += ASCII_H;
        }

        /* Force a screen update. */
        screen_update(&rectall);
}

int console_get_y(void)
{
        return (foogod_get_y() + foogod_get_h());
}

int console_get_h(void)
{
        return (SCREEN_H - BORDER_H - console_get_y());
}
