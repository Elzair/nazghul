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
#include "console.h"
#include "screen.h"
#include "common.h"
#include "sprite.h"
#include "play.h"
#include "event.h"
#include "cmd.h" // for getkey()
#include "foogod.h"  // for foogod_get_y()

#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>
#include <stdio.h>

#define MAX_MSG_SZ 512
#define CONS_BUF_SIZE (CONS_LINES * (CONSOLE_MAX_MSG_SZ + 1))

static struct console {
        SDL_Rect screenRect;
        char *buf;             /* text buffer */
        char **lines;          /* pointers into buf, one per line */
        int line;              /* current line index */
        char *cursor;          /* next byte on current line */
        int room;              /* bytes left on current line */
        int numLines;          /* num lines filled + current line */

        int firstLine;         // index into 'lines' array of the first filled
                               // line (the lines array is a circular buffer, *
                               // so once all the lines are filled this value *
                               // advances every time the 'line' value *
                               // advances)

        int repeat;           // counts number of times last line repeats

#ifdef DEBUG
        FILE *log;
#endif
} Console;

static char console_scratch_buf[MAX_MSG_SZ + 1];
static char console_last_msg[MAX_MSG_SZ + 1];

static void consoleNewline(void);

static int consolePage(void)
{
        int yesno = 1;

        consolePrint("---More-Y/N?");
        consoleRepaint();
        getkey(&yesno, yesnokey);
        consoleBackspace(12);
        return yesno == 'y';
}

int consoleInit(void)
{
        int i;

        Console.screenRect.x = CONS_X;
        Console.screenRect.w = CONS_W;

        //console_set_y(foogod_get_y() + FOOGOD_H + BORDER_H);

        Console.buf = (char *)malloc(CONS_BUF_SIZE);
        if (! Console.buf)
                return -1;
        memset(Console.buf, 0, CONSOLE_MAX_MSG_SZ + 1);

        Console.cursor = Console.buf;
        Console.room = CONSOLE_MAX_MSG_SZ;
        Console.line = 0;
        Console.firstLine = 0;
        Console.repeat = 0;

        /* Unintuitively, numLines must begin with 1. I only advance it for
         * newlines, and if someone prints text on the very first line without
         * issuing a newline then it needs to be rendered in the repaint
         * routine. In other words, I always pretend that the current line has
         * something on it. */
        Console.numLines = 1;
        Console.lines = (char**)malloc(CONS_LINES * sizeof(char*));
        if (!Console.lines)
                return -1;
        for (i = 0; i < CONS_LINES; i++)
                Console.lines[i] = &Console.buf[i * (CONSOLE_MAX_MSG_SZ + 1)];

#ifdef DEBUG
	/* Open the console log file. */
	Console.log = fopen(".console", "w");
	if (Console.log == NULL) {
		perror(".console");
	}
#endif

        return 0;
}

static int console_handle_repeated_msg(void)
{

        // Is this a repeat?
        if (strchr(console_scratch_buf, '\n') == 0 ||
            (strcmp(console_scratch_buf, console_last_msg) != 0) ||
            (strcmp(console_scratch_buf, "\n") == 0)) {

                // No. Was the previous message a repeat?
                if (Console.repeat != 0) {
                        // Yes, So we need to advance past the 'repeat' msg
                        // which occupies its own line
                        consoleNewline();
                }
                // Setup for new msg.
                Console.repeat = 0;
                strcpy(console_last_msg, console_scratch_buf);
                return 0;
        }

        // Yes, this is a repeat.
        Console.repeat++;

        // Is this the first repeat for the msg?
        if (Console.repeat == 1) {
                // Yes. Are we at the beginning of a new line?
                if (Console.room != CONSOLE_MAX_MSG_SZ) {
                        // No. So advance to a new line.
                        consoleNewline();
                }
                // Print the repeat notice for the first time.
                snprintf(Console.lines[Console.line], CONSOLE_MAX_MSG_SZ, 
                         "[again]");
                Console.room -= strlen(Console.lines[Console.line]);
                Console.cursor += strlen(Console.lines[Console.line]);

        } else {
                // No. This is not the first time we've repeated this
                // msg. Erase the previous 'repeat' notice and print a new one
                // with the updated repeat count.
                memset(Console.lines[Console.line], 0, CONSOLE_MAX_MSG_SZ + 1);
                snprintf(Console.lines[Console.line], CONSOLE_MAX_MSG_SZ,
                         "[%d times]", Console.repeat);
        }

        consoleRepaint();

        return 1;
}

void consolePrint(const char *fmt, ...)
{
        int printed;
        va_list args;
        char *ptr, *eow;
        int n_lines = 0, wlen;
        int words = 0;
        int wrapped;

        /* Print message to the scratch buffer */
        va_start(args, fmt);
        printed = vsnprintf(console_scratch_buf, MAX_MSG_SZ, fmt, args);
        va_end(args);

#ifdef DEBUG
        if (Console.log != NULL)
                vfprintf(Console.log, fmt, args);
#endif

        /* Check if message was truncated to fit. Note that I discard anything
         * that will not fit. */
        if (printed == -1)
                printed = MAX_MSG_SZ;

        // Is this a repeat of the last msg?
        if (console_handle_repeated_msg())
                // Yes. Already handled.
                return;

        /* 
         * Now transfer the contents of the scratch buffer to the line
         * buffers. I use the following rules:
         * 
         * 1. A word is a run of non-ws chars.
         * 
         * 2. When starting a new line after wrapping discard FIRST initial ws.
         * 
         * 3. When starting a new line print the first word. If the first will
         * not fit on the line then print as much as possible and start a new
         * line with the remainder.
         * 
         * 4. After printing a word print all whitespace up to the next word or
         * the end of the current line.
         * 
         * 5. After printing ws probe the next word to find it's length. If the
         * word will not fit then start a new line and apply rule 3. Otherwise
         * print the word and apply rule 4.
         * 
         * 6. After starting a new line check if the message window is full. If
         * so then prompt the user before continuing. This gives the user a
         * chance to read everything printed thus far.
         * 
         * 7. All but the first newline are indented by a space to help make
         * visual blocks of text.
         * 
         */
        ptr = console_scratch_buf;

        wrapped = 0;

        /* copy from the super buffer to the line buffer(s) */
        while (printed) {

                /* Check if this is the start of a new line and if so then
                 * apply rules 2 and 3. */
                if (Console.room == CONSOLE_MAX_MSG_SZ) {

                        /* Rule 2 */
                        while (isspace(*ptr) && printed) {

                                if (!wrapped) {
                                        if (*ptr == '\n') {
                                                ptr++;
                                                printed--;
                                                goto newline;
                                        }

                                        /* copy the char */
                                        *Console.cursor = *ptr;
                                        Console.room--;
                                        Console.cursor++;
                                }
                                wrapped = 0;
                                ptr++;
                                printed--;
                        }

                        /* Rule 3 */
                        while (!isspace(*ptr) && printed && Console.room) {

                                /* copy the char */
                                *Console.cursor = *ptr;

                                /* advance source and destination */
                                printed--;
                                ptr++;
                                Console.room--;
                                Console.cursor++;
                        }

                        words++;

                        /* Check for end-of-line */
                        if (!Console.room) {
                                wrapped = 1;
                                goto newline;
                        }

                }

                /* Ok. We have already printed at least one word on this line
                 * and now need to apply Rule 4: print all ws up to end-of-line
                 * or start-of-word. */

                wrapped = 0;

                while (isspace(*ptr) && Console.room && printed) {

                        if (*ptr == '\n') {
                                ptr++;
                                printed--;
                                goto newline;
                        }

                        /* copy the char */
                        *Console.cursor = *ptr;

                        /* advance source and destination */
                        printed--;
                        ptr++;
                        Console.room--;
                        Console.cursor++;
                }

                if (!Console.room) {
                        wrapped = 1;
                        goto newline;
                }

                if (!printed)
                        break;

                /* All right. At this point I know I have space left and
                 * another word to print. Rule 5. */
                eow = strpbrk(ptr, " \t\n\r");
                if (eow)
                        wlen = eow - ptr;
                else
                        wlen = printed;

                if (wlen > Console.room) {
                        wrapped = 1;
                        goto newline;
                }

                while (wlen) {
                        /* copy the char */
                        *Console.cursor = *ptr;

                        /* advance source and destination */
                        printed--;
                        ptr++;
                        Console.room--;
                        Console.cursor++;
                        wlen--;
                }

                words++;

                continue;

              newline:

                /* advance to the next line */
                consoleNewline();
                n_lines++;

                /* Rule 6 */
                if (n_lines == (CONS_LINES - 1)) {
                        if (!consolePage())
                                return;
                        n_lines = 0;
                }

                /* Start of new line so go back to top-of-loop so I can run the
                 * start-of-line code. */
                continue;
        }

        consoleRepaint();
}

void consoleBackspace(int n)
{
        int len;

        /* Don't allow backspace beyond end of prompt */
        len = CONSOLE_MAX_MSG_SZ - Console.room;
        if (len > n)
                len = n;

        /* Backup */
        Console.cursor -= len;
        Console.room += len;

        /* Erase everything beyond the Console.cursor */
        memset(Console.cursor, 0, Console.room);
        // *Console.cursor = 0;

#ifdef DEBUG
        if (Console.log != NULL)
                fprintf(Console.log, "\b");
#endif
}

static void consoleNewline(void)
{
  Console.line = (Console.line + 1) % CONS_LINES;

  if (Console.numLines < (CONS_LINES))
          Console.numLines++;
  else
          Console.firstLine = (Console.firstLine + 1) % CONS_LINES;

  Console.cursor = Console.lines[Console.line];
  
  memset(Console.cursor, 0, CONSOLE_MAX_MSG_SZ + 1);
  Console.room = CONSOLE_MAX_MSG_SZ;
  
#ifdef DEBUG
  if (Console.log != NULL)
          fprintf(Console.log, "\n");
#endif
  
  //consoleRepaint();
}

void consoleRepaint(void)
{
        SDL_Rect rect;
        int n_lines;
        int max_lines;
        int line;

        //screenErase(&Console.screenRect);

        rect.x = Console.screenRect.x;
        rect.w = Console.screenRect.w;
        rect.y = console_get_y();
        rect.h = console_get_h();

        screenErase(&rect);

        max_lines = rect.h / ASCII_H;
        n_lines = min(Console.numLines, max_lines);

        /* Find the first visible line in the line buffer by backing up from
         * the current line. If I need to back up past the beginning then wrap
         * around to the end. */
        if (n_lines > (Console.line + 1)) {
                line = CONS_LINES - (n_lines - (Console.line + 1));
        } else {
                line = (Console.line + 1) - n_lines;
        }

        while (n_lines--) {
                screenPrint(&rect, 0, Console.lines[line]);
                line = (line + 1) % CONS_LINES;
                rect.y += ASCII_H;
        }

        screenUpdate(&Console.screenRect);
}

int console_get_y(void)
{
        return (foogod_get_y() + foogod_get_h());
}

int console_get_h(void)
{
        return (SCREEN_H - BORDER_H - console_get_y());
}

int console_max_lines (void) {
        int max_lines;

        max_lines = Console.screenRect.h / ASCII_H;
        return min(Console.numLines, max_lines);
}
