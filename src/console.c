/* Copyright (c) 2002 Gordon McNutt */
#include "console.h"
#include "screen.h"
#include "common.h"
#include "sprite.h"
#include "play.h"
#include "event.h"

#include <string.h>
#include <SDL/SDL_ttf.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>
#include <stdio.h>

#define MAX_MSG_SZ 1024

static struct console {
	SDL_Rect screenRect;
	char buf[CONS_LINES * (CONSOLE_MAX_MSG_SZ + 1)];
	char *lines[CONS_LINES];	/* pointers into buf, one per line */
	int line;		/* index into lines array of the current line */
	char *cursor;		/* next byte on current line */
	int room;		/* bytes left on current line */
	int numLines;		/* num lines filled + current line */
	int firstLine;		/* index into 'lines' array of the first filled 
				 * line (the lines array is a circular buffer,
				 * so once all the lines are filled this value
				 * advances every time the 'line' value
				 * advances) */
	FILE *log;
} Console;

static char bigBuf[MAX_MSG_SZ + 1];	/* tmp buf used for line-wrapping code */

static int consolePage(void)
{
	int yesno = 1;

	consolePrint("---More-Y/N?");
	consoleRepaint();
	getkey(&yesno, yesnokey);
	consoleBackspace(12);
	return yesno == 'y';
}

void consoleInit(void)
{
	int i;

	Console.screenRect.x = CONS_X;
	Console.screenRect.y = CONS_Y;
	Console.screenRect.w = CONS_W;
	Console.screenRect.h = CONS_H;
	memset(Console.buf, 0, CONSOLE_MAX_MSG_SZ + 1);
	Console.cursor = Console.buf;
	Console.room = CONSOLE_MAX_MSG_SZ;
	Console.line = 0;
	Console.firstLine = 0;

	/* Unintuitively, numLines must begin with 1. I only advance it for
	 * newlines, and if someone prints text on the very first line without
	 * issuing a newline then it needs to be rendered in the repaint
	 * routine. In other words, I always pretend that the current line has
	 * something on it. */
	Console.numLines = 1;

	for (i = 0; i < CONS_LINES; i++)
		Console.lines[i] = &Console.buf[i * (CONSOLE_MAX_MSG_SZ + 1)];

	/* Open the console log file. */
	Console.log = fopen(".console", "w");
	if (Console.log == NULL) {
		perror(".console");
	}
}

void consolePrint(char *fmt, ...)
{
	int printed;
	va_list args;
	char *ptr, *eow;
	int n_lines = 0, wlen;
	int words = 0;
	int wrapped;

	/* Print message to the super buffer */
	va_start(args, fmt);
	printed = vsnprintf(bigBuf, MAX_MSG_SZ, fmt, args);
	va_end(args);

	if (Console.log != NULL)
		vfprintf(Console.log, fmt, args);

	/* Check if message was truncated to fit. Note that I discard anything
	 * that will not fit. */
	if (printed == -1)
		printed = MAX_MSG_SZ;

	/* 
	 * Now transfer the contents of the super buffer to the line buffers. I
	 * use the following rules:
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
	ptr = bigBuf;

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

	if (Console.log != NULL)
		fprintf(Console.log, "\b");
}

void consoleNewline(void)
{
	Console.line = (Console.line + 1) % CONS_LINES;

	if (Console.numLines < (CONS_LINES))
		Console.numLines++;
	else
		Console.firstLine = (Console.firstLine + 1) % CONS_LINES;

	Console.cursor = Console.lines[Console.line];

	memset(Console.cursor, 0, CONSOLE_MAX_MSG_SZ + 1);
	Console.room = CONSOLE_MAX_MSG_SZ;

	if (Console.log != NULL)
		fprintf(Console.log, "\n");

	consoleRepaint();
}

void consoleRepaint(void)
{
	SDL_Rect rect;
	int n_lines;
	int max_lines;
	int line;

	screenErase(&Console.screenRect);

	rect.x = Console.screenRect.x;
	rect.y = Console.screenRect.y;
	rect.w = Console.screenRect.w;
	rect.h = Console.screenRect.h;

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

void console_set_y(int y)
{
	Console.screenRect.y = y;
	Console.screenRect.h = (SCREEN_H - BORDER_H - y);
}

int console_get_y(void)
{
	return Console.screenRect.y;
}
