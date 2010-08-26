/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2002, 2003 Gordon McNutt
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 *
 * Gordon McNutt
 * gmcnutt@users.sourceforge.net
 */

#ifndef cmdwin_h
#define cmdwin_h

/* The command window is the little window below the map that displays all the
 * command prompts. It's very interactive. The gist of the API is that
 * multi-step commands that want to prompt the user will "push" a prompt, pop
 * it and push the reply, push the next prompt, etc. */

/* cmdwin_init -- startup init */
extern int cmdwin_init(void);

/* cmdwin_clear -- erase the contents */
extern void cmdwin_clear(void);

/* cmdwin_repaint_cursor -- repaint just the cursor prompt (used to animate the
 * cursor sprite) */
extern void cmdwin_repaint_cursor(void);

/* cmdwin_repaint -- repaint the cmdwin window (cmdwin_push and _spush do this
 * automatically, so this is rarely necessary) */
extern void cmdwin_repaint(void);

/* cmdwin_flush -- write the contents of the cmdwin to the console */
extern void cmdwin_flush(void);

/* cmdwin_push -- append a string to the cmdwin prompt (this can be undone with
 * cmdwin_pop). Works just like printf for string formatting. */
extern void cmdwin_push(const char *fmt, ...);

/* cmdwin_spush -- same as push, but print a '-' after it if anything else is
 * pushed (the 's' is for 'segment', you're pushing a prompt segment) */
extern void cmdwin_spush(const char *fmt, ...);

/* cmdwin_pop -- remove a string appended by one of the push commands */
extern void cmdwin_pop(void);

/* cmdwin_push_mark -- a mark is invisible, but when pop_to_mark is called any
 * string appended after the last mark is popped. */
extern void cmdwin_push_mark();

/* cmdwin_pop_to_mark -- pop all strings appended since the last mark. This
 * does not pop the mark itself (use cmdwin_pop for that). */
extern void cmdwin_pop_to_mark();

#endif
