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

#ifndef log_h
#define log_h

/* Called on startup */
extern void log_init(void);

/* A group of messages are all printed with no blank line between them. Blank
 * lines appear between top-level groups. */
extern void log_begin_group();
extern void log_end_group();

/* Begin a message that will start a group and prevent members of the group
 * from printing until it finishes. This is to de-interleave messages. */
extern void log_begin(const char *fmt, ...);
extern void log_continue(const char *fmt, ...);
extern void log_end(const char *fmt, ...);
extern void log_abort();

/* log_flush - force a partial entry started with log_begin() to print now
 * rather than waiting for log_end(). Warning: calling log_abort() after this
 * will not erase what was written to the console. Normally you don't need to
 * use log_flush().
 */
extern void log_flush();

/* Log a single message as its own group. */
extern void log_msg(const char *fmt, ...);

extern void log_disable();
extern void log_enable();

/* Like log_msg, but with fancy borders. */
extern void log_banner(const char *fmt, ...);

#endif
