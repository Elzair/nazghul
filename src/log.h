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
extern void log_begin(char *fmt, ...);
extern void log_continue(char *fmt, ...);
extern void log_end(char *fmt, ...);

/* Log a single message as its own group. */
extern void log_msg(char *fmt, ...);

extern void log_disable();
extern void log_enable();

#endif
