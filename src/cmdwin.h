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

#ifndef cmdwin_h
#define cmdwin_h

extern int cmdwin_init(void);
extern void cmdwin_print(char *fmt, ...);
extern void cmdwin_backspace(int n);
extern void cmdwin_clear(void);
extern void cmdwin_repaint_cursor(void);
extern void cmdwin_repaint(void);
extern void cmdwin_mark(void);
extern void cmdwin_erase_back_to_mark(void);
extern void cmdwin_flush_to_console(void);
extern void cmdwin_flush(void);

#endif
