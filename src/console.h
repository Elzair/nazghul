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
#ifndef console_h
#define console_h

#ifdef __cplusplus
extern "C" {
#endif

	extern int consoleInit(void);
	extern void consolePrint(const char *fmt, ...);
	extern void consoleBackspace(int n);
	//extern void consoleNewline(void);
	extern void consoleRepaint(void);
	//extern void console_set_y(int y);
	extern int console_get_y(void);
	extern int console_get_h(void);

#ifdef __cplusplus
}
#endif
#endif
