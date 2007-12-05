/* $Id$
 *
 * Copyright (C) 2007 Gordon McNutt
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
 */

#ifndef nazghul_h
#define nazghul_h

extern int FullScreenMode;
extern int DeveloperMode;
extern int ExitProgram;

/**
 * The main menu uses this to rest the splash image if the user enters the load
 * menu but then changes his mind and backs out to the main menu.
 */
void nazghul_splash(void);


#endif
