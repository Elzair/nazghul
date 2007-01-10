/*
 * nazghul - an old-school RPG engine
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
 *
 * Gordon McNutt
 * gmcnutt@users.sourceforge.net
 */
#ifndef repstr_h
#define repstr_h

/* Replace a string value with a new value. If the string already has a value
 * it is first freed with free(). The new value is created with
 * strdup(). Checks for the special case where *sptr==val and does nothing. */
void repstr(char **sptr, const char *val);

#endif
