/* $Id$
 *
 * Copyright (C) 2006 Gordon McNutt
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

/* Enumerate the supported screen dimensions and map them to the map viewer
 * width (in tiles). Note that the usual #ifndef include guards are not
 * desirable for this file, nor are C++-style '//' comments. */
ADD_SCREEN_DIM("640x480", 11)
ADD_SCREEN_DIM("800x480", 12)
ADD_SCREEN_DIM("1280x960", MAX_MAP_SIZE)
