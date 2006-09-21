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
#ifndef dimensions_h
#define dimensions_h

#include "macros.h"

BEGIN_DECL

#define STAT_CHARS_PER_LINE 46

#define TILE_W   32
#define TILE_H   32
#define ASCII_W  8
#define ASCII_H  16
#define BORDER_W 16
#define BORDER_H 16

#define MIN_MAP_SIZE 11
#define MAX_MAP_SIZE 19
#define DEF_MAP_SIZE 19

extern int STATUS_MAX_MSG_SZ;
extern int SCREEN_W;
extern int SCREEN_H;
extern int CONSOLE_MAX_MSG_SZ;

extern int MAP_TILE_W;
extern int MAP_TILE_H;
extern int MAP_X;
extern int MAP_Y;
extern int MAP_W;
extern int MAP_H;

extern int CMD_X;
extern int CMD_Y;
extern int CMD_W;
extern int CMD_H;

extern int STAT_X;
extern int STAT_Y;
extern int STAT_W;
extern int STAT_H;
extern int STAT_H_MAX;

extern int FOOGOD_X;
extern int FOOGOD_Y;
extern int FOOGOD_W;
extern int FOOGOD_H;

extern int WIND_X;
extern int WIND_Y;
extern int WIND_W;
extern int WIND_H;

extern int CONS_X;
extern int CONS_Y;
extern int CONS_W;
extern int CONS_H;
extern int CONS_LINES;

extern int SKY_X;
extern int SKY_Y;
extern int SKY_W;
extern int SKY_H;
extern int SKY_SPRITE_W;

/* Set all the runtime-configurable UI dimensions */
int dimensions_init();

END_DECL

#endif
