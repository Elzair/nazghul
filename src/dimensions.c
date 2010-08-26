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

#include "dimensions.h"
#include "common.h" /* for MOON_WINDOW_W */
#include "cfg.h"

#include <string.h>

int STATUS_MAX_MSG_SZ;
int SCREEN_W;
int SCREEN_H;
int CONSOLE_MAX_MSG_SZ;
int MAP_TILE_W;
int MAP_TILE_H;
int MAP_X;
int MAP_Y;
int MAP_W;
int MAP_H;
int CMD_X;
int CMD_Y;
int CMD_W;
int CMD_H;
int STAT_X;
int STAT_Y;
int STAT_W;
int STAT_H;
int STAT_H_MAX;
int FOOGOD_X;
int FOOGOD_Y;
int FOOGOD_W;
int FOOGOD_H;
int WIND_X;
int WIND_Y;
int WIND_W;
int WIND_H;
int CONS_X;
int CONS_Y;
int CONS_W;
int CONS_H;
int CONS_LINES;
int SKY_X;
int SKY_Y;
int SKY_W;
int SKY_H;
int SKY_SPRITE_W;

/* dimensions_get_map_size -- figure out the biggest map window that will
 * satisfy the screen dimensions.  */
static int dimensions_get_map_size(char *dimstr)
{
        struct dimstr2mapsz {
                const char *dimstr;
                int map_sz;
        };
#       define ADD_SCREEN_DIM(dim,mapw) { (dim), (mapw) },
        struct dimstr2mapsz tbl[] = {
#               include "screen_dims.h"
        };

        int i;

        if (!dimstr) {
                warn("warn: NULL dimensions");
                return -1;
        }

        for (i = 0; i < array_sz(tbl); i++) {
                if (! strcmp(tbl[i].dimstr, dimstr)) {
                        return tbl[i].map_sz;
                }
        }

        warn("warn: screen res %s not found in table\n", dimstr);
        return -1;
}

int dimensions_init()
{
        int map_size = dimensions_get_map_size(cfg_get("screen-dims"));
        if (map_size < 0)
                return -1;

        MAP_TILE_W = map_size;
        MAP_TILE_H = map_size;

        MAP_X = BORDER_W;
        MAP_Y = BORDER_H;
        MAP_W = (TILE_W * MAP_TILE_W);
        MAP_H = (TILE_H * MAP_TILE_H);

        CMD_X = MAP_X;
        CMD_Y = (MAP_Y + MAP_H + BORDER_H);
        CMD_W = MAP_W;
        CMD_H = ASCII_H;

        SCREEN_H = (BORDER_H * 3 + MAP_H + CMD_H);

        STATUS_MAX_MSG_SZ = 128;
        STAT_X =  (MAP_X + MAP_W + BORDER_W);
        STAT_Y =  BORDER_H;
        STAT_W = (/*BORDER_W * 2*/ + ASCII_W * STAT_CHARS_PER_LINE);
        STAT_H =  (3 * TILE_H);
        STAT_H_MAX = (16 * TILE_H);

        CONS_X =  STAT_X;
        CONS_Y =  (FOOGOD_Y + FOOGOD_H + BORDER_H);
        CONS_W =  STAT_W;
        CONS_H =  (SCREEN_H - BORDER_H - CONS_Y);
        CONS_LINES = (CONS_H / ASCII_H);

        CONSOLE_MAX_MSG_SZ = (CONS_W / ASCII_W);

        FOOGOD_X = STAT_X;
        FOOGOD_Y = (STAT_Y + STAT_H + BORDER_H);
        FOOGOD_W = STAT_W;
        FOOGOD_H = (2 * ASCII_H);

        WIND_W =  (strlen("wind:northeast") * ASCII_W);
        WIND_H =  BORDER_H;
        WIND_X =  (BORDER_W + (MAP_W - WIND_W) / 2);
        WIND_Y =  (MAP_Y + MAP_H);

        SKY_W =   MOON_WINDOW_W;
        SKY_H =   BORDER_H;
        SKY_X =   (MAP_X + (MAP_W - SKY_W) / 2);
        SKY_Y =   0;
        SKY_SPRITE_W = (TILE_W/2);

        SCREEN_W = (BORDER_W * 3 + MAP_W + CONS_W);

        return 0;
}
