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
#ifndef common_h
#define common_h

#ifdef __cplusplus
extern "C" {
#endif

#include "list.h"
#include "debug.h"
#include "los.h"
#include "dup_constants.h"

#include <stdlib.h>
#include <string.h>
#include <SDL.h>

/* Constants *****************************************************************/

/* Dimensions */
#define SCREEN_W                        1024	/* 1028 */
#define SCREEN_H                        (BORDER_H * 3 + MAP_H + CMD_H)
#define DEF_SCREEN_BPP                  16
#define TILE_W                          32
#define TILE_H                          32
#define STATUS_MAX_MSG_SZ               128
#define ASCII_W                         8
#define ASCII_H                         16
#define BORDER_W                        16
#define BORDER_H                        16
#define CONSOLE_MAX_MSG_SZ              (CONS_W / ASCII_W)	/* 128 */

#define MAP_LEFT
#ifdef MAP_LEFT

#define MAP_TILE_W 19
#define MAP_TILE_H 19
#define MAP_X   BORDER_W
#define MAP_Y   BORDER_H
#define MAP_W   (TILE_W * MAP_TILE_W)
#define MAP_H   (TILE_H * MAP_TILE_H)

#define CMD_X   MAP_X
#define CMD_Y   (MAP_Y + MAP_H + BORDER_H)

#define CMD_W   MAP_W
#define CMD_H   ASCII_H

#define STAT_X  (MAP_X + MAP_W + BORDER_W)
#define STAT_Y  BORDER_H
#define STAT_W  (SCREEN_W - STAT_X - BORDER_W)
//#define STAT_H  (MAP_H - FOOGOD_H - BORDER_H)
#define STAT_H  (3 * TILE_H)
#define STAT_H_MAX (16 * TILE_H)

#define FOOGOD_X STAT_X
#define FOOGOD_Y (STAT_Y + STAT_H + BORDER_H)
#define FOOGOD_W STAT_W
#define FOOGOD_H (2 * ASCII_H)

#define WIND_X  (BORDER_W + (MAP_W - WIND_W) / 2)
#define WIND_Y  (MAP_Y + MAP_H)
#define WIND_W  (strlen("wind:north") * ASCII_W)
#define WIND_H  BORDER_H

#ifdef BOTTOM_CONSOLE
#define CONS_X  BORDER_W
#define CONS_Y  (WIND_Y + WIND_H)
#define CONS_W  (SCREEN_W - (2 * BORDER_W))
#define CONS_H  (SCREEN_H - BORDER_H - (WIND_Y + WIND_H))
#define CONS_LINES (CONS_H / ASCII_H)
#else
#define CONS_X  STAT_X
#define CONS_Y  (FOOGOD_Y + FOOGOD_H + BORDER_H)
#define CONS_W  STAT_W
#define CONS_H  (SCREEN_H - BORDER_H - CONS_Y)
#define CONS_LINES (CONS_H / ASCII_H)
#endif

#define SKY_X   (MAP_X + (MAP_W - SKY_W) / 2)
#define SKY_Y   0
#define SKY_W   MOON_WINDOW_W
#define SKY_H   BORDER_H
#define SKY_SPRITE_W (TILE_W/2)

#else /* ! MAP_LEFT */

#define MAP_TILE_W 19
#define MAP_TILE_H 19
#define MAP_W   (TILE_W * MAP_TILE_W)
#define MAP_H   (TILE_H * MAP_TILE_H)

#define STAT_X  BORDER_W
#define STAT_Y  BORDER_H
#define STAT_W  (SCREEN_W - STAT_X - BORDER_W * 2 - MAP_W)
//#define STAT_H  (MAP_H - FOOGOD_H - BORDER_H)
#define STAT_H  (3 * TILE_H)
#define STAT_H_MAX (16 * TILE_H)

#define FOOGOD_X STAT_X
#define FOOGOD_Y (STAT_Y + STAT_H + BORDER_H)
#define FOOGOD_W STAT_W
#define FOOGOD_H (2 * ASCII_H)

#define MAP_X   (STAT_X + STAT_W + BORDER_W)
#define MAP_Y   BORDER_H

#define CMD_X   MAP_X
#define CMD_Y   (MAP_Y + MAP_H + BORDER_H)
#define CMD_W   MAP_W
#define CMD_H   ASCII_H

#define WIND_X  (MAP_X + (MAP_W - WIND_W) / 2)
#define WIND_Y  (MAP_Y + MAP_H)
#define WIND_W  (strlen("wind:north") * ASCII_W)
#define WIND_H  BORDER_H

#define CONS_X  STAT_X
#define CONS_Y  (FOOGOD_Y + FOOGOD_H + BORDER_H)
#define CONS_W  STAT_W
#define CONS_H  (SCREEN_H - BORDER_H - CONS_Y)
#define CONS_LINES (CONS_H / ASCII_H)

#define SKY_X   (MAP_X + (MAP_W - SKY_W) / 2)
#define SKY_Y   0
#define SKY_W   MOON_WINDOW_W
#define SKY_H   BORDER_H
#define SKY_SPRITE_W (TILE_W/2)

#endif /* ! MAP_LEFT */

/* Directions */
#define DIRECTION_NONE -1
#define NORTHWEST 0
#define NORTH     1
#define NORTHEAST 2
#define WEST      3
#define HERE      4
#define EAST      5
#define SOUTHWEST 6
#define SOUTH     7
#define SOUTHEAST 8
#define UP        9
#define DOWN      10
#define NUM_PLANAR_DIRECTIONS 9
#define CANCEL    SDLK_ESCAPE
#define NUM_WIND_DIRECTIONS 4

/* Time -- In future, these likely come from GhulScript */
#define TURNS_PER_MINUTE        60
#define MINUTES_PER_HOUR        60
#define HOURS_PER_DAY           24
#define TURNS_PER_HOUR          (TURNS_PER_MINUTE * MINUTES_PER_HOUR)
#define TURNS_PER_DAY           (TURNS_PER_HOUR   * HOURS_PER_DAY)

#define MINUTES_PER_DAY         (MINUTES_PER_HOUR * HOURS_PER_DAY)
#define MINUTES_PER_WEEK        (MINUTES_PER_DAY * DAYS_PER_WEEK)
#define MINUTES_PER_MONTH       (MINUTES_PER_WEEK * WEEKS_PER_MONTH)
#define MINUTES_PER_YEAR        (MINUTES_PER_MONTH * MONTHS_PER_YEAR)

#define DAYS_PER_WEEK           7
#define WEEKS_PER_MONTH         4
#define MONTHS_PER_YEAR         13
#define DAYS_PER_MONTH          (DAYS_PER_WEEK  * WEEKS_PER_MONTH)
#define DAYS_PER_YEAR           (DAYS_PER_MONTH * MONTHS_PER_YEAR)
#define TURNS_PER_WEEK          (DAYS_PER_WEEK  * TURNS_PER_DAY)
#define TURNS_PER_MONTH         (DAYS_PER_MONTH * TURNS_PER_DAY)
#define TURNS_PER_YEAR          (DAYS_PER_YEAR  * TURNS_PER_DAY)

#define HOURS_OF_NIGHT          8
#define HOURS_OF_SUNLIGHT       (HOURS_PER_DAY - HOURS_OF_NIGHT)
#define SUNRISE_HOUR            4
#define SUNSET_HOUR             (SUNRISE_HOUR + HOURS_OF_SUNLIGHT)
#define NOON_HOUR               12
#define MIDNIGHT_HOUR_LATE      24
#define MIDNIGHT_HOUR_EARLY     0

#define DEGREES_PER_HOUR        (360 / HOURS_PER_DAY)
#define DEGREES_PER_MINUTE      (360 / MINUTES_PER_DAY)
#define MINUTES_PER_DEGREE      (MINUTES_PER_DAY / 360)
#define SUNRISE_DEGREE          (SUNRISE_HOUR * DEGREES_PER_HOUR)
#define SUNSET_DEGREE           (SUNSET_HOUR  * DEGREES_PER_HOUR)
#define NOON_DEGREE             (NOON_HOUR    * DEGREES_PER_HOUR)
#define MIDNIGHT_DEGREE_LATE    (MIDNIGHT_HOUR_LATE  * DEGREES_PER_HOUR)
#define MIDNIGHT_DEGREE_EARLY   (MIDNIGHT_HOUR_EARLY * DEGREES_PER_HOUR)

/* Moons */
/* gmcnutt: turned off moons for new loader. Need to completely clean all that
 * up with a rewrite. */
#define NUM_MOONS                       0 /*2*/
#define MOON_WINDOW_DEGREES             (HOURS_OF_SUNLIGHT * DEGREES_PER_HOUR)
#define MOON_WINDOW_PIXELS_PER_DEGREE   1	/* needs to be an integer */
#define MOON_WINDOW_W     (MOON_WINDOW_PIXELS_PER_DEGREE *      \
                                         MOON_WINDOW_DEGREES)
#define MOON_HOURS_PER_REVOLUTION       (HOURS_PER_DAY - 1)
#define MOON_DEGREES_PER_HOUR           (360 / MOON_HOURS_PER_REVOLUTION)
#define MOON_DEGREES_PER_MINUTE         (MOON_DEGREES_PER_HOUR * 60)
#define MOON_MINUTES_PER_DEGREE         ((MOON_HOURS_PER_REVOLUTION * 60) / 360)
#define MOON_PIXELS_PER_HOUR            (MOON_DEGREES_PER_HOUR *              \
                                         MOON_WINDOW_PIXELS_PER_DEGREE)
#define TURNS_PER_MOON_CYCLE            (TURNS_PER_DAY * 30)
#define MOON_TURNS_PER_DEGREE           (TURNS_PER_HOUR /                     \
                                         MOON_DEGREES_PER_HOUR)
#define SUN_DEGREES_PER_HOUR            (360 / 24)
#define SUN_TURNS_PER_DEGREE            (TURNS_PER_HOUR /                     \
                                         SUN_DEGREES_PER_HOUR)

/* Font info */
#define FONT_FILENAME   "fonts/cour.ttf"
#define FONT_PTSIZE      14
#define FONT_STYLE       TTF_STYLE_NORMAL

/* Timing */
#define ANIMATION_TICKS  10	/* Ticks between animation frame changes */
#define MS_PER_TICK      100

/* Misc */
#define TURNS_TO_FIRE_VEHICLE_WEAPON    2
#define APPLICATION_NAME        "Nazghul"
#define WIND_CHANGE_PROBABILITY 5
#define PLAYER_MAX_PROGRESS     100
#define STAT_TEXT_LEN           ((STAT_W - TILE_W) / ASCII_W)
#define MAX_NAME_LEN            STAT_TEXT_LEN
#define MIN_VISION_RADIUS       1
#define HP_PER_LVL              30
#define MAX_ATTRIBUTE_VALUE     999

// MAX_VISION_RADIUS must be <= MAX_SIGHT in angband.c, but it cannot be
// greater than the map viewer. The reason it can't be greater than the map
// viewer is because we set our visibility mask to be the same dimensions as
// the map viewer, and if we let the LOS alg exceed these dimensions it will
// produce artifacts on the vmask.
//#define MAX_VISION_RADIUS       (MAP_TILE_W / 2)
#define MAX_VISION_RADIUS       (MAP_TILE_W)
#define MAX_VIEWPORT_RANGE      (MAX_VISION_RADIUS + (MAX_VISION_RADIUS / 2))

#define FRAME_IMAGE             "images/frame.bmp"
#define SPLASH_IMAGE            "images/splash.bmp"
#define MAX_WORDS_IN_SPELL_NAME  4

/* The maximum intensity of ambient sunlight. No point making it more than 255
 * because the lightmap only allows tiles to have a max value of 255.*/
#define MAX_AMBIENT_LIGHT       255
#define MAX_SUNLIGHT            255
#define MAX_MOONLIGHT           128

/* The minimum amount of light the player generates. This is so that the user
	* can at least see the player icon in total darkness. */
#define MIN_PLAYER_LIGHT        128

// The sun sprite is SKY_SPRITE_W pixels wide, and it advances
// MOON_WINDOW_PIXELS_PER_DEGREE every time it moves, and I want its luminence
// to drop to zero by the time it passes out of sight of the sky window.
#define DELTA_SUNLIGHT          (MAX_SUNLIGHT / \
                                 (SKY_SPRITE_W * MOON_WINDOW_PIXELS_PER_DEGREE))

#define TURNS_PER_FOOD          (TURNS_PER_DAY/3)
#define MAX_SPEED               100
#define WILDERNESS_SCALE        32
#define NON_WILDERNESS_SCALE     1
#define MAX_N_REAGENTS          32

/* Resting */
#define HP_RECOVERED_PER_HOUR_OF_REST   3
#define MANA_RECOVERED_PER_HOUR_OF_REST 10
#define MAX_USEFUL_REST_HOURS_PER_DAY   9
#define TURNS_PER_REST_CREDIT     (TURNS_PER_DAY/MAX_USEFUL_REST_HOURS_PER_DAY)
#define PROB_AWAKEN                     25

/* My events */
#define TICK_EVENT              1

/* Damages */
#define DAMAGE_FIRE       10
#define DAMAGE_POISON     1
#define DAMAGE_STARVATION 1
#define DAMAGE_ACID       10
#define DAMAGE_BOMB       25

/* Targets */
#define TARG_NONE       0
#define TARG_SELF       1
#define TARG_FRIEND     2

/* Type ID's (TIDs) */
#define OBJECT_ID               1
#define VEHICLE_ID              (OBJECT_ID + 1)
#define CHARACTER_ID            (OBJECT_ID + 2)
#define MOONGATE_ID             (OBJECT_ID + 3)
#define PORTAL_ID               (OBJECT_ID + 4)
#define PARTY_ID                (OBJECT_ID + 5)

#define SPRITE_ID               (OBJECT_ID + 7)
#define OBJECT_TYPE_ID          100
#define ARMS_TYPE_ID            101
#define ITEM_TYPE_ID            102
#define VEHICLE_TYPE_ID         103
#define ORDNANCE_TYPE_ID        104
#define CHARACTER_TYPE_ID       105
#define ENTITY_TYPE_ID          106
#define MOONGATE_TYPE_ID        107
#define REAGENT_TYPE_ID         108
#define SPELL_TYPE_ID           109
//#define AMMO_TYPE_ID            110 -- obsolete
#define FIELD_TYPE_ID           111
#define FOOD_TYPE_ID            112
#define TRAP_TYPE_ID            113
#define PARTY_TYPE_ID           114

#define RESPONSE_TYPE_ID        116
#define CONVERSATION_TYPE_ID    117
#define OCC_ID                  118
#define SPECIES_ID              119
#define SCHEDULE_ID             120
#define PLACE_ID                121
#define MAP_ID                  122

#define TERRAIN_PALETTE_ID      124
#define TERRAIN_ID              125
#define MECH_TYPE_ID            126
#define MECH_ID                 127
#define IMAGES_ID               128
#define FORMATION_TYPE_ID       129

/* Services (loadfile must match) */
#define SRV_HEAL        1
#define SRV_CURE        2
#define SRV_RESURRECT   3
#define SRV_MIN         SRV_HEAL
#define SRV_MAX         SRV_RESURRECT

/* Elevation (leveling-up) */
#define XP_PER_ATTACK 2
#define XP_PER_DEFEND 1
#define XP_PER_COMBAT 10
#define XP_PER_SPELL  2
#define XP_PER_TRAP   1

/* Contexts */
#define CONTEXT_WILDERNESS 1
#define CONTEXT_TOWN       2

/* Spells (loadfile must match) */
#define SPELL_TARGET_NONE              0
#define SPELL_TARGET_CHARACTER         1
#define SPELL_TARGET_MECH              2
#define SPELL_TARGET_DIRECTION         3
#define SPELL_TARGET_LOCATION          4
#define SPELL_TARGET_UP                5
#define SPELL_TARGET_DOWN              6
#define SPELL_TARGET_ALL_PARTY_MEMBERS 7
#define SPELL_TARGET_CASTER_LOCATION   8
#define SPELL_TARGET_PARTY_MEMBER      9

// Compile-time configuration parameters
#define CONFIG_MOVEMENT_COST_FOR_CHARACTERS false

/* Action point values (temporary - should be moved to the load file) */
#define NAZGHUL_BASE_ACTION_POINTS  1

/* Macros ********************************************************************/

#define perror_sdl(msg) err("%s: %s\n", (msg), SDL_GetError())
// #define perror_sdl(msg) fprintf(stderr, "%s:%s\n", (msg), SDL_GetError())
#define max(a,b) ((a) > (b) ? (a) : (b))
#define min(a,b) ((a) < (b) ? (a) : (b))
#define clamp(v,a,b) ((v) = (v) < (a) ? (a) : ((v) > (b) ? (b) : (v)))
#define array_sz(a) (int)(sizeof((a))/sizeof((a)[0]))
#define outcast(ptr,type,field) \
        ((type*)((char*)(ptr)-(unsigned long)(&((type *)0)->field)))
#define CREATE(ptr,type,err) \
  (ptr) = (type*)malloc(sizeof(type)); \
  if (!(ptr)) \
    return (err); \
  memset((ptr), 0, sizeof(type));
#define distance(dx,dy) (((dx)>(dy)) ? ((dx)+((dy)>>1)) : ((dy)+ (dx)>>1)))


// SAM: The below are used by palette_print(), terrain_map_print(), etc.
#define INITIAL_INDENTATION   0
#define INDENTATION_FACTOR    2
#define INDENT fprintf(fp, "%*s", indent, "")

/* Enums *********************************************************************/

/* Structures ****************************************************************/

/* Global Functions **********************************************************/

char * version_as_string(void);  // From nazghul.c

extern int commonInit(void);
extern char *get_dir_str(int dx, int dy);
extern void turnAdvance(int turns);
// extern void windSetDirection(int dir);
extern void busywait(int msec);
extern int vector_to_dir(int dx, int dy);
extern int vector_to_facing(int dx, int dy);
extern int vector_to_rotation(int dx, int dy);

extern int stringToDirection(char *str);
extern int keyToDirection(int key);
extern int directionToDx(int dir);
extern int directionToDy(int dir);
extern char *directionToString(int dir);
extern bool isvowel(char c);
extern bool point_in_rect(int x, int y, SDL_Rect *rect);

#define keyIsDirection(key) ((key) >= KEY_SOUTHWEST && (key) <= KEY_NORTHEAST)

/* Global Variables **********************************************************/

extern struct list Terrain_Palettes;
extern struct list Sprites;
extern int Turn;
extern int AnimationTicks;
extern int Tick;
extern int TickMilliseconds;
// extern int WindDirection;
extern bool Quit;
extern struct los *LosEngine;
extern char *LOS;
extern bool TurnChanged;
extern int ShowAllTerrain;
extern struct sprite *CursorSprite;
extern int SCREEN_BPP;
// #define SAVEFILE "mapfile"
extern char *SAVEFILE;	/* nazghul.c */


#ifdef __cplusplus
}
#endif
#endif
