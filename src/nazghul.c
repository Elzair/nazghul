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
#include "foogod.h"
#include "constants.h"
#include "common.h"
#include "screen.h"
#include "sound.h"
#include "play.h"
#include "event.h"
#include "combat.h"
#include "images.h"
#include "sprite.h"
#include "player.h"
#include "place.h"
#include "wind.h"
#include "cmdwin.h"
#include "formation.h"
#include "map.h"
#include "vmask.h"
#include "status.h"
#include "log.h"

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <SDL.h>
#include <SDL_image.h>
#include <SDL_thread.h>
#include <unistd.h>
#include <getopt.h>

extern char *optarg;
extern int optind, opterr, optopt;


// gmcnutt: by default I'd like it on :). For one thing, printing all those
// "Playing sound %s" messages to the console breaks all the regression tests
// :).
static bool useSound = true;	// SAM: Sound drivers on my dev laptop are
char *LOS            = "angband";
int SCREEN_BPP       = DEF_SCREEN_BPP;
char *SAVEFILE       = 0;
char *RecordFile     = 0;
char *PlaybackFile   = 0;
int PlaybackSpeed    = 100;

static char program_name[] = "nazghul";
static int version_major = 0;
static int version_minor = 2;
static int version_incr  = 1;

#define VERSION_STRLEN 8  // Length of "xx.yy.zz" == 6+2
char * version_as_string(void)
{
  static char version_string[VERSION_STRLEN+1];
  snprintf(version_string, VERSION_STRLEN, "%d.%d.%d", 
           version_major, version_minor, version_incr);
  return version_string;
}

static void print_version(void)
{
        printf("%s %s\n", program_name, version_as_string() );
        printf("Copyright (C) 2003 Gordon McNutt, Sam Glasby\n"
               "%s comes with NO WARRANTY,\n"
               "to the extent permitted by law.\n"
               "You may redistribute copies of %s\n"
               "under the terms of the GNU General Public License.\n"
               "For more information about these matters,\n"
               "see the files named COPYING.\n",
               program_name, program_name
                );
}

static void print_usage(void)
{
	printf("Usage:  %s [options] <load-file>\n"
	       "Options: \n"
               "    --help \n"
	       "    --los <line-of-sight(floodfill|angband)> \n"
	       "    --tick <game tick period in msec> \n"
	       "    --animate <period in ticks> \n"
	       "    --sound <0 to disable> \n"
	       "    --bpp=<bits per pixel> \n"
	       "    --record <filename>    \n"
	       "    --playback <filename>  \n"
	       "    --playback_speed <ms delay> \n"
               "    --version \n"
               "<load-file> is the session to load\n",
               program_name);
}				// print_usage()

static void parse_args(int argc, char **argv)
{
	static struct option long_options[] = {
		{"animate", 1, 0, 'a'},
		{"bpp", 1, 0, 'b'},
		{"circular_vision_radius", 0, 0, 'c'},
                {"help", 0, 0, 'h'},
		{"los", 1, 0, 'l'},
		{"ShowAllTerrain", 0, 0, 'T'},
		{"sound", 1, 0, 's'},
		{"playback", 1, 0, 'P'},
		{"playback_speed", 1, 0, 'S'},
		{"record", 1, 0, 'R'},
		{"tick", 1, 0, 't'},
                {"version", 0, 0, 'v'},
		{0, 0, 0, 0}
	};
	int c = 0;
	char *tmp;
        int option_index = 0;

	TickMilliseconds = MS_PER_TICK;
	AnimationTicks = ANIMATION_TICKS;

	while ((c = getopt_long(argc, argv, "w:h:l:t:a:s:b:c", long_options,
				&option_index)) != -1) {
		switch (c) {
		case 'b':
			SCREEN_BPP = atoi(optarg);
			break;
		case 'l':
			if ((tmp = strdup(optarg)))
				LOS = tmp;
			break;
		case 't':
			TickMilliseconds = atoi(optarg);
			break;
		case 'a':
			AnimationTicks = atoi(optarg);
			break;
		case 's':
			useSound = atoi(optarg) != 0;
			break;
		case 'T':
			ShowAllTerrain = 1;
			break;
		case 'c':
			map_use_circular_vision_radius = 1;
			break;
		case 'R':
			// Set the global RecordFile pointer. Used by
			// eventInit().
			RecordFile = strdup(optarg);
			if (!RecordFile) {
				err("Failed to allocate string for record "
				    "filename\n");
				exit(-1);
			}
			break;
		case 'S':
			PlaybackSpeed = atoi(optarg);
			break;
		case 'P':
			// Set the global PlaybackFile pointer. Used by
			// eventInit().
			PlaybackFile = strdup(optarg);
			if (!PlaybackFile) {
				err("Failed to allocate string for playback "
				    "filename\n");
				exit(-1);
			}
			break;
                case 'v':
                        print_version();
                        exit(0);
                        break;
                case 'h':
                        print_usage();
                        exit(0);
                case '?':
                default:
			print_usage();
			exit(-1);
                        break;
		}		// switch (c)
	}			// while (c)

        // --------------------------------------------------------------------
        // Any remaining option is assumed to be the save-file to load the game
        // from. If there is none then abort.
        // --------------------------------------------------------------------

        if (optind < argc) {
                SAVEFILE = argv[optind];
        } else {
                print_usage();
                exit(-1);
        }

}				// parse_args()

static void tick_sig_handler(int signo)
{
        exit(0);
}

int tick_fx(void *data)
{
	unsigned int tick_usecs = TickMilliseconds * 1000;
	SDL_Event tick_event;

	tick_event.type = SDL_USEREVENT;
	tick_event.user.code = TICK_EVENT;

	for (;;) {
		usleep(tick_usecs);
		SDL_PushEvent(&tick_event);
	}
}				// tick_fx()

static void nazghul_init_internal_libs(void)
{
        struct lib_entry {
                char *name;
                int (*init)(void);
        };

        struct lib_entry libs[] = {
                { "commonInit",     commonInit     },
                { "screenInit",     screenInit     },
                { "spriteInit",     spriteInit     },
                { "player_init",    player_init    },
                { "eventInit",      eventInit      },
                { "windInit",       windInit       },
                { "formation_init", formation_init },
                { "astar_init",     astar_init     },
                { "cmdwin_init",    cmdwin_init    },
                { "consoleInit",    consoleInit    },
                { "mapInit",        mapInit        },
                { "vmask_init",     vmask_init     },
                { "combatInit",     combatInit     },
                { "foogodInit",     foogodInit     },
                { "statusInit",     statusInit     },
        };

        int i;

        for (i = 0; i < array_sz(libs); i++) {
                if (libs[i].init() < 0) {
                        err("Error in %s\n", libs[i].name);
                        exit(-1);
                }
        }

        log_init();

	if (useSound)
		soundInit();
}

int main(int argc, char **argv)
{
	SDL_Thread *tick_thread = NULL;

	parse_args(argc, argv);

        nazghul_init_internal_libs();

        if (TickMilliseconds > 0) {
                tick_thread = SDL_CreateThread(tick_fx, 0);
        }

	playRun();

        if (NULL != tick_thread)
                SDL_KillThread(tick_thread);	/* Note: don't try to wait after this */

	return 0;
}				// main()

// eof
