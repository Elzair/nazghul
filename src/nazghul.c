/* Copyright (c) 2002 Gordon McNutt */
#include "lexer.h"
#include "util.h"
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
#include "wq.h"
#include "place.h"
#include "conv.h"
#include "Spell.h"
#include "wind.h"
#include "cmdwin.h"

#include <stdio.h>
#include <stdlib.h>
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include <SDL/SDL_ttf.h>
#include <SDL/SDL_thread.h>
#include <unistd.h>
#include <getopt.h>

// gmcnutt: by default I'd like it on :). For one thing, printing all those
// "Playing sound %s" messages to the console breaks all the regression tests
// :).
static bool useSound = true;	// SAM: Sound drivers on my dev laptop are
				// borken...
char *LOS = "angband";		// hack 
int SCREEN_BPP = DEF_SCREEN_BPP;
char *SAVEFILE = "mapfile";
char *RecordFile = 0;
char *PlaybackFile = 0;
int PlaybackSpeed = 100;

static char program_name[] = "nazghul";
static int version_major = 0;
static int version_minor = 1;
static int version_incr = 0;

static void print_version(void)
{
        printf("%s %d.%d.%d\n", program_name, version_major, version_minor, 
               version_incr);
        printf("Copyright (C) 2003 Gordon McNutt, Sam Glasby\n"
               "%s comes with NO WARRANTY,\n"
               "to the extent permitted by law.\n"
               "You may redistribute copues of %s\n"
               "under the terms of the GNU General Public License.\n"
               "For more information about these matters,\n"
               "see the files named COPYING.\n",
               program_name, program_name
                );
}

static void print_usage(void)
{
	printf("Usage:  %s [options] \n"
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
               "    --version \n",
               program_name);
}				// print_usage()

static void parse_args(int argc, char **argv)
{
	static struct option long_options[] = {
		{"animate", 1, 0, 'a'},
		{"bpp", 1, 0, 'b'},
		{"circular_vision_radius", 0, 0, 'c'},
		{"file", 1, 0, 'f'},
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
	int c, optind;
	char *tmp;

	TickMilliseconds = MS_PER_TICK;
	AnimationTicks = ANIMATION_TICKS;

	while ((c = getopt_long(argc, argv, "f:w:h:l:t:a:s:b:c", long_options,
				&optind)) != -1) {
		switch (c) {
		case 'b':
			SCREEN_BPP = atoi(optarg);
			break;
		case 'f':
			if ((tmp = strdup(optarg)))
				SAVEFILE = tmp;
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
		default:
			print_usage();
			exit(-1);
		}		// switch (c)
	}			// while (c)

}				// parse_args()

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

int main(int argc, char **argv)
{
	SDL_Thread *tick_thread;

	parse_args(argc, argv);

	commonInit();
	screenInit();
	wqInit();
	combatInit();
	spriteInit();
	if (player_init())
		exit(-1);
	if (eventInit())
		exit(-1);
	placeInit();
	convInit();
	Spell_init();
	windInit();
	if (cmdwin_init() < 0) {
		err("Error initializing command window\n");
		exit(-1);
	}

	if (useSound)
		soundInit();

	tick_thread = SDL_CreateThread(tick_fx, 0);

	playRun();

	SDL_KillThread(tick_thread);	/* Note: don't try to wait after this */

	return 0;
}				// main()

// eof
