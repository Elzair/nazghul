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
#include "../config.h"
#include "foogod.h"
#include "constants.h"
#include "file.h"
#include "dimensions.h"
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
#include "tick.h"
#include "cmd.h"
#include "session.h"
#include "kern.h"
#include "cfg.h"
#include "menus.h"
#include "nazghul.h"
#include "conv.h"

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <SDL.h>
#include <SDL_image.h>
#include <SDL_thread.h>
#include <unistd.h>
#include <getopt.h>

/* Name of the file to load the game from. */
static char *nazghul_load_fname = 0;

int FullScreenMode = 0;
int DeveloperMode = 0;
int ExitProgram = 0;

static char program_name[] = "nazghul";

static void print_version(void)
{
        printf("%s %s\n", program_name, PACKAGE_VERSION);
        printf("Copyright (C) 2003 Gordon McNutt, Sam Glasby\n"
               "%s comes with NO WARRANTY,\n"
               "to the extent permitted by law.\n"
               "You may redistribute copies of %s\n"
               "under the terms of the GNU General Public License.\n"
               "For more information about these matters,\n"
               "see the file named COPYING.\n",
               program_name, program_name
                );
}

static void print_usage(void)
{
	printf("Usage:  %s [options] <load-file>\n"
	       "Options: \n"
               "    -h:	help\n"
               "    -v: version\n"
               "    -d: developer mode\n"
               "    -f: fullscreen mode\n"
	       "    -t: tick <period in msec> \n"
	       "    -a: animation <period in ticks> \n"
	       "    -s: sound <0 to disable> \n"
	       "    -R: recorder <filename>    \n"
	       "    -P: playback <filename>  \n"
	       "    -S: speed <playback ms delay> \n"
               "    -I: game data dir\n"
               "    -G: save game dir\n"
               "    -r: screen size <pixels> (eg, 640x480)\n"
               "    -T: show all terrain\n"
               "<load-file>\n",
               program_name);
}				// print_usage()

static void parse_args(int argc, char **argv)
{
	int c = 0;

	TickMilliseconds = MS_PER_TICK;
	AnimationTicks = ANIMATION_TICKS;

	while ((c = getopt(argc, argv, "t:a:s:TdfR:S:P:I:G:vhr:")) != -1) {
		switch (c) {
		case 't':
			TickMilliseconds = atoi(optarg);
			break;
		case 'a':
			AnimationTicks = atoi(optarg);
			break;
		case 's':
                        cfg_set("sound-enabled", 
                                atoi(optarg) != 0 ? "yes" : "no");
			break;
		case 'T':
			ShowAllTerrain = 1;
			break;
                case 'd':
                        DeveloperMode = 1;
                        break;
                case 'f':
                        FullScreenMode = 1;
                        break;
		case 'R':
                        /* Set the filename for recording keystrokes. */
			cfg_set("record-filename", optarg);
			break;
		case 'S':
                        /* Set the speed to play back recorded keystrokes. */
                        cfg_set("playback-speed", optarg);
			break;
		case 'P':
                        /* Set the file to play back keystrokes from. */
                        cfg_set("playback-filename", optarg);
			break;
		case 'I':
                        /* Set the directory for read-only game and cfg
                         * files. */
			cfg_set("include-dirname", optarg);
			break;
		case 'G':
                        /* Set the directory for read-write game and cfg
                         * files. */
			cfg_set("saved-games-dirname", optarg);
			break;
                case 'v':
                        print_version();
                        exit(0);
                        break;
                case 'h':
                        print_usage();
                        exit(0);
                case 'r':
                        /* set the screen dimensions */
                        cfg_set("screen-dims", optarg);
                        break;
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
                nazghul_load_fname = argv[optind];
        }
}				// parse_args()

/**
 * This initializes the various submodules.
 */
static void nazghul_init_internal_libs(void)
{
        struct lib_entry {
                const char *name;
                int (*init)(void);
        };

        struct lib_entry libs[] = {
                { "commonInit",     commonInit     },
                { "screenInit",     screenInit     },
                { "asciiInit",      asciiInit      },
                { "sprite_init",    sprite_init    },
                { "eventInit",      eventInit      },
                { "windInit",       windInit       },
                { "formation_init", formation_init },
                { "astar_init",     astar_init     },
                { "cmdwin_init",    cmdwin_init    },
                { "consoleInit",    consoleInit    },
                { "mapInit",        mapInit        },
                { "vmask_init",     vmask_init     },
                { "combatInit",     combatInit     },
                { "statusInit",     statusInit     },
                { "foogodInit",     foogodInit     },
                { "menu_init",      menu_init      },
                { "conv_init",      conv_init      }
        };

        int i;

        for (i = 0; i < array_sz(libs); i++) {
                if (libs[i].init() < 0) {
                        /* this will not return: */
                        menu_startup_error("Error in %s", libs[i].name);
                }
        }

        log_init();

        if (!strcmp("yes", cfg_get("sound-enabled"))) {
                sound_init();
                music_init();
                set_music_volume(cfg_get("music-volume"));
        }

}

/* nazghul_splash -- show the splash image */
void nazghul_splash(void)
{
        static SDL_Surface *splash = 0;
        SDL_Rect rect;

        /* The first time through load the splash image from a file. */
        if (! splash) {
                char *dims = cfg_get("screen-dims");
                const char *suffix = "-splash-image-filename";
                char *key;
                char *basename;
                char *filename;
                
                /* Build a key for the splash image filename based on the
                 * screen dimensions. */
                key = (char*)malloc(strlen(dims) + strlen(suffix) + 1);
                assert(key);
                strcpy(key, dims);
                strcat(key, suffix);
                
                /* Lookup the filename. */
                basename = cfg_get(key);
                if (! basename) {
                        warn("cfg_get: no key matches '%s'", key);
                        free(key);
                        return;
                }
                free(key);
                
                /* Look for the splash image, check the include dir first, then
                 * check the current working dir */
                filename = file_mkpath(cfg_get("include-dirname"), basename);
                if (filename) {
                        splash = IMG_Load(filename);
                        free(filename);
                }
                if (! splash) {
                        warn("IMG_Load failed: %s\n", SDL_GetError());
                        return;
                }

        }

        rect.x = MAP_X;
        rect.y = MAP_Y;
        rect.w = MAP_W;
        rect.h = MAP_H;
        screenErase(&rect);

        /* Fill out the screen destination rect */
        rect.x = max(0, (MAP_W - splash->w) / 2) + MAP_X;
        rect.y = max(0, (MAP_H - splash->h) / 2) + MAP_Y;
        rect.w = min(splash->w, MAP_W);
        rect.h = min(splash->h, MAP_H);

        screenBlit(splash, NULL, &rect);
        screenUpdate(&rect);
}

/* init_default_cfg -- initialize the global cfg settings to start-up defaults
 * and prepare it for loading the cfg script */
static void init_default_cfg()
{
        cfg_init();
        cfg_set("init-script-filename", "kern-init.scm");
        cfg_set("options-script-filename", "options.scm");
        cfg_set("splash-image-filename", "splash.png");
        cfg_set("screen-dims", "1280x960" /*"640x480"*/);
        cfg_set("sound-enabled", "yes");
        cfg_set("music-volume", "100%");
        cfg_set("keyword-highlighting", "yes");
}

int main(int argc, char **argv)
{
        int print_version = 1;

        /* Initialize the cfg environment before parsing args. */
        init_default_cfg();

	parse_args(argc, argv);

        /* Load the cfg script after parsing args */
        if (file_load_from_include_dir(cfg_get("init-script-filename"))) {
                menu_startup_error("Error loading %s: %s", 
                                   cfg_get("init-script-filename"), 
                                   file_get_error());
        }

        /* Load the options script */
        if (file_exists_in_save_dir(cfg_get("options-script-filename"))) {
                if (file_load_from_save_dir
                    (cfg_get("options-script-filename"))) {
                        warn("Could not load options script\n");
                }
        }

        if (dimensions_init()) {
                err("dimensions_init() failed\n");
                exit(-1);
        }

        nazghul_init_internal_libs();

        tick_start(TickMilliseconds);

 main_loop:
        /* blank out the whole screen */
        screenErase(NULL);
        screenUpdate(NULL);

        /* pause animation tick generation */
        tick_pause();

        /* Show the splash screen on startup */
        nazghul_splash();
        
        /* The first time we start print the vesrsion info. */
        if (print_version) {
                print_version = 0;
                log_banner("^c+bNazghul version^c- ^c+G%s^c-", PACKAGE_VERSION );
        }

        /* paint the border for the first time */
        screen_repaint_frame();

        /* if no load file specified on the command line then run the main
         * menu */
        if (! nazghul_load_fname)
                nazghul_load_fname = main_menu();

        /* Clear out the vmask cache */
        vmask_flush_all();

        /* run the game, don't return until the user quits */
	playRun(nazghul_load_fname);

        /* cleanup modules that need it */
        eventExit();

        /* FIXME: need to free nazghul_load_fname */
        /* reset save file so main menu runs */
        nazghul_load_fname=0;

        /* ExitProgram is set when the player closes the window; in that case
         * drop out of the program without showing the main menu again. */
        if (! ExitProgram) {
                /* In the past, memory leaks made this a bad idea. Let's give
                 * it another go and try to work through them if they crop up
                 * again. */
                goto main_loop;
        }

        tick_kill();

	return 0;
}

