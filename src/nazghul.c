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
#include "tick.h"
#include "cmd.h"
#include "session.h"
#include "kern.h"

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <SDL.h>
#include <SDL_image.h>
#include <SDL_thread.h>
#include <unistd.h>

#define NEW_GAME_FILE "start-new-game.scm"

extern char *optarg;
extern int optind, opterr, optopt;

// gmcnutt: by default I'd like it on :). For one thing, printing all those
// "Playing sound %s" messages to the console breaks all the regression tests
// :).
static bool useSound = true;	// SAM: Sound drivers on my dev laptop are
char *LOS            = "angband";
int SCREEN_BPP       = DEF_SCREEN_BPP;
char *SAVEFILE       = 0;
char *IncludeDir     = 0;
char *SavedGamesDir  = 0;
char *RecordFile     = 0;
char *PlaybackFile   = 0;
int PlaybackSpeed    = 100;
int DeveloperMode    = 0;

static char program_name[] = "nazghul";
static char *NAZGHUL_SPLASH_IMAGE_FILENAME = "splash.png";

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
	       "    -t: tick <period in msec> \n"
	       "    -a: animation <period in ticks> \n"
	       "    -s: sound <0 to disable> \n"
	       "    -b: bits-per-pixel <bpp> \n"
	       "    -R: recorder <filename>    \n"
	       "    -P: playback <filename>  \n"
	       "    -S: speed <playback ms delay> \n"
               "    -I: game data dir\n"
               "    -G: save game dir\n"
               "<load-file>\n",
               program_name);
}				// print_usage()

static void parse_args(int argc, char **argv)
{
	int c = 0;

	TickMilliseconds = MS_PER_TICK;
	AnimationTicks = ANIMATION_TICKS;

	while ((c = getopt(argc, argv, "b:t:a:s:TdR:S:P:I:G:vh")) != -1) {
		switch (c) {
		case 'b':
			SCREEN_BPP = atoi(optarg);
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
                case 'd':
                        DeveloperMode = 1;
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
		case 'I':
			// Set the global include dir. So that saved games
			// can be stored in the local directory, while
			// we cannot write in the game directory.
			IncludeDir =  strdup(optarg);
			if (!IncludeDir) {
				err("Failed to allocate string for include "
				    "directory\n");
				exit(-1);
			}
			break;
		case 'G':
			// Set the dir to create new saved games into.
			SavedGamesDir =  strdup(optarg);
			if (!SavedGamesDir) {
				err("Failed to allocate string for saved "
				    "games directory\n");
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
        }
}				// parse_args()

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
		sound_init();
}

static void nazghul_splash(void)
{
        SDL_Surface *splash;
        SDL_Rect rect;
	char *filename;

        /* Load the image from the well-known filename */
	filename = dirConcat(IncludeDir, NAZGHUL_SPLASH_IMAGE_FILENAME);
	if (filename) {
		splash = IMG_Load(filename);
		free(filename);
	} else
		splash = IMG_Load(NAZGHUL_SPLASH_IMAGE_FILENAME);
	if (! splash) {
                warn("IMG_Load failed: %s\n", SDL_GetError());
                return;
        }
        
        /* Fill out the screen destination rect */
        rect.x = max(0, (MAP_W - splash->w) / 2) + MAP_X;
        rect.y = max(0, (MAP_H - splash->h) / 2) + MAP_Y;
        rect.w = min(splash->w, MAP_W-MAP_X);
        rect.h = min(splash->h, MAP_H-MAP_Y);

        screenBlit(splash, NULL, &rect);
        screenUpdate(&rect);

        SDL_FreeSurface(splash);
}

static int start_main_menu_session(void)
{
        scheme *sc = NULL;
        FILE *file = NULL;
        char *fname="main-menu.scm";

        /* Open the load file. */
	char *filename = dirConcat(IncludeDir,fname);
	if (filename) {
		file = fopen(filename, "r");
		free(filename);
	} else
		file = fopen(fname, "r");
	if (! file) {
                load_err("could not open script file '%s%s' for reading: %s",
                           IncludeDir?IncludeDir:"", fname, strerror(errno));
                return -1;
        }

        /* Create a new interpreter. */
        if (! (sc = kern_init())) {
                load_err("could not create interpreter");
                fclose(file);
                return -1;
        }

        /* create the initial session */
        Session = session_new(sc);

        /* load the scheme file */
        scheme_load_named_file(sc, file, fname);

        /* sanity checks */
        if (load_err_any()) {
                goto abort;
        }

        if (! Session->frame.llc) {
                load_err("no frame sprites (use kern-set-frame)");
        }

        if (! Session->ascii.images) {
                load_err("no ASCII sprites (use kern-set-ascii)");
        }
        
        if (! Session->cursor_sprite) {
                load_err("no cursor sprite (use kern-set-cursor)");
        }

        return 0;

 abort:
        session_del(Session);
        scheme_deinit(sc);
        free(sc);
        Session=NULL;
        return -1;
}

static int file_exists(char *fname)
{
        FILE *file = fopen(fname,"r");
        int ret = file ? 1:0;
        if (file)
                fclose(file);
        return ret;
}

static bool main_menu_quit_handler(struct QuitHandler *kh)
{
        exit(0);
        return(0); /* for Sun compiler */
}

static void show_credits(void)
{
        struct KeyHandler kh;
        char *title = "CREDITS";
        char *text = 
                "Engine Programming\n"\
                "...Gordon McNutt\n"\
                "...Sam Glasby\n"\
                "...Tim Douglas\n"\
                "...Janne Johansson\n"\
                "...Karl Garrison\n"\
                "Build System\n"\
                "...Andreas Bauer\n"\
                "Game Scripting\n"\
                "...Gordon McNutt\n"\
                "...Sam Glasby\n"
                "Art Provided by\n"\
                "...Joshua Steele\n"\
                "...David Gervais\n"\
                "...Kevin Gabbert\n"\
                "...Gordon McNutt\n"\
                "...Sam Glasby\n"\
                "...Steve Riberdy\n"\
                "...Kris Parker\n"
                ;

        statusSetPageText(title, text);
        statusSetMode(Page);
        consolePrint("[Hit ESC to continue]\n");

        kh.fx = scroller;
        kh.data = NULL;
	eventPushKeyHandler(&kh);
	eventHandle();
	eventPopKeyHandler();
}

static char *START_NEW_GAME="Start New Game";
static char *JOURNEY_ONWARD="Journey Onward";
static char *CREDITS="Credits";
static char *QUIT="Quit";
static char *TUTORIAL="Tutorial";

static int confirm_selection()
{
        int yesno;
        log_msg("Existing saved game will be overwritten! Are you sure?");
        cmdwin_clear();
        cmdwin_print("Confirm-Y/N?");
        getkey(&yesno, yesnokey);
        cmdwin_backspace(4);
        if (yesno=='y') {
                cmdwin_print("Yes!");
                log_msg("Ok!");
                return 1;
        } else {
                cmdwin_print("No!");
                log_msg("Canceled!");
                return 0;
        }
}

static void main_menu(void)
{
        char *menu[5];
        int n_items = 0;
        struct KeyHandler kh;
	struct ScrollerContext data;
        char *selection = NULL;
	struct QuitHandler qh;

        if (start_main_menu_session()) {
                fprintf(stderr, "fatal error in main_menu\n");
                exit(-1);
        }

        screen_repaint_frame();

        /* if save file specified on command line skip the menu */
        if (SAVEFILE)
                return;

        /* setup main menu quit handler so player can click close window to
         * exit */
	qh.fx = main_menu_quit_handler;
	eventPushQuitHandler(&qh);


 start_main_menu:
        n_items = 0;

	// Perhaps use QUICKSAVE_FNAME instead?
        if (file_exists("save.scm")) {
                menu[n_items] = JOURNEY_ONWARD;
                n_items++;
        } else {
		char *tmp = dirConcat(SavedGamesDir,"save.scm");
		if (tmp) {
			if (file_exists(tmp)) {
				menu[n_items] = JOURNEY_ONWARD;
				n_items++;
			}
			free(tmp);
		}
	}

        menu[n_items] = START_NEW_GAME;
        n_items++;

        if (file_exists("tutorial.scm")) {
                menu[n_items] = TUTORIAL;
                n_items++;
        } else {
		char *tmp = dirConcat(IncludeDir,"tutorial.scm");
		if (tmp) {
			if (file_exists(tmp)) {
				menu[n_items] = TUTORIAL;
				n_items++;
			}
			free(tmp);
		}
        }

        menu[n_items] = CREDITS;
        n_items++;

        menu[n_items] = QUIT;
        n_items++;

        statusSetStringList(n_items, menu);
        statusSetMode(StringList);

        data.selection = NULL;
        data.selector  = String;
        kh.fx   = scroller;
        kh.data = &data;
	eventPushKeyHandler(&kh);
	eventHandle();
	eventPopKeyHandler();

        selection = (char*)data.selection;

        if (! selection) {
                goto start_main_menu;
        }

        if (! strcmp(selection, START_NEW_GAME)) {

                /* prompt before over-writing save file */
                if (file_exists("save.scm")) {
                        if (! confirm_selection()) {
                                goto start_main_menu;
                        }
                }
		/* Why is this haxima and not something game-unspecific? */
                SAVEFILE=NEW_GAME_FILE;
        }
        else if (! strcmp(selection, JOURNEY_ONWARD)) {
                SAVEFILE="save.scm";
        }
        else if (! strcmp(selection, CREDITS)) {
                show_credits();
                goto start_main_menu;
        }
        else if (! strcmp(selection, TUTORIAL)) {
                SAVEFILE="tutorial.scm";
        }
        else if (! strcmp(selection, QUIT))
                exit(0);
        else {
                fprintf(stderr, "Invalid selection: '%s'\n", selection);
                exit(-1);
        }

        /* turn off status while new session is loading */
        statusSetMode(DisableStatus);

        /* pop main menu quit handler, new one will be pushed in play.c */
        eventPopQuitHandler();
        
}

int main(int argc, char **argv)
{
	parse_args(argc, argv);

        nazghul_init_internal_libs();

        tick_start(TickMilliseconds);

// main_loop:
        /* blank out the whole screen */
        screenErase(NULL);
        screenUpdate(NULL);

        /* pause animation tick generation */
        tick_pause();

        /* Show the splash screen on startup */
        nazghul_splash();
        main_menu();

	playRun();

        /* reset save file so main menu runs */
        SAVEFILE=0;

        // memory leaks prevent this from being a good idea:
        //goto main_loop;

        tick_kill();

	return 0;
}

