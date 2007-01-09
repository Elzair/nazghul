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
/* 12/14/2002  Added HANDLE command by Sam Glasby.
 */
#include "config.h"
#include "foogod.h"
#include "play.h"
#include "session.h"
#include "place.h"
#include "constants.h"
#include "images.h"
#include "sprite.h"
#include "los.h"
#include "astar.h"
#include "common.h"
#include "screen.h"
#include "console.h"
#include "status.h"
#include "player.h"
#include "sky.h"
#include "map.h"
#include "wq.h"
#include "combat.h"
#include "cursor.h"
#include "Arms.h"
#include "event.h"
#include "wind.h"
#include "Container.h"
#include "dup_constants.h"
#include "cmdwin.h"
#include "vehicle.h"
#include "terrain.h"
#include "cmd.h"
#include "debug.h"
#include "log.h"
#include "tick.h"
#include "vmask.h"
#include "menus.h"

#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
// #include <sys/types.h>  // getpid()
#include <unistd.h>     // getpid()
#include <errno.h>

#ifndef PROFILE_PLAY_LOOP
#define PROFILE_PLAY_LOOP 0
#endif

#ifndef DETAILED_PROFILE
#define DETAILED_PROFILE 0
#endif

bool Quit;

static bool tickHandler(struct TickHandler *th)
{
	Tick++;
        wqRunToTick(&TickWorkQueue, Tick);
        sprite_advance_ticks(1);
	return Quit;
}

static bool quitHandler(struct QuitHandler *kh)
{
        extern int ExitProgram; /* see nazghul.c */
	cmdQuit();
        if (Quit) {
                ExitProgram = 1;
        }
	return Quit;
}

static void play_print_end_of_game_prompt()
{
	consolePrint("\n\n*** YOU HAVE DIED ***\n\n");
	consolePrint("Press any key to exit.\n");
	getkey(NULL, anykey);
}

int G_exec_loops = 0;

static void play_reload()
{
        int result = 0;
        char *fname = 0;

        Reload = 0;

        fname = load_game_menu();
        if (!fname)
                return;

        tick_pause();
        log_begin("Loading from %s...", fname);
        log_flush();
        log_disable();
        result = session_load(fname);
        free(fname);
        log_enable();
        if (result)
                log_end("error!");
        else
                log_end("ok!");
        place_synchronize(Place);
        tick_run();
        vmask_flush_all();
}

static void play_loop(void)
{
        int times[8];
        int total_time = 0;
        

        Turn = 0;

        // --------------------------------------------------------------------
        // Enter the main game loop. Do not exit until the player is dead or
        // wants to quit.
        // --------------------------------------------------------------------

        while (true) {

                // ------------------------------------------------------------
                // If the player party is camping then run the camping hook
                // so the script can do things like ambushes, visions, etc.
                // Since we don't know what the script might do check for party
                // death and the Quit condition when it comes back.
                // ------------------------------------------------------------

                if (player_party->isCamping() && Session->camping_proc) {
                        closure_exec(Session->camping_proc, "pp", 
                                     player_party, Place);
                        if (player_party->allDead()) {
                                play_print_end_of_game_prompt();
                                break;
                        }
                        if (Quit)
                                break;
                }

                // ------------------------------------------------------------
                // "Execute" the current place. This will loop through all
                // objects in the current place and give them a chance to
                // run. This includes player-controlled objects.
                // ------------------------------------------------------------

                times[0] = SDL_GetTicks();
                place_exec(Place);

                // ------------------------------------------------------------
                // Check if the player requested to reload during that last
                // turn.
                // ------------------------------------------------------------

                if (Reload) {
                        play_reload();
                }

                // ------------------------------------------------------------
                // Do a non-blocking check to see if any non-user events need
                // to run (e.g., animation ticks). Normally these events get
                // run in place_exec() when the game waits for player input,
                // but if the player party is resting (for example) then that
                // does not happen. To keep animation looking smooth I added
                // this next call.
                // ------------------------------------------------------------

                times[2] = SDL_GetTicks();
                eventHandlePending();

                // ------------------------------------------------------------
                // Check for end-of-game conditions.
                // ------------------------------------------------------------

                times[3] = SDL_GetTicks();
                if (player_party->allDead()) {
                        play_print_end_of_game_prompt();
                        break;
                }

                if (Quit)
                        break;

                // ------------------------------------------------------------
                // Run all the non-object stuff in the game world.
                //
                // NOTE: you should keep the clock update first since most
                //       things take their cue from the current time.
                // ------------------------------------------------------------

                times[4] = SDL_GetTicks();

                dec_time_stop(session_ticks_per_turn());

                if (! TimeStop) {
                        
                        dec_reveal(session_ticks_per_turn());
                        dec_quicken(session_ticks_per_turn());
                        dec_magic_negated(session_ticks_per_turn());
                        dec_xray(session_ticks_per_turn());
                        
                        player_party->advanceTurns(session_ticks_per_turn());

                        int oldHour = Session->clock.hour;
                        clock_advance(session_ticks_per_turn());

                        // On each change of the hour check characters with
                        // multi-place schedules to see if they need to be
                        // introduced to the current place.
                        if (oldHour != Session->clock.hour) {
                                session_intro_sched_chars(Session);
                        }

                        foogodAdvanceTurns();
                        sky_advance(&Session->sky, 
                                    NULL != Place && ! Place->underground);
                        windAdvanceTurns();
                        wqRunToTick(&TurnWorkQueue, Turn);

                        /* Update the "Turn" counter. This drives the
                         * TurnWorkQueue, which is still used for scheduled
                         * things like torches burning out and doors closing.
                         * Note: this is NOT the turn count shown in the foogod
                         * window, that one is stored in the session struct and
                         * is incremented above.
                         */
                        Turn += session_ticks_per_turn();
                }

                G_exec_loops++;
                times[5] = SDL_GetTicks();

                if (PROFILE_PLAY_LOOP) {
                        total_time += (times[5]-times[0]);
                        printf("Loop time=%d avg=%d\n", times[5] - times[0],
                               total_time/G_exec_loops);
                        if (DETAILED_PROFILE) {
                          printf("      place_exec: %d\n", 
                                 times[1] - times[0]);
                          printf("     end-of-turn: %d\n", 
                                 times[2] - times[1]);
                          printf("  pending events: %d\n", 
                                 times[3] - times[2]);
                          printf("     end-of-game: %d\n", 
                                 times[4] - times[3]);
                          printf("    advance misc: %d\n", 
                                 times[5] - times[4]);
                        }
                }
                
        }
}

static void updateAfterEvent(void)
{
	mapUpdate(REPAINT_IF_DIRTY|REPAINT_IF_OLD);
}

int playRun(char *fname)
{
	struct QuitHandler qh;
	struct TickHandler th;

        // -------------------------------------------------------------------
        // Load a new session from a saved file.
        // -------------------------------------------------------------------

        log_begin("Loading from %s...", fname);
        log_flush();
        log_disable();

	session_load(fname);

        log_enable();
        if (! Session) {
                log_end("error!");
		return -1;
	}
        log_end("ok!");
        log_begin_group();
        log_msg("*********************************");
	log_msg("Welcome to Nazghul version %s", PACKAGE_VERSION );
        log_msg("*********************************");
        log_end_group();
        log_msg("'?' for help.");

        // Run the optional startup script.
        session_run_start_proc(Session);

        /* bugfix: Place may not be set until after the startup script runs, so
         * now is the first time we can be sure that the following will repaint
         * the sun and moons. */
        sky_advance(&Session->sky, 
                    NULL != Place && ! Place->underground);

        if (! Place) {
                log_msg("Error: no starting place!");
                return -1;
        }

        /* bugfix for [ 1475929 ]: sleeping npcs wake on reloads */
        place_synchronize(Place);

        mapUpdate(REPAINT_IF_DIRTY);
        statusRepaint();

	// Setup all the event handlers.
	qh.fx = quitHandler;
	th.fx = tickHandler;
	eventPushQuitHandler(&qh);
	eventPushTickHandler(&th);
	eventAddHook(updateAfterEvent);

	Quit = false;
        tick_run();

	// Enter the main event loop. This won't exit until the player quits or
	// dies.
        play_loop();

	// Cleanup the event handlers.
	eventPopTickHandler();
	eventPopQuitHandler();
	//eventPopKeyHandler();

        session_del(Session); /* here we go... */
        Session = 0;

	return 1;
}
