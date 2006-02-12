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

#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
// #include <sys/types.h>  // getpid()
#include <unistd.h>     // getpid()
#include <errno.h>

#ifndef USE_AMBUSH
# define USE_AMBUSH 1
#endif

#ifndef PROFILE_PLAY_LOOP
#define PROFILE_PLAY_LOOP 0
#endif

enum cmdstate {
	CMD_IDLE,
	CMD_GET,
	CMD_INV,
	CMD_DROP,
	CMD_FIRE,
	CMD_LOOK,
	CMD_ZTATS,
	CMD_ZTATS2,
	CMD_RDY,
	CMD_RDY2,
	CMD_USE,
	CMD_DEAD
};

bool Quit;

static bool tickHandler(struct TickHandler *th)
{
	Tick++;
        wqRunToTick(&TickWorkQueue, Tick);
        spriteAdvanceTicks(1);
	return Quit;
}

static bool quitHandler(struct QuitHandler *kh)
{
	cmdQuit();
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
        tick_pause();
        log_begin("Loading from %s...", QUICKSAVE_FNAME);
        log_flush();
        log_disable();
        result = session_load(QUICKSAVE_FNAME);
        log_enable();
        if (result)
                log_end("error!");
        else
                log_end("ok!");
        Reload = 0;
        tick_run();
        vmask_flush_all();
}

static void play_loop(void)
{
        int times[8];

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
                // Check for changes in the combat state as a result of the
                // last turn. This check might force a transition from combat
                // to wilderness mode.
                // ------------------------------------------------------------
                
                times[1] = SDL_GetTicks();
                if (combat_get_state() != COMBAT_STATE_DONE) {
                        combat_analyze_results_of_last_turn();
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

                        // ----------------------------------------------------
                        // Update the "Turn" counter. This drives the
                        // TurnWorkQueue, which is still used for scheduled
                        // things like torches burning out and doors closing.
                        // ----------------------------------------------------
                        Turn += session_ticks_per_turn();
                }

                G_exec_loops++;
                times[5] = SDL_GetTicks();
                
                if (PROFILE_PLAY_LOOP) {
                        printf("Loop time=%d\n", times[5] - times[0]);
                        printf("      place_exec: %d\n", times[1] - times[0]);
                        printf("     end-of-turn: %d\n", times[2] - times[1]);
                        printf("  pending events: %d\n", times[3] - times[2]);
                        printf("     end-of-game: %d\n", times[4] - times[3]);
                        printf("    advance misc: %d\n", times[5] - times[4]);
                }
                
        }
}

static void updateAfterEvent(void)
{
	mapUpdate(REPAINT_IF_DIRTY|REPAINT_IF_OLD);
}

int playRun(void)
{
	struct QuitHandler qh;
	struct TickHandler th;

        // -------------------------------------------------------------------
        // Load a new session from a saved file.
        // -------------------------------------------------------------------

        log_begin("Loading from %s...", SAVEFILE);
        log_flush();
        log_disable();

	session_load(SAVEFILE);

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

        // Bugfix: sun & moon not shown until player takes a step
        sky_advance(&Session->sky, 
                    NULL != Place && ! Place->underground);

        // Run the optional startup script.
        session_run_start_proc(Session);

        if (! Place) {
                log_msg("Error: no starting place!");
                return -1;
        }

        mapUpdate(REPAINT_IF_DIRTY);

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

	return 1;
}
