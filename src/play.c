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

#include "foogod.h"
#include "play.h"
#include "session.h"
#include "game.h"
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
#include "Portal.h"
#include "terrain.h"
#include "cmd.h"
#include "debug.h"

#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
// #include <sys/types.h>  // getpid()
#include <unistd.h>     // getpid()
#include <errno.h>

#define PROFILE_PLAY_LOOP 0

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
	CMD_DEAD,
};

bool Quit;


static void myExpireRevealEffect(struct wq_job *job, struct list *wq)
{
	char *cause = (char *) job->data;
	Reveal = false;
	consolePrint("%s expired\n", cause);
	free(job);
}

void effectReveal(char *name, int duration)
{
	Reveal = true;
	wqCreateJob(&TurnWorkQueue, Turn + duration,
		    0, name, myExpireRevealEffect);
}

static void myExpireQuickenEffect(struct wq_job *job, struct list *wq)
{
	char *cause = (char *) job->data;
	Quicken = 0;
	consolePrint("%s expired\n", cause);
        foogodRepaint();
	free(job);
}

void effectQuicken(char *name, int duration)
{
	Quicken = 2;
	wqCreateJob(&TurnWorkQueue, Turn + duration, 0, name, 
                    myExpireQuickenEffect);
        foogodRepaint();
}


static void myExpireTimeStopEffect(struct wq_job *job, struct list *wq)
{
	char *cause = (char *) job->data;
	TimeStop = 0;
	consolePrint("%s expired\n", cause);
        foogodRepaint();
	free(job);
}

void effectTimeStop(char *name, int duration)
{
	TimeStop = 1;
	wqCreateJob(&TurnWorkQueue, Turn + duration, 0, name, 
                    myExpireTimeStopEffect);
        foogodRepaint();
}

static void myExpireNegateMagicEffect(struct wq_job *job, struct list *wq)
{
	char *cause = (char *) job->data;
	MagicNegated--;
	consolePrint("%s expired\n", cause);
        foogodRepaint();
	free(job);
}

void effectNegateMagic(char *name, int duration)
{
	MagicNegated++;
	wqCreateJob(&TurnWorkQueue, Turn + duration, 0, name, 
                    myExpireNegateMagicEffect);
        foogodRepaint();
}

static void myExpireShowTerrainEffect(struct wq_job *job, struct list *wq)
{
	char *cause = (char *) job->data;
	ShowAllTerrain--;
	consolePrint("%s expired\n", cause);
	free(job);
}

void effectShowTerrain(char *name, int duration)
{
	ShowAllTerrain++;
	wqCreateJob(&TurnWorkQueue, Turn + duration,
		    0, name, myExpireShowTerrainEffect);
}

static bool tickHandler(struct TickHandler *th)
{
	Tick++;
	wqRunToTick(&TickWorkQueue, Tick);
	return Quit;
}

static bool quitHandler(struct QuitHandler *kh)
{
	cmdQuit();
	return Quit;
}

#if 0
static void myGenerateRandomEncounter(void)
{
	int x, y, dir;
	class Party *npc;

	npc = place_random_encounter(Place);

	if (!npc)
		return;

	// Roll to pick a direction
	dir = random() % 4;

	// Convert to map coordinates
	switch (dir) {
	case 0:
		x = player_party->getX();
		y = player_party->getY() - MAP_TILE_H / 2;
		break;
	case 1:
		x = player_party->getX() + MAP_TILE_W / 2;
		y = player_party->getY();
		break;
	case 2:
		x = player_party->getX();
		y = player_party->getY() + MAP_TILE_H / 2;
		break;
	case 3:
		x = player_party->getX() - MAP_TILE_W / 2;
		y = player_party->getY();
		break;
	default:
		assert(0);
		break;
	}

	// Wrap
	x = place_wrap_x(Place, x);
	y = place_wrap_y(Place, y);

	// Check for passability
	if (!place_is_passable(Place, x, y, npc->getPmask(), 0)) {
		delete npc;
		return;
	}
	// Generate an NPC party
	npc->relocate(Place, x, y);
	// place_add_object(Place, npc);
}
#endif

static void play_print_end_of_game_prompt()
{
	consolePrint("\n\n*** YOU HAVE DIED ***\n\n");
	consolePrint("Press any key to exit.\n");
	getkey(NULL, anykey);
}

int G_exec_loops = 0;

static void play_loop(void)
{
        struct exec_context context;
        memset(&context, 0, sizeof(context));
        int times[8];

        Turn = 0;

        // --------------------------------------------------------------------
        // Enter the main game loop. Do not exit until the player is dead or
        // wants to quit.
        // --------------------------------------------------------------------

        while (true) {

                // ------------------------------------------------------------
                // "Execute" the current place. This will loop through all
                // objects in the current place and give them a chance to
                // run. This includes player-controlled objects.
                // ------------------------------------------------------------

                times[0] = SDL_GetTicks();
                place_exec(Place, &context);

                if (Session->reloaded)
                        /* Safe now. */
                        Session->reloaded = 0;

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

                /* Safely decrement the special global effect counters: */
                dec_reveal(place_get_scale(Place));
                dec_time_stop(place_get_scale(Place));
                dec_quicken(place_get_scale(Place));
                dec_magic_negated(place_get_scale(Place));
                dec_xray(place_get_scale(Place));

                if (! TimeStop) {
                        clock_advance(place_get_scale(Place));
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
                        Turn += place_get_scale(Place);
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
	mapUpdate(REPAINT_IF_DIRTY);
}

int playRun(void)
{
	struct QuitHandler qh;
	struct TickHandler th;

        // -------------------------------------------------------------------
        // Load a new session from a saved file.
        // -------------------------------------------------------------------

	session_load(SAVEFILE);
        if (! Session) {
		err("Error loading game from '%s'", SAVEFILE);
		return -1;
	}

        mapUpdate(REPAINT_IF_DIRTY);

	consolePrint("Welcome to Nazghul version %s\n", version_as_string() );

	// Setup all the event handlers.
	qh.fx = quitHandler;
	th.fx = tickHandler;
	eventPushQuitHandler(&qh);
	eventPushTickHandler(&th);
	eventAddHook(updateAfterEvent);

	Quit = false;

	// Enter the main event loop. This won't exit until the player quits or
	// dies.
        play_loop();

	// Cleanup the event handlers.
	eventPopTickHandler();
	eventPopQuitHandler();
	//eventPopKeyHandler();

	return 1;
}
