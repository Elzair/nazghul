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
#include "play.h"
#include "util.h"
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
#include "moongate.h"
#include "wq.h"
#include "foogod.h"
#include "combat.h"
#include "cursor.h"
#include "Arms.h"
#include "event.h"
#include "wind.h"
#include "Item.h"
#include "Container.h"
#include "Trap.h"
#include "conv.h"
#include "Mech.h"
#include "Spell.h"
#include "Mech.h"
#include "dup_constants.h"
#include "cmdwin.h"
#include "vehicle.h"
#include "portal.h"
#include "terrain.h"
#include "cmd.h"

#define DEBUG
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

struct play {
	char *savefile;
	struct game *game;
	char *los_name;
	enum cmdstate cmdstate;
};

struct LightEffectContext {
	char *name;
	int amount;
	class Character *owner;
};

struct play *Play;		/* hack */
bool Quit;

static void myExpireLightEffect(struct wq_job *job, struct list *wq)
{
	struct LightEffectContext *context;
	context = (struct LightEffectContext *) job->data;
	context->owner->changeLight(-context->amount);
	player_party->updateView();
	consolePrint("Light from %s expired\n", context->name);
	delete context;
	free(job);		// fixme -- use delete
}

void effectLight(char *name, int amount, int duration, class Character * target)
{
	struct LightEffectContext *context;
	context = new struct LightEffectContext;
	if (context) {
		context->name = name;
		context->amount = amount;	// careful...
		context->owner = target;
		target->changeLight(amount);
		if (Place->type != combat_place)
			// Right now combat is the only "small-scale"
			// place, where the Player structure's location
			// is invalid, and where everything relates to
			// the individual party members and not the
			// party as a whole.
			player_party->updateView();
		wqCreateJob(&TurnWorkQueue,
			    Turn + duration, 0, context, myExpireLightEffect);
	}
}

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
	wqCreateJob(&TurnWorkQueue, Turn + duration, 0, name, myExpireQuickenEffect);
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
	wqCreateJob(&TurnWorkQueue, Turn + duration, 0, name, myExpireTimeStopEffect);
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
	wqCreateJob(&TurnWorkQueue, Turn + duration, 0, name, myExpireNegateMagicEffect);
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
	class NpcParty *npc;

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

        // ---------------------------------------------------------------------
        // Enter the main game loop. Do not exit until the player is dead or
        // wants to quit.
        // ---------------------------------------------------------------------

        while (true) {


                // -------------------------------------------------------------
                // "Execute" the current place. This will loop through all
                // objects in the current place and give them a chance to
                // run. This includes player-controlled objects.
                // -------------------------------------------------------------

                times[0] = SDL_GetTicks();
                place_exec(Place, &context);
                

                // -------------------------------------------------------------
                // Check for changes in the combat state as a result of the
                // last turn. This check might force a transition from combat
                // to wilderness mode.
                // -------------------------------------------------------------
                
                times[1] = SDL_GetTicks();
                if (combat_get_state() != COMBAT_STATE_DONE) {
                        combat_analyze_results_of_last_turn();
                }

                // -------------------------------------------------------------
                // Do a non-blocking check to see if any non-user events need
                // to run (e.g., animation ticks). Normally these events get
                // run in place_exec() when the game waits for player input,
                // but if the player party is resting (for example) then that
                // does not happen. To keep animation looking smooth I added
                // this next call.
                // -------------------------------------------------------------

                times[2] = SDL_GetTicks();
                eventHandlePending();

                // -------------------------------------------------------------
                // Check for end-of-game conditions.
                // -------------------------------------------------------------

                times[3] = SDL_GetTicks();
                if (player_party->allDead()) {
                        play_print_end_of_game_prompt();
                        break;
                }

                if (Quit)
                        break;

                // -------------------------------------------------------------
                // Run all the non-object stuff in the game world.
                //
                // NOTE: you should keep the clock update first since most
                //       things take their cue from the current time.
                // -------------------------------------------------------------

                times[4] = SDL_GetTicks();
                if (! TimeStop)
                        clock_advance(place_get_scale(Place));
                foogodAdvanceTurns();
                sky_advance();
                windAdvanceTurns();
                wqRunToTick(&TurnWorkQueue, Turn);

                // -------------------------------------------------------------
                // Update the "Turn" counter. This drives the TurnWorkQueue,
                // which is still used for scheduled things like torches
                // burning out and doors closing. 
                // -------------------------------------------------------------

                //Turn += place_get_scale(player_party->getPlace());
                Turn += place_get_scale(Place);

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

static int play_init(struct play *play)
{

	Cursor = NULL;

	/* Load the game */
	if (loadGame(play->savefile) < 0) {
		err("Error loading game from '%s'", play->savefile);
		return -1;
	}

	if (Cursor == NULL) {
		err("No CROSSHAIR defined in savefile '%s'", play->savefile);
		return -1;
	}

	statusInit();		/* before painting the frame for the first time
				 */
	foogodInit();		/* before painting the frame for the first time
				 */

	statusSetMode(ShowParty);	/* note: must do this after painting
					 * the frame because the title is
					 * always painted OVER the frame. */

	mapSetLosStyle(play->los_name);	// must be before placeEnter()
	//mapSetPlace(Place);	        // must be before placeEnter()
	//consoleInit();	                // must be before placeEnter()
        combatInit();
	//place_enter(Place);

	moongateSetAnimationWorkQueue(&TickWorkQueue);
	skyInit();

	screen_repaint_frame(); // after skyInit (otherwise one of the
                                // astronomical sprites might get painted over
                                // the frame on the right side of the sky
                                // window)

	windSetDirection(NORTH, 1);


	foogodRepaint();

	/* Clear the message window */
	consoleRepaint();

	/* Show the status window */
	statusRepaint();

	spriteStartAnimation(&TickWorkQueue, Tick + 1);

	play->cmdstate = CMD_IDLE;


        // ---------------------------------------------------------------------
        // Hack: force a party relocation. This will ensure that if the party
        // starts out in a town or dungeon then it will get broken out into
        // character mode.
        // ---------------------------------------------------------------------

        //Place = NULL;
        player_party->relocate(Place,
                               player_party->getX(), 
                               player_party->getY());


        mapUpdate(REPAINT_IF_DIRTY);

	return 0;
}

void play_destroy(struct play *play)
{

	if (LosEngine)
		los_destroy(LosEngine);
	free(play);
}
struct play *play_create(void)
{
	struct play *play;

	CREATE(play, struct play, 0);

	play->savefile = SAVEFILE;
	play->los_name = LOS;

	Play = play;		/* hack */
	return play;
}

static void updateAfterEvent(void)
{
	// mapRepaintView(0, REPAINT_ACTIVE|REPAINT_IF_DIRTY);
	mapUpdate(REPAINT_IF_DIRTY);
}

int playRun(void)
{
	struct QuitHandler qh;
	struct TickHandler th;

	struct play *play;

	play = play_create();

	if (play_init(play) < 0)
		return -1;

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
