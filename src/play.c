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

struct play *Play;		/* hack */

bool Quit;

struct LightEffectContext {
	char *name;
	int amount;
	class Character *owner;
};

static void myExpireLightEffect(struct wq_job *job, struct list *wq)
{
	struct LightEffectContext *context;
	context = (struct LightEffectContext *) job->data;
	context->owner->changeLight(-context->amount);
	player_party->recompute_los();
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
			player_party->recompute_los();
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
	free(job);
}

void effectQuicken(char *name, int duration)
{
	Quicken = 2;		// Setting it to a multiple of two will make
	// sure that the
	// player sees the effects right away
	wqCreateJob(&TurnWorkQueue, Turn + duration,
		    0, name, myExpireQuickenEffect);
}

static void myExpireNegateMagicEffect(struct wq_job *job, struct list *wq)
{
	char *cause = (char *) job->data;
	MagicNegated--;
	consolePrint("%s expired\n", cause);
	free(job);
}

void effectNegateMagic(char *name, int duration)
{
	MagicNegated++;
	wqCreateJob(&TurnWorkQueue, Turn + duration,
		    0, name, myExpireNegateMagicEffect);
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

void dead(void)
{
	consolePrint("\n*** YOU HAVE DIED ***\n\n");
	consolePrint("Press any key to exit.\n");
	getkey(NULL, anykey);
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

static bool keyHandler(struct KeyHandler *kh, int key, int keymod)
{
        // This handler is always on the bottom of the key handler stack. When
        // it returns true the event loop exits, effectively ending the
        // game. So return true iff the player is dead or wants to quit.

        // Decode and run player commands. Any commands which require further
        // input from the player will push new key handlers on the stack and
        // pop them upon return. I like to think that they are "closed" in some
        // sense of the word, and don't intermingle their command input state
        // with ours.

        int saved_turn = Turn;
        int turns_used = 1;	// default used by most actions

        cmdwin_clear();
        cmdwin_repaint();

        if (keymod == KMOD_LCTRL || keymod == KMOD_RCTRL) {

                turns_used = 0;

                // SAM: This seemed like a less ugly way of setting off a group
                // of keybindings for "DM Mode" use or the like.  If we find
                // something more aesthetic wrt/ switch() syntax, we will
                // surely prefer it...
                // 
                // Control-key bindings for "DM Mode" commands like terrain
                // editing.  In future, these may be enabled/disabled at
                // compile time, or via a GhulScript keyword in the mapfile.
                switch (key) {
      
                case 't':
                        cmdTerraform(NULL);
                        break;

                case 's':
                        cmdSaveTerrainMap(NULL);
                        break;

                case 'z':
                        mapTogglePeering();
                        break;

                default:
                        break;
                } // switch(key)
        } // keymod

        else {
                // !keymod
                switch (key) {

                case KEY_NORTH:
                case KEY_EAST:
                case KEY_SOUTH:
                case KEY_WEST:
                {
                        int dir = keyToDirection(key);
                        player_party->move(directionToDx(dir),
                                           directionToDy(dir), false);
                        turns_used = 0;	// turns already advanced in
                        // player_party->move
                        mapSetDirty();
                }
                break;

                case 'a':
                        cmdAttack();
                        break;
                case 'b':
                        player_party->board_vehicle();
                        break;
                case 'c':
                        cmdCastSpell(NULL);
                        break;
                case 'e':
                        // SAM:
                        // Perhaps this command should be merged with '>' ?
                        player_party->enter_portal();
                        break;
                case 'f':
                        cmdFire();
                        break;
                case 'g':
                        cmdGet(player_party->getX(), player_party->getY(), 
                               true);
                        mapSetDirty();
                        break;
                case 'h':
                        // SAM: Adding (H)andle command...
                        cmdHandle(NULL);
                        break;
                case 'k':
                        turns_used = cmdCamp();
                        break;
                case 'm':
                        cmdMixReagents();
                        break;
                case 'n':
                        cmdNewOrder();
                        break;
                case 'o':
                        cmdOpen(NULL);
                        break;
                case 'q':
                        cmdQuit();
                        break;
                case 'r':
                        cmdReady(NULL, CMD_SELECT_MEMBER|CMD_PRINT_MEMBER);
                        break;
                case 's':
                        cmdSearch(player_party->getX(), player_party->getY());
                        break;
                case 't':
                        cmdTalk(player_party->getX(), player_party->getY());
                        break;
                case 'u':
                        cmdUse(NULL, CMD_SELECT_MEMBER|CMD_PRINT_MEMBER);
                        break;
                case 'x':
                        cmdXamine(NULL);
                        turns_used = 0;
                        break;
                case 'z':
                        cmdZtats(NULL);
                        turns_used = 0;
                        break;
                case '@':
                        // SAM: 'AT' command for party-centric information
                        cmdAT(NULL);
                        turns_used = 0;
                        break;
                case ' ':
                        consolePrint("Pass\n");
                        turns_used = Place->scale;
                        break;
                case '>':
                        // This key was chosen to be a cognate for '>' in
                        // NetHack and other roguelike games.
                        cmdZoomIn();
                        break;
                default:
                        turns_used = 0;
                        break;
                } // switch(key)
        } // !keymod

        //cmdwin_flush_to_console();

	// Quit now before advancing turns or anything of that sort. It makes
	// life easier.
        if (Quit)
                return Quit;

        int loops = 0;


        // Loop at least once, and more if the party is immobilized (currently
        // true if all party members are sleeping, but paralysis and other
        // effects will eventually apply)
        do {

                if (loops) {
                        statusRepaint();
                        mapBlackout(1);
                        mapUpdate(0);
                        saved_turn = Turn;
                        if (player_party->resting()) {
                                turns_used = TURNS_PER_HOUR;
                        } else {
                                turns_used = 1;
                        }
                        SDL_Delay(500);
                }

                loops++;

                turnAdvance(turns_used);

                if (Turn != saved_turn) {

                        // Note: always update the clock before the turn
                        // wq. For example, when entering a place all the NPC
                        // parties use the wall clock time to synchronize their
                        // schedules, so it needs to be set BEFORE calling
                        // them.
                        clockUpdate();

                        foogodAdvanceTurns();
                        placeAdvanceTurns();
                        player_party->advance_turns();
                        skyAdvanceTurns();
                        windAdvanceTurns();

                        // Most commands burn through at least one turn. Let
                        // the turn-based work queue catch up.
                        wqRunToTick(&TurnWorkQueue, Turn);
                }

                // The player may have died as a result of executing a command
                // or running the work queue.
                if (player_party->all_dead()) {
                        dead();
                        return true;
                }

        } while (player_party->immobilized());

        if (loops > 1) {
                mapBlackout(0);
                mapUpdate(0);
                statusRepaint();
        }

        if (!Quit)		// fixme: is this check necessary? 
                myGenerateRandomEncounter();

        return Quit;
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

	mapInit(play->los_name);	// must be before placeEnter()
	mapSetPlace(Place);	        // must be before placeEnter()
	consoleInit();                  // must be before placeEnter()
	placeEnter();

	moongateSetAnimationWorkQueue(&TickWorkQueue);
	skyInit();

	screen_repaint_frame(); // after skyInit (otherwise one of the
                                // astronomical sprites might get painted over
                                // the frame on the right side of the sky
                                // window)

	windSetDirection(NORTH, 1);

	player_party->view = mapCreateView();
	if (!player_party->view)
		return -1;
	mapAddView(player_party->view);
	mapCenterView(player_party->view, player_party->getX(),
		      player_party->getY());
	player_party->recompute_los();
	mapCenterCamera(player_party->getX(), player_party->getY());
	mapUpdate(0);
	screenUpdate(0);

	foogodRepaint();

	/* Setup the astar pathfinding lib */
	if (astar_init())
		return -1;

	/* Clear the message window */
	consoleRepaint();

	/* Show the status window */
	statusRepaint();

	spriteStartAnimation(&TickWorkQueue, Tick + 1);

	play->cmdstate = CMD_IDLE;

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
	struct KeyHandler kh;
	struct QuitHandler qh;
	struct TickHandler th;

	struct play *play;

	play = play_create();

	if (play_init(play) < 0)
		return -1;

	consolePrint("Welcome to nazghul v0.1\n");

	// Setup all the event handlers.
	kh.fx = keyHandler;
	qh.fx = quitHandler;
	th.fx = tickHandler;
	eventPushKeyHandler(&kh);
	eventPushQuitHandler(&qh);
	eventPushTickHandler(&th);
	eventAddHook(updateAfterEvent);

	Quit = false;

	// Major hack warning: if the game loads up with the player in a
	// dungeon then we need to force the game into dungeon mode. The
	// easiest way to do that is to have the player party "enter" the
	// dungeon it's already in. The last two args are the direction vector
	// - just fake them to "north".
	if (place_is_dungeon(player_party->getPlace())) {
		if (!player_party->enter_dungeon(player_party->getPlace(),
						 player_party->getX(),
						 player_party->getY(), 0, 1)) {
			err("Bad starting position for party: %s [%d %d]\n",
			    player_party->getPlace()->name,
			    player_party->getX(), player_party->getY());
			return -1;
		}
	}

	// Enter the main event loop. This won't exit until the player quits or
	// dies.
	eventHandle();

	// Cleanup the event handlers.
	eventPopTickHandler();
	eventPopQuitHandler();
	eventPopKeyHandler();

	return 1;
}
