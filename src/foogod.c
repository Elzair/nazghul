/* Copyright (c) 2002 Gordon McNutt */
#include "foogod.h"
#include "screen.h"
#include "common.h"
#include "player.h"
#include "wq.h"
#include "combat.h"
#include "vehicle.h"

struct {
	SDL_Rect screenRect;
	SDL_Rect turnRect;
	SDL_Rect foodRect;
	SDL_Rect combatRect;
	SDL_Rect goldRect;
	SDL_Rect hullRect;
} Foogod;

void foogodAdvanceTurns(void)
{
	foogodRepaint();
}

void foogodInit(void)
{
	Foogod.screenRect.x = FOOGOD_X;
	Foogod.screenRect.y = FOOGOD_Y;
	Foogod.screenRect.w = FOOGOD_W;
	Foogod.screenRect.h = FOOGOD_H;

	// turns
	Foogod.turnRect.x = FOOGOD_X;
	Foogod.turnRect.y = FOOGOD_Y;
	Foogod.turnRect.w = FOOGOD_W / 3;
	Foogod.turnRect.h = ASCII_H;

	// food
	Foogod.foodRect.x = FOOGOD_X;
	Foogod.foodRect.y = FOOGOD_Y + ASCII_H;
	Foogod.foodRect.w = FOOGOD_W / 3;
	Foogod.foodRect.h = ASCII_H;

	// hull
	Foogod.hullRect.x = FOOGOD_X + FOOGOD_W / 3;
	Foogod.hullRect.y = FOOGOD_Y;
	Foogod.hullRect.w = FOOGOD_W / 3;
	Foogod.hullRect.h = ASCII_H;

	// gold
	Foogod.goldRect.w = FOOGOD_W / 3;
	Foogod.goldRect.x = FOOGOD_X + FOOGOD_W - FOOGOD_W / 3;
	Foogod.goldRect.y = FOOGOD_Y;
	Foogod.goldRect.h = ASCII_H;

	// mode
	Foogod.combatRect.w = FOOGOD_W / 3;
	Foogod.combatRect.x = FOOGOD_X + FOOGOD_W - FOOGOD_W / 3;
	Foogod.combatRect.y = FOOGOD_Y + ASCII_H;
	Foogod.combatRect.h = ASCII_H;
}

void foogodRepaint(void)
{
	screenErase(&Foogod.screenRect);
	screenPrint(&Foogod.turnRect, 0, "Turn: %d", Turn);
	screenPrint(&Foogod.foodRect, 0, "Food: %d", player_party->food);
	screenPrint(&Foogod.goldRect, SP_RIGHTJUSTIFIED, "Gold: %d",
		    player_party->gold);
	screenPrint(&Foogod.combatRect, SP_RIGHTJUSTIFIED, "Combat: %c",
		    combatGetState());

	if (player_party->vehicle) {
		screenPrint(&Foogod.hullRect, 0, "Hull: %d",
			    player_party->vehicle->hp);
	}

	screenUpdate(&Foogod.screenRect);
}

void foogod_set_y(int y)
{
	Foogod.screenRect.y = y;
	Foogod.turnRect.y = y;
	Foogod.foodRect.y = y + ASCII_H;
	Foogod.goldRect.y = y;
	Foogod.combatRect.y = y + ASCII_H;
}

int foogod_get_y(void)
{
	return Foogod.screenRect.y;
}
