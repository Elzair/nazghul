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
#include "status.h"  // for status_get_h()
#include "screen.h"
#include "common.h"
#include "player.h"
#include "wq.h"
#include "combat.h"
#include "session.h"
#include "vehicle.h"

struct foogod {
	SDL_Rect screenRect;
	SDL_Rect turnRect;
	SDL_Rect foodRect;
	SDL_Rect combatRect;
	SDL_Rect goldRect;
	SDL_Rect hullRect;
        SDL_Rect effectsRect;
        foogod_mode_t mode;
        int hintLen;
        char *hintText;
} Foogod;

void foogodAdvanceTurns(void)
{
	foogodRepaint();
}

int foogodInit(void)
{
        memset(&Foogod, 0, sizeof(Foogod));

        Foogod.mode = FOOGOD_DEFAULT;

        // hint text buffer
        Foogod.hintLen = FOOGOD_W / ASCII_W;
        Foogod.hintText = (char*)malloc(Foogod.hintLen+1);
        memset(Foogod.hintText, 0, Foogod.hintLen);

	Foogod.screenRect.x = FOOGOD_X;
	Foogod.screenRect.w = FOOGOD_W;
	// Foogod.screenRect.y = STAT_Y + Status.screenRect.h + BORDER_H;
        foogod_set_y(STAT_Y + status_get_h() + BORDER_H);
        Foogod.screenRect.h = FOOGOD_H;
    
	// turns
	Foogod.turnRect.x = FOOGOD_X;
	Foogod.turnRect.y = foogod_get_y();
	Foogod.turnRect.w = FOOGOD_W / 3;
	Foogod.turnRect.h = ASCII_H;

	// food
	Foogod.foodRect.x = FOOGOD_X;
	Foogod.foodRect.y = foogod_get_y() + ASCII_H;
	Foogod.foodRect.w = FOOGOD_W / 3;
	Foogod.foodRect.h = ASCII_H;

	// hull
	Foogod.hullRect.x = FOOGOD_X + FOOGOD_W / 3;
	Foogod.hullRect.y = foogod_get_y();
	Foogod.hullRect.w = FOOGOD_W / 3;
	Foogod.hullRect.h = ASCII_H;

        // effects
	Foogod.effectsRect.x = FOOGOD_X + FOOGOD_W / 3;
	Foogod.effectsRect.y = foogod_get_y() + ASCII_H;
	Foogod.effectsRect.w = FOOGOD_W / 3;
	Foogod.effectsRect.h = ASCII_H;

	// gold
	Foogod.goldRect.w = FOOGOD_W / 3;
	Foogod.goldRect.x = FOOGOD_X + FOOGOD_W - FOOGOD_W / 3;
	Foogod.goldRect.y = foogod_get_y();
	Foogod.goldRect.h = ASCII_H;

	// mode
	Foogod.combatRect.w = FOOGOD_W / 3;
	Foogod.combatRect.x = FOOGOD_X + FOOGOD_W - FOOGOD_W / 3;
	Foogod.combatRect.y = foogod_get_y() + ASCII_H;
	Foogod.combatRect.h = ASCII_H;

        return 0;
}

void foogodRepaint(void)
{
	screenErase(&Foogod.screenRect);

        if (FOOGOD_DEFAULT == Foogod.mode) {
                if (Session) {
                        screenPrint(&Foogod.turnRect, 0, "Turn: %d", 
                                    session_get_turn_count());
                        screenPrint(&Foogod.effectsRect, 0, "Eff: %s%s%s%s"
                                    , (TimeStop     ? "T" : "")
                                    , (Quicken      ? "Q" : "")
                                    , (MagicNegated ? "N" : "")
                                    , (Reveal       ? "R" : "")
                        );
                }
                if (player_party) {
                        screenPrint(&Foogod.foodRect, 0, "Food: %d", 
                                    player_party->food);
                        screenPrint(&Foogod.goldRect, 
                                    SP_RIGHTJUSTIFIED, "Gold: %d",  
                                    player_party->gold);
                        if (player_party->getVehicle()) {
                                screenPrint(&Foogod.hullRect, 0, "Hull: %d", 
                                            player_party->getVehicle()->
                                            getHp());
                        }
                }
                screenPrint(&Foogod.combatRect, SP_RIGHTJUSTIFIED, 
                            "Combat: %c", combatGetState());
        } else {
                screenPrint(&Foogod.screenRect, 0, Foogod.hintText);
        }

	screenUpdate(&Foogod.screenRect);
}

void foogod_set_y(int y)
{
	Foogod.screenRect.y  = y;
	Foogod.turnRect.y    = y;
	Foogod.foodRect.y    = y + ASCII_H;
	Foogod.goldRect.y    = y;
	Foogod.combatRect.y  = y + ASCII_H;
        Foogod.hullRect.y    = y;
        Foogod.effectsRect.y = y + ASCII_H;
}

int foogod_get_y(void)
{
	return Foogod.screenRect.y;
}

int foogod_get_h(void)
{
        return (FOOGOD_H + BORDER_H);
}

void foogodSetMode(foogod_mode_t mode)
{
        Foogod.mode = mode;
        foogodRepaint();
}

void foogodSetHintText(char *text)
{
        strncpy(Foogod.hintText, text, Foogod.hintLen);
}
