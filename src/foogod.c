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
#include "stdarg.h"
#include "screen.h"
#include "common.h"
#include "player.h"
#include "wq.h"
#include "combat.h"
#include "session.h"
#include "vehicle.h"
#include "sprite.h"
#include "images.h"
#include "effect.h"
#include "cfg.h"

#include <SDL_image.h>

#define FOOGOD_CHARS_PER_LINE STAT_CHARS_PER_LINE
#define FOOGOD_MAX_TITLE_LEN (FOOGOD_CHARS_PER_LINE-2)

struct foogod {
	SDL_Rect screenRect;
	SDL_Rect turnRect;
	SDL_Rect foodRect;
	SDL_Rect combatRect;
	SDL_Rect goldRect;
	SDL_Rect hullRect;
        SDL_Rect effectsRect;
	SDL_Rect titleRect;
        foogod_mode_t mode;
        int hintLen;
        char *hintText;
        unsigned int progress_bar_max_steps;
        unsigned int progress_bar_steps;
        char *progress_bar_title;
        struct images *image;
        struct sprite *progress_bar_sprites[3];
	char title[FOOGOD_MAX_TITLE_LEN+1];
} Foogod;

static void foogod_repaint_title(void)
{
    screenErase(&Foogod.titleRect);
    screenPrint(&Foogod.titleRect, SP_CENTERED | SP_ONBORDER, "%s", Foogod.title);
    screenUpdate(&Foogod.titleRect);
}

static int foogod_load_progress_bar_sprites(void)
{
        int i;
        char *fname = cfg_get("progress-bar-image-filename");

        if (!fname) {
                return -1;
        }

	Foogod.image = (struct images *)calloc(1, sizeof(*Foogod.image));
        assert(Foogod.image);

	Foogod.image = images_new(0, 8, 16, 1, 3, 0, 0, fname);
	if (!Foogod.image) {
	    err("images_new() failed for file '%s': '%s'\n", fname, SDL_GetError() );
	    goto fail;
	}

        for (i = 0; i < 3; i++) {
                if (!(Foogod.progress_bar_sprites[i] = sprite_new(0, 1, i, 0, 0, Foogod.image))) {
                        err("sprite_new() failed\n");
                        goto fail;
                }
        }

        return 0;

 fail:
        for (i = 0; i < 3; i++) {
                if ((Foogod.progress_bar_sprites[i])) {
                        sprite_del(Foogod.progress_bar_sprites[i]);
                        Foogod.progress_bar_sprites[i] = 0;
                }
        }

        if (Foogod.image) {
                free(Foogod.image);
                Foogod.image = 0;
        }

        return -1;
}

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

        // title (on the border)
        Foogod.titleRect.x = FOOGOD_X;
        Foogod.titleRect.y = foogod_get_y() - BORDER_H;
        Foogod.titleRect.w = FOOGOD_W;
        Foogod.titleRect.h = BORDER_H;

        return foogod_load_progress_bar_sprites();
}

static void foogodPaintEffect(SDL_Rect *rect, struct sprite *sprite)
{
        sprite_paint(sprite, 0, rect->x, rect->y);
        rect->x += ASCII_W;
}

static int foogod_paint_effect_wrapper(struct hook_entry *entry, void *data)
{
	SDL_Rect *rect = (SDL_Rect*)data;
	struct effect *effect = entry->effect;
        if (effect->sprite) {
                foogodPaintEffect(rect, effect->sprite);
        }
        return 0;
}

static void foogodPaintEffects()
{
        SDL_Rect rect = Foogod.effectsRect;

        /* Effects well-known to the engine */
        if (TimeStop) {
                foogodPaintEffect(&rect, time_stop_effect_sprite());
        }

        if (Reveal) {
                foogodPaintEffect(&rect, reveal_effect_sprite());
        }

        if (Quicken) {
                foogodPaintEffect(&rect, quicken_effect_sprite());
        }

        if (MagicNegated) {
                foogodPaintEffect(&rect, magic_negated_effect_sprite());
        }

        if (XrayVision) {
                foogodPaintEffect(&rect, xray_vision_effect_sprite());
        }

        /* Custom effects added by the game */
	for (int i = 0; i < OBJ_NUM_HOOKS; i++) {
                player_party->hookForEach(i, foogod_paint_effect_wrapper, 0);
	}


}

static void foogodPaintSessionInfo()
{
        screenPrint(&Foogod.turnRect, 0, "Turn: %d", session_get_turn_count());
        foogodPaintEffects();

        if (player_party) {
                screenPrint(&Foogod.foodRect, 0, "Food: %d", 
                            player_party->food);
                screenPrint(&Foogod.goldRect, SP_RIGHTJUSTIFIED, "Gold: %d",  
                            player_party->gold);
                if (player_party->getVehicle()) {
                        screenPrint(&Foogod.hullRect, 0, "Hull: %d", 
                                    player_party->getVehicle()->getHp());
                }
        }
}

static void foogod_progress_bar_paint()
{
        int i;
        int ticks;
        int max_ticks;
        SDL_Rect rect = Foogod.screenRect;

        /* title */
        rect.h = ASCII_H;
        screenPrint(&rect, SP_CENTERED, Foogod.progress_bar_title);
        
        /* bar */
        rect.y += ASCII_H;
        rect.w = ASCII_W;

        /* (ticks : maxTicks) = (steps : totalSteps) */
        max_ticks = (Foogod.screenRect.w / ASCII_W);

        /* Subtract two for the edges pieces of the progress bar. */
        max_ticks -= 2;

        ticks = (Foogod.progress_bar_steps * max_ticks) 
                / Foogod.progress_bar_max_steps;

        /* Paint the left edge. */
        sprite_paint(Foogod.progress_bar_sprites[0], 0, rect.x, rect.y);
        rect.x += ASCII_W;

        /* Paint the center. */
        for (i = 0; i < ticks; i++) {
                sprite_paint(Foogod.progress_bar_sprites[1], 0, rect.x, rect.y);
                //screenPrint(&rect, 0, ".");
                rect.x += ASCII_W;
        }

        /* Paint the right edge. */
        sprite_paint(Foogod.progress_bar_sprites[2], 0, rect.x, rect.y);
}

void foogodRepaint(void)
{
	screenErase(&Foogod.screenRect);

        switch (Foogod.mode) {

        default:
        case FOOGOD_DEFAULT:
                if (Session) {
                        foogodPaintSessionInfo();
                }
                screenPrint(&Foogod.combatRect, SP_RIGHTJUSTIFIED, 
                            "Combat: %c", combatGetState());
                break;

        case FOOGOD_HINT:
                screenPrint(&Foogod.screenRect, 0, Foogod.hintText);
                break;

        case FOOGOD_PROGRESS_BAR:
                foogod_progress_bar_paint();
                break;
        }

        foogod_repaint_title();
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
        Foogod.titleRect.y   = y - BORDER_H;
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

void foogodSetHintText(const char *text)
{
        strncpy(Foogod.hintText, text, Foogod.hintLen);
}

void foogod_progress_bar_set_title(const char *title)
{
        if (Foogod.progress_bar_title) {
                free(Foogod.progress_bar_title);
                Foogod.progress_bar_title = 0;
        }

        if (title) {
                Foogod.progress_bar_title = strdup(title);
        }
}

void foogod_progress_bar_set_max_steps(unsigned int val)
{
        Foogod.progress_bar_max_steps = val;
        Foogod.progress_bar_steps = 0;
}

void foogod_progress_bar_advance(unsigned int steps)
{
        Foogod.progress_bar_steps += steps;
        if (Foogod.progress_bar_steps > Foogod.progress_bar_max_steps) {
                Foogod.progress_bar_steps = Foogod.progress_bar_max_steps;
        }
}

void foogod_progress_bar_finish()
{
        dbg("Foogod.progress_bar_steps=%d\n", Foogod.progress_bar_steps);
        Foogod.progress_bar_steps = Foogod.progress_bar_max_steps;
}

void foogod_set_title(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vsnprintf(Foogod.title, FOOGOD_MAX_TITLE_LEN, fmt, args);
    va_end(args);
}

