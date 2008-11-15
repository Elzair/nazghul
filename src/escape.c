/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2008 Gordon McNutt
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 *
 * Gordon McNutt
 * gmcnutt@users.sourceforge.net
 */

#include "cmd.h"
#include "event.h"
#include "log.h"
#include "session.h"

#include <SDL.h>

static struct KeyHandler esc_key_hndlr;

static void esc_help(void)
{
    log_begin("F)ollow mode\n");
    log_continue("Q)uit\n");
    log_continue("[1-9] Solo mode\n");
    log_continue("CTRL-R)eload\n");
    log_continue("CTRL-S)ave\n");
    log_end("ESC to continue game\n");
}

static int esc_menu_key_fx(struct KeyHandler *kh, int key, int keymod)
{
    switch (key) {
    case 'f':
        cmdToggleFollowMode();
        return 1;
    case 'q':
        cmdQuit();
        return 1;
    case SDLK_1:
    case SDLK_2:
    case SDLK_3:
    case SDLK_4:
    case SDLK_5:
    case SDLK_6:
    case SDLK_7:
    case SDLK_8:
    case SDLK_9:                        
        cmdSetSoloMode(key - SDLK_1);
        return 1;
    case KEY_CTRL_S:
        cmdSave();
        break;
    case KEY_CTRL_R:
        cmdReload();
        return 1;
    case SDLK_F10:
        cmdSettings();
        break;
    case SDLK_ESCAPE:
        log_msg("Continue");
        return 1;
    case '?':
        esc_help();
        break;
    default:
        //log_msg("Sorry, %d is not a valid command.");
        break;
    }
    return 0;
}

static int esc_key_fx(struct KeyHandler *esckh, int key, int keymod)
{
    struct KeyHandler kh;

    if (SDLK_ESCAPE != key) {
        return 0;
    }

    log_banner("ESC mode - press '?' for help");

    kh.fx = esc_menu_key_fx;
    eventPushKeyHandler(&kh);
    eventHandle();
    eventPopKeyHandler();
    return 0;
}

void escape_start_handler(void)
{
    esc_key_hndlr.fx = esc_key_fx;
    eventPushKeyHandler(&esc_key_hndlr);
}

void escape_stop_handler(void)
{
    eventPopKeyHandler();
}
