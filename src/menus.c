/* $Id$
 *
 * Copyright (C) 2006 Gordon McNutt
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
 */

#include "menus.h"

#include "cfg.h"
#include "cmd.h"
#include "cmdwin.h"
#include "console.h"
#include "event.h"
#include "log.h"
#include "node.h"
#include "status.h"
#include "file.h"

#include <assert.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>

static struct node menu_saved_games;

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

void menu_add_saved_game(char *fname)
{
        struct node *node = node_new(strdup(fname));
        node_add(&menu_saved_games, node);
}

static void menu_cleanup_saved_game_list()
{
        struct node *node = menu_saved_games.next;
        while (node != &menu_saved_games) {
                struct node *tmp = node;
                node = node->next;
                node_remove(tmp);
                free(tmp->ptr);
                node_unref(tmp);                
        }
}

/* load_game_menu -- show the player the available saved games, return the full
 * pathname. */
char * load_game_menu(void)
{
        char **menu = 0;
        int n = 0;
        int i = 0;
        struct node *nodep = 0;
        struct KeyHandler kh;
	struct ScrollerContext data;
        static char *selection = 0;
        enum StatusMode omode = statusGetMode();

        /* Erase any previous selection. */
        if (selection) {
                free(selection);
                selection = 0;
        }

        file_load_from_save_dir(cfg_get("save-game-filename"));
        n = node_list_len(&menu_saved_games);
        menu = (char**)malloc(sizeof(menu[0]) * n);
        assert(menu);
        node_for_each(&menu_saved_games, nodep) {
                menu[i] = (char*)nodep->ptr;
                i++;
        }
        
        statusSetStringList(n, menu);
        statusSetMode(StringList);

        data.selection = NULL;
        data.selector  = String;
        kh.fx   = scroller;
        kh.data = &data;
	eventPushKeyHandler(&kh);
	eventHandle();
	eventPopKeyHandler();

        /* If the player selected something then build the full pathname for
         * return. */
        if (data.selection) {
                selection = file_mkpath(cfg_get("saved-games-dirname"),
                                        (char*)data.selection);
                assert(selection);
        }

        statusSetMode(omode);

        menu_cleanup_saved_game_list();
        free(menu);

        return selection;
}

static char *prompt_for_fname()
{
        char buf[32];

        log_msg("Enter the new filename.");
        cmdwin_clear();
        cmdwin_print("Filename-");

        if (ui_getline_plain(buf, sizeof(buf))) {
                return strdup(buf);
        }
        return 0;
}

static int menu_rewrite_saves(char **menu, int n)
{
        FILE *file = 0;
        int i;
        char *fname = cfg_get("save-game-filename");

        file = file_open_in_save_dir(fname, "w");
        if (! file) {
                warn("Problem updating %s: %s\n", fname, file_get_error());
                return -1;
        }
        
        for (i = 0; i < n; i++) {
                fprintf(file, "(kern-add-save-game \"%s\")\n", menu[i]);
        }

        fclose(file);
        return 0;
}

char * save_game_menu(void)
{
        static char *NEW_SAVED_GAME = "New Saved Game";
        char **menu = 0;
        int n = 0;
        int i = 0;
        struct node *nodep = 0;
        struct KeyHandler kh;
	struct ScrollerContext data;
        static char *selection = 0;
        enum StatusMode omode = statusGetMode();

        /* erase previous selection */
        if (selection) {
                free(selection);
                selection = 0;
        }

        if (file_exists_in_save_dir(cfg_get("save-game-filename"))) {
                file_load_from_save_dir(cfg_get("save-game-filename"));
        }
        n = node_list_len(&menu_saved_games) + 1;
        menu = (char**)malloc(sizeof(menu[0]) * n);
        assert(menu);

        /* first entry is always N)ew Save */
        menu[i++] = NEW_SAVED_GAME;

        node_for_each(&menu_saved_games, nodep) {
                menu[i++] = (char*)nodep->ptr;
        }
        
        statusSetStringList(n, menu);
        statusSetMode(StringList);

reselect:
        data.selection = NULL;
        data.selector  = String;
        kh.fx   = scroller;
        kh.data = &data;
	eventPushKeyHandler(&kh);
	eventHandle();
	eventPopKeyHandler();

        if (data.selection) {
                if (!strcmp((char*)data.selection, NEW_SAVED_GAME)) {
                        int need_rewrite = 1;
                        selection = prompt_for_fname();
                        if (!selection)
                                goto reselect;

                        /* Check if the player re-typed an existing filename */
                        for (i = 1; i < n; i++) {
                                if (!strcmp(selection, menu[i])) {
                                        if (!confirm_selection()) {
                                                free(selection);
                                                selection = 0;
                                                goto reselect;
                                        }
                                        need_rewrite = 0;
                                        break;
                                }
                        }
                        menu[0] = selection;
                        menu_rewrite_saves(menu, i);

                } else if (confirm_selection()) {
                        selection = strdup((char*)data.selection);
                        assert(selection);
                } else {
                        goto reselect;
                }
        }

        /* Restore the original status mode before deleting the list. */
        statusSetMode(omode);

        menu_cleanup_saved_game_list();
        free(menu);

        return selection;
}

char * main_menu(void)
{
        static char *START_NEW_GAME="Start New Game";
        static char *JOURNEY_ONWARD="Journey Onward";
        static char *CREDITS="Credits";
        static char *QUIT="Quit";
        static char *TUTORIAL="Tutorial";
        char *menu[5];
        int n_items = 0;
        struct KeyHandler kh;
	struct ScrollerContext data;
        char *selection = NULL;
	struct QuitHandler qh;
        static char *new_game_fname =
                file_mkpath(cfg_get("include-dirname"),
                            cfg_get("new-game-filename"));
        static char *tutorial_fname =  
                file_mkpath(cfg_get("include-dirname"),
                            cfg_get("tutorial-filename"));
        char *load_fname = 0;
        char *save_game_fname = cfg_get("save-game-filename");

        /* setup main menu quit handler so player can click close window to
         * exit */
	qh.fx = main_menu_quit_handler;
	eventPushQuitHandler(&qh);


 start_main_menu:
        n_items = 0;

        /* check for a previously saved game to Journey Onward */
        if (file_exists_in_save_dir(save_game_fname)) {
                menu[n_items] = JOURNEY_ONWARD;
                n_items++;
        }

        /* check for the default script for Start New Game */
        if (file_exists(new_game_fname)) {
                menu[n_items] = START_NEW_GAME;
                n_items++;
        }

        /* check for a tutorial script for Tutorial */
        if (file_exists(tutorial_fname)) {
                menu[n_items] = TUTORIAL;
                n_items++;
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
                load_fname = new_game_fname;
                assert(load_fname);
        }
        else if (! strcmp(selection, JOURNEY_ONWARD)) {
                load_fname = load_game_menu();
                if (!load_fname)
                        goto start_main_menu;
        }
        else if (! strcmp(selection, CREDITS)) {
                show_credits();
                goto start_main_menu;
        }
        else if (! strcmp(selection, TUTORIAL)) {
                load_fname = tutorial_fname;
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
        
        return load_fname;
}

int menu_init(void)
{
        node_init(&menu_saved_games);
        return 0;
}
