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
        cmdwin_spush("Confirm");
        cmdwin_spush("<y/n>");
        getkey(&yesno, yesnokey);
        cmdwin_pop();
        if (yesno=='y') {
                cmdwin_spush("yes!");
                log_msg("Ok!");
                return 1;
        } else {
                cmdwin_spush("no!");
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
        cmdwin_push("Filename: ");

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
        static char *SETTINGS = "Settings";
        char *menu[6];
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
        cmdwin_clear();

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

        menu[n_items] = SETTINGS;
        n_items++;

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
        else if (! strcmp(selection, SETTINGS)) {
                options_menu();
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

/*****************************************************************************
 * Options Menu
 */
#include "cmd.h" /* for yesnokey() */
#include "sound.h" 

#define OPTION_MAXNAMESTRLEN 16
#define OPTION_MAXVALSTRLEN  16
#define OPTION_MAXMENUSTRLEN (OPTION_MAXNAMESTRLEN + OPTION_MAXVALSTRLEN + 4)

enum {
        OPTION_SCREEN_DIMS = 0,
        OPTION_SOUND_ENABLE,
        OPTION_NUMOPTIONS /* keep last */
};

struct option {
        char name[OPTION_MAXNAMESTRLEN];
        char *comment;
        char *key;
        char *val;
        char *entry_val;
        char *startup_val;
        void (*handler)(struct option *);
        char restart : 1;
        char changed : 1;
};

static void option_screen_dims(struct option *opt);
static void option_sound(struct option *opt);

static struct option options[OPTION_NUMOPTIONS] = {
        { "Screen Size", 
          "Set the dimensions of the game screen.",
          "screen-dims", 0, 0, 0,
          option_screen_dims,
          0, 
          0
        },
        { "Sound", 
          "Turn sound on or off.",
          "sound-enabled", 0, 0, 0,
          option_sound,
          0,
          0
        }
};

/* option_screen_dims -- let player select desired screen size from a list */
static void option_screen_dims(struct option *opt)
{
#       define ADD_SCREEN_DIM(dim,mapw) (dim),
        char *menu[] = {
#               include "screen_dims.h"
        };
        struct KeyHandler kh;
	struct ScrollerContext data;
        
        log_msg("Choose your screen size");
        cmdwin_clear();
        cmdwin_spush("Screen size");
        cmdwin_spush("<select>");

        statusSetStringList(array_sz(menu), menu);
        statusSetMode(StringList);
        data.selection = NULL;
        data.selector  = String;
        kh.fx   = scroller;
        kh.data = &data;
        eventPushKeyHandler(&kh);
        eventHandle();
        eventPopKeyHandler();

        if (!data.selection) {
                return;
        }

        if (! strcmp((char*)data.selection, opt->startup_val)) {
                opt->restart = 0;
        } else {
                opt->restart = 1;
        }

        opt->val = (char*)data.selection;
}

/* option_sound -- prompt player to enable or disable sound */
static void option_sound(struct option *opt)
{
        if (!strcmp(opt->val, "yes")) {
                opt->val = "no";
                sound_off();
                opt->restart = 0;
        } else {
                opt->val = "yes";
                sound_on();
                if (strcmp(opt->startup_val, "yes")) {
                        opt->restart = 1;
                } else {
                        opt->restart = 0;
                }
        }
}

static int options_save(void)
{
        int i;
        char *fname = cfg_get("options-script-filename");
        FILE *file = file_open_in_save_dir(fname, "w");
        if (!file) {
                warn("Could not open '%s': %s", fname, strerror(errno));
                return -1;
        }

        fprintf(file, 
                ";; %s -- This file contains user-specified options that override the\n"
                ";; game defaults.\n"
                "(kern-cfg-set \n\n",
                fname);

        for (i = 0; i < OPTION_NUMOPTIONS; i++) {
                fprintf(file, ";; %s\n \"%s\" \"%s\"\n\n",
                        options[i].comment,
                        options[i].key,
                        options[i].val);
        }

        fprintf(file, ")\n");
        fclose(file);

        return 0;
}        

/* options_menu -- show the user-configurable options. Upon exit prompt to save
 * the current settings to the options file. */
void options_menu(void)
{
        int i;
        int yesno;
        int any_changed = 0;
        int any_restart = 0;
        char menu_strings[OPTION_NUMOPTIONS][OPTION_MAXMENUSTRLEN] = {};
        char *menu[OPTION_NUMOPTIONS];
        struct KeyHandler kh;
	struct ScrollerContext data;

        /* Get current values and build the menu list. */
        for (i = 0; i < OPTION_NUMOPTIONS; i++) {

                /* Make a copy of the original value on startup. */
                if (! options[i].startup_val) {
                        options[i].startup_val = strdup(cfg_get(options[i].key));
                }

                /* Make a copy of the values on entry. */
                options[i].entry_val = strdup(cfg_get(options[i].key));

                /* Clear the flags on entry */
                options[i].changed = 0;

                /* Init the current val. */
                options[i].val = cfg_get(options[i].key);

                /* Build the menu entry. */
                snprintf(menu_strings[i], OPTION_MAXMENUSTRLEN,
                         "%s [%s] %s",
                         options[i].name,
                         options[i].val,
                         options[i].restart ? "(restart)" : ""
                        );

                /* Add it to the menu list. */
                menu[i] = menu_strings[i];
        }

        /* Menu loop */
        for (;;) {

                /* Setup status browser (do this every time through the loop,
                 * as the handler functions might change things) */
                cmdwin_clear();
                cmdwin_spush("Settings");
                cmdwin_push("<select/ESC>");
                statusSetStringList(OPTION_NUMOPTIONS, (char**)menu);
                statusSetMode(StringList);
                data.selection = NULL;
                data.selector  = String;
                kh.fx   = scroller;
                kh.data = &data;
                eventPushKeyHandler(&kh);
                eventHandle();
                eventPopKeyHandler();

                /* Player ESC */
                if (! data.selection) {
                        break;
                }

                /* Handle the option */
                for (i = 0; i < OPTION_NUMOPTIONS; i++) {

                        if (strcmp((char*)data.selection, menu[i])) {
                                continue;
                        }
                        
                        /* Invoke the option handler. */
                        options[i].handler(&options[i]);

                        /* Update the menu string. */
                        options[i].changed = strcmp(options[i].entry_val, 
                                                    options[i].val);
                        snprintf(menu_strings[i], OPTION_MAXMENUSTRLEN,
                                 "%s [%s] %c %s",
                                 options[i].name,
                                 options[i].val,
                                 options[i].changed ? '*' : ' ',
                                 options[i].restart ? "(restart)" : ""
                                );

                        break;
                }
        }

        cmdwin_clear();

        /* If none of the options changed from their entry values then return
         * without bothering the player. */
        for (i = 0; i < OPTION_NUMOPTIONS; i++) {
                any_changed |= options[i].changed;
                any_restart |= options[i].restart;
        }
        if (! any_changed) {
                return;
        }

        /* Prompt to save */
        log_msg("Save settings?");
        cmdwin_spush("Save");
        cmdwin_push("<y/n>");
        getkey(&yesno, yesnokey);
        cmdwin_pop();

        if (yesno == 'y') {
                /* Set everything and save to the options file. */
                for (i = 0; i < OPTION_NUMOPTIONS; i++) {
                        cfg_set(options[i].key, options[i].val);
                }
                if (options_save()) {
                        log_msg("Error while saving!");
                } else {
                        log_msg("Settings saved!");
                        if (any_restart) {
                                log_msg("NOTE: some of your changes won't "
                                        "take effect until you Quit and "
                                        "restart the program. Sorry for the "
                                        "inconvenience.");
                        }
                }
        } else {
                log_msg("Settings unchanged.");
        }        
}
