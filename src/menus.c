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
#include "list.h"
#include "map.h"
#include "screen.h"
#include "status.h"
#include "file.h"

#include <assert.h>
#include <errno.h>
#include <png.h>
#include <time.h>
#include <SDL_image.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

/**
 * Struct used to keep information about saved game files.
 */
typedef struct saved_game {
        struct list list;        /**< For menu_saved_games list */
        char *fname;             /**< Filename (w/o path) */
        char *path;              /**< Full pathname to save file */
        time_t timestamp;        /**< Last modification time */
        int ref;                 /**< Reference count on this struct */
        SDL_Surface *screenshot; /**< Map screenshot */
        char is_current;         /**< is this the currently loaded game? */
} saved_game_t;


/**
 * The list of saved games, built when we evaluate the saved-game script for
 * the load and save menus.
 */
static struct list menu_saved_games;

/**
 * Keep track of the name of the currently loaded game so we can mark it in the
 * load and save menus.
 */
static saved_game_t *menu_current_saved_game = 0;

/**
 * Delete a saved game struct and all it's strings. Don't call this, use
 * saved_game_unref().
 *
 * @param save The struct to delete.
 */
static void saved_game_del(saved_game_t *save)
{
        assert(! save->ref);

        if (save->fname)
                free(save->fname);
        if (save->path)
                free(save->path);
        if (save->screenshot)
                SDL_FreeSurface(save->screenshot);
        free(save);
}

static char *saved_game_mk_screenshot_fname(saved_game_t *save)
{
        char *s_fname = (char*)malloc(strlen(save->path)+5);
        assert(s_fname);
        sprintf(s_fname, "%s.png", save->path);
        return s_fname;
}

/**
 * Create a new saved game struct and populate it's fields. This makes a copy
 * of the fname, creates a copy of the full pathname, and gets the modification
 * time of the file.
 *
 * @param fname Name of the saved game file.
 * @returns The new struct or 0 if there was a problem accessing the file or
 * allocating memory.
 */
static saved_game_t *saved_game_new(char *fname)
{
        struct stat fileinfo;
        char *s_fname = 0;
        saved_game_t *save = (saved_game_t*)malloc(sizeof(*save));
        if (!save) {
                warn("Could not alloc save");
                return 0;
        }

        memset(save, 0, sizeof(*save));
        
        list_init(&save->list);        

        /* Keep a copy of the file name. */
        save->fname = strdup(fname);
        if (! save->fname) {
                warn("Could not alloc fname");
                goto abort;
        }

        /* Build the full path. */
        save->path = file_mkpath(cfg_get("saved-games-dirname"), fname);
        if (!save->path) {
                warn("Could not alloc filename");
                goto abort;
        }

        /* Get the timestamp on the file. */
        if (! stat(save->path, &fileinfo)) {
                save->timestamp = fileinfo.st_mtime;
        } else {
                /* This is probably a new save that hasn't been written to file
                 * yet. Use the current time as it's timestamp. */
                save->timestamp = time(0);
        }

        /* Load the screenshot. Ignore failure. */
        s_fname = saved_game_mk_screenshot_fname(save);
        if (file_exists(s_fname)) {
                save->screenshot = IMG_Load(s_fname);
        }
        free(s_fname);

        save->ref = 1;
        return save;

 abort:
        saved_game_del(save);
        return 0;
}

/**
 * Release a reference to a saved game struct. This could destroy it.
 *
 * @param save The struct to release.
 */
static void saved_game_unref(saved_game_t *save)
{
        assert(save->ref);
        save->ref--;
        if (!save->ref) {
                saved_game_del(save);
        }
}

/**
 * Called when the user kills the window during the main menu. Exits the
 * program.
 *
 * @param kh Unused
 * @returns Nothing, the program exits.
 */
static bool main_menu_quit_handler(struct QuitHandler *kh)
{
        exit(0);
        return(0); /* for Sun compiler */
}

/**
 * Shows the game credits in the status window.
 */
static void show_credits(void)
{
        struct KeyHandler kh;
        char *title = "Credits";
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

/**
 * Prompts the user to confirm that they want to overwrite a saved game.
 *
 * @returns 1 On confirm, 0 on cancel.
 */
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

/**
 * Add another saved game to the list. This is called as a result of executing
 * the saved-games script, which is just a sequence of (kern-add-saved-game
 * <fname>) procedure calls, each of which lands here. Based on the filename
 * we'll build out the other info associated with the saved game like the
 * timestamp and screenshot image. This also adds the saved game to the list in
 * order sorted by timestamp, using a simple insertion sort algorithm.
 *
 * @param fname The name of the saved game file.
 */
void menu_add_saved_game(char *fname)
{
        struct list *lptr;

        /* Create a new saved game list element. */
        saved_game_t *save = saved_game_new(fname);
        if (!save) {
                warn("menu_add_saved_game: could not add '%s'", fname);
                return;
        }

        /* Insert it in the list ordered by timestamp. Find the first saved
         * game in the list which has a timestamp after this one.  */
        lptr = menu_saved_games.next;
        while (lptr != &menu_saved_games) {
                saved_game_t *save2 = outcast(lptr, saved_game_t, list);
                if (save->timestamp > save2->timestamp) {
                        break;
                }
                lptr = lptr->next;
        }

        /* Insert this one previous to it. Works for empty lists, too. */
        list_add_tail(lptr, &save->list);
}

/**
 * Print the saved game's name, timestamp and other info for display in the
 * status window.
 *
 * @param buf The string buffer to hold the printed line.
 * @param n The size in bytes of the string buffer (including a terminating 
 * NULL).
 * @param fname The name of the saved game file, not including the full path.
 */
static int sprintf_game_info(char *buf, int n, saved_game_t *save)
{
        struct tm timeinfo;
        int ret = -1;
        char datebuf[n];
        int padlen;
        char mark = ' ';

        /* Convert the timestamp from epoch to a time structure. */
        localtime_r(&save->timestamp, &timeinfo);

        /* Print the date to a temp buffer to see how big it is. */
        snprintf(datebuf, n, "%02d:%02d %02d/%02d/%d", timeinfo.tm_hour, 
                 timeinfo.tm_min, timeinfo.tm_mon, timeinfo.tm_mday, 
                 1900+timeinfo.tm_year);

        /* Calculate necessary padding to right-justify the date. */
        padlen = n - (strlen(save->fname) + strlen(datebuf) + 1);

        /* We'll mark the current game with an '*'. */
        if (save->is_current) {
                mark = '*';
        }

        /* Print to the buffer. */
        snprintf(buf, n, "%s %*c%c%s", save->fname, padlen, ' ', mark, 
                 datebuf);
        return ret;
                 
}

/**
 * Extract the filename from the menu entry strings used in the load and save
 * game menus.
 * @returns A strdup'd copy of the fname.
 */
static char *menu_entry_to_fname(char *entry)
{
        char *fname = 0;
        char *end = strchr(entry, ' ');
        int mod = end ? 1 : 0;
        if (mod)
                *end = 0;
        fname = strdup(entry);
        assert(fname);
        if (mod)
                *end = ' ';
        return fname;
}

/**
 * Reset the current saved game.
 * @param save A pointer to the new saved game struct.
 */
static void menu_set_current_saved_game(saved_game_t *save)
{
        if (menu_current_saved_game) {
                menu_current_saved_game->is_current = 0;
                saved_game_unref(menu_current_saved_game);
                menu_current_saved_game = 0;
        }
        menu_current_saved_game = save;
        if (save) {
                save->is_current = 1;
                save->ref++;
                /* Move it to the front of the list. */
                list_remove(&save->list);
                list_add(&menu_saved_games, &save->list);
        }
}

/**
 * Search the list of saved games for one with the given file name.
 *
 * @param fname Filename to search for.
 * @returns The saved game with the matching filename, or 0 if none found.
 */
static saved_game_t *saved_game_lookup(char *fname)
{
        struct list *lptr;
        list_for_each(&menu_saved_games, lptr) {
                saved_game_t *save = outcast(lptr, saved_game_t, list);
                if (! strcmp(fname, save->fname))
                        return save;
        }
        return 0;
}

char * load_game_menu(void)
{
        char **menu = 0;
        char *menubuf, *menubufptr;
        int n = 0;
        int i = 0;
        struct list *lptr = 0;
        struct KeyHandler kh;
	struct ScrollerContext data;
        static char *selection = 0;
        enum StatusMode omode = statusGetMode();
        int linew = STAT_CHARS_PER_LINE;

        /* Erase any previous selection. */
        if (selection) {
                free(selection);
                selection = 0;
        }
        
        n = list_len(&menu_saved_games);
        menubuf = (char*)calloc(n, linew+1);
        assert(menubuf);
        menu = (char**)calloc(n, sizeof(menu[0]));
        assert(menu);

        /* Add each saved game to the menu list. */
        menubufptr = menubuf;
        list_for_each(&menu_saved_games, lptr) {
                saved_game_t *save = outcast(lptr, saved_game_t, list);
                menu[i] = menubufptr;
                sprintf_game_info(menubufptr, linew+1, save);
                menubufptr += linew+1;
                i++;
        }
        
        statusSetStringList("Load Game", n, menu);
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
                char *fname = menu_entry_to_fname((char*)data.selection);
                saved_game_t *save = saved_game_lookup(fname);
                assert(save);
                selection = strdup(save->path);
                assert(selection);
                menu_set_current_saved_game(save);
                free(fname);
        }

        statusSetMode(omode);
        free(menu);
        free(menubuf);

        return selection;
}

/**
 * Find the most recently saved game.
 *
 * @returns A copy of the full pathname of the most recently saved game, or 0
 * if there are no saved games.
 */
char * journey_onward(void)
{
        char *ret = 0;
        saved_game_t *save;

        if (list_empty(&menu_saved_games)) {
                return 0;
        }

        /* Since the saved game list is kept sorted by modification time, the
         * first element in the list is the most recent. */
        save = outcast(menu_saved_games.next, saved_game_t, list);
        ret = strdup(save->path);
        menu_set_current_saved_game(save);
        return ret;
}

/**
 * Get a file name from the player.
 * @returns A strdup'd copy of the filename.
 */
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

/**
 * Rewrite the saved-game script, using the current list of saved games.
 *
 * @returns -1 on error, 0 on success.
 */
static int menu_rewrite_saves()
{
        FILE *file;
        struct list *lptr;
        char *fname = cfg_get("save-game-filename");

        file = file_open_in_save_dir(fname, "w");
        if (! file) {
                warn("Problem updating %s: %s\n", fname, file_get_error());
                return -1;
        }
        
        list_for_each(&menu_saved_games, lptr) {
                saved_game_t *save = outcast(lptr, saved_game_t,  list);
                fprintf(file, "(kern-add-save-game \"%s\")\n", save->fname);
        }

        fclose(file);
        return 0;
}

/**
 * Let the player select a file to save the current game.
 *
 * @returns A strdup'd copy of the name of the file to save to, or 0 if the
 * player aborts. The filename is NOT the full path, like some of the other
 * menu functions return, because we're not going to load anything with the
 * result.
 */
char * save_game_menu(void)
{
        static char *NEW_SAVED_GAME = "New Saved Game";
        char **menu = 0;
        char *menubuf, *menubufptr;
        int n = 0;
        int i = 0;
        struct list *lptr = 0;
        struct KeyHandler kh;
	struct ScrollerContext data;
        char *selection = 0;
        enum StatusMode omode = statusGetMode();
        int linew = STAT_CHARS_PER_LINE;

        n = list_len(&menu_saved_games) + 1;
        menubuf = (char*)calloc(n, linew+1);
        assert(menubuf);
        menu = (char**)calloc(n, sizeof(menu[0]));
        assert(menu);

        i = 0;
        menubufptr = menubuf;

        /* The first entry is always the currently loaded game (if there is
         * one), which should always be first in the list. */
        if (menu_current_saved_game) {
                assert(menu_saved_games.next 
                       == &menu_current_saved_game->list);
                sprintf_game_info(menubufptr, linew+1, 
                                  menu_current_saved_game);
                menu[i++] = menubufptr;
                menubufptr += linew+1;

                /* Test: blit the screenshot */
                if (menu_current_saved_game->screenshot) {
                        mapSetImage(menu_current_saved_game->screenshot);
                }
        }

        /* The next entry is always New Save Game. */
        sprintf(menubufptr, NEW_SAVED_GAME);
        menu[i++] = menubufptr;
        menubufptr += linew+1;

        /* The remaining saved games are listed in timestamp order, as they
         * appear in the list. */
        if (menu_current_saved_game) {
                lptr = menu_current_saved_game->list.next;
                while (lptr != &menu_saved_games) {
                        saved_game_t *save = outcast(lptr, saved_game_t, list);
                        lptr = lptr->next;
                        menu[i] = menubufptr;
                        sprintf_game_info(menubufptr, linew+1, save);
                        menubufptr += linew+1;
                        i++;
                }
        }
        
        statusSetStringList("Save Game", n, menu);
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

                /* New saved game? */
                if (!strcmp((char*)data.selection, NEW_SAVED_GAME)) {
                        int need_rewrite = 1;
                        selection = prompt_for_fname();
                        if (!selection)
                                goto reselect;

                        /* Check if the player re-typed an existing filename */
                        for (i = 1; i < n; i++) {
                                char *fname = menu_entry_to_fname(menu[i]);
                                if (!strcmp(selection, fname)) {
                                        if (!confirm_selection()) {
                                                free(selection);
                                                selection = 0;
                                                free(fname);
                                                goto reselect;
                                        }
                                        need_rewrite = 0;
                                        break;
                                }
                                free(fname);
                        }

                        /* Replace "New Saved Game" with what the player 
                           typed */
                        snprintf(menu[0], linew, selection);

                        /* Add a new saved game struct to the list. */
                        menu_add_saved_game(selection);

                        /* Re-write the saved games file to add the new
                         * save. */
                        menu_rewrite_saves();

                } else if (confirm_selection()) {
                        selection = menu_entry_to_fname((char*)data.selection);
                } else {
                        goto reselect;
                }
        }

        /* Set the selected saved game as current and store a screenshot. */
        if (selection) {
                saved_game_t *save = saved_game_lookup(selection);
                char *s_fname = saved_game_mk_screenshot_fname(save);
                SDL_Rect rect;
                rect.x = MAP_X;
                rect.y = MAP_Y;
                rect.w = MAP_W;
                rect.h = MAP_H;
                screenCapture(s_fname, &rect);
                save->screenshot = IMG_Load(s_fname);
                menu_set_current_saved_game(save);
                free(s_fname);
        }

        /* Restore the original status mode before deleting the list. */
        statusSetMode(omode);
        mapClearImage();

        free(menu);
        free(menubuf);

        return selection;
}

char * main_menu(void)
{
        static char *START_NEW_GAME="Start New Game";
        static char *JOURNEY_ONWARD="Journey Onward";
        static char *LOAD_GAME="Load Game";
        static char *CREDITS="Credits";
        static char *QUIT="Quit";
        static char *TUTORIAL="Tutorial";
        static char *SETTINGS = "Settings";
        char *menu[7];
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
                menu[n_items] = LOAD_GAME;
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

        statusSetStringList("Main Menu", n_items, menu);
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
                load_fname = journey_onward();
                if (! load_fname)
                        goto start_main_menu;
        }
        else if (! strcmp(selection, LOAD_GAME)) {
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
        char *fname = cfg_get("save-game-filename");
        list_init(&menu_saved_games);
        if (file_exists_in_save_dir(fname)) {
                file_load_from_save_dir(fname);
        }
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

        statusSetStringList("Screen Dimensions", array_sz(menu), menu);
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
                statusSetStringList("Settings", OPTION_NUMOPTIONS, 
                                    (char**)menu);
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
