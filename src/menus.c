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
#include "foogod.h"
#include "log.h"
#include "list.h"
#include "map.h"
#include "screen.h"
#include "status.h"
#include "file.h"

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <png.h>
#include <time.h>
#include <SDL_image.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define LOADSAVE_HINT "\005\006=scroll ENT=select DEL=delete ESC=exit"

/**
 * Information about saved game files.
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
 * The context used for the menu_scroll function. This keeps track of the
 * currently selected saved game from the menu and communicates a player abort
 * back to the menu code.
 */
typedef struct {
        saved_game_t *save; /**< Currently highlighted saved game */
        char *entry;        /**< Currently highlighted string entry */
        char *hotkeys;      /**< Hotkey characters, in order of listing */
        char **menu;        /**< Menu strings */
        int n_menu;         /**< Number of menu strings */
        char *title;        /**< Menu title */
        char abort : 1;     /**< The player aborted the selection */
} menu_scroll_data_t;


/**
 * The main menu uses this to rest the splash image if the user enters the load
 * menu but then changes his mind and backs out to the main menu.
 */
extern void nazghul_splash(void);

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
 * This is what the Save Game menu shows for the new game option.
 */
static char *MENU_NEW_GAME_STR = "N)ew Saved Game";

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

/**
 * Infer the filename of a screenshot from the filename for the saved game.
 *
 * @param save The saved game that goes with the screenshot.
 * @returns The full pathname to the screenshot file. The caller should free()
 * the string when done using it.
 */
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
                warn("Could not alloc save\n");
                return 0;
        }

        memset(save, 0, sizeof(*save));
        
        list_init(&save->list);        

        /* Keep a copy of the file name. */
        save->fname = strdup(fname);
        if (! save->fname) {
                warn("Could not alloc fname\n");
                goto abort;
        }

        /* Build the full path. */
        save->path = file_mkpath(cfg_get("saved-games-dirname"), fname);
        if (!save->path) {
                warn("Could not alloc filename\n");
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
                warn("menu_add_saved_game: could not add '%s'\n", fname);
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
static int sprintf_game_info(char *buf, int n, saved_game_t *save, char hotkey)
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
        padlen = n - (strlen(save->fname) 
                      + strlen(datebuf) 
                      + 3 
                      + (hotkey ? 3 : 0));

        /* We'll mark the current game with an '*'. */
        if (save->is_current) {
                mark = '*';
        }

        /* Print to the buffer. */
        if (hotkey) {
                snprintf(buf, n, "%c) %s %*c%c%s", hotkey, save->fname, 
                         padlen, ' ', mark, datebuf);
        } else {
                snprintf(buf, n, "%s %*c%c%s", save->fname, 
                         padlen, ' ', mark, datebuf);
        }
        return ret;
                 
}

/**
 * Extract the filename from the menu entry strings used in the load and save
 * game menus.
 * @returns A strdup'd copy of the fname.
 */
static char *menu_entry_to_fname(char *entry)
{
        char *fname, *end;
        int mod;

        /* Most saved games entries start with x), where x is a 0-9 */
        if (isdigit(entry[0]) && ')' == entry[1]) {
                entry += 3;
        }

        end = strchr(entry, ' ');
        mod = end ? 1 : 0;
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

/**
 * Show a screenshot over the map viewer with the words "SCREEN SHOT" in the
 * middle.
 *
 * @param screenshot The image to show, or 0 to just show a blank screen. For a
 * blank screen the words "SCREEN SHOT" are still printed.
 */
static void menu_show_screenshot(SDL_Surface *screenshot)
{
        static char *MENU_SCREEN_SHOT_STR = "^c+ySCREEN SHOT^c-";
        SDL_Rect rect;

        mapSetImage(screenshot);
        rect.x = (((MAP_X + MAP_W) / 2) - (5 * ASCII_W));
        rect.y = (MAP_Y + MAP_H)/2;
        rect.w = strlen(MENU_SCREEN_SHOT_STR);
        rect.h = ASCII_H;
        screenPrint(&rect, 0, MENU_SCREEN_SHOT_STR);

        rect.w *= ASCII_W;
        screenUpdate(&rect);
}

/**
 * Get the highlighted menu item from the status viewer and figure out which
 * saved game it corresponds to.
 *
 * @returns The saved game struct that goes with the menu entry, or 0 if the
 * "New Game" option is highlighted.
 */
static saved_game_t *menu_scroller_get_selected()
{
        char *fname;
        char *entry_str = (char*)statusGetSelected(String);
        if (!entry_str) {
                return 0;
        }
        if (! strcmp(entry_str, MENU_NEW_GAME_STR)) {
                return 0;
        }
        fname = menu_entry_to_fname(entry_str);
        return saved_game_lookup(fname);
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
 * Prompt the user to delete the currently highlighted save-game. This checks
 * if the highlighted entry is a valid save game, prompts the user to confirm
 * the deletetion, deletes the save file and the screenshot file, removes the
 * saved game struct from the list, unrefs it, and re-writes the saved game
 * script.
 *
 * If the save-game or screenshot files can't be deleted the operation warns
 * the user but continues to remove and unreference the saved game. It will
 * appear again the next time nazghul is restarted.
 */
static void menu_prompt_to_delete(menu_scroll_data_t *data)
{
        char *selstr = 0;
        saved_game_t *save = 0;
        int yesno = 0;
        int i1, i2;

        /* Check if user tried to delete the N)ew Saved Game option */
        selstr =  (char*)statusGetSelected(String);
        if (! strcmp(selstr, MENU_NEW_GAME_STR))
                return;

        /* Get the saved game struct for the selection. */
        save =  menu_scroller_get_selected();
        if (!save)
                return;

        /* Prompt to confirm. */
        log_begin("Delete %s?", save->fname);
        log_flush();
        cmdwin_clear();
        cmdwin_push("Delete-");
        cmdwin_push("<y/n>");
        getkey(&yesno, yesnokey);
        cmdwin_pop();

        /* If confirmation denied then cancel. */
        if (yesno == 'n') {
                cmdwin_spush("abort!");
                log_end(" Canceled!");
                return;
        }

        /* Confirmed, try to delete the save file. Abort if it doesn't work. */
        cmdwin_push("yes!");
        statusFlashSelected(Red);
        if (unlink(save->path)) {
                log_end(" WARNING! Failed to delete save file %s: %s", 
                        save->path, strerror(errno));
        }

        /* Try to delete the screenshot. Warn the user and continue if it
         * doesn't work. */
        if (save->screenshot) {
                char *scr_fname = saved_game_mk_screenshot_fname(save);
                if (unlink(scr_fname)) {
                        log_end(" WARNING! Failed to delete screenshot file "\
                                "%s: %s:", scr_fname, strerror(errno));
                }
                free(scr_fname);
        }

        /* Remove and unreference the saved game struct. */
        list_remove(&save->list);
        if (save == menu_current_saved_game) {
                menu_set_current_saved_game(0);
        }
        saved_game_unref(save);

        /* Re-write the saved game script. */
        menu_rewrite_saves();

        /* Reshuffle all the menu entries to close the gap and reset the status
         * menu list. */
        i1 = statusGetSelectedIndex(String);
        assert(i1 >= 0);
        for (i2 = i1; i2 < data->n_menu-1; i2++) {
                data->menu[i2] = data->menu[i2+1];
        }
        data->menu[i2] = 0;
        data->n_menu--;
        statusSetStringList(data->title, data->n_menu, data->menu);
        statusSetSelectedIndex(i1 ? (i1 - 1) : 0);
        statusRepaint();

        log_end(" Deleted!");
}

/**
 * Scroll the status window for the load/save menus. As the player scrolls over
 * a saved game, show its screenshot on the map window.
 *
 * @param kh Keyhandler with the menu context as its data element.
 * @param key The key pressed by the player.
 * @param keymod Reflects the status of the SHIFT, CTRL and ALT keys.
 * @returns 0 to keep the Status window in scroll mode, 1 to end it.
 */
int menu_scroller(struct KeyHandler * kh, int key, int keymod)
{
	menu_scroll_data_t *data = (menu_scroll_data_t *) kh->data;
        enum StatusScrollDir dir;
        int i1;

        if (data->hotkeys && key < 128) {
                char ckey = (char)key;
                char *hotkey = strchr(data->hotkeys, ckey);
                if (hotkey) {
                        int index = hotkey - data->hotkeys;
                        printf("ckey=%c index=%d\n", ckey, index);
                        statusSetSelectedIndex(index);
                        statusFlashSelected(Green);
                        data->entry = (char*)statusGetSelected(String);
                        data->save = menu_scroller_get_selected();
                        return 1;
                }
        }

	switch (key) {
	case KEY_NORTH:
                dir = ScrollUp;
		break;
	case KEY_SOUTH:
                dir = ScrollDown;
		break;
	case SDLK_PAGEUP:
                dir = ScrollPageUp;
		break;
	case SDLK_PAGEDOWN:
                dir = ScrollPageDown;
		break;
	case SDLK_RETURN:
	case SDLK_SPACE:
	case '\n':
                i1 = statusGetSelectedIndex(String);
                statusFlashSelected(Green);
                data->entry = (char*)statusGetSelected(String);
                data->save = menu_scroller_get_selected();
		return 1;
	case SDLK_ESCAPE:
	case 'q':
                data->abort = 1;
		return 1;
        case SDLK_DELETE:
        case 'd':
                menu_prompt_to_delete(data);
                return data->n_menu ? 0 : 1;
                break;
	default:
		break;
	}

        statusScroll(dir);
        data->entry = (char*)statusGetSelected(String);
        data->save = menu_scroller_get_selected();
        menu_show_screenshot(data->save ? data->save->screenshot : 0);

	return 0;
}

/**
 * Scroll the status window for the main menu.
 *
 * @param kh Keyhandler with the menu context as its data element.
 * @param key The key pressed by the player.
 * @param keymod Reflects the status of the SHIFT, CTRL and ALT keys.
 * @returns 0 to keep the Status window in scroll mode, 1 to end it.
 */
int main_menu_scroller(struct KeyHandler * kh, int key, int keymod)
{
	menu_scroll_data_t *data = (menu_scroll_data_t *) kh->data;
        enum StatusScrollDir dir;

        if (data->hotkeys && key < 128) {
                char ckey = (char)key;
                char *hotkey = strchr(data->hotkeys, ckey);
                if (hotkey) {
                        int index = hotkey - data->hotkeys;
                        printf("ckey=%c index=%d\n", ckey, index);
                        statusSetSelectedIndex(index);
                        data->entry = (char*)statusGetSelected(String);
                        return 1;
                }
        }

	switch (key) {
	case KEY_NORTH:
                dir = ScrollUp;
		break;
	case KEY_SOUTH:
                dir = ScrollDown;
		break;
	case SDLK_PAGEUP:
                dir = ScrollPageUp;
		break;
	case SDLK_PAGEDOWN:
                dir = ScrollPageDown;
		break;
	case SDLK_RETURN:
	case SDLK_SPACE:
	case '\n':
                data->entry = (char*)statusGetSelected(String);
		return 1;
	case SDLK_ESCAPE:
	case 'q':
                data->abort = 1;
		return 1;
	default:
		break;
	}

        statusScroll(dir);
        data->entry = (char*)statusGetSelected(String);

	return 0;
}

/**
 * Get a suitable hotkey for the numeral.
 *
 * @param i The number of the hotkey.
 * @returns ASCII 0-9 or NULL if i is out of range.
 */
static char menu_hotkey(int i)
{
        return (i < 10) ? (i + '0') : 0;
}

char * load_game_menu(void)
{
        char **menu = 0;
        char *menubuf, *menubufptr;
        int n = 0;
        int i = 0;
        struct list *lptr = 0;
        struct KeyHandler kh;
	menu_scroll_data_t data;
        char *selection = 0;
        enum StatusMode omode = statusGetMode();
        int linew = STAT_CHARS_PER_LINE;

        memset(&data, 0, sizeof(data));

        /* Allocate the memory for the menu strings. */
        n = list_len(&menu_saved_games);
        menubuf = (char*)calloc(n, linew+1);
        assert(menubuf);
        menu = (char**)calloc(n, sizeof(menu[0]));
        assert(menu);

        data.hotkeys = (char*)calloc(n + 1, 1);
        assert(data.hotkeys);
        data.menu = menu;
        data.n_menu = n;
        data.title = "Load Game";

        /* Add each saved game to the menu list. */
        menubufptr = menubuf;
        list_for_each(&menu_saved_games, lptr) {
                saved_game_t *save = outcast(lptr, saved_game_t, list);
                menu[i] = menubufptr;
                data.hotkeys[i] = menu_hotkey(i);
                sprintf_game_info(menubufptr, linew+1, save, data.hotkeys[i]);
                menubufptr += linew+1;
                i++;
        }

        foogodSetHintText(LOADSAVE_HINT);
        foogodSetMode(FOOGOD_HINT);
        statusSetStringList(data.title, n, menu);
        statusSetMode(StringList);

        /* Setup the initial screen shot */
        if (list_empty(&menu_saved_games)) {
                menu_show_screenshot(0);
        } else {
                saved_game_t *save = outcast(menu_saved_games.next, 
                                             saved_game_t, list);
                menu_show_screenshot(save->screenshot);
        }

        kh.fx = menu_scroller;
        kh.data = &data;
	eventPushKeyHandler(&kh);
	eventHandle();
	eventPopKeyHandler();

        /* If the player selected something then build the full pathname for
         * return. */
        if (! data.abort
            && data.save) {
                selection = strdup(data.save->path);
                assert(selection);
                menu_set_current_saved_game(data.save);
        }

        mapClearImage();

        /* If the original status mode was already StringList don't reset the
         * mode, this will cause status to highlight the top list entry of our
         * list and it looks funny while we're loading the game. */
        if (omode != StringList)
                statusSetMode(omode);

        foogodSetMode(FOOGOD_DEFAULT);
        free(menu);
        free(menubuf);
        free(data.hotkeys);

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
 * Let the player select a file to save the current game.
 *
 * @returns A strdup'd copy of the name of the file to save to, or 0 if the
 * player aborts. The filename is NOT the full path, like some of the other
 * menu functions return, because we're not going to load anything with the
 * result.
 */
char * save_game_menu(void)
{
        char **menu = 0;
        char *menubuf, *menubufptr;
        int n = 0;
        int i = 0;
        struct list *lptr = 0;
        struct KeyHandler kh;
        menu_scroll_data_t data;
        enum StatusMode omode = statusGetMode();
        int linew = STAT_CHARS_PER_LINE;
        saved_game_t *selected_game = 0;

        memset(&data, 0, sizeof(data));

        /* Allocate the string buffers to display the menu. */
        n = list_len(&menu_saved_games) + 1;
        menubuf = (char*)calloc(n, linew+1);
        assert(menubuf);
        menu = (char**)calloc(n, sizeof(menu[0]));
        assert(menu);

        data.hotkeys = (char*)calloc(n+1, 1);
        assert(data.hotkeys);
        data.menu = menu;
        data.n_menu = n;
        data.title = "Save Game";

        /* Prepare to fill in the menu list. */
        i = 0;
        menubufptr = menubuf;

        /* The first entry is always the New Save Game option. */
        sprintf(menubufptr, MENU_NEW_GAME_STR);
        data.hotkeys[i] = 'n';
        menu[i++] = menubufptr;
        menubufptr += linew+1;

        /* Is there a game already loaded? */
        if (menu_current_saved_game) {

                /* It should be first in the list of saved games. */
                assert(menu_saved_games.next 
                       == &menu_current_saved_game->list);
                
                data.hotkeys[i] = menu_hotkey(i);

                /* Put it as the next item in the menu. */
                sprintf_game_info(menubufptr, linew+1, 
                                  menu_current_saved_game, data.hotkeys[i]);
                menu[i++] = menubufptr;
                menubufptr += linew+1;
        }

        /* Prepare to list the remaining saved games. */
        if (menu_current_saved_game) {
                lptr = menu_current_saved_game->list.next;
        } else {
                lptr = menu_saved_games.next;
        }

        /* The remaining saved games are in timestamp order on the list; add
         * them to the menu in this order. */
        while (lptr != &menu_saved_games) {
                saved_game_t *save = outcast(lptr, saved_game_t, list);
                lptr = lptr->next;
                menu[i] = menubufptr;
                data.hotkeys[i] = menu_hotkey(i);
                sprintf_game_info(menubufptr, linew+1, save, data.hotkeys[i]);
                menubufptr += linew+1;
                i++;
        }

        foogodSetHintText(LOADSAVE_HINT);
        foogodSetMode(FOOGOD_HINT);

        /* Setup the menu in the status window. */
        statusSetStringList(data.title, n, menu);
        statusSetMode(StringList);
        
        /* Highlight the current saved game. */
        if (menu_current_saved_game) {
                statusSetSelectedIndex(1);
        }

        /* Show the initial screenshot. */
        menu_show_screenshot(menu_current_saved_game 
                             ? menu_current_saved_game->screenshot
                             : 0);

reselect:
        kh.fx = menu_scroller;
        kh.data = &data;
	eventPushKeyHandler(&kh);
	eventHandle();
	eventPopKeyHandler();

        if (data.abort) {
                selected_game = 0;
                goto done;
        }

        /* Did the player select an existing saved game? */
        if (data.save) {

                /* Yes. Overwrite? */
                if (confirm_selection()) {
                        selected_game = data.save;
                } else {
                        goto reselect;
                }
        }

        /* No. Did player abort? */
        else if (!data.abort) {

                /* No. Must be a new saved game. */
                struct list *lptr;
                char *new_name = prompt_for_fname();
                if (!new_name)
                        goto reselect;

                /* Did player re-type an existing filename? */
                list_for_each(&menu_saved_games, lptr) {
                        saved_game_t *exist = outcast(lptr, saved_game_t, 
                                                      list);
                        if (!strcmp(new_name, exist->fname)) {

                                /* Yes. Confirm overwrite? */
                                if (!confirm_selection()) {
                                        /* No. Reselect. */
                                        free(new_name);
                                        goto reselect;
                                }

                                /* Ok, overwrite. Nothing new to add to the
                                 * saved-game script. */
                                break;
                        }
                }
                
                /* Replace the "New Saved Game" menu line with what the player
                   typed. */
                strncpy(menu[0], new_name, linew);
                
                /* Add a new saved game struct to the list. */
                selected_game = saved_game_new(new_name);
                list_add(&menu_saved_games, &selected_game->list);
                
                /* Re-write the saved games file to add the new
                 * save. */
                menu_rewrite_saves();

                free(new_name);
        }

        /* Save selected? */
        if (selected_game) {

                char *s_fname = saved_game_mk_screenshot_fname(selected_game);
                SDL_Rect rect;

                /* Does it have an old screenshot? */
                if (selected_game->screenshot) {
                        /* Yes. Get rid of it. */
                        SDL_FreeSurface(selected_game->screenshot);
                        selected_game->screenshot = 0;
                }

                /* Restore map view so we can get a new screenshot. */
                mapClearImage();
                mapUpdate(0);

                /* Take a new screenshot. */
                rect.x = MAP_X;
                rect.y = MAP_Y;
                rect.w = MAP_W;
                rect.h = MAP_H;
                screenCapture(s_fname, &rect);
                selected_game->screenshot = IMG_Load(s_fname);
                menu_set_current_saved_game(selected_game);
                free(s_fname);
        }

 done:
        /* Restore the original status mode before deleting the list. */
        statusSetMode(omode);
        foogodSetMode(FOOGOD_DEFAULT);
        foogodRepaint();
        mapClearImage();

        free(menu);
        free(menubuf);
        free(data.hotkeys);

        if (selected_game) {
                return strdup(selected_game->fname);
        }
        return 0;
}

char * main_menu(void)
{
        static char *START_NEW_GAME="S)tart New Game";
        static char *JOURNEY_ONWARD="J)ourney Onward";
        static char *LOAD_GAME="L)oad Game";
        static char *CREDITS="C)redits";
        static char *QUIT="Q)uit";
        static char *TUTORIAL="T)utorial";
        static char *SETTINGS = "S(e)ttings";
        char *menu[7];
        char hotkeys[7+1];
        int n_items = 0;
        struct KeyHandler kh;
        menu_scroll_data_t data;
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
        nazghul_splash();

        /* check for a previously saved game to Journey Onward */
        if (file_exists_in_save_dir(save_game_fname)) {
                menu[n_items] = JOURNEY_ONWARD;
                hotkeys[n_items] = 'j';
                n_items++;
                menu[n_items] = LOAD_GAME;
                hotkeys[n_items] = 'l';
                n_items++;
        }

        /* check for the default script for Start New Game */
        if (file_exists(new_game_fname)) {
                menu[n_items] = START_NEW_GAME;
                hotkeys[n_items] = 's';
                n_items++;
        }

        /* check for a tutorial script for Tutorial */
        if (file_exists(tutorial_fname)) {
                menu[n_items] = TUTORIAL;
                hotkeys[n_items] = 't';
                n_items++;
        }

        menu[n_items] = SETTINGS;
        hotkeys[n_items] = 'e';
        n_items++;

        menu[n_items] = CREDITS;
        hotkeys[n_items] = 'c';
        n_items++;

        menu[n_items] = QUIT;
        hotkeys[n_items] = 'q';
        n_items++;

        hotkeys[n_items] = 0;

        foogodSetHintText("\005\006=scroll ENT=select");
        foogodSetMode(FOOGOD_HINT);
        statusSetStringList("Main Menu", n_items, menu);
        statusSetMode(StringList);

        data.hotkeys = hotkeys;
        kh.fx   = main_menu_scroller;
        kh.data = &data;
	eventPushKeyHandler(&kh);
	eventHandle();
	eventPopKeyHandler();

        selection = data.entry;

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
        foogodSetMode(FOOGOD_DEFAULT);

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
                warn("Could not open '%s': %s\n", fname, strerror(errno));
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
                foogodSetHintText(SCROLLER_HINT);
                foogodSetMode(FOOGOD_HINT);
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
        foogodSetMode(FOOGOD_DEFAULT);

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
