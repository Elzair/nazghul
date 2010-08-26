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
#ifndef cmd_h
#define cmd_h

#include "macros.h"
#include "event.h" /* for v_funcpointer_ii */

#define MIN_XAMINE_LIGHT_LEVEL 32

class Character;
class Object;
class Party;

BEGIN_DECL;

// I really shouldn't pollute this interface with these, but they don't really
// fit anywhere else right now, either.
#include "status.h"
struct ScrollerContext {
        enum StatusSelection selector;
        const void *selection;
        bool abort;
        bool done;
        bool mixing; // for mix reagents
};

/**
 * Request format for select_target_generic().
 */
typedef struct ui_select_target_req 
{
        struct place *place;   /* place this is happening in       */
        int x1, y1;            /* center of target zone            */
        int x2, y2;            /* initial/final cursor position    */
        struct templ *tiles;   /* template of tiles in target zone */
        struct list suggest;   /* quick-target tiles               */
        void *data;            /* caller context                   */

        /* called when the cursor moves over a new tile */
        void (*move)(struct place *, int, int, void *);

        /* Called when user hits 'enter' or otherwise selects a tile. Returns
         * non-zero to end the targeting session, zero otherwise. */
        int (*select)(struct place *, int, int, void *);

} ui_select_target_req_t;

#define SCROLLER_HINT "\005\006=scroll ENT=select ESC=exit"
#define PAGER_HINT  "\005\006=scroll ESC=exit"

extern int dirkey(struct KeyHandler *kh, int key, int keymod);
extern int cardinaldirkey(struct KeyHandler *kh, int key, int keymod);
extern int yesnokey(struct KeyHandler *kh, int key, int keymod);
extern int anykey(struct KeyHandler *kh, int key, int keymod);
extern int scroller(struct KeyHandler *kh, int key, int keymod);
extern int movecursor(struct KeyHandler *kh, int key, int keymod);
extern int getnum(struct KeyHandler *kh, int key, int keymod);
extern void getkey(void *data, int(*handler) (struct KeyHandler * kh,
                                               int key, int keymod));
#define CMD_SELECT_MEMBER (1 << 0)
#define CMD_PRINT_MEMBER  (1 << 1)
  
extern void cmdAttack(void);
extern int cmd_camp_in_town(class Character *camper);
extern int cmd_camp_in_wilderness(class Party *camper);
extern void cmdFire(void);
extern void cmdHelp(void);
extern void cmdLoiter(class Being *subject);
extern bool cmdMixReagents(class Character *mixer);
extern void cmdNewOrder(void);
extern bool cmdSave(void);
extern void cmdReload(void);
extern bool cmdSearch(class Character *pc);
extern void cmdTalk(Object *member);
extern void cmdZoomIn(void);
extern bool cmdUse(class Character * pc, int flags);
extern bool cmdHandle(class Character * pc);
extern bool cmdReady(class Character * pc);
extern bool cmdZtats(class Character * pc);
extern bool cmdXamine (class Object *examiner);
extern bool cmdAT(class Character * pc);
extern bool cmdGet(class Object *actor);
extern bool cmdOpen(class Character * pc);
extern bool cmdCastSpell(class Character * pc);
extern bool cmdQuit(void);
extern bool cmdLook(int x, int y);
extern bool cmd_terraform(struct place *place, int x, int y);
extern bool cmd_save_current_place (struct place * place);
extern bool cmdSaveTerrainMap(class Character * pc);
extern bool cmdSetSoloMode(int party_member_index);
extern bool cmdToggleFollowMode(void);
extern void cmdSettings(void);
extern void cmdDrop(class Character *pc);
extern void cmdYuse(class Character *pc);
extern void cmdDeveloperEval(struct session *);
  
extern class Character *select_party_member(void);
    
struct location_list {
        struct list list;
        int x;
        int y;
};

extern int select_target(int ox, int oy, int *x, int *y, int range, 
                         struct list *suggest);

extern const char * name_of_context (void);

extern int ui_get_quantity(int max);
  
// the new ui api
extern int ui_get_direction(void);
extern int ui_get_yes_no(const char *asked_persons_name);
extern int ui_getline(char *buf, int len);
extern int ui_getline_plain(char *buf, int len);
extern void ui_name_vehicle(class Vehicle *vehicle);

/**
 * The merchant information for a trading session.
 */
struct merchant {
        const char *name;                /* The merchant's name */
        int n_trades;              /* Num entries in the array of items */
        struct trade_info *trades; /* The array of trade items */
};

/**
 * These three functions all engage in the trading UI with a merchant. ui_trade
 * will allow the player to choose between buying and selling, and will call
 * the other two functions.
 *
 * @param merch is the merchant info
 * @returns the quantity of items traded
 */
extern int ui_trade(struct merchant *merch);
extern int ui_buy(struct merchant *merch);
extern int ui_sell(struct merchant *merch);

/**
 * Prompt the player to enter a line. By default (no filter provided) this will
 * accept all printable characters as valid input from the player. Especially
 * the ESC, '\n' and '\b' characters are taken to mean control characters and
 * aren't considered printable.
 *
 * @param buf The string buffer to fill with the response. On success this
 * contains a null-terminated string. The NULL will be there, and the buffer
 * length will not be exceeded. The user reply will be truncated if necessary.
 *
 * @param len The length of buf.
 *
 * @param filter An optional filter function. This function should return
 * non-zero to reject a key. Note that all non-printable characters are already
 * filtered automatically; this param let's you filter even more. The filter is
 * applied after checking for ESC, '\n' and '\b', which are processed as
 * control characters and never appear in the result anyway.
 *
 * @returns The actual number of characters stored. Note that if the player
 * hits ESC this will be zero as if no keys were pressed.
 */
extern int ui_getline_filtered(char *buf, int len, int (*filter)(int key));

/**
 * More general version of select_target(). Prompts the player to select a tile
 * within a range or template of tiles.
 *
 * @parm req tells the function how to carry out its business.
 *
 * @returns zero iff the (x2, y2) fields of the req hold the player-selected
 * target location. The only time it won't is if the player aborts the prompt.
 */
extern int ui_select_target_generic(ui_select_target_req_t *req);

/**
 * Initialize a target request to safe defaults. Note that this is for a new
 * request, if you try to re-init without doing some manual cleanup first
 * you'll get a memory leak.
 *
 * @parm req will be initialized.
 */
extern void ui_select_target_req_init(ui_select_target_req_t *req);

/**
 * Prompt the player to select something from inventory. This assumes that the
 * status mode has already been set by the caller.
 */
extern struct inv_entry *ui_select_item(void);

END_DECL;

#endif
