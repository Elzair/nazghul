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
#ifndef status_h
#define status_h

#include "macros.h"
#include "common.h"
#include "dimensions.h"
#include "node.h"

BEGIN_DECL

#define STAT_LIST_CHARS_PER_LINE (STAT_CHARS_PER_LINE - (TILE_W / ASCII_W))

struct trade_info {
        struct sprite *sprite;
        char *name;
        int quantity;
        int cost;
        void *data; /* object type */
        char show_quantity:1;
        char show_sprite:1;
};

struct stat_list_entry {
        struct sprite *sprite;
        char line1[STAT_LIST_CHARS_PER_LINE + 1];
        char line2[STAT_LIST_CHARS_PER_LINE + 1];
        void *data;
};

struct stat_super_generic_data {
        char *title;
        struct node list;
        struct node *selected;
        struct node *first_shown;
        void (*paint)(struct stat_super_generic_data *self, struct node *node, 
                      SDL_Rect *rect);
        void (*unref)(struct stat_super_generic_data *self);
        int refcount;
        void *data;
};

enum StatusScrollDir {
        ScrollUp,
        ScrollDown,
        ScrollRight,
        ScrollLeft,
        ScrollPageUp,
        ScrollPageDown,
        ScrollTop,
        ScrollBottom
};
								
enum StatusMode {
        ShowParty,
        SelectCharacter,
        Ztats,
        Ready,
        Use,
        Page,
        Trade,
        MixReagents,
        GenericList,
        StringList,
        DisableStatus,
        SuperGeneric,
        Drop
};
					
enum StatusSelection {
        Character,
        InventoryItem,
        TradeItem,
        Reagents,
        Generic,
        String,
        SelectSuperGeneric
};

extern int statusInit(void);
extern void statusRepaint(void);
extern void statusFlash(int line, unsigned int color);
								
extern void statusSetMode(enum StatusMode mode);
extern enum StatusMode statusGetMode(void);
extern void statusScroll(enum StatusScrollDir dir);
extern void *statusGetSelected(enum StatusSelection sel);
extern void statusSelectCharacter(int partyOrderIndex);

extern void statusSetPageText(char *title, char *text);
extern void statusSetTradeInfo(int list_sz, struct trade_info *trades);
extern void statusUpdateTradeInfo(int list_sz, 
                                  struct trade_info *trades);
extern int status_get_h(void);
        

/**
 * Setup a generic list for use with the GenericList mode. A generic list has
 * two lines and an optional icon per list entry. This call has no effect on
 * the UI until it is followed by a statusSetMode(GenericList) call.
 *
 * @param title The title to show in the GenericList mode. This call does not
 * make its own copy, so 'title' must remain a valid pointer until the caller
 * is done using this mode and changes to another mode.
 *
 * @param list_sz The number of entries in the list.
 *
 * @param list An array of pointers to the list entries. Like the 'title', this
 * call does not make its own copy, so all the pointers must remain valid while
 * the GenericList mode is in use.
 */
extern void statusSetGenericList(char *title, int list_sz, 
                                 struct stat_list_entry *list);

/**
 * Setup a list of plain string entries for use with the StringList mode. Each
 * string will take up one line in the status display. This call has no effect
 * on the UI until it is followed by a statusSetMode(StringList) call.
 *
 * @param title The title to show in the StringList mode. This call does not
 * make its own copy, so 'title' must remain a valid pointer until the caller
 * is done using this mode and changes to another mode.
 *
 * @param list_sz The number of entries in the list.
 *
 * @param list An array of pointers to the list entries. Like the 'title', this
 * call does not make its own copy, so all the pointers must remain valid while
 * the StringList mode is in use.
 */
extern void statusSetStringList(char *title, int list_sz, char **list);

/**
 * When in one of the list modes, this forces the list entry at the given index
 * to be highlighted.
 *
 * @param index The index of the entry to be selected. The first entry has
 * index 0.
 */
extern void statusSetSelectedIndex(int index);

/**
 * Similar to statusGetSelected, but get the integer index.
 *
 * @param sel The type of index to get. Supported values are Character,
 * Generic, TradeItem and String.
 *
 * @returns The index or -1 if an invalid type is used or the corresponding
 * list has no entries.
 */
extern int statusGetSelectedIndex(enum StatusSelection sel);

/**
 * Flash the line of the currently selected item.
 *
 * @param color The color to flash. See the list in screen.h.
 */
extern void statusFlashSelected(unsigned int color);

/**
 * Temporarily disable repainting. This increments an internal "suppression
 * counter" inside status. When the counter is non-zero the status window will
 * not repaint itself.
 */
extern void statusDisableRepaint();

/**
 * Enable repainting again. This decrements an internal "suppression
 * counter" inside status. When the counter is non-zero the status window will
 * not repaint itself.
 */
extern void statusEnableRepaint();

extern void statusPushMode(enum StatusMode mode);
extern void statusPopMode();
extern void statusSetSuperGenericData(struct stat_super_generic_data *data);

END_DECL

#endif
