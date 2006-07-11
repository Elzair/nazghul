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

enum StatusScrollDir {
        ScrollUp,
        ScrollDown,
        ScrollRight,
        ScrollLeft,
        ScrollPageUp,
        ScrollPageDown
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
        DisableStatus
};
					
enum StatusSelection {
        Character,
        InventoryItem,
        TradeItem,
        Reagents,
        Generic,
        String
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
extern void statusSetGenericList(int list_sz, struct stat_list_entry *list);
extern void statusUpdateGenericList(int list_sz, struct stat_list_entr *list);
extern void statusSetStringList(int list_sz, char **list);

extern int status_get_h(void);
        
END_DECL

#endif
