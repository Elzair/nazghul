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

class Character;
class Object;
class Party;

BEGIN_DECL;

// I really shouldn't pollute this interface with these, but they don't really
// fit anywhere else right now, either.
#include "status.h"
struct ScrollerContext {
        enum StatusSelection selector;
        void *selection;
        bool abort;
        bool done;
        bool mixing; // for mix reagents
};

extern int dirkey(struct KeyHandler *kh, int key, int keymod);
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
extern int cmdCampInTown(class Character *camper);
extern int cmdCampInWilderness(class Party *camper);
extern void cmdFire(void);
extern bool cmdMixReagents(void);
extern void cmdNewOrder(void);
extern void cmdQuickSave(void);
extern void cmdReload(void);
extern bool cmdSearch(struct place *place, int x, int y);
extern bool cmdTalk(Object *member);
extern void cmdDumpPalette(void);
extern void cmdZoomIn(void);
extern bool cmdUse(class Character * pc, int flags);
extern bool cmdHandle(class Character * pc);
extern bool cmdReady(class Character * pc);
extern bool cmdZtats(class Character * pc);
extern bool cmdXamine (class Object *examiner);
extern bool cmdYuse(class Character *pc);
extern bool cmdAT(class Character * pc);
extern bool cmdGet(class Object *actor, bool scoop_all);
extern bool cmdOpen(class Character * pc);
extern bool cmdCastSpell(class Character * pc);
extern bool cmdQuit(void);
extern bool cmdLook(int x, int y);
extern bool cmdTerraform(class Character * pc);
extern bool cmdSaveTerrainMap(class Character * pc);
  
extern class Character *select_party_member(void);
    
extern int select_target(int ox, int oy, int *x, int *y, int range);

extern char * name_of_context (void);

// SAM: This typedef also exists in event.h
extern int select_target_with_doing (int ox, int oy, int *x, int *y, 
                                     int range, 
                                     v_funcpointer_ii each_point_func,
                                     v_funcpointer_ii each_target_func);
extern int select_quantity(int max);
  
// the new ui api
extern int ui_get_direction(void);
extern int ui_get_yes_no(char *asked_persons_name);
extern int ui_getline(char *buf, int len);

struct merchant {
        char *name;
        int n_trades;
        struct trade_info *trades;
};

extern void ui_trade(struct merchant *merch);

END_DECL;

#endif
