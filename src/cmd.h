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

class Character;


#ifdef __cplusplus
extern "C" {
#endif

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

        extern bool dirkey(struct KeyHandler *kh, int key, int keymod);
        extern bool yesnokey(struct KeyHandler *kh, int key, int keymod);
        extern bool anykey(struct KeyHandler *kh, int key, int keymod);
        extern bool scroller(struct KeyHandler *kh, int key, int keymod);
        extern bool movecursor(struct KeyHandler *kh, int key, int keymod);
        extern bool getnum(struct KeyHandler *kh, int key, int keymod);
        extern void getkey(void *data, bool(*handler) (struct KeyHandler * kh,
                                                       int key, int keymod));
        extern int num_for_key (int key);

#define CMD_SELECT_MEMBER (1 << 0)
#define CMD_PRINT_MEMBER  (1 << 1)
  
        extern void cmdAttack(void);
        extern int cmdCamp(void);
        extern void cmdFire(void);
        extern bool cmdMixReagents(void);
        extern void cmdNewOrder(void);
        extern bool cmdSearch(int x, int y);
        extern bool cmdTalk(int x, int y);
        extern void cmdZoomIn(void);
        extern bool cmdUse(class Character * pc, int flags);
        extern bool cmdHandle(class Character * pc);
        extern bool cmdReady(class Character * pc, int flags);
        extern bool cmdZtats(class Character * pc);
        extern bool cmdXamine (class Character * pc);
        extern bool cmdAT(class Character * pc);
        extern bool cmdGet(int x, int y, bool scoop_all);
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
        typedef void (*v_funcpointer_ii) (int x, int y);
        extern int select_target_with_doing (int ox, int oy, int *x, int *y, 
                                             int range, 
                                             v_funcpointer_ii each_point_func,
                                             v_funcpointer_ii each_target_func);
        extern int select_quantity(int max);
  
        // the new ui api
        extern int ui_get_direction(void);



#ifdef __cplusplus
}
#endif

#endif
