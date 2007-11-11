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
#ifndef combat_h
#define combat_h

#define COMBAT_MAP_W    19
#define COMBAT_MAP_H    19

class Character;

extern int combatInit(void);
extern int combatLoad(class Loader *loader);
extern char combatGetState(void);

/* This is needed for walking around while zoomed out. Currently only the
   special ^Z command (which is intended for map editing) uses it. */
extern void combatGetCameraCenter(int *x, int *y);


#include "player.h"		// for struct move_info
struct combat_info {
	int hours;		// for moving into camping
	class Character *guard;	// for moving into camping
	bool defend;		// for moving into combat- true if PC is defending
	bool camping;		// for moving into combat
	struct move_info *move;
	int pc_x;	// cache for final location of player party
	int pc_y;
	int npc_x;	// cache for final location of npc party
	int npc_y;
	int pc_dx;  // cache for final delta
	int pc_dy;
};

enum combat_state {
        COMBAT_STATE_FIGHTING,
        COMBAT_STATE_LOOTING,
        COMBAT_STATE_CAMPING,
        COMBAT_STATE_DONE
};

extern enum combat_state combat_get_state(void);
extern void combat_set_state(enum combat_state);
extern void combat_reset_state(void);
extern bool combat_enter(struct combat_info *info);
extern bool combat_rendezvous_party(int max_path_len);
extern void combat_exit(void);
extern void combat_analyze_results_of_last_turn(void);
extern void combat_fill_position_info(struct position_info *info, struct place *place, int x, int y, int dx, int dy, bool defend);
extern bool combat_place_character(class Character * pm, void *data);
extern int combat_add_party(class Party *party, int dx, int dy, int located,
                            struct place *place, int x, int y);

#endif
