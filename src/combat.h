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

class Character;
struct place;

extern int combatInit(void);

extern char combatGetState(void);
extern bool combatAddNpcParty(class NpcParty * party, int dx, int dy,
			      bool located, int x, int y);
//extern void combatKIA(class Character *killed);

#include "player.h"		// for struct move_info
struct combat_info {
	int hours;		// for moving into camping
	class Character *guard;	// for moving into camping
	bool defend;		// for moving into combat
	bool camping;		// for moving into combat
	struct move_info *move;
};

extern bool combat_enter(struct combat_info *info);

#endif
