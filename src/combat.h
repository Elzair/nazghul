/* Copyright (c) 2002 Gordon McNutt */
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
