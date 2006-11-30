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
#ifndef ctrl_h
#define ctrl_h

extern void ctrl_party_ui(class PlayerParty *party);
extern void ctrl_party_ai(class Party *party);
extern void ctrl_character_ui(class Character *character);
extern void ctrl_character_ai(class Character *character);
extern void ctrl_wander(class Object *obj);
extern void ctrl_do_attack(class Character *character, class ArmsType *weapon, 
                           class Character *target, int penalty);

#endif
