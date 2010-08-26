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

#ifndef factions_h
#define factions_h

#include "session.h"
#include "dtable.h"
#include "Being.h"

#define INVALID_HANDLE      -1
#define INVALID_FACTION     -1
#define NIL_FACTION          0
#define PLAYER_PARTY_FACTION 1

static inline int are_hostile(Being *a, Being *b)
{
        return dtable_are_hostile(session_dtable(),
                                  a->getCurrentFaction(),
                                  b->getCurrentFaction());
}

static inline int are_natively_hostile(Being *a, Being *b)
{
        return dtable_are_hostile(session_dtable(),
                                  a->getBaseFaction(),
                                  b->getBaseFaction());
}

static inline int are_allies(Being *a, Being *b)
{
        return dtable_are_allies(session_dtable(),
                                  a->getCurrentFaction(),
                                  b->getCurrentFaction());
}

static inline void make_hostile(Being *a, Being *b)
{
        dtable_set(session_dtable(),
                   a->getCurrentFaction(),
                   b->getCurrentFaction(),
                   dtable_hostile(session_dtable()));
}

static inline void make_allies(Being *a, Being *b)
{
        dtable_set(session_dtable(),
                   a->getCurrentFaction(),
                   b->getCurrentFaction(),
                   dtable_allies(session_dtable()));
}

static inline void improve_relations(Being *a, Being *b)
{
        dtable_inc(session_dtable(),
                   a->getCurrentFaction(),
                   b->getCurrentFaction());
}

static inline void harm_relations(Being *a, Being *b)
{		
	if (a->getCurrentFaction() != b->getCurrentFaction())
	{
        dtable_dec(session_dtable(),
                   a->getCurrentFaction(),
                   b->getCurrentFaction());
	}
}

static inline const char * diplomacy_string(Being *a, Being *b)
{
        return dtable_describe(session_dtable(),
                               a->getCurrentFaction(),
                               b->getCurrentFaction());
}

#endif
