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
#ifndef pinfo_h
#define pinfo_h

struct position_info {

        // The coordinates of the 'party'; the center of the placement
        // rectangle for a party.
        int x, y;

        // The direction vector for the party.
        int dx, dy;

        // The placement rectangle. Upon entry to combat all party members must
        // be placed somewhere in this rectangle or they will not be placed at
        // all.
        int rx, ry, rw, rh;

        // When running the placement algorithm, the initial preferred location
        // of a party member.
        int px, py;

        // When running the placement algorithm, the pmask of the party member
        // being placed.
        int pmask;

        // When running the placement algorithm, true iff the party member must
        // be able to pathfind back to the edge.
        bool find_edge;

        // When running the placement algorithm, true iff the party member
        // must be able to pathfind back to the party coordinates.
        bool find_party;

        // Count of the number of party members actually placed. Used to detect
        // when none of a party found a location.
        int placed;

        // The formation to use when placing party members.
        struct formation *formation;

};

#endif
