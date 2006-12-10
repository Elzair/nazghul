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

// ----------------------------------------------------------------------------
// The ugly position_info struct is used by the algorithm which places party
// members on a map. It is unsightly, but not wholy foul, and will likely live
// on for a while until the placement algorithm is completely revisited.
// ----------------------------------------------------------------------------

struct position_info {

        struct place *place; // The place where the members are getting
                             // distributed.

        int x, y;            // The "home position". The placement rectangle
                             // (see below) will be centered on this
                             // location. If the placement algorithm fails to
                             // find a safe place to put a member (due to
                             // passability, etc) it will put the member
                             // here. In extreme cases the whole party may get
                             // stacked on this one location.

        int dx, dy;          // The orientation vector (most formations are
                             // directional). Typically set to the last
                             // direction the party was travelling in.

        int rx, ry, rw, rh;  // The placement rectangle.

        int px, py;          // The initial preferred location of a party
                             // member (this is set just before running the
                             // placement algorithm on a member). This depends
                             // on the "order" of the member in the party, the
                             // formation, and the direction vector.

        class Object *subject; // Use the subject being positioned instead of
                               // the obsolete pmask.

        bool find_party;     // When running the placement algorithm, true iff
                             // the party member must be able to pathfind back
                             // to the party coordinates.

        int placed;          // Count of the number of party members actually
                             // placed. Used to detect when none of a party
                             // found a location.

        struct formation *formation;     // The formation to use when placing
                                         // party members.


};

#endif
