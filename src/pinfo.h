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
