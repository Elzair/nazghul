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
#ifndef sky_h
#define sky_h

#ifdef __cplusplus
extern "C" {
#endif

#include "common.h"

        struct moon_info {
                int phases;
                struct sprite **sprite;
                int turns;
        };

        struct moon {
                int phase;
                int arc;
                int light;
                int initial_phase;
                int initial_arc;
                int minutes_per_phase;
                int days_per_cycle;
		void (*openMoongate) (int phase);
		void (*closeMoongate) (int phase);
        };

        struct sun {
                int arc;
                int light;
                struct sprite *sprite;
        };

        extern struct moon_info MoonInfo;
        extern struct moon Moons[NUM_MOONS];
        extern struct sun Sun;
        extern struct clock Clock;

        extern void skyInit(void);
        extern void skyRepaint(void);
        extern void sky_advance(void);

        extern int sun_is_up   (void);
        extern int sun_is_down (void);
        extern int is_noon     (void);
        extern int is_midnight (void);

        extern int moon_is_visible (int arc);
        extern int sky_get_ambient_light(void);
        
#ifdef __cplusplus
}
#endif

#endif
