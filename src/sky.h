/* Copyright (c) 2002 Gordon McNutt */
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
                int next_arc_turn;
                int next_phase_turn;
                int arc; /* 0 to 360 */
                int days_per_cycle;
                int turns_per_phase;
		void (*openMoongate) (int phase);
		void (*closeMoongate) (int phase);
        };

        struct sun {
                int next_arc_turn;
                int arc;
                struct sprite *sprite;
                int light;
        };

        struct clock {
                int hour, min, day, baseTurn;
        };

        extern struct moon_info MoonInfo;
        extern struct moon Moons[NUM_MOONS];
        extern struct sun Sun;
        extern struct clock Clock;

        extern void skyInit(void);
        extern void skyRepaint(void);
        extern void skyAdvanceTurns(void);

        extern void clockSet(void);
        extern void clockUpdate(void);

#ifdef __cplusplus
}
#endif

#endif
