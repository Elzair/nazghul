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
#ifndef clock_h
#define clock_h

#include "macros.h"

BEGIN_DECL

#define CLOCK_TICKS_PER_MINUTE TURNS_PER_MINUTE

#define clock_alarm_save(clk,save) ((save)->write((save), "%d\n", (clk)))

typedef unsigned int clock_alarm_t;

struct clock {
        int year;
        int month;
        int week;
        int day_w;  // Day of week (0..6)
        int day;    // Day of month (0..27)
        int hour;
        int min;
        int baseTurn;

        unsigned int total_minutes;
        int tick;
        int tick_to_change_time;
        int set : 1;
};

extern void clock_advance(int ticks);
extern void clock_alarm_set(clock_alarm_t *alarm, unsigned int minutes);
extern int clock_alarm_is_expired(clock_alarm_t *alarm);
extern int clock_alarm_remaining(clock_alarm_t *alarm);
extern unsigned int clock_time_of_day(void);
extern unsigned int clock_time(void);
extern int is_noon(void);
extern int is_midnight(void);
extern int clock_year(void);
extern int clock_month(void);
extern int clock_week(void);
extern int clock_day(void);
extern int clock_hour(void);
extern int clock_minute(void);
extern int clock_tick(void);

extern char * vague_time_as_string       (void);
extern char * time_HHMM_as_string       (void);
extern char * time_YYYY_MM_DD_as_string (void);

extern const char * month_name (void);
extern const char * week_name  (void);
extern const char * day_name   (void);


#ifdef INCLUDE_UNUSED_CLOCK_ROUTINES
extern void clock_reset(struct clock *clock);
extern void clock_set_alarm(struct clock *clock, struct clock *offset);
extern int clock_alarm_expired(struct clock *clock);
#endif

#ifdef OTHER_TIME_STRING_FUNCTIONS
extern const char * time_YYYY_as_string (void);
extern const char * time_MM_as_string   (void);
extern const char * time_DD_as_string   (void);
#endif // OTHER_TIME_STRING_FUNCTIONS
        
END_DECL

#endif
