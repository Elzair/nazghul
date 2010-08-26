//
// nazghul - an old-school RPG engine
// Copyright (C) 2002, 2003 Gordon McNutt
//
// Thi program is free software; you can redistribute it and/or modify it
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
#include "sky.h"
#include "clock.h"
#include "screen.h"
#include "common.h"
#include "sprite.h"
#include "place.h"
#include "map.h"
#include "player.h"
#include "wq.h"
#include "session.h"

#include <assert.h>
#include <math.h>

#define Clock (Session->clock)

int clock_year(void)
{
        return Clock.year;
}

int clock_month(void)
{
        return Clock.month;
}

int clock_week(void)
{
        return Clock.week;
}

int clock_day(void)
{
        return Clock.day;
}

int clock_hour(void)
{
        return Clock.hour;
}

int clock_minute(void)
{
        return Clock.min;
}

int clock_tick(void)
{
        return Clock.tick;
}

unsigned int clock_time_of_day(void)
{
        return (Clock.hour * 60 + Clock.min);
}

unsigned int clock_time(void)
{
        return Clock.total_minutes;
}

void clock_advance(int ticks)
{
        assert(Clock.tick < Clock.tick_to_change_time);

        while (ticks-- > 0) {

                Clock.tick++;
                
                if (Clock.tick == Clock.tick_to_change_time) {
                        
                        Clock.total_minutes++;
                        Clock.tick = 0;
                        Clock.tick_to_change_time = CLOCK_TICKS_PER_MINUTE;
                        
                        Clock.min++;
                        if (Clock.min == MINUTES_PER_HOUR) {
                                Clock.hour++;
                                Clock.min = 0;
                                if (Clock.hour == HOURS_PER_DAY) {
                                        Clock.day++;
                                        Clock.hour = 0;
                                        if (Clock.day == DAYS_PER_WEEK) {
                                                Clock.week++;
                                                Clock.day = 0;
                                                if (Clock.week == WEEKS_PER_MONTH) {
                                                        Clock.month++;
                                                        Clock.week = 0;
                                                        if (Clock.month == MONTHS_PER_YEAR) {
                                                                Clock.year++;
                                                                Clock.month = 0;
                                                        }
                                                }
                                        }
                                }
                        }
                }
        }
                
	mapRepaintClock();
}

char *time_HHMM_as_string(void)
{
	static char str[] = "HH:MMPM";
	static int maxlen = strlen("HH:MMPM") + 1;
	int hr = Clock.hour;
	int min = Clock.min;
	int n;

	hr = (hr > 12) ? (hr - 12) : hr;
	hr = (hr == 0) ? 12 : hr;

	n = snprintf(str, maxlen, "%2d:%02d%2s",
		     hr, min, (Clock.hour >= 12) ? "PM" : "AM");
	assert(n != -1);
	return str;
}				// time_HHMM_as_string()

char *vague_time_as_string(void)
{
	static char str[] = "late afternoon";
	static int maxlen = strlen("late afternoon") + 1;
	int hr = Clock.hour;
	int n;

	if (hr < 4)
	{
		n = snprintf(str, maxlen, "night");
	}
	else if (hr < 7)
	{
		n = snprintf(str, maxlen, "early morning");	
	}
	else if (hr < 11)
	{
		n = snprintf(str, maxlen, "morning");	
	}
	else if (hr < 13)
	{
		n = snprintf(str, maxlen, "noon");	
	}
	else if (hr < 15)
	{
		n = snprintf(str, maxlen, "afternoon");	
	}
	else if (hr < 18)
	{
		n = snprintf(str, maxlen, "late afternoon");	
	}
	else if (hr < 20)
	{
		n = snprintf(str, maxlen, "evening");	
	}
	else
	{
		n = snprintf(str, maxlen, "night");	
	}
	assert(n != -1);
	return str;
}				// vague_time_as_string()

char *time_YYYY_MM_DD_as_string(void)
{
	static char str[] = "YYYY/MM/DD";
	static int maxlen = strlen("YYYY/MM/DD") + 1;
	int n = snprintf(str, maxlen, "%04d/%02d/%02d",
			 Clock.year, Clock.month, Clock.day);
	assert(n != -1);
	return str;
}				// time_YYYY_MM_DD_as_string()

#ifdef OTHER_TIME_STRING_FUNCTIONS
char *time_YYYY_as_string(void)
{
	static char str[] = "YYYY";
	static int maxlen = strlen("YYYY") + 1;
	int n = snprintf(str, maxlen, "%4d", Clock.year);
	assert(n != -1);
	return str;
}

char *time_MM_as_string(void)
{
	static char str[] = "MM";
	static int maxlen = strlen("MM") + 1;
	int n = snprintf(str, maxlen, "%2d", Clock.month);
	assert(n != -1);
	return str;
}

char *time_DD_as_string(void)
{
	static char str[] = "DD";
	static int maxlen = strlen("DD") + 1;
	int n = snprintf(str, maxlen, "%2d", Clock.day);
	assert(n != -1);
	return str;
}
#endif				// OTHER_TIME_STRING_FUNCTIONS

// SAM: 
// A proper implementation of 
// month_name(), week_name(), day_name()
// will wait until we have GhulScript
// for week and month names and such.
// 
const char *month_name(void)
{
	int month = Clock.month;
	switch (month) {
	case 0:
		return "1st Month";
	case 1:
		return "2nd Month";
	case 2:
		return "3rd Month";
	case 3:
		return "4th Month";
	case 4:
		return "5th Month";
	case 5:
		return "6th Month";
	case 6:
		return "7th Month";
	case 7:
		return "8th Month";
	case 8:
		return "9th Month";
	case 9:
		return "10th Month";
	case 10:
		return "11th Month";
	case 11:
		return "12th Month";
	default:
		assert(0);
	}
}				// month_name()

const char *week_name(void)
{
	int week = Clock.week;
	switch (week) {
	case 0:
		return "1st Week";
	case 1:
		return "2nd Week";
	case 2:
		return "3rd Week";
	case 3:
		return "4th Week";
	default:
		assert(0);
	}
}				// week_name()

const char *day_name(void)
{
	int day = Clock.day;
	switch (day) {
	case 0:
		return "1st Day";
	case 1:
		return "2nd Day";
	case 2:
		return "3rd Day";
	case 3:
		return "4th Day";
	case 4:
		return "5th Day";
	case 5:
		return "6th Day";
	case 6:
		return "7th Day";
	default:
		assert(0);
	}
}				// day_name()

void clock_alarm_set(clock_alarm_t *alarm, unsigned int minutes)
{
        *alarm = Clock.total_minutes + minutes;
}

int clock_alarm_is_expired(clock_alarm_t *alarm)
{
        return (Clock.total_minutes >= *alarm);
}

int clock_alarm_remaining(clock_alarm_t *alarm)
{
	if (Clock.total_minutes >= *alarm) return 0;
	return (*alarm - Clock.total_minutes);
}

int is_noon(void)
{
        return (Clock.hour == 12 && Clock.min == 0);
}

int is_midnight(void)
{
        return (Clock.hour == 0 && Clock.min == 0);
}

#ifdef INCLUDE_UNUSED_CLOCK_ROUTINES
void clock_reset(struct clock *clock)
{
        memset(clock, 0, sizeof(*clock));
}

void clock_set_alarm(struct clock *clock, struct clock *offset)
{

        /* Copy the current time */
        memcpy(clock, &Clock, sizeof(*clock));

        /* Set the alarm to the current time plus the offset */
        clock->min += offset->min;
        if (clock->min >= MINUTES_PER_HOUR) {
                offset->hour += clock->min / MINUTES_PER_HOUR;
                clock->min %= MINUTES_PER_HOUR;
        }

        clock->hour += offset->hour;
        if (clock->hour >= HOURS_PER_DAY) {
                offset->day += clock->hour / HOURS_PER_DAY;
                clock->hour %= HOURS_PER_DAY;
        }

        clock->day += offset->day;
        if (clock->day >= DAYS_PER_WEEK) {
                offset->week += clock->day / DAYS_PER_WEEK;
                clock->day %= DAYS_PER_WEEK;
        }

        clock->week += offset->week;
        if (clock->week >= WEEKS_PER_MONTH) {
                offset->month += clock->week / WEEKS_PER_MONTH;
                clock->week %= WEEKS_PER_MONTH;
        }

        clock->month += offset->month;
        if (clock->month >= MONTHS_PER_YEAR) {
                offset->year += clock->month / MONTHS_PER_YEAR;
                clock->month %= MONTHS_PER_YEAR;
        }
        
        clock->year += offset->year;

        consolePrint("Set alarm for year %d, month %d, week %d, day %d, "
                     "hour %d, min %d\n", clock->year, clock->month,
                     clock->week, clock->day, clock->hour, clock->min);
}

int clock_alarm_expired(struct clock *clock)
{
        int total_minute;

        total_minute  = (clock->year  - Clock.year)  * MINUTES_PER_YEAR;
        total_minute += (clock->month - Clock.month) * MINUTES_PER_MONTH;
        total_minute += (clock->week  - Clock.week)  * MINUTES_PER_WEEK;
        total_minute += (clock->day   - Clock.day)   * MINUTES_PER_DAY;
        total_minute += (clock->hour  - Clock.hour)  * MINUTES_PER_HOUR;
        total_minute += (clock->min   - Clock.min);

        return (total_minute <= 0);
}
#endif // INCLUDE_UNUSED_CLOCK_ROUTINES
