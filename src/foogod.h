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
#ifndef foogod_h
#define foogod_h

#include "macros.h"

BEGIN_DECL

/* Food-Gold-Date window */
typedef enum {
        FOOGOD_DEFAULT = 0
        , FOOGOD_HINT
        , FOOGOD_PROGRESS_BAR
} foogod_mode_t;

typedef enum {
        FOOGOD_QUICKEN = 0,
        FOOGOD_REVEAL,
        FOOGOD_MAGIC_NEGATED,
        FOOGOD_TIME_STOP,
        FOOGOD_XRAY_VISION,
        FOOGOD_NUM_EFFECTS
} foogod_effect_t;

extern int foogodInit(void);
extern void foogodRepaint(void);
extern void foogodAdvanceTurns();
extern void foogod_set_y(int y);
extern int foogod_get_y(void);
extern int foogod_get_h(void);

extern void foogodSetMode(foogod_mode_t mode);
extern void foogodSetHintText(const char *text);

extern void foogod_progress_bar_set_title(const char *title);
extern void foogod_progress_bar_set_max_steps(unsigned int val);
extern void foogod_progress_bar_advance(unsigned int steps);
extern void foogod_progress_bar_finish(void);
extern void foogod_set_title(const char *fmt, ...);

END_DECL

#endif
