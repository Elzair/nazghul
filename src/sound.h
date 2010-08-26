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
#ifndef sound_h
#define sound_h

#include "macros.h"

BEGIN_DECL

#define NULL_SOUND NULL

struct sound;
typedef struct sound sound_t;

extern int SOUND_MAX_VOLUME;

extern int sound_init(void);
extern void sound_exit(void);
extern sound_t *sound_new(const char *tag, const char *file);
extern void sound_del(sound_t *sound);
extern void sound_play(sound_t *sound, int volume, bool ambient = false);
extern const char *sound_get_tag(sound_t *sound);
extern void sound_flush_ambient();
extern void sound_haltall();

/* These allow enabling/disabling sound at runtime. sound_on() will have no
 * effect unless sound_init() has already been called. */
extern void sound_on();
extern void sound_off();

/**
 * Turn sound on or off (duplicates sound_on/sound_off with a single
 * parameter).
 *
 * @param enable is non-zero to turn sound on, else it turns it off.
 */
extern void sound_enable(int enable);

/* Returns non-zero iff sounds are ready to play (ie sound_init() has been
 * called) */
extern int sound_is_activated();

extern void set_music_volume(const char *setting);
extern void music_load_track(const char *file);
extern bool music_need_track();
extern void music_init(void);

END_DECL

#endif

