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

#define NULL_SOUND NULL

struct sound;
typedef struct sound sound_t;

extern int SOUND_MAX_VOLUME;

extern int sound_init(void);
extern void sound_exit(void);
extern sound_t *sound_new(char *tag, char *file);
extern void sound_del(sound_t *sound);
extern void sound_play(sound_t *sound, int volume);
extern char *sound_get_tag(sound_t *sound);

#endif
