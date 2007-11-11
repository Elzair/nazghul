/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2002, 2003, 2004 Gordon McNutt
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 *
 *----------------------------------------------------------------------------
 *
 * This file implements the sound-playing library for the nazghul engine. It
 * wraps the SDL sound library and provides a simple mixer. The code which
 * interacts with the SDL sound library is borrowed straight from the SDL
 * examples, which are in the public domain (see www.libsdl.org).
 *
 * Gordon McNutt
 * gmcnutt@users.sourceforge.net
 */

#include "sound.h"
#include "debug.h"
#include "file.h"
#include "cfg.h"

#include <assert.h>
#include <SDL.h>
#include <stdlib.h>
#include <string.h>
#include <SDL_mixer.h>

#define NUM_SOUNDS 64
#define SOUND_MAGIC "sound"

#define IS_SOUND(ptr) ((ptr)->magic == SOUND_MAGIC)


struct sound {
        char *magic;
	     char *tag;
        Mix_Chunk *data;
        int channel;
        //char *fname;
        int refcount;
        bool ambient;
        int volume;
        int nextVolume;
};

sound_t *sound_reverse_lookup[NUM_SOUNDS];

int SOUND_MAX_VOLUME = SDL_MIX_MAXVOLUME;

/* This indicates whether or not the player has turned sound on or off. */
static int sound_enabled = 1;

/* This is 1 iff SDL_Audio() is initialized and ready for use. */
static int sound_activated = 0;

static SDL_mutex *sound_mutex = NULL;

static void sound_unref(sound_t *sound)
{
	assert(sound->refcount > 0);
	
	sound->refcount--;
	if (! sound->refcount)
	{
		// make sure it isnt being played
		if (sound->channel >= 0)
		{
			Mix_HaltChannel(sound->channel);
		}
		free(sound->tag);
		Mix_FreeChunk(sound->data);
		free(sound);
	}
	
}

void sound_play(sound_t *sound, int volume, bool ambient)
{


	if (!sound_enabled || !sound_activated) {              
		return;
	}	

	if (NULL_SOUND == sound)
	{         
	    return;
	}
	assert(IS_SOUND(sound)); 
	
	if (sound->channel < 0)
	{
		if (ambient)
		{
			sound->channel = Mix_PlayChannel(-1, sound->data,-1);
			Mix_VolumeChunk(sound->data,volume);
			sound->volume = volume;
			sound->ambient = true;
			sound->nextVolume = volume;
		}
		else
		{
			sound->channel = Mix_PlayChannel(-1, sound->data,0);
			Mix_VolumeChunk(sound->data,volume);
			sound->volume = volume;
			sound->ambient = false;
			sound->nextVolume = 0;
		}		
		sound->refcount++;
		sound_reverse_lookup[sound->channel] = sound;		
	}
	else if (ambient)
	{
		if (sound->volume < volume)
		{
			Mix_VolumeChunk(sound->data,volume);
			sound->volume = volume;
		}
		if (sound->nextVolume < volume)
		{
			sound->nextVolume = volume;
		}
	}
	else
	{
		if (sound->volume < volume)
		{
			sound->volume = volume;
			Mix_VolumeChunk(sound->data,volume);
		}
	}
}

void sound_played(int channel)
{
	sound_t *sound = sound_reverse_lookup[channel];
	sound->channel=-1;
	sound->volume=0;
	sound->refcount--;
}

void sound_flush_ambient()
{
	unsigned int i;
	sound_t *active;
	
	for (i = 0; i < NUM_SOUNDS; ++i)
	{
		active = sound_reverse_lookup[i];
		
		// Skip idle or oneoff entries
		if (! active || !active->ambient)
			continue;
		
		active->volume = active->nextVolume;
		active->nextVolume = 0;
		Mix_VolumeChunk(active->data,active->volume);
	}
}

void sound_del(sound_t *sound)
{
        if (sound != NULL_SOUND)
                sound_unref(sound);
}

sound_t *sound_new(char *tag, char *file)
{
   sound_t *sound;
	char *fn;
	Mix_Chunk *wave = NULL;

	if (!sound_activated) {
		return NULL_SOUND;
	}

        if (file == NULL)
                return NULL_SOUND;

	fn = file_mkpath(cfg_get("include-dirname"), file);
	/* Load the sound file and convert it to 16-bit stereo at 22kHz */
	
	wave = Mix_LoadWAV(fn?fn:file);
	if (!wave)
	{
      warn("Mix_LoadWav:%s:%s", fn?fn:file, SDL_GetError());
      	free(fn);
		return NULL_SOUND;
	}
	free(fn);

        /* Allocate the sound structure */
        sound = (sound_t *)calloc(1, sizeof(*sound));
        assert(sound);
        
        /* Copy the tag */
        sound->tag = strdup(tag);
        assert(sound->tag);

        /* Initialized defaults */
        sound->magic = SOUND_MAGIC;
        sound->refcount = 1;
        sound->channel = -1;
        sound->data = wave;
        
        return sound;
}

int sound_init(void)
{

        if (sound_activated)
                return 0;

        /* Init the active sound list */

	/* Set 16-bit stereo audio at 22Khz */
	/* Open the audio device and start playing sound! */
	if (Mix_OpenAudio(22050,AUDIO_S16,2,1024) < 0) {
          warn("Mix_OpenAudio: %s", SDL_GetError());
		return -1;
	}
	
        /* Create the mutex */


	atexit(Mix_CloseAudio);

	sound_activated = 1;

	for (int i=0;i<NUM_SOUNDS;i++)
	{
		sound_reverse_lookup[i]=NULL;	
	}
	Mix_AllocateChannels(NUM_SOUNDS);
	Mix_ChannelFinished(sound_played);

        return 0;
}

void sound_exit(void)
{
        if (! sound_activated)
                return;

        sound_activated = 0;

        /* Does this invoke the mixer callback on all active sounds? If not
         * then how will I unref active sounds? */
        Mix_CloseAudio();
}

void sound_haltall()
{
	//Mix_HaltChannel(-1);
}

char *sound_get_tag(sound_t *sound)
{
        return sound->tag;
}

void sound_on(void)
{
        sound_enabled = 1;
}

void sound_off(void)
{
        sound_enabled = 0;
}

int sound_is_activated(void)
{
        return sound_activated;
}

//////////////////////////////////////////////////////////////////////////////
// Music API

Mix_Music *music_track;

void music_load_track(char *file)
{
	Mix_Music *prev_track = NULL;
	if (Mix_PlayingMusic())
	{
		Mix_HaltMusic();
		Mix_FadeOutMusic(3000);
		prev_track = music_track;
	}
	char *fn;
	fn = file_mkpath(cfg_get("include-dirname"), file);
	music_track=Mix_LoadMUS(fn?fn:file);
	if (music_track)
	{
		Mix_PlayMusic(music_track,-1);
	}
	else
	{
      warn("Mix_LoadMusic:%s:%s\n", fn?fn:file, SDL_GetError());
	}	
	free(fn);
	if (prev_track)
	{
		Mix_FreeMusic(prev_track);
	}
}


