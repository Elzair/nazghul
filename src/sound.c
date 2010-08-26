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
#define SOUND_MAGIC 50043

#define IS_SOUND(ptr) ((ptr)->magic == SOUND_MAGIC)


struct sound {
        int magic;
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

static int config_to_soundvolume(const char* config)
{
	//our input percentages have nicely unique initial characters
	const char* comp="O2571";
	int i;
	for (i=0;i<5;i++)
	{
		if (config[0] == comp[i])
		{
			break;	
		}
	}
	if (i>4)
	{
		i=0;	
	}
	return i;
}

static void sound_unref(sound_t *sound)
{
	assert(sound->refcount > 0);
	
	sound->refcount--;
	if (! sound->refcount)
	{
		// make sure it isnt being played
		if (sound->channel >= 0)
		{
			// it *shouldnt* be being played, so lets die if it is.
			assert(false);
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
	sound_reverse_lookup[channel] = NULL;
	sound->volume=0;
	sound_unref(sound);
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

sound_t *sound_new(const char *tag, const char *file)
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

const char *sound_get_tag(sound_t *sound)
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
Mix_Music *prev_track = NULL;

int music_volume=0;
bool music_needtrack=false;

//setting must be one of Off 25% 50% 75% 100%
void set_music_volume(const char *setting)
{
	music_volume = config_to_soundvolume(setting);
	fprintf(stderr, "vol: %s\n", setting);
	fprintf(stderr, "vol: %d\n", music_volume);
	int mvm = MIX_MAX_VOLUME * music_volume / 4;
	fprintf(stderr, "vol: %d\n", mvm);
	Mix_VolumeMusic(mvm);
	fprintf(stderr, "vol?: %d\n", Mix_VolumeMusic(-1));
}

void music_load_track(const char *file)
{
	// nasty hack:
	// SDL mixer seems to have some rather odd timing requirements.
	// For example, I cant stop a track and immediately dispose of it
	// so instead I will stop this track, and dispose of one I stopped earlier
	if (prev_track)
	{
		Mix_FreeMusic(prev_track);
		prev_track = NULL;
	}
	if (music_volume==0)
	{
		music_needtrack=false;
		return;	
	}
	if (Mix_PlayingMusic())
	{
		Mix_HaltMusic();
		prev_track = music_track;
	}
	char *fn = NULL;
	if (!file_exists(file))
	{
		fn = file_mkpath(cfg_get("include-dirname"), file);
	}
	music_track=Mix_LoadMUS(fn?fn:file);
	if (music_track)
	{
		Mix_PlayMusic(music_track,1);
	}
	else
	{
      warn("Mix_LoadMusic:%s:%s\n", fn?fn:file, SDL_GetError());
	}	
	music_needtrack=false;
	free(fn);
}

void music_finished()
{
	music_needtrack=true;
}

// SDL_Mixer has a music-has-finished hook, but  you arent allowed to do anything
// involving SDL_Mixer in that thread, so I am reduced to polling...
bool music_need_track()
{
	return music_needtrack;	
}

void music_init()
{
	Mix_HookMusicFinished(music_finished);
}

void sound_enable(int enable)
{
        if (enable) {
                sound_on();
        } else {
                sound_off();
        }
}
