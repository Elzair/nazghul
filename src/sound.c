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

/*struct active_sound {
	Uint8 *data;
	Uint32 dpos;
	Uint32 dlen;
        int oneoffVolume;   // one off sound (minimum sound over gameturns)
        int repeatVolume;   // max sound to repeatedly play now
        int nextVolume;	    // max sound queued for next turn
        sound_t *sound;
} active_sounds[NUM_SOUNDS];*/

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
	
	/* what if this fails? */
	SDL_LockMutex(sound_mutex);
	
	sound->refcount--;
	if (! sound->refcount)
	{
		fprintf(stderr,"Del sound: %d\n",(int)sound);
		// make sure it isnt being played
		if (sound->channel >= 0)
		{
			Mix_HaltChannel(sound->channel);
		}
		//free(sound->cvt.buf);
		free(sound->tag);
		Mix_FreeChunk(sound->data);
		free(sound);
	}
	
	/* what if this fails? */
	SDL_UnlockMutex(sound_mutex);
}

static void sound_mix(void *unused, Uint8 * stream, int len)
{
	/*unsigned int i;
	int amount;
        struct active_sound *active;
        int volume;

	for (i = 0; i < NUM_SOUNDS; ++i) {

                active = &active_sounds[i];

                // Skip idle entries
                if (! active->sound)
                        continue;

                // Calculate how many more bytes can be played
		amount = (active->dlen - active->dpos);
		
                // If done then stop playing 
                if (0 == amount) {
	                if (active->repeatVolume == 0)
	                {
                        sound_unref(active->sound);
                        active->sound = 0;
                        continue;
                	}
                	else
                	{
	                	  active->oneoffVolume=0;
	               	  active->dpos = 0;
	               	  amount = active->dlen;
                	}
                }
                
      if (active->oneoffVolume > active->repeatVolume)
      {
	   	volume = active->oneoffVolume;   
      }
      else
      {
	     	volume = active->repeatVolume;
   	}

                // Clip to the number of bytes allowed
		if (amount > len) {
			amount = len;
		}

                // Tell SDL to continue playing
		SDL_MixAudio(stream, &active->data[active->dpos], amount,
			     volume);

		active->dpos += amount;
	}*/
	
	
}

void sound_play(sound_t *sound, int volume, bool ambient)
{
	/*int index;
        struct active_sound *active = NULL;

	if (!sound_enabled || !sound_activated) {                
		return;
	}

        if (NULL_SOUND == sound)
                return;

        assert(IS_SOUND(sound));
        
	// Look for an empty (or finished) sound slot, or if the sound is already playing
	for (index = 0; index < NUM_SOUNDS; ++index) {
		if (active_sounds[index].sound == sound)
		{
			// dont repeat playing a sound, but up the volume to match our volume
			if (ambient)
			{
				if (active_sounds[index].repeatVolume < volume)
				{
					active_sounds[index].repeatVolume = volume;
				}
				if (active_sounds[index].nextVolume < volume)
				{
					active_sounds[index].nextVolume = volume;
				}
			}
			else
			{
				if (active_sounds[index].oneoffVolume < volume)
					active_sounds[index].oneoffVolume = volume;
			}
			return;
		}
		if (! active_sounds[index].sound && active == NULL) {
                        active = &active_sounds[index];
		}
	}
	if (NULL == active)
		return;

	// Put the sound data in the slot (it starts playing immediately) 
	SDL_LockAudio();
        sound->refcount++;
        active->sound = sound;
	active->data = sound->cvt.buf;
	active->dlen = sound->cvt.len_cvt;
	active->dpos = 0;
         if (ambient)
         {
	      	active->repeatVolume=volume;
	      	active->nextVolume=volume;   
	      	active->oneoffVolume=0;
      	}
      	else
      	{
	      	active->repeatVolume=0;
	      	active->nextVolume=0;
	      	active->oneoffVolume=volume;      	
      	}
	SDL_UnlockAudio();*/
	fprintf(stderr,"playsound\n");

	if (!sound_enabled || !sound_activated) {   
		fprintf(stderr,"playsound- nosound\n");             
		return;
	}	
		fprintf(stderr,"playsound a\n");
	if (NULL_SOUND == sound)
	{
		fprintf(stderr,"playsound- nullsound\n");             
	    return;
	}
		fprintf(stderr,"playsound b\n");
	assert(IS_SOUND(sound)); 
	    	fprintf(stderr,"playsound c\n");
	fprintf(stderr,"playsound! (%d)\n",sound->channel);
	
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
		fprintf(stderr,"playing sound %d\n",sound->channel);
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
	fprintf(stderr,"played sound %d\n",sound->channel);
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
	//SDL_AudioSpec wave;
	//Uint8 *data;
	//Uint32 dlen;
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
        
        /* Initialize the SDL convert structure */
	//SDL_BuildAudioCVT(&sound->cvt, wave.format, wave.channels, wave.freq,
			 // AUDIO_S16, 2, 22050);

        /* Allocate the sound buffer */
	//sound->cvt.buf = (Uint8 *) malloc(dlen * sound->cvt.len_mult);
       // assert(sound->cvt.buf);

        /* Copy the data into the sound buffer */
	//memcpy(sound->cvt.buf, data, dlen);
	//sound->cvt.len = dlen;
	//SDL_ConvertAudio(&sound->cvt);

        /* Release the memory holding the original WAV file */
        //SDL_FreeWAV(data);
        
		fprintf(stderr,"New sound: %s @ %d\n",tag,(int)sound);
        return sound;
}

int sound_init(void)
{
	SDL_AudioSpec fmt;

        if (sound_activated)
                return 0;

        /* Init the active sound list */
       // memset(active_sounds, 0, sizeof(active_sounds));

	/* Set 16-bit stereo audio at 22Khz */
	fmt.freq = 22050;
	fmt.format = AUDIO_S16;
	fmt.channels = 2;
	fmt.samples = 1024;	/* A good value for games */
	fmt.callback = sound_mix;
	fmt.userdata = NULL;

	/* Open the audio device and start playing sound! */
	if (Mix_OpenAudio(fmt.freq,fmt.format,fmt.channels,fmt.samples) < 0) {
          warn("Mix_OpenAudio: %s", SDL_GetError());
		return -1;
	}
	
        /* Create the mutex */
        //sound_mutex = SDL_CreateMutex();
        //assert(sound_mutex);

	atexit(Mix_CloseAudio);

	sound_activated = 1;
	//SDL_PauseAudio(0);
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

Mix_Chunk *currentTrack;

bool music_load_track()
{
	
	
}


