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
#include "sound.h"
#include "common.h"
#include "console.h"		// For consolePrint()

#include <SDL.h>
#include <stdlib.h>
#include <string.h>

#define NUM_SOUNDS 3

struct sound {
	char *tag;
	Uint8 *data;
	Uint32 dpos;
	Uint32 dlen;
} sounds[NUM_SOUNDS];

static bool enableSound = false;

static void soundCB(void *unused, Uint8 * stream, int len)
{
	unsigned int i;
	int amount;

	for (i = 0; i < NUM_SOUNDS; ++i) {
		amount = (sounds[i].dlen - sounds[i].dpos);
		if (amount > len) {
			amount = len;
		}
		SDL_MixAudio(stream, &sounds[i].data[sounds[i].dpos], amount,
			     SDL_MIX_MAXVOLUME);
		sounds[i].dpos += amount;
	}
}

void soundPlay(char *file)
{
	int index;
	SDL_AudioSpec wave;
	Uint8 *data;
	Uint32 dlen;
	SDL_AudioCVT cvt;

        if (file == NULL)
                return;

	if (!enableSound) {
		// printf("playing sound %s\n", file);
		consolePrint("Playing sound %s\n", file);
		return;
	}

	/* Look for an empty (or finished) sound slot */
	for (index = 0; index < NUM_SOUNDS; ++index) {
		if (sounds[index].dpos == sounds[index].dlen) {
			break;
		}
	}
	if (index == NUM_SOUNDS)
		return;

	/* Load the sound file and convert it to 16-bit stereo at 22kHz */
	if (SDL_LoadWAV(file, &wave, &data, &dlen) == NULL) {
		fprintf(stderr, "Couldn't load %s: %s\n", file, SDL_GetError());
		return;
	}
	SDL_BuildAudioCVT(&cvt, wave.format, wave.channels, wave.freq,
			  AUDIO_S16, 2, 22050);
	cvt.buf = (Uint8 *) malloc(dlen * cvt.len_mult);
	memcpy(cvt.buf, data, dlen);
	cvt.len = dlen;
	SDL_ConvertAudio(&cvt);
	SDL_FreeWAV(data);

	/* Put the sound data in the slot (it starts playing immediately) */
	if (sounds[index].data) {
		free(sounds[index].data);
	}
	SDL_LockAudio();
	sounds[index].data = cvt.buf;
	sounds[index].dlen = cvt.len_cvt;
	sounds[index].dpos = 0;
	SDL_UnlockAudio();
}

void soundInit(void)
{
	SDL_AudioSpec fmt;

	/* Set 16-bit stereo audio at 22Khz */
	fmt.freq = 22050;
	fmt.format = AUDIO_S16;
	fmt.channels = 2;
	fmt.samples = 512;	/* A good value for games */
	fmt.callback = soundCB;
	fmt.userdata = NULL;

	/* Open the audio device and start playing sound! */
	if (SDL_OpenAudio(&fmt, NULL) < 0) {
		perror_sdl("SDL_OpenAudio");
		return;
	}

	enableSound = true;
	SDL_PauseAudio(0);

}
