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
#include "moongate.h"
#include "screen.h"
#include "common.h"
#include "sprite.h"
#include "place.h"
#include "map.h"
#include "wq.h"
#include "player.h"

#include <assert.h>
#include <unistd.h>

class Moongate **Moongates;

static int DestinationPhase;

extern struct list TickWorkQueue;

struct {
	struct wq_job openJob;
	struct wq_job closeJob;
	struct list *wq;
} Moongate;

void moongateSetAnimationWorkQueue(struct list *wq)
{
	Moongate.wq = wq;
}

static void myRunOpenJob(struct wq_job *job, struct list *wq)
{
	class Moongate *gate;

	gate = (class Moongate *) job->data;
	gate->open();
	// mapMarkAsDirty(player_party->view);
	mapSetDirty();
	if (!gate->isOpen())
		wqReschedule(wq, job);
}

static void myRunCloseJob(struct wq_job *job, struct list *wq)
{
	class Moongate *gate;

	gate = (class Moongate *) job->data;
	gate->close();
	// mapMarkAsDirty(player_party->view);
	mapSetDirty();
	if (!gate->isClosed())
		wqReschedule(wq, job);
}

void moongateOpenSourceGate(int phase)
{
	assert(phase < MoonInfo.phases);
	assert(phase >= 0);

	if (Moongates[phase]) {

		Moongate.openJob.tick = Tick + 1;
		Moongate.openJob.period = 1;
		Moongate.openJob.run = myRunOpenJob;
		Moongate.openJob.data = Moongates[phase];

		wqAddJob(Moongate.wq, &Moongate.openJob);
	}
}

void moongateCloseSourceGate(int phase)
{
	assert(phase < MoonInfo.phases);
	assert(phase >= 0);

	if (Moongates[phase]) {

		Moongate.closeJob.tick = Tick + 1;
		Moongate.closeJob.period = 1;
		Moongate.closeJob.run = myRunCloseJob;
		Moongate.closeJob.data = Moongates[phase];

		wqAddJob(Moongate.wq, &Moongate.closeJob);
	}
}

void moongateOpenDestinationGate(int phase)
{
	DestinationPhase = phase;
}

void moongateCloseDestinationGate(int phase)
{
	DestinationPhase = -1;
}

class Moongate *moongateGetDestinationGate(void)
{
	if (DestinationPhase == -1)
		return 0;
	return Moongates[DestinationPhase];
}

MoongateType::~MoongateType()
{
	if (enter_sound)
		free(enter_sound);
	if (sprite)
		delete sprite;
}

bool MoongateType::init(char *tag, char *name, struct sprite *sprite,
			int n_phases, char *enter_sound)
{
	if (!ObjectType::init(tag, name, portal_layer, sprite))
		return false;
	this->n_phases = n_phases;
	this->sprite = new struct sprite *[n_phases];
	this->enter_sound = strdup(enter_sound);
	return (this->enter_sound != 0 && this->sprite != 0);
}

void Moongate::paint(int sx, int sy)
{
	if (state == MOONGATE_CLOSED)
		return;
	spritePaint(getSprite(), 0, sx, sy);
}

void Moongate::animateOpening()
{
	int oframe = frame;
        
        view = mapCreateView();

        addView();
        mapSetPlace(getPlace());
        mapCenterCamera(getX(), getY());

	enum moongate_state ostate = state;	// fixme -- necessary?

	state = MOONGATE_OPENED;	// fixme -- necessary?

	for (frame = 0; frame < getNumFrames(); frame++) {
                updateView();
                mapUpdate(0);
		usleep(MS_PER_TICK * 1000);
	}

        rmView();
        mapDestroyView(view);

	frame = oframe;
	state = ostate;
}

void Moongate::animateClosing()
{
	int oframe = frame;

	for (frame = getNumFrames() - 1; frame >= 0; frame--) {
		// mapRepaintView(0, REPAINT_ACTIVE);
		mapUpdate(0);
		usleep(MS_PER_TICK * 1000);
	}

	frame = oframe;
}

int Moongate::getLight()
{
	// Return the light intensity of the moongate in its current state
	int lightPerFrame = getObjectType()->getMaxLight() / getNumFrames();
	return lightPerFrame * frame;
}

void Moongate::open()
{
        state = MOONGATE_OPENING;
        frame++;
        assert(frame <= getNumFrames());
        if (frame == (getNumFrames() - 1))
                state = MOONGATE_OPENED;
        
}

void Moongate::close() {
        state = MOONGATE_CLOSING;
        frame--;
        assert(frame >= 0);
        if (frame == 0)
                state = MOONGATE_CLOSED;
}
