/* Copyright (c) 2002 Gordon McNutt */
#ifndef moongate_h
#define moongate_h

#include "object.h"

struct list;

enum moongate_state {
	MOONGATE_CLOSED,
	MOONGATE_OPENING,
	MOONGATE_OPENED,
	MOONGATE_CLOSING,
};

class MoongateType:public ObjectType {
      public:
	virtual int getType() {
		return MOONGATE_TYPE_ID;
	}
	virtual bool isType(int classID) {
		if (classID == getType())
			return true;
		return ObjectType::isType(classID);
	}

      MoongateType():n_phases(0), sprite(NULL), enter_sound(NULL),
	    maxLight(0) {
	}
	virtual ~ MoongateType();
	virtual bool init(char *tag, char *name, struct sprite * sprite,
			  int n_phases, char *enter_sound);
	virtual void setSprite(int phase, struct sprite *sprite) {
		assert(phase >= 0 && phase < n_phases);
		this->sprite[phase] = sprite;
	}
	virtual struct sprite *getSprite(int phase) {
		assert(phase >= 0 && phase < n_phases);
		return sprite[phase];
	}
	virtual char *getEnterSound() {
		return enter_sound;
	}
	virtual int getNumPhases() {
		return n_phases;
	}
	virtual int getMaxLight() {
		return maxLight;
	}
	virtual void setMaxLight(int val) {
		maxLight = val;
	}
      protected:
	int n_phases;
	struct sprite **sprite;
	char *enter_sound;
	int maxLight;
};

class Moongate:public Object {
      public:
	virtual int getType() {
		return MOONGATE_ID;
	}
	virtual bool isType(int classID) {
		if (classID == getType())
			return true;
		return Object::isType(classID);
	}

      Moongate():phase(0), frame(0), state(MOONGATE_CLOSED) {
	}
	virtual ~ Moongate() {
	}

	virtual class MoongateType *getObjectType() {
		return (class MoongateType *) Object::getObjectType();
	}
	virtual struct sprite *getSprite() {
		return getObjectType()->getSprite(frame);
	}
	virtual char *getName() {
		return "moongate";
	}
	virtual bool isOpen() {
		return (state == MOONGATE_OPENED);
	}
	virtual bool isClosed() {
		return (state == MOONGATE_CLOSED);
	}
	virtual char *getEnterSound() {
		return getObjectType()->getEnterSound();
	}
	virtual int getNumFrames() {
		return getObjectType()->getNumPhases();
	}

	virtual void init(int x, int y, struct place *place,
			  class MoongateType * type, int phase) {
		Object::init(x, y, place, type);
		this->phase = phase;
	}

	virtual int getLight();

	virtual void paint(int sx, int sy);

	virtual void open() {
		state = MOONGATE_OPENING;
		frame++;
		if (frame == (getNumFrames() - 1))
			state = MOONGATE_OPENED;
	}

	virtual void close() {
		state = MOONGATE_CLOSING;
		frame--;
		if (frame == 0)
			state = MOONGATE_CLOSED;
	}

	virtual void animateOpening();
	virtual void animateClosing();

      protected:
	int phase;
	int frame;
	enum moongate_state state;
};

#ifdef __cplusplus
extern "C" {
#endif

	extern class Moongate **Moongates;

	extern void moongateOpenSourceGate(int phase);
	extern void moongateCloseSourceGate(int phase);
	extern void moongateOpenDestinationGate(int phase);
	extern void moongateCloseDestinationGate(int phase);
	extern class Moongate *moongateGetDestinationGate(void);
	extern void moongateSetAnimationWorkQueue(struct list *wq);

#ifdef __cplusplus
}
#endif
#endif
