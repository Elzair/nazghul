/* Copyright (c) 2002 Gordon McNutt */
/* Concept and design hashed out with Sam Glasby */
#ifndef Mech_h
#define Mech_h

#include "object.h"
#include "place.h"
#include "conv.h"

struct mech_state {
	char *name;
	struct sprite *sprite;
	int pmask;
	int light;
	bool opaque;
};

#ifdef OLD_MECH_ACTION
struct mech_alarm_parms {
	int signal;
	int turns;
};

struct mech_blit_map_parms {
	struct place *dst;
	int dst_x;
	int dst_y;
	struct terrain_map *src;
	int src_x;
	int src_y;
	int w;
	int h;
	int rot;
};

struct mech_action {
	void (*fx) (struct mech_action *, class Mech * mech);
	union {
		int signal;	// for actions that propagate a signal value
		char *string;
		struct mech_alarm_parms alarm;
		struct mech_blit_map_parms blit_map;
	} parms;
};
#endif				// OLD_MECH_ACTION

struct mech_transition {
	struct mech_state *from;
	int method;		// fixme: rename as 'event'
	struct mech_state *to;

#ifdef OLD_MECH_ACTIONS
	int n_actions;
	struct mech_action *actions;
#else
	struct response *actions;
#endif
};

class MechType:public ObjectType {
      public:
	virtual bool isType(int classID) {
		return (classID == MECH_TYPE_ID);
	}
	virtual int getType() {
		return MECH_TYPE_ID;
	}
	MechType();
	~MechType();
	virtual class Object *createInstance();
	virtual bool load(class Loader * loader);

	struct mech_transition *MechType::load_transitions(class Loader *
							   loader, int *n);
	int n_states;
	struct mech_state *states;
	int n_transitions;
	struct mech_transition *transitions;
};

class Mech:public Object {
      public:
	virtual bool isType(int classID) {
		return (classID == MECH_ID);
	}
	virtual int getType() {
		return MECH_ID;
	}
	class MechType *getObjectType() {
		return (class MechType *) Object::getObjectType();;
	}
	Mech();
	~Mech();
	virtual bool load(class Loader * loader);
	virtual bool activate(int method);
	virtual struct sprite *getSprite();
	virtual char *getName();
	virtual int getPmask();
	virtual int getLight();
	virtual bool is_opaque();

	class Mech *port;
	struct mech_state *state;
	struct conv conv;
};

#endif				// Mech_h
