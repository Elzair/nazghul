/* Copyright (c) 2002 Gordon McNutt */
#ifndef play_h
#define play_h

class Character;


#ifdef __cplusplus
extern "C" {
#endif

        extern int playRun(void);

// I really shouldn't pollute this interface with these, but they don't really
// fit anywhere else right now, either.
#include "status.h"
        struct ScrollerContext {
                enum StatusSelection selector;
                void *selection;
                bool abort;
                bool done;
                bool mixing; // for mix reagents
        };

// these are defined in play.c
        extern bool dirkey(struct KeyHandler *kh, int key);
        extern bool yesnokey(struct KeyHandler *kh, int key);
        extern bool anykey(struct KeyHandler *kh, int key);
        extern bool scroller(struct KeyHandler *kh, int key);
        extern bool movecursor(struct KeyHandler *kh, int key);
        extern bool getnum(struct KeyHandler *kh, int key);
	extern void getkey(void *data, bool(*handler) (struct KeyHandler * kh,
                                                     int key));

	extern bool cmdUse(class Character * pc);
	extern bool cmdHandle(class Character * pc);
	extern bool cmdReady(class Character * pc);
	extern bool cmdZtats(class Character * pc);
        extern bool cmdGet(int x, int y, bool scoop_all);
	extern bool cmdOpen(class Character * pc);
	extern bool cmdCastSpell(class Character * pc);
        extern bool cmdQuit(void);
        extern bool cmdLook(int x, int y);

        extern class Character *select_party_member(void);

        extern void effectLight(char *name, int amount, int duration, 
				class Character * target);
        extern void effectReveal(char *name, int duration);
        extern void effectQuicken(char *name, int duration);
        extern void effectNegateMagic(char *name, int duration);
        extern void effectShowTerrain(char *name, int duration);

        extern int select_target(int ox, int oy, int *x, int *y, int range);
        extern int select_quantity(int max);

        // the new ui api
        extern int ui_get_direction(void);

#ifdef __cplusplus
}
#endif

#endif
