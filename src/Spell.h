/* Copyright (c) 2002 Gordon McNutt */

#ifndef Spell_h
#define Spell_h

#include "object.h"
#include "list.h"

#define MAX_SPELL_WORDS 26

// Note: Spell must descend from ObjectType to go into player inventory.

class Spell : public ObjectType {

public:
        Spell();
        virtual ~Spell();

        virtual bool isType(int id);
        virtual int getType();

        enum cast_result {
                ok,
                no_room_on_battlefield,
                unknown_failure,
                magic_negated,
                missed_target,
                no_effect,
                teleport_failed,
        };

        virtual enum cast_result cast(class Character *caster, 
                                      class Object *target, 
                                      int direction, int tx, int ty);
        virtual bool load(class Loader *loader);
        virtual bool bindTags(class Loader *loader);
        virtual void teleport_horizontally(class Character *caster, 
                                           int direction);
        virtual void teleport_vertically(class Character *caster, 
                                         int direction);

        class Spell *left; // hooks for the Spell tree ordered by name
        class Spell *right; // hooks for the Spell tree ordered by name

        char *code;
        int cost;       // in mana points
        int range;      // in tiles
        int effects;    // bitmask (see common.h)
        int strength;   // of effect, interpretation depends on effect
        int duration;
        int context;    // in which the Spell may be cast
        int level;      // which a character must be to cast this Spell
        int target;     // type of target required for spell
        class Missile *missile;
        int n_reagents;
        class Reagent **reagents;    // required to mix the Spell

        // 'parms' is a list of effect-specific data types.
        int n_parms;
        void **parms;

        enum cast_result success;
};

extern class Spell *SpellTree;
extern char *Spell_words[MAX_SPELL_WORDS];

extern bool Spell_init(void);
extern class Spell *Spell_lookup_by_code(char *code);
extern bool Spell_load_magic_words(class Loader *loader);

#endif
