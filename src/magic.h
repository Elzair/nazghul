/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2004 Gordon McNutt
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
 * Gordon McNutt
 * gmcnutt@users.sourceforge.net
 *
 */

#ifndef magic_h
#define magic_h

#include "object.h" /* for ObjectType */
#include "macros.h"

BEGIN_DECL

/* The number of spell words or "syllables" is limited to the number of letters
 * in the English alphabet. That's because the first letter of each syllable
 * uniquely identifies it. */
#define MAX_SPELL_WORDS 26

/* The max number of reagents allowed in the mixture for a spell. */
#define MAX_MIX_REAGENTS 8

/* Spell flags */
#define SPELL_RANGE_LIMITED 1

/* Spells are stored in a tree indexed by their "code". The code is the first
 * letter of each word in the spell. For example, An Nox has the code AN. I
 * also store the mixture here with the associated spell. For cimplicity I
 * hard-code the max number of reagents permitted in a spell. */
struct spell {
        ObjectType *type;
        char *code;
        int level;
        int cost;
        int context;
        int range;    /* n/a if SPELL_RANGE_LIMITED flag clear */
        int flags;
        int action_points;
        int n_reagents;
        ObjectType *reagents[MAX_MIX_REAGENTS];
        struct spell *left;
        struct spell *right;
};

/* One of these is embedded in the global session structure. It manages all the
 * magic-related information related to a session. */
struct magic {
        char *words[MAX_SPELL_WORDS];
        struct spell *spells;
};

/* Initialize before loading a new session */
extern void magic_init(struct magic *);

/* Cleanup at end-of-session */
extern void magic_end_session(struct magic *);

/* Add a spell during session load. Returns the newly added spell or NULL on
 * error. */
extern struct spell *magic_add_spell(struct magic *, char *code);

/* Add a word during session load. Returns 0 on success or -1 on error. */
extern int magic_add_word(struct magic *, char *word);

/* Lookup a word based on its first letter. Returns NULL on error or if no such
 * word exists, otherwise the desired word. */
extern char *magic_lookup_word(struct magic *magic, char first_letter);

/* Lookup spells when casting them during play */
extern struct spell *magic_lookup_spell(struct magic *, char *code);

/* Add another reagent to a spell mixture during session load. */
extern int spell_add_reagent(struct spell *spell, ObjectType *reagent);

END_DECL

#endif
