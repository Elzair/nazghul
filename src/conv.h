/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2002, 2003, 2008 Gordon McNutt
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
 */
#ifndef conv_h
#define conv_h

#define CONV_PC_COLOR  'g'
#define CONV_NPC_COLOR 'b'

/**
 * Flags for the return value of conv_is_keyword().
 */
#define CONV_IS_KEYWORD (1<<0) /* word is a keyword */
#define CONV_IS_MARKED  (1<<1) /* keyword has been used */

struct conv;

/**
 * Allocate a conversation struct and a fixed number of keywords slots.
 *
 * @param proc becomes the proc field with an added refcount.
 * @returns a new converstation struct on success, else NULL if allocaition
 * failed.
 */
struct conv *conv_new(struct closure *proc);

/**
 * Save a conversation to the session file.
 *
 * @param conv is the conversation to save. Only the closure is saved.
 * @param save is the save object.
 */
void conv_save(struct conv *conv, struct save *save);

/**
 * Release a reference to a conversation. This may delete it.
 *
 * @param is the conversation.
 */
void conv_unref(struct conv *conv);

/**
 * Add a reference to a conversation. This prevents it from being destroyed.
 *
 * @param is the conversation.
 */
void conv_ref(struct conv *conv);

/**
 * Start a conversation. This will start the conversation session in the
 * console, and return when and only when the conversation is over.
 *
 * @param npc is the NPC being spoken to.
 * @param pc is the player party member doing the talking.
 * @param conv is the conversation.
 */
void conv_enter(class Object *npc, class Object *pc, struct conv *conv);

/**
 * End a conversation. This is provided so that NPC scripts can end the
 * conversation prematurely (before the player says 'bye').
 */
void conv_end(void);

/**
 * Substitute for ctype.h's isprint(), which does not work properly on all
 * systems.
 *
 * @param c is the character to test.
 * @returns non-zero if c is a printable character, else zero.
 */
int isprintable(int c);

/**
 * Given an input string get pointers to the beginning and end of the next
 * word. The beginning of a word is any alphabetical character which either
 * starts the string or follows whitespace or punctuation. 
 *
 * @param instr is the input string.  
 *
 * @param beg will point to the beginning of the next work on exit, iff one was
 * found. If not its value is undefined.
 * @param end likewise will point to the end of the next work in the string.
 * @returns non-zero iff a word was found in the string.
 */
int conv_get_word(char *instr, char **beg, char **end);

/**
 * Check if a word is a keyword in a conversation (and, if so, if it has
 * already been used in a conversation since the last session reload). This
 * works by testing if any of the keywords are a prefix of the word.
 *
 * @param conv is the conversation with the keywords to check.
 * @param word if the word to test for.
 * @returns zero if the word is not a keyword, else a non-zero union of the
 * flags CONV_IS_KEYWORD and CONV_IS_MARKED.
 *
 */
int conv_is_keyword(struct conv *conv, char *word);

/**
 * Initialize module before using.
 *
 * @returns non-zero on error.
 */
int conv_init(void);

/**
 * Turn keyword highlighting on or off.
 *
 * @param enable is non-zero to turn it on.
 */
void conv_enable_keyword_highlighting(int enable);


#endif
