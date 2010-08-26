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

#include "magic.h"
#include "debug.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/*****************************************************************************
 *
 * spell api
 *
 *****************************************************************************/

static struct spell *spell_new(char *code)
{
        struct spell *spell;

        spell = (struct spell*)calloc(1, sizeof(*spell));
        assert(spell);
        
        spell->code = strdup(code);
        assert(spell->code);

        return spell;
}

static void spell_del(struct spell *entry)
{
        assert(entry);

        if (entry->code)
                free(entry->code);
        free(entry);
}

int spell_add_reagent(struct spell *spell, ObjectType *reagent)
{
        if (spell->n_reagents == MAX_MIX_REAGENTS) {
                warn("spell_add_reagent: spell %s already maxed out on "\
                     "reagents\n", spell->code);
                return -1;
        }

        spell->reagents[spell->n_reagents++] = reagent;
        return 0;
}

/*****************************************************************************
 *
 * helper functions
 *
 *****************************************************************************/

static int magic_check_code(struct magic *magic, char *code)
{
        if (! code)
                return -1;

        while (*code) {
                if (! magic->words[(*code - 'A')]) {
                        warn("magic_check_code: no magic word starts with %c\n",
                             *code);
                        return -1;
                }
                code++;
        }

        return 0;
}

static void magic_del_spell_tree(struct spell *root)
{
        if (root->left)
                magic_del_spell_tree(root->left);
        if (root->right)
                magic_del_spell_tree(root->right);
        spell_del(root);
}

static int magic_is_code(char code)
{
        int index = code - 'A';
        return (index >= 0 && index < MAX_SPELL_WORDS);
}

/*****************************************************************************
 *
 * public api
 *
 *****************************************************************************/


/* Initialize at start-of-session prior to loading */
void magic_init(struct magic *magic)
{
        memset(magic->words, 0, sizeof(magic->words));
        magic->spells = NULL;
}

/* Cleanup at end-of-session */
void magic_end_session(struct magic *magic)
{
        int i;

        for (i = 0; i < MAX_SPELL_WORDS; i++) {
                if (NULL != magic->words[i])
                        free(magic->words[i]);
                magic->words[i] = NULL;
        }

        if (magic->spells != NULL) {
                magic_del_spell_tree(magic->spells);
                magic->spells = NULL;
        }
}

struct spell *magic_add_spell(struct magic *magic, char *code)
{
	struct spell *parent = 0;
	struct spell *current = magic->spells;
        int dir;

        /* Make sure the code is valid */
        if (magic_check_code(magic, code))
                return NULL;

        /* Descend the spell tree to a vacant leaf */
	while (current) {

                /* Parent always points to last valid node */
		parent = current;

                dir = strcmp(code, current->code);

                /* Check if a spell was already inserted with this code */
		if (dir == 0) {
                        warn("magic_add_spell: there's already a spell with "\
                             "code '%s'\n", code);
			return NULL;
                }
		else if (dir < 0)
			current = current->left;
		else
			current = current->right;
	}

        current = spell_new(code);

	if (!parent) {
                /* First entry (root) */
		magic->spells = current;
	} else if (strcmp(code, parent->code) < 0) {
                /* Left child vacant */
		parent->left = current;
	} else {
                /* Right child vacant */
		parent->right = current;
	}

        /* Success */
	return current;

}

/* Lookup spells when casting them during play */
struct spell *magic_lookup_spell(struct magic *magic, char *code)
{
        struct spell *current;
        
        current = magic->spells;

        while (current) {

                int dir = strcmp(code, current->code);

                if (! dir) {
                        /* Found it! */
                        return current;
                } else if (dir < 0) {
                        current = current->left;
                } else {
                        current = current->right;
                }
        }

        /* Didn't find it */
        return NULL;
}

int magic_add_word(struct magic *magic, char *word)
{
        int index = word[0] - 'A';

        if (! magic_is_code(word[0])) {
                warn("magic_add_word: letter '%c' out of range [A, %c]\n",
                     word[0], MAX_SPELL_WORDS + 'A' - 1);
                return -1;
        }

        if (magic->words[index]) {
                warn("magic_add_word: cannot add word '%s' because word "\
                     "'%s' already starts with '%c'\n", word, 
                     magic->words[index],
                     word[0]);
                return -1;
        }

        magic->words[index] = strdup(word);
        assert(magic->words[index]);

        return 0;
}

char *magic_lookup_word(struct magic *magic, char first_letter)
{
        int index = first_letter - 'A';

        if (! magic_is_code(first_letter)) {
                warn("magic_lookup_word: letter '%c' out of range [A, %c]\n",
                     first_letter, MAX_SPELL_WORDS + 'A' - 1);
                return NULL;
        }

        return magic->words[index];
}

int magic_spell_code_to_name(struct magic *magic, char *buf, int len, 
                             char *code)
{
        int n = 0;

        while (*code) {
                char *word = magic_lookup_word(magic, *code);
                if (! word)
                        return -1;
                n = snprintf(buf, len, "%s ", word); 
                if (n < 0)
                        return -1;
                buf += n;
                len -= n;
                code++;
        }
        
        // back up over the last space
        if (! *code) {
                buf--;
                *buf = 0;
        }

        return 0;
}

int magic_spell_name_to_code(struct magic *magic, char *code, int len, 
                             const char *name)
{
        int state = 2, i = 0;
        const char *ptr = name;

        len--; /* leave space for null terminator */

        while (*ptr && len) {
                char c = *ptr;
                ptr++;
                switch (state) {
                case 0:
                        /* looking for start of next word */
                        if (! isspace(c)) {

                                /* If this is a valid word then store it's
                                 * first letter in the code buffer, else ignore
                                 * it. Ignoring it lets us handle things like
                                 * "vas Flam spell", where we ignore the
                                 * "spell" part. */
                                if (magic_is_code(c)) {
                                        code[i] = c;
                                        i++;
                                        len--;
                                }
                                state = 1;
                        }
                        break;
                case 1:
                        /* looking for end of current word */
                        if (isspace(c)) {
                                state = 0;
                        }
                        break;
                case 2:
                			/* spell names have code in <> */
                			if (c == '<') {
	                			state = 0;		
                			}
                			break;
                default:
                        assert(0);
                        break;
                }
        }

        code[i] = 0; /* null-terminate */

        return 0;
}
