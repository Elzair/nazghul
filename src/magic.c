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

        if (index < 0 || index >= MAX_SPELL_WORDS) {
                warn("magic_add_word: letter '%c' out of range [A, %c]\n",
                     word[0], MAX_SPELL_WORDS - 'A');
                return -1;
        }

        if (magic->words[index]) {
                warn("magic_add_word: cannot add word '%s' because word "\
                     "'%s' already starts with '%c'\n", word, magic->words[index],
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

        if (index < 0 || index >= MAX_SPELL_WORDS) {
                warn("magic_lookup_word: letter '%c' out of range [0, %c]\n",
                     first_letter, MAX_SPELL_WORDS - 'A');
                return NULL;
        }

        return magic->words[index];
}
