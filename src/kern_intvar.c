/* $Id$
 *
 * Copyright (C) 2006 Gordon McNutt
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
 */

#include "kern_intvar.h"
// #include "repstr.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define KERN_INTVAR_HASH_SIZE 31

struct kern_intvar_entry {
    struct kern_intvar_entry *next;
    char *key;
    int value;
} *kern_intvar_hash[KERN_INTVAR_HASH_SIZE];

static struct kern_intvar_entry *kern_intvar_entry_new(char *key, const int value)
{
    struct kern_intvar_entry *entry = (struct kern_intvar_entry*) malloc(sizeof(*entry));
    assert(entry);

    entry->next = 0;

    entry->key = strdup(key);
    assert(entry->key);

    entry->value = value;

    return entry;
}

static int hashfn(char *key)
{
    unsigned int hashed = 0; 
    const char *c; 
    static const int bits_per_int = sizeof(unsigned int)*8; 

    for (c = key; *c; c++) { 
        /* letters have about 5 bits in them */ 
        hashed = (hashed<<5) | (hashed>>(bits_per_int-5)); 
        hashed ^= *c; 
    } 
    return hashed % KERN_INTVAR_HASH_SIZE;
}

int kern_intvar_init()
{
    memset(kern_intvar_hash, 0, sizeof(kern_intvar_hash));
    return 0;
}

void kern_intvar_set(char *key, int value)
{
    int hashkey = hashfn(key);
    struct kern_intvar_entry *entry = kern_intvar_hash[hashkey];
    if (! entry) {
        kern_intvar_hash[hashkey] = kern_intvar_entry_new(key, value);
    } 
    else {
        int match = !strcmp(entry->key, key);
        while (! match
               && entry->next) {
            entry = entry->next;
            match = !strcmp(entry->key, key);
        }
        if (match) {
	    entry->value = value;
        } else {
            entry->next = kern_intvar_entry_new(key, value);
        }
    }
}

int kern_intvar_get(char *key)
{
    int hashkey = hashfn(key);
    struct kern_intvar_entry *entry = kern_intvar_hash[hashkey];
    if (! entry) {
        return 0;
    } 
    else {
        int match = !strcmp(entry->key, key);
        while (! match
               && entry->next) {
            entry = entry->next;
            match = !strcmp(entry->key, key);
        }
        if (match) {
            return entry->value;
        } else {
            return 0;  // SAM: Perhaps -1 or MAXINT would be a useful return for this case?
        }
    }
}


// eof
