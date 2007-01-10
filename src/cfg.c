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

#include "cfg.h"
#include "repstr.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define CFG_HASH_SIZE 31

struct cfg_entry {
        struct cfg_entry *next;
        char *key, *val;        
} *cfg_hash[CFG_HASH_SIZE];

static struct cfg_entry *cfg_entry_new(char *key, const char *val)
{
        struct cfg_entry *entry = (struct cfg_entry*)malloc(sizeof(*entry));
        assert(entry);
        entry->next = 0;
        entry->key = strdup(key);
        assert(entry->key);
        if (val) {
                entry->val = strdup(val);
                assert(entry->val);
        } else {
                entry->val = 0;
        }
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
        return hashed % CFG_HASH_SIZE;
}

int cfg_init()
{
        memset(cfg_hash, 0, sizeof(cfg_hash));
        return 0;
}

void cfg_set(char *key, const char *val)
{
        int hashkey = hashfn(key);
        struct cfg_entry *entry = cfg_hash[hashkey];
        if (! entry) {
                cfg_hash[hashkey] = cfg_entry_new(key, val);
        } else {
                int match = !strcmp(entry->key, key);
                while (! match
                       && entry->next) {
                        entry = entry->next;
                        match = !strcmp(entry->key, key);
                }
                if (match) {
                        repstr(&entry->val, val);
                } else {
                        entry->next = cfg_entry_new(key, val);
                }
        }
}

char *cfg_get(char *key)
{
        int hashkey = hashfn(key);
        struct cfg_entry *entry = cfg_hash[hashkey];
        if (! entry) {
                return 0;
        } else {
                int match = !strcmp(entry->key, key);
                while (! match
                       && entry->next) {
                        entry = entry->next;
                        match = !strcmp(entry->key, key);
                }
                if (match) {
                        return entry->val;
                } else {
                        return 0;
                }
        }
}
