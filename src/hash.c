//
// nazghul - an old-school RPG engine
// Copyright (C) 2002, 2003 Gordon McNutt
//
// This program is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 2 of the License, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
// more details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
// Suite 330, Boston, MA 02111-1307 USA
//
// Gordon McNutt
// gmcnutt@users.sourceforge.net
//
#include "hash.h"
#include "olist.h"
#include "common.h"

struct hash *hash_create(int n)
{
	struct hash *hash;
	int i;

	CREATE(hash, struct hash, 0);
	hash->n = n;
	hash->buckets = (struct olist *) malloc(n * sizeof(struct olist));
	if (!hash->buckets) {
		hash_destroy(hash);
		return 0;
	}
	for (i = 0; i < n; i++) {
		list_init(&hash->buckets[i].list);
	}
	return hash;
}

void hash_destroy(struct hash *hash)
{
	if (hash->buckets)
		free(hash->buckets);
	free(hash);
}

void hash_add(struct hash *hash, struct olist *val)
{
	struct olist *b = &hash->buckets[val->key % hash->n];
	olist_add(b, val);
}

struct olist *hash_lookup(struct hash *hash, int key)
{
	struct olist *b = &hash->buckets[key % hash->n];
	return olist_lookup(b, key, 1);
}
