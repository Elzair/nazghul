/* Copyright (c) 2002 Gordon McNutt */
#include "hash.h"
#include "olist.h"
#include "util.h"

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
