/* Copyright (c) 2002 Gordon McNutt */
#ifndef hash_h
#define hash_h

#ifdef __cplusplus
extern "C" {
#endif

	struct olist;

	struct hash {
		struct olist *buckets;
		int n;
	};

	extern struct hash *hash_create(int n);
	extern void hash_destroy(struct hash *hash);
	extern void hash_add(struct hash *hash, struct olist *val);
	extern struct olist *hash_lookup(struct hash *hash, int key);

#ifdef __cplusplus
}
#endif
#endif
