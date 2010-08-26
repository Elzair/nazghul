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
#ifndef tree_h
#define tree_h

#ifdef __cplusplus
extern "C" {
#endif

#define tree_entry(ptr,type,field) \
        ((type*)((char*)(ptr)-(unsigned long)(&((type *)0)->field)))

        enum tree_key_type {
                tree_i_key, // integer key
                tree_s_key  // string key
        };

        struct tree {
                enum tree_key_type key_type;
                union {
                        int i_key;
                        char *s_key;
                } key;
                struct tree *left;
                struct tree *right;
                struct tree *p;
        };

        extern void tree_insert(struct tree **root, struct tree *node);
        extern void tree_delete(struct tree **root, struct tree *node);
        extern struct tree *tree_successor(struct tree *node);
        extern struct tree *tree_minimum(struct tree *node);
        extern void tree_replace(struct tree **root, struct tree *out, struct tree *in);
        extern struct tree *tree_i_search(struct tree *root, int key);
        extern struct tree *tree_s_search(struct tree *root, const char *key);

#ifdef __cplusplus
}
#endif

#endif
