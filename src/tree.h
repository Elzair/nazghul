/* Copyright (c) 2002 Gordon McNutt */
#ifndef tree_h
#define tree_h

#ifdef __cplusplus
extern "C" {
#endif

#define tree_entry(ptr,type,field) \
        ((type*)((char*)(ptr)-(unsigned long)(&((type *)0)->field)))

        struct tree {
                int key;
                struct tree *left;
                struct tree *right;
                struct tree *p;
        };

	static inline struct tree *tree_search(struct tree *root, int key) {
                while (root && root->key != key) {
                        if (key < root->key)
                                root = root->left;
                        else
                                root = root->right;
                }
                return root;
        }

        extern void tree_insert(struct tree **root, struct tree *node);
        extern void tree_delete(struct tree **root, struct tree *node);
        extern struct tree *tree_successor(struct tree *node);
        extern struct tree *tree_minimum(struct tree *node);
        extern void tree_replace(struct tree **root, struct tree *out,
                                 struct tree *in);

#ifdef __cplusplus
}
#endif

#endif
