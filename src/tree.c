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
#include "tree.h"

void tree_insert(struct tree **root, struct tree *node)
{
	struct tree *parent = 0;
	struct tree *current = *root;

	while (current) {

		parent = current;

		if (node->key < current->key)
			current = current->left;
		else
			current = current->right;
	}

	node->p = parent;
	if (!parent) {
		*root = node;
	} else if (node->key < parent->key) {
		parent->left = node;
	} else {
		parent->right = node;
	}
}

void tree_delete(struct tree **root, struct tree *node)
{
	struct tree *splout;
	struct tree *child;

	/* Find a node to splice out */
	if (!node->left || !node->right)
		splout = node;
	else
		splout = tree_successor(node);

	/* Find the non-null child of the node being spliced, or use null if
	 * there are no children. */
	if (splout->left)
		child = splout->left;
	else
		child = splout->right;

	/* Splice out the node we picked */
	if (child)
		child->p = splout->p;
	if (!splout->p) {
		*root = child;
	} else if (splout == splout->p->left) {
		splout->p->left = child;
	} else {
		splout->p->right = child;
	}

	/* If the node spliced out was not the target node then use it to
	 * replace the target node */
	if (splout != node)
		tree_replace(root, node, splout);
}

struct tree *tree_successor(struct tree *node)
{
	struct tree *ptr;

	if (node->right)
		return tree_minimum(node->right);

	ptr = node->p;

	while (ptr && ptr->right == node) {
		node = ptr;
		ptr = ptr->p;
	}

	return ptr;
}

struct tree *tree_minimum(struct tree *node)
{
	while (node->left)
		node = node->left;
	return node;
}

void tree_replace(struct tree **root, struct tree *out, struct tree *in)
{
	in->p = out->p;

	if (!out->p)
		*root = in;
	else if (out == out->p->right)
		out->p->right = in;
	else
		out->p->left = in;

	in->left = out->left;
	if (in->left)
		in->left->p = in;

	in->right = out->right;
	if (in->right)
		in->right->p = in;
}
