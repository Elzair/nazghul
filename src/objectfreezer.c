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

#include "objectfreezer.h"
#include "tree.h"
#include "list.h"
#include <stdlib.h>
#include <assert.h>

struct objectfreezernode* new_objectfreezernode(char *key)
{
	objectfreezernode *newnode= (struct objectfreezernode *) malloc(sizeof(struct objectfreezernode));
	assert(newnode);
	newnode->treenode.key.s_key=key;
	newnode->treenode.key_type=tree_s_key;
	newnode->treenode.left=NULL;
	newnode->treenode.right=NULL;
	newnode->treenode.p=NULL;
	list_init(&(newnode->objectlist));
	return newnode;
}

struct objectfreezerlist* new_objectfreezerlistnode(class Object *value, int x, int y)
{
	objectfreezerlist *newnode= (struct objectfreezerlist *) malloc(sizeof(struct objectfreezerlist));
	assert(newnode);
	newnode->objectlist.next=NULL;
	newnode->objectlist.prev=NULL;
	newnode->x=x;
	newnode->y=y;
	newnode->obj=value;
	return newnode;
}

struct objectfreezernode* ensureFreezerTreeNode(char *key)
{
	struct tree* ofntree = tree_s_search((tree *)Session->freezer, key);
	if (ofntree == NULL)
	{
		ofntree = (tree *)new_objectfreezernode(key);
		tree_insert((tree **)&(Session->freezer),ofntree);
	}
	return (objectfreezernode *) ofntree;
}

void freezeObject(char *key, int x, int y, class Object *o)
{
	struct objectfreezernode *ofn = ensureFreezerTreeNode(key);
	struct objectfreezerlist *ofln = new_objectfreezerlistnode(o,x,y);
	list_add(&(ofn->objectlist),(list *)ofln);
}

class Object* thawObject(char* key,int* xout, int* yout)
{
	struct tree* ofntree = tree_s_search((tree *)Session->freezer, key);
	if (ofntree == NULL)
	{
		return NULL;
	}
	struct objectfreezernode *ofn = (objectfreezernode *) ofntree;
	struct objectfreezerlist *ofln = (objectfreezerlist *) ofn->objectlist.next;
	list_remove((list *) ofln);
	class Object* objtemp = ofln->obj;
	*xout = ofln->x;
	*yout = ofln->y;
	free(ofln);
	//check if that was the last entry- if so, delete tree node
	if (list_empty(&(ofn->objectlist)))
	{
		tree_delete((tree **)&(Session->freezer), ofntree);
		free(ofn);
	}
	return objtemp;
}
