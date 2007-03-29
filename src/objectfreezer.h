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
#ifndef objectfreezer_h
#define objectfreezer_h

#include "tree.h"
#include "list.h"
#include "session.h"

struct objectfreezerlist {
	struct list objectlist;
	int x;
	int y;
	class Object *obj;
};

struct objectfreezernode {
	struct tree treenode;
	struct list objectlist;
};

struct objectfreezernode* new_objectfreezernode(char *key);
struct objectfreezerlist* new_objectfreezerlistnode(Object *value, int x, int y, objectfreezerlist *list);

void freezer_freezeObject(char* key, int x, int y, class Object *o);
class Object* freezer_thawObject(char* key,int* xout, int* yout);
void freezer_save(save_t *save);
void freezer_del();
void freezer_start_contents();

#endif
