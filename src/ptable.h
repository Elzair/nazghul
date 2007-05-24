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

#ifndef ptable_h
#define ptable_h

#include "macros.h"

BEGIN_DECL

struct ptable {
        int n_mmode;  /* number of movement modes */
        int n_pclass; /* number of passability classes */
        int *table;   /* 2d lookup indexed by mmode & pclass */
};

#define PTABLE_IMPASSABLE 255
#define PCLASS_NONE 0
#define PTABLE_NO_DROP		100 		/* movement cost that disallows dropping objects */


#define ptable_is_passable(ptable,mmode,pclass) \
        (ptable_get((ptable),(mmode),(pclass)) != PTABLE_IMPASSABLE)

extern struct ptable *ptable_new(int n_mmodes, int n_pclass);
extern void ptable_set(struct ptable *ptable, int mmode, int pclass, 
                       int cost);
extern int ptable_get(struct ptable *ptable, int mmode, int pclass);
extern void ptable_del(struct ptable *pass);

END_DECL

#endif
