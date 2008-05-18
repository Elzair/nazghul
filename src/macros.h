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

// This file needs to be safe for both c and c++ compilation. It should also
// contain only the most generic types of macros, stuff not really specific to
// nazghul.

#ifndef macros_h
#define macros_h

#ifdef __cplusplus
#define BEGIN_DECL extern "C" {
#define END_DECL   }
#else
#define BEGIN_DECL
#define END_DECL
#endif

#define maxstrlen(array) (sizeof((array)) - 1)

#ifndef clamp
#define clamp(v,a,b) ((v) = (v) < (a) ? (a) : ((v) > (b) ? (b) : (v)))
#endif

#ifndef normalize
#define normalize(dv) ((dv)>1?1:((dv)<-1?-1:0))
#endif

#define DECL_CAST(type,var,ptr) type * var = (type *)(ptr)

#endif
