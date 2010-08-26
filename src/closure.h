//
// nazghul - an old-school RPG engine
// Copyright (C) 2004 Gordon McNutt
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
#ifndef closure_h
#define closure_h

#include "macros.h"
#include "scheme.h"

#include <stdarg.h>

BEGIN_DECL

typedef struct closure {
        scheme *sc;
        pointer env;
        pointer code;
        int ref;
} closure_t;

#define closure_unref_safe(clx) if ((clx)) closure_unref(clx)

extern closure_t *closure_new(scheme *interp, pointer code);
extern closure_t *closure_new_ref(scheme *interp, pointer code);
extern void closure_init(closure_t *closure, scheme *interp, pointer code);
extern int closure_exec(closure_t *closure, const char *fmt, ...);
extern pointer closure_execv(closure_t *closure, const char *fmt, va_list args);
extern int closure_translate_result(scheme *sc, pointer result);
extern  void closure_save(closure_t *closure, struct save *save);
extern void closure_ref(closure_t *closure);
extern void closure_unref(closure_t *closure);

/* Special form of closure_exec() made for running object hooks. */
int closure_execlpv(closure_t *closure, pointer gob, void *obj, 
                     const char *fmt, va_list args);

/* Special form of closure_exec() made for running object hooks where an effect
 * is attached to more than one hook. */
int closure_execlpiv(closure_t *closure, pointer gob, void *obj, int hook_id,
                     const char *fmt, va_list args);

/* Another special form - for session_run_hook */
int closure_execvl(closure_t *closure, const char *fmt, va_list args, pointer cell);

END_DECL

#endif
