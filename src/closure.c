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

#include "closure.h"
#include "debug.h"
#include "session.h"

#include "scheme.h"
#include "scheme-private.h"

#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>

#define scm_is_pair(sc,arg) ((sc)->vptr->is_pair(arg))
#define scm_mk_ptr(sc,ptr) mk_foreign_func((sc), (foreign_func)(ptr))
#define scm_cdr(p) ((p)->_object._cons._cdr)
#define scm_car(p) ((p)->_object._cons._car)
#define scm_is_symbol(sc, arg) ((sc)->vptr->is_symbol(arg))
#define scm_is_closure(sc, arg) ((sc)->vptr->is_closure(arg))

/* Defined in kern.c: */
extern pointer vpack(scheme *sc, char *fmt, va_list ap);

closure_t *closure_new(scheme *sc, pointer code)
{
        closure_t *clx = (closure_t*)calloc(1, sizeof(*clx));
        assert(clx);
        clx->sc = sc;
        clx->code = code;
        return clx;
}

void closure_init(closure_t *clx, scheme *sc, pointer code)
{
        clx->sc = sc;
        clx->code = code;
}

void closure_del(closure_t *closure)
{
        free(closure);
}

int closure_exec(closure_t *closure, char *fmt, ...)
{
        pointer head;
        pointer func;
        pointer result;
        va_list ap;
        void *ptr;
        pointer old_dump;

        assert(closure->code);

        /* Don't permit recursively calling into the scheme intepreter. It
         * messes up its state when we eventually return back into it. */
        if (closure->sc->inside) {
                warn("script->kernel->script recursion detected!\n");
        }

        head = closure->sc->NIL;

        /* Convert the args to a scheme list */
        if (fmt) {
                va_start(ap, fmt);
                head = vpack(closure->sc, fmt, ap);
                va_end(ap);
        }

        if (scm_is_closure(closure->sc, closure->code)) {
                result = scheme_call(closure->sc, closure->code, head);
        } else if (scm_is_symbol(closure->sc, closure->code)) {

                pointer pair;
                pointer proc;

                /* The 'code' pointer is a pointer to a scheme symbol. Looking
                 * it up in the scheme environment will return a (symbol,
                 * value) pair. We then take the cdr of the pair to get the
                 * actual procedure we want to call. */
                pair = closure->sc->vptr->find_slot_in_env(closure->sc, 
                                                           closure->sc->envir, 
                                                           closure->code, 
                                                           1);
                assert(scm_is_pair(closure->sc, pair));
                proc = closure->sc->vptr->pair_cdr(pair);
                result = scheme_call(closure->sc, proc, head);

                /* WARNING: it is no longer safe to reference the closure at
                 * this point, because it may have destroyed itself as part of
                 * the scheme_call! Probably need to implement a lock mechanism
                 * like that used for place tiles. */

        } else {
                /* This can happen if the interpreter gc's our closure. */
                assert(false);
        }

        /* FIXME: need to return integer results, too */

 evaluate_result:

        if (result == closure->sc->NIL ||
            result == closure->sc->F)
                return 0;

        if (closure->sc->vptr->is_number(result) &&
            closure->sc->vptr->is_integer(result))
                return closure->sc->vptr->ivalue(result);

        if (scm_is_symbol(closure->sc, result)) {
                pointer pair;
                pair = closure->sc->vptr->find_slot_in_env(closure->sc, 
                                                           closure->sc->envir, 
                                                           result, 
                                                           1);
                assert(scm_is_pair(closure->sc, pair));
                result = closure->sc->vptr->pair_cdr(pair);
                goto evaluate_result;
        }

        return 1;
}

void closure_save(closure_t *closure, struct save *save)
{
        char *proc_name;

        assert(scm_is_symbol(closure->sc, closure->code));

        /* The 'code' pointer is a pointer to a scheme symbol. To save the
         * closure we only need to write this symbol out with a leading
         * tick. */
        proc_name = closure->sc->vptr->symname(closure->code);
        save->write(save, "'%s\n", proc_name);
}

void closure_ref(closure_t *closure)
{
        closure->ref++;
}

void closure_unref(closure_t *closure)
{
        assert(closure->ref);
        closure->ref--;
        if (! closure->ref)
                closure_del(closure);
}
