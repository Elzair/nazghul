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
#include <stdlib.h>

/* Defined in kern.c: */
extern pointer vpack(scheme *sc, const char *fmt, va_list ap);

/*
 * closure_del - free a closure (external code should use closure_unref)
 */
static void closure_del(closure_t *closure)
{
        assert(0 == closure->ref);
        closure->sc->vptr->unprotect(closure->sc, closure->code);
        free(closure);
}

/**
 * Evaluate a Scheme procedure with Scheme args. This is an internal helper
 * function.
 *
 * @param closure The closure to invoke.
 * @param args A scheme list of parameters.
 * @returns The result of evaluating the closure.
 */
static pointer closure_exec_with_scheme_args(closure_t *closure, pointer args)
{
        pointer result;

        /* Lock the closure against deletion while it is being called. */
        closure_ref(closure);

        /* Straight procedure call? */
        if (scm_is_closure(closure->sc, closure->code)) {
                result = scheme_call(closure->sc, closure->code, args);
        }

        /* Need to lookup it up first? */
        else if (scm_is_sym(closure->sc, closure->code)) {

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
                result = scheme_call(closure->sc, proc, args);

        }

        /* Invalid or garbage-collected closure? */
        else {
                /* There's a bug somewhere. Happy hunting. */
                assert(false);
        }

        closure_unref(closure);
        return result;
}

/** 
 * Evaluate a Scheme procedure with C-style varargs.
 * @param closure The procedure to evaluate.
 * @param fmt Character encoding for the args, similar to printf, but the codes
 * are different. See kern.c's vpack() function.
 * @returns The Scheme result of evaluation.
 */
pointer closure_execv(closure_t *closure, const char *fmt, va_list args)
{
        pointer head;

        /* Convert the C args to Scheme. */
        if (fmt) {
                head = vpack(closure->sc, fmt, args);
        } else {
                head = closure->sc->NIL;
        }

        return closure_exec_with_scheme_args(closure, head);
}

/* Do our best to translate a Scheme evaluation result into a C integer. */
int closure_translate_result(scheme *sc, pointer result)
{
        if (result == sc->NIL ||
            result == sc->F) {
                return 0;
        } 
        
        if (sc->vptr->is_number(result)) {
                if (sc->vptr->is_integer(result)) {
                        return sc->vptr->ivalue(result);
                }
                /* coerce it */
                return (int)sc->vptr->rvalue(result);
        }
        
        if (scm_is_sym(sc, result)) {
                pointer pair;
                pair = sc->vptr->find_slot_in_env(sc, 
                                                  sc->envir, 
                                                  result, 
                                                  1);
                assert(scm_is_pair(sc, pair));
                result = sc->vptr->pair_cdr(pair);
                /* recursive call... */
                return closure_translate_result(sc, result);
        }
        
        if (scm_is_ptr(sc, result)) {
                return (long)sc->vptr->ffvalue(result);
        }

        return 1;
}

closure_t *closure_new(scheme *sc, pointer code)
{
        closure_t *clx = (closure_t*)calloc(1, sizeof(*clx));
        assert(clx);
        clx->sc = sc;
        clx->code = code;
        sc->vptr->protect(sc, code);
        return clx;
}

closure_t *closure_new_ref(scheme *sc, pointer code)
{
        closure_t *clx = closure_new(sc, code);
        closure_ref(clx);
        return clx;
}

void closure_init(closure_t *clx, scheme *sc, pointer code)
{
        clx->sc = sc;
        clx->code = code;
}

int closure_exec(closure_t *closure, const char *fmt, ...)
{
        pointer result;
        va_list ap;
        int ret = 1;

        closure_ref(closure);

        va_start(ap, fmt);
        result = closure_execv(closure, fmt, ap);
        va_end(ap);

        ret = closure_translate_result(closure->sc, result);

        closure_unref(closure);
        return ret;
}

void closure_save(closure_t *closure, struct save *save)
{
        char *proc_name;

        assert(scm_is_sym(closure->sc, closure->code));

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
        assert(closure->ref > 0);
        closure->ref--;
        if (! closure->ref)
                closure_del(closure);
}

int closure_execlpv(closure_t *closure, pointer cell, void *ptr, 
                    const char *fmt, va_list args)
{
        pointer head, tmp, result;
        scheme *sc = closure->sc;

        /* Convert the C args to Scheme. */
        if (fmt) {
                head = vpack(sc, fmt, args);
        } else {
                head = sc->NIL;
        }

        /* Protect the list while allocating cells. */
        tmp = head;
        if (tmp != sc->NIL) {
                sc->vptr->protect(sc, tmp);
        }

        /* Prepend the cell and ptr to the arg list. */
        head = _cons(sc, scm_mk_ptr(sc, ptr), head, 0);
        head = _cons(sc, cell, head, 0);

        /* Unprotect the list now that we're done allocating. */
        if (tmp != sc->NIL) {
                sc->vptr->unprotect(sc, tmp);
        }

        /* Evaluate the closure. */
        result = closure_exec_with_scheme_args(closure, head);

        /* Translate the result to an int. */
        return closure_translate_result(closure->sc, result);
}

int closure_execlpiv(closure_t *closure, pointer cell, void *ptr, int id,
                    const char *fmt, va_list args)
{
        pointer head, tmp, result;
        scheme *sc = closure->sc;

        /* Convert the C args to Scheme. */
        if (fmt) {
                head = vpack(sc, fmt, args);
        } else {
                head = sc->NIL;
        }

        /* Protect the list while allocating cells. */
        tmp = head;
        if (tmp != sc->NIL) {
                sc->vptr->protect(sc, tmp);
        }

        /* Prepend the cell, ptr and id to the arg list. */
        head = _cons(sc, scm_mk_integer(sc, id), head, 0);
        head = _cons(sc, scm_mk_ptr(sc, ptr), head, 0);
        head = _cons(sc, cell, head, 0);

        /* Unprotect the list now that we're done allocating. */
        if (tmp != sc->NIL) {
                sc->vptr->unprotect(sc, tmp);
        }

        /* Evaluate the closure. */
        result = closure_exec_with_scheme_args(closure, head);

        /* Translate the result to an int. */
        return closure_translate_result(closure->sc, result);
}

int closure_execvl(closure_t *closure, const char *fmt, va_list args, pointer cell)
{
        pointer head, result;
        scheme *sc = closure->sc;

        /* Convert the C args to Scheme. */
        if (fmt) {
                head = vpack(sc, fmt, args);
        } else {
                head = sc->NIL;
        }

        /* Append args to the list */
        if (head == sc->NIL) {
                /* args is the only thing on the list */
                head = _cons(sc, cell, sc->NIL, 0);
        } else {

                /* Protect the list while allocating for _cons */
                sc->vptr->protect(sc, head);

                /* Find the end of the list */
                pointer tail = head;
                while (scm_cdr(sc, tail) != sc->NIL) {
                        tail = scm_cdr(sc, tail);
                }

                /* Append the args to the tail of the list */
                tail->_object._cons._cdr = _cons(sc, cell, sc->NIL, 0);
                
                /* Unprotect the list now that we're done allocating. */
                sc->vptr->unprotect(sc, head);
        }

        /* Evaluate the closure. */
        result = closure_exec_with_scheme_args(closure, head);

        /* Translate the result to an int. */
        return closure_translate_result(closure->sc, result);
}
