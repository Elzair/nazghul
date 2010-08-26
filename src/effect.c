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

#include "effect.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

const int EFFECT_ID = 0xeffec1;

struct effect *effect_new(char *tag, scheme *sc, pointer exec_proc,
                          pointer apply_proc, pointer rm_proc,
                          pointer restart,
                          char *name)
{
        struct effect *et;

        et = (struct effect*)calloc(1, sizeof(*et));
        assert(et);

        et->ID = EFFECT_ID;

        if (exec_proc) {
                et->exec = closure_new_ref(sc, exec_proc);
        }

        if (apply_proc) {
                et->apply = closure_new_ref(sc, apply_proc);
        }

        if (rm_proc) {
                et->rm = closure_new_ref(sc, rm_proc);
        }

        if (restart) {
                et->restart = closure_new_ref(sc, restart);
        }

        et->tag = strdup(tag);
        assert(et->tag);

        /* Effects with no name should be considered invisible to the UI */
        if (name) {
                et->name = strdup(name);
                assert(et->name);
        }

        return et;
}

extern void effect_del(struct effect *et)
{
        free(et->tag);
        free(et->name);
        closure_unref_safe(et->exec);
        closure_unref_safe(et->apply);
        closure_unref_safe(et->rm);
        closure_unref_safe(et->restart);
        /* Need to free sprite? Nope -- sprites are global resources
         * managed by the session. When the session is torn down it frees all
         * of the sprites created when it loaded. */
        free(et);
}
