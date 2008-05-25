#include "gob.h"
#include "session.h"

#include <assert.h>
#include <stdlib.h>

#define scm_car(sc, arg) ((sc)->vptr->pair_car(arg))
#define scm_cdr(sc, arg) ((sc)->vptr->pair_cdr(arg))

/* Fixme: gob_new should start with refcount 1 */
struct gob *gob_new(scheme *sc, pointer p)
{
        struct gob *gob = (struct gob *)calloc(1, sizeof(*gob));
        assert(gob);
        gob->sc = sc;
        gob->p = p;
        sc->vptr->protect(sc, p);
        return gob;
}

/* Fixme: this should not be public; clients should be required to use gob_unref */
void gob_del(struct gob *gob)
{
        gob->sc->vptr->unprotect(gob->sc, gob->p);
        free(gob);
}

void gob_save(struct gob *gob, struct save *save)
{
        pointer cell;

        /* The car of the gob pointer is often non-data (kobj, etc). So only
         * serialize the car of the cdr unless the creator specifically wants
         * to. */
        cell = (gob->flags & GOB_SAVECAR) ? gob->p : 
                scm_car(gob->sc, scm_cdr(gob->sc, gob->p));

        scheme_serialize(gob->sc, cell, save);
}

void gob_unref(struct gob *gob)
{
        assert(gob->refcount > 0);
        gob->refcount--;
        if (! gob->refcount)
                gob_del(gob);
}
