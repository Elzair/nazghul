#ifndef gob_h
#define gob_h

#include "scheme-private.h"

#define GOB_SAVECAR (1 << 0)

struct gob {
        scheme *sc;
        pointer p;
        int flags;
        int refcount;
};

#define gob_ref(gob) ((gob)->refcount++)

extern struct gob *gob_new(scheme *sc, pointer p);
extern void gob_del(struct gob *gob);
extern void gob_save(struct gob *gob, struct save *save);
extern void gob_unref(struct gob *gob);

#endif
