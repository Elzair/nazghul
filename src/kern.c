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

#include "session.h"

#include "character.h"
#include "cmd.h"
#include "ctrl.h"
#include "dice.h"
#include "effect.h"
#include "event.h"
#include "gob.h"
#include "object.h"
#include "sched.h"
#include "sprite.h"
#include "terrain.h"
#include "vmask.h"
#include "wq.h"
#include "place.h"
#include "ptable.h"
#include "images.h"
#include "Party.h"
#include "common.h"
#include "player.h"
#include "sky.h"
#include "ascii.h"
#include "map.h"
#include "cursor.h"
#include "Arms.h"
#include "Field.h"
#include "occ.h"
#include "species.h"
#include "Reagent.h"
#include "screen.h"
#include "vehicle.h"
#include "formation.h"
#include "combat.h"
#include "Container.h"
#include "clock.h"
#include "wind.h"
#include "foogod.h"
#include "sound.h"
#include "Missile.h"
#include "conv.h"
#include "mmode.h"
#include "log.h"
#include "dtable.h"
#include "factions.h"
#include "cmdwin.h"

#include <assert.h>
#include <ctype.h>              // isspace()
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <SDL_image.h>
#include <unistd.h>
#include <stdarg.h>
#include <SDL.h>            // for SDL_GetTicks()

#include "scheme-private.h"

#define scm_protect(sc, cell) \
        (sc)->vptr->protect((sc), (cell))

#define scm_unprotect(sc, cell) \
        (sc)->vptr->unprotect((sc), (cell))

#define scm_mk_ptr(sc, val) \
        (sc)->vptr->mk_foreign_func((sc), (foreign_func)(val))

#define scm_mk_integer(sc, val) \
        (sc)->vptr->mk_integer((sc), (val))

#define scm_mk_symbol(sc, val) \
        (sc)->vptr->mk_symbol((sc), (val))

#define scm_mk_string(sc, val) \
        (sc)->vptr->mk_string((sc), (val))

#define scm_define(sc, sym, val) \
        (sc)->vptr->scheme_define((sc),  \
                                  (sc)->global_env,  \
                                  (sc)->vptr->mk_symbol(sc, (sym)), \
                                  (val))

#define API_DECL(sc, sym, val) \
        scm_define(sc, sym, scm_mk_ptr(sc, val))

#define scm_define_int(sc, sym, val) \
        scm_define(sc, sym, (sc)->vptr->mk_integer((sc), (val)))

#define scm_define_bool(sc, sym, val) \
        scm_define(sc, sym, (val) ? (sc)->T : (sc)->F)

#define scm_define_ptr(sc, sym, val) \
        scm_define(sc, sym, scm_mk_ptr(sc, val))

#define scm_is_pair(sc,arg) ((sc)->vptr->is_pair(arg))
#define scm_is_num(sc, arg) ((sc)->vptr->is_number(arg))
#define scm_is_int(sc, arg) ((sc)->vptr->is_integer(arg))
#define scm_is_real(sc, arg) ((sc)->vptr->is_real(arg))
#define scm_is_str(sc, arg) ((sc)->vptr->is_string(arg))
#define scm_is_sym(sc, arg) ((sc)->vptr->is_symbol(arg))
#define scm_is_ptr(sc, arg) ((sc)->vptr->is_foreign(arg))
#define scm_is_closure(sc, arg) ((sc)->vptr->is_closure(arg))

#define scm_car(sc, arg) ((sc)->vptr->pair_car(arg))
#define scm_cdr(sc, arg) ((sc)->vptr->pair_cdr(arg))

#define scm_str_val(sc, arg) ((sc)->vptr->string_value(arg))
#define scm_ptr_val(sc, arg) ((void*)(arg)->_object._ff)
#define scm_int_val(sc, arg) ((sc)->vptr->ivalue(arg))
#define scm_real_val(sc, arg) ((sc)->vptr->rvalue(arg))
#define scm_sym_val(sc, arg) ((sc)->vptr->symname(arg))
#define scm_closure_code(sc, arg) ((sc)->vptr->closure_code(arg))
#define scm_closure_env(sc, arg) ((sc)->vptr->closure_env(arg))

#define KERN_API_CALL(name) static pointer name(scheme *sc, pointer args)


#define TAG_UNK "<tag?>"

/*****************************************************************************
 *
 * kjob - wrapper for work queue jobs
 *
 *****************************************************************************/
struct kjob {
        void *data;
        closure_t *clx;
};

static struct kjob * kjob_new(void *data, closure_t *clx)
{
        struct kjob *kjob;
        kjob = (struct kjob *)malloc(sizeof(*kjob));
        assert(kjob);
        kjob->data = data;
        kjob->clx = clx;
        return kjob;
}

static void kjob_del(struct kjob *kjob)
{
        closure_del(kjob->clx);
        free(kjob);
}

static void kern_run_wq_job(struct wq_job *job, struct list *wq)
{
        struct kjob *kjob;
        kjob = (struct kjob*)job->data;
        //dbg("kjob_run: %08lx\n", kjob);
        closure_exec(kjob->clx, "p", kjob->data);
        kjob_del(kjob);
        wq_job_del(job);
}
/*****************************************************************************/

/* Struct used by callbacks which build scheme lists */
struct kern_append_info {
        scheme *sc;
        pointer head;
        pointer tail;
        int (*filter)(Object *, struct kern_append_info *);
        void *data;
};


static int scm_len(scheme *sc, pointer list)
{
        int len = 0;

        while (scm_is_pair(sc, list)) {
                len++;
                list = scm_cdr(sc, list);
        }

        return len;
}

static void image_dtor(void *val)
{
        images_del((struct images*)val);
}

static void sprite_dtor(void *val)
{
        sprite_del((struct sprite*)val);
}

static void terrain_dtor(void *val)
{
        terrain_del((struct terrain*)val);
}

static void sound_dtor(void *val)
{
        sound_del((sound_t*)val);
}

static void terrain_palette_dtor(void *val)
{
        terrain_palette_del((struct terrain_palette*)val);
}

static void terrain_map_dtor(void *val)
{
        terrain_map_del((struct terrain_map*)val);
}

static void incfile_dtor(void *val)
{
        free((char*)val);
}

static void incfile_save(save_t *save, void *val)
{
        save->write(save, "(kern-load \"%s\")\n\n", (char*)val);
}

static void place_dtor(void *val)
{
        place_del((struct place*)val);
}

static void species_dtor(void *val)
{
        species_del((struct species*)val);
}

static void occ_dtor(void *val)
{
        occ_del((struct occ*)val);
}

static void party_dtor(void *val)
{
        delete (class PartyType*)val;
}

static void arms_type_dtor(void *val)
{
        delete (class ArmsType *)val;
}

static void field_type_dtor(void *val)
{
        delete (class FieldType *)val;
}

static void obj_type_dtor(void *val)
{
        delete (class ObjectType *)val;
}

static void vehicle_type_dtor(void *val)
{
        delete (class VehicleType*)val;
}


static int unpack(scheme *sc, pointer *cell, char *fmt, ...)
{
        va_list args;
        int expect, count = 0, errs = 0;
        pointer car;
        char **strval;
        int *ival;
        float *rval;
        void **ptrval;
        pointer *cval;

        expect = strlen(fmt);

        va_start(args, fmt);

        while (*fmt && scm_is_pair(sc, *cell)) {

                count++;
                car = scm_car(sc, *cell);
                *cell = scm_cdr(sc, *cell);

                switch(*fmt++) {
                case 'b': /* bool */
                        ival = va_arg(args, int*);
                        if (car == sc->T) {
                                *ival = 1;
                        } else if (car == sc->F) {
                                *ival = 0;
                        } else {
                                errs++;
                                load_err("arg %d not a bool", count);
                        }
                        break;
                case 'c': /* closure */
                        cval = va_arg(args, pointer*);
                        if (car == sc->NIL) {
                                *cval = sc->NIL;
                        } else if (! scm_is_sym(sc, car)) {
                                errs++;
                                load_err("arg %d not a symbol", count);
                        } else {
                                *cval = car;
                        }
                        break;
                case 'd': /* integer */
                        ival = va_arg(args, int*);
                        if (! scm_is_num(sc, car)) {
                                errs++;
                                load_err("arg %d not a number", count);
                        } else if (! scm_is_int(sc, car)) {
                                /*errs++;
                                  load_err("arg %d not an int", count);*/
                                /* coerce it */
                                *ival = (int)scm_real_val(sc, car);
                        } else {
                                *ival = scm_int_val(sc, car);
                        }
                        break;
                case 'o': /* procedure */
                        cval = va_arg(args, pointer*);
                        if (car == sc->NIL) {
                                *cval = sc->NIL;
                        } else if (! scm_is_closure(sc, car)) {
                                errs++;
                                load_err("arg %d not a closure", count);
                        } else {
                                *cval = car;
                        }
                        break;
                case 'p': /* C pointer */
                        ptrval = va_arg(args, void**);
                        if (car == sc->NIL) {
                                *ptrval = 0;
                        } else if (scm_is_ptr(sc, car)) {
                                *ptrval = scm_ptr_val(sc, car);
                        } else {
                                errs++;
                                load_err("arg %d not a C ptr", count);
                        }
                        break;
                case 'r': /* real number */
                        rval = va_arg(args, float*);
                        if (! scm_is_num(sc, car)) {
                                errs++;
                                load_err("arg %d not a number", count);
                        } else if (! scm_is_real(sc, car)) {
                                errs++;
                                load_err("arg %d not an int", count);
                        } else {
                                *rval = scm_real_val(sc, car);
                        }
                        break;
                case 's': /* string */
                        strval = va_arg(args, char**);
                        if (car == sc->NIL) {
                                *strval = 0;
                        } else if (scm_is_str(sc, car)) {
                                *strval = scm_str_val(sc, car);
                        } else {
                                errs++;
                                load_err("arg %d not a string", count);
                        }
                        break;
                case 'y': /* symbol */
                        strval = va_arg(args, char**);
                        if (car == sc->NIL) {
                                *strval = 0;
                        } else if (scm_is_sym(sc, car)) {
                                *strval = scm_sym_val(sc, car);
                        } else {
                                errs++;
                                load_err("arg %d not a symbol", count);
                        }
                        break;
                case 'l':
                        cval = va_arg(args, pointer*);
                        *cval = car;
                        break;
                default:
                        dbg("unknown format char: %c\n", *(fmt - 1));
                        assert(0);
                        break;
                }
        }
        
        if (*fmt) {
                load_err("received only %d of %d arguments",
                         count, count + strlen(fmt));
        }

        va_end(args);

        return (! *fmt && ! errs) ? 0 : -1;
}

static Object *unpack_obj(scheme *sc, pointer *args, char *caller)
{
        class Object *obj;

        if (unpack(sc, args, "p", &obj)) {
                rt_err("%s: bad args", caller);
                return NULL;
        }

        if (obj == NULL) {
                rt_err("%s: null kernel object", caller);
                return NULL;
        }

        return obj;
}

static int unpack_loc(scheme *sc, pointer *args, struct place **place, int *x,
                      int *y, char *func)
{
        pointer loc;

        if (! scm_is_pair(sc, *args)) {
                rt_err("%s: location not a list", func);
                return -1;
        }

        loc = scm_car(sc, *args);
        *args = scm_cdr(sc, *args);

        if (unpack(sc, &loc, "pdd", place, x, y)) {
                rt_err("%s: bad location list", func);
                return -1;
        }

        if (!place) {
                rt_err("%s: null place", func);
                return -1;
        }

        return 0;
}

pointer vpack(scheme *sc, char *fmt, va_list ap)
{
        pointer head;
        pointer tail;
        pointer cell;
        pointer arg;
        void *ptr;
        int ival;
        char *strval;

        head = sc->NIL;

        while (*fmt) {

                switch(*fmt++) {
                case 'p':
                        ptr = va_arg(ap, void*);
                        arg = scm_mk_ptr(sc, ptr);
                        break;
                case 'd':
                        ival = va_arg(ap, int);
                        arg = scm_mk_integer(sc, ival);
                        break;
                case 'y':
                        strval = va_arg(ap, char*);
                        arg = scm_mk_symbol(sc, strval);
                        break;
                case 'l':
                        arg = va_arg(ap, pointer);
                        break;
                deefault:
                        assert(false);
                        break;
                }

                /* Note: arg already protected during _cons */
                cell = _cons(sc, arg, sc->NIL, 0);

                if (head == sc->NIL) {
                        head = cell;
                        tail = cell;

                        /* By protecting the head we protect the rest of the
                         * list */
                        scm_protect(sc, head);
                } else {
                        tail->_object._cons._cdr = cell;
                        tail = cell;
                }
        }

        /* Allocations are over so unprotect the list */
        if (head != sc->NIL)
                scm_unprotect(sc, head);

        return head;
}

struct mview * kern_unpack_mview(scheme *sc, pointer *args, char *caller)
{
        struct mview *obj;

        if (unpack(sc, args, "p", &obj)) {
                rt_err("%s: bad args", caller);
                return NULL;
        }

        if (obj == NULL) {
                rt_err("%s: null view object");
                return NULL;
        }

        return obj;
}

static int kern_unpack_loc(scheme *sc, pointer *args, struct place **place,
                           int *x, int *y, char *caller)
{
        pointer loc;

        loc = scm_car(sc, *args);
        if (! scm_is_pair(sc, loc)) {
                rt_err("%s: location not a list", caller);
                return 0;
        }

        *args = scm_cdr(sc, *args);

        if (unpack(sc, &loc, "pdd", place, x, y)) {
                rt_err("%s: bad location args", caller);
                return 0;
        }

        if (!(*place)) {
                rt_err("%s: null place", caller);
                return 0;
        }
        
        return 1;
}

static pointer pack(scheme *sc, char *fmt, ...)
{
        pointer head;
        va_list ap;

        va_start(ap, fmt);
        head = vpack(sc, fmt, ap);
        va_end(ap);

        return head;
}

static pointer kern_mk_sprite_set(scheme *sc, pointer args)
{
        int width, height, rows, cols, offx, offy, argno = 1;
        char *fname;
        struct images *image = NULL;
        char *tag = TAG_UNK;
        pointer ret;

        if (unpack(sc, &args, "ydddddds", &tag, &width, &height, &rows, &cols,
                   &offx, &offy, &fname)) {
                load_err("kern-mk-sprite-set %s: bad args", tag);
                return sc->NIL;
        }

        image = images_new(tag, width, height, rows, cols, offx, offy, fname);
        session_add(Session, image, image_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, image);
        scm_define(sc, tag, ret);

        return ret;
}

static pointer kern_mk_sprite(scheme *sc, pointer args)
{
        struct images *images;
        int n_frames, index, facings, wave, argno = 1;
        struct sprite *sprite;
        char *tag = TAG_UNK;
        pointer ret;

        if (unpack(sc, &args, "ypddbd", &tag, &images, &n_frames, &index,
                   &wave, &facings)) {
                load_err("kern-mk-sprite %s: bad args", tag);
                return sc->NIL;
        }

        sprite = sprite_new(tag, n_frames, index, wave, facings, images); 
        session_add(Session, sprite, sprite_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, sprite);
        scm_define(sc, tag, ret);

        return ret;
}

struct connection {
        Object *from;
        char *to;
};

static struct connection *connection_new(class Object *from, char *to)
{
        struct connection *con;
        con = (struct connection*)malloc(sizeof(*con));
        assert(con);
        con->from = from;
        con->to = strdup(to);
        assert(con->to);
        return con;
}

static void connection_save(struct save *save, void *val)
{
        struct connection *con = (struct connection*)val;
        save->write(save, "(connect %s '%s)\n", con->from->tag,
                    con->to);
}

static void connection_dtor(void *val)
{
        struct connection *con = (struct connection*)val;
        free(con->to);
        free(con);
}

static pointer kern_mk_terrain(scheme *sc, pointer args)
{
        int pmask, movement_cost, alpha, light;
        void *sprite;
        struct terrain *terrain;
        char *tag = TAG_UNK, *name;
        pointer ret;
        int pclass;
        pointer proc = NULL;
        closure_t *clx = NULL;

        /* Revisit: ignore effects for now */

        if (unpack(sc, &args, "ysdpddc", &tag, &name, &pclass, &sprite, 
                   &alpha, &light, &proc)) {
                load_err("kern-mk-terrain %s: bad args", tag);
                return sc->NIL;
        }

        terrain = terrain_new(tag, name, (struct sprite*)sprite, pclass, 
                              alpha, light);

        if (proc != sc->NIL) {
                terrain->effect = closure_new(sc, proc);
        }

        session_add(Session, terrain, terrain_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, terrain);
        scm_define(sc, tag, ret);

        return ret;
}

static pointer kern_mk_sound(scheme *sc, pointer args)
{
        sound_t *sound;
        char *tag = TAG_UNK, *name;
        pointer ret;

        if (unpack(sc, &args, "ys", &tag, &name)) {
                load_err("kern-mk-sound %s: bad args", tag);
                return sc->NIL;
        }

        sound = sound_new(tag, name);

        /* Add it to the session */
        session_add(Session, sound, sound_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, sound);
        scm_define(sc, tag, ret);

        return ret;
}

static pointer kern_mk_palette(scheme *sc, pointer args)
{
        int argno = 1;
         char *tag = TAG_UNK;
        pointer ret;

        if (Session->palette) {
                load_err("kern-mk-palette: %s already set as the session "\
                         "palette",
                         Session->palette->tag);
                return sc->NIL;
        }

        if (unpack(sc, &args, "y", &tag)) {
                load_err("kern-mk-palette %s: bad args", tag);
                return sc->NIL;
        }

        Session->palette = terrain_palette_new(tag);

        /* The next argument after the tag shoud be a list of (glyph, terrain)
         * pairs. Since these are pairs - and not lists - */

        if (! scm_is_pair(sc, args)) {
                load_err("kern-mk-palette arg %d: arg list too short", argno);
                terrain_palette_del(Session->palette);
                return sc->NIL;
        }

        args = scm_car(sc, args);
        argno = 1;

        while (scm_is_pair(sc, args)) {

                char *glyph;
                void *terrain;
                pointer pair;

                pair = scm_car(sc, args);
                args = scm_cdr(sc, args);
                unpack(sc, &pair, "sp", &glyph, &terrain);
                terrain_palette_add(Session->palette, glyph, 
                                    (struct terrain*)terrain);
        }

        session_add(Session, Session->palette, terrain_palette_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, Session->palette);
        scm_define(sc, tag, ret);

        return ret;
}

static pointer kern_mk_map(scheme *sc, pointer args)
{
        int width, height, x, y, i;
        struct terrain_palette *pal;
        char *tag = TAG_UNK;
        struct terrain_map *map;
        pointer ret;

        if (unpack(sc, &args, "yddp", &tag, &width, &height, &pal)) {
                load_err("kern-mk-map %s: bad args", tag);
                return sc->NIL;
        }

        /* Final argument should be a list of strings. */

        if (! scm_is_pair(sc, args)) {
                load_err("kern-mk-map %s: arg list too short", tag);
                return sc->NIL;
        }

        args = scm_car(sc, args);
        map = terrain_map_new(tag, width, height, pal);
        i = 0;

        for (y = 0; y < height; y++) {

                char *map_line;
                char *glyph;

               if (unpack(sc, &args, "s", &map_line))
                        goto abort;

                for (x = 0; x < width; x++) {

                        struct terrain *tt;

                        glyph = strtok(x ? NULL : map_line, " ");
                        if (! glyph) {
                                load_err("kern-mk-map %s: line %d only "\
                                         "%d wide, should be %d wide", tag,
                                         y, x, width);
                                goto abort;
                        }

                        tt = palette_terrain_for_glyph(pal, glyph);
                        if (! tt) {
                                load_err("kern-mk-map %s: line %d "\
                                         "column %d: glyph %s "\
                                         "has no terrain in palette %s", tag,
                                         y, x, glyph, pal->tag);
                                goto abort;
                        }

                        map->terrain[i] = tt;
                        i++;
                        
                }
        }

        map->handle = session_add(Session, map, terrain_map_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, map);

        /* Embedded maps (those defined within and used exclusively for) place
         * constructors may not have and do not need tags. */
        if (tag)
                scm_define(sc, tag, ret);

        return ret;

 abort:
        terrain_map_del(map);
        return sc->NIL;
}

static int kern_place_load_subplaces(scheme *sc, pointer *args, struct place *place)
{
        pointer subplaces;

        if (! scm_is_pair(sc, *args)) {
                load_err("kern-mk-place %s: missing the subplaces list",
                         place->tag);
                return -1;
        }

        subplaces = scm_car(sc, *args);
        *args = scm_cdr(sc, *args);

        while (scm_is_pair(sc, subplaces)) {
                struct place *subplace = 0;
                int x, y;
                pointer cell;

                cell = scm_car(sc, subplaces);
                subplaces = scm_cdr(sc, subplaces);

                if (unpack(sc, &cell, "pdd", &subplace, &x, &y)) {
                        load_err("kern-mk-place %s: bad arg in subplaces list",
                                 place->tag);
                        return -1;
                }

                if (!subplace) {
                        load_err("kern-mk-place %s: null place in subplaces "\
                                 "list", place->tag);
                        return -1;
                }

                if (subplace->magic != PLACE_MAGIC) {
                        load_err("kern-mk-place %s: subplace is not a place",
                                 place->tag);
                        return -1;
                }

                if (place_add_subplace(place, subplace, x, y)) {
                        load_err("kern-mk-place %s: failed to put %s as a "\
                                 "subplace at [%d, %d]; is another subplace "\
                                 "already there? Are the coordinates off-map?",
                                 place->tag, subplace->tag, x, y);
                        return -1;
                }
        }

        return 0;
}

static int kern_place_load_neighbors(scheme *sc, pointer *args, 
                                     struct place *place)
{
        pointer neighbors;

        if (! scm_is_pair(sc, *args)) {
                load_err("kern-mk-place %s: missing the contents list",
                         place->tag);
                return -1;
        }

        neighbors = scm_car(sc, *args);
        *args = scm_cdr(sc, *args);

        while (scm_is_pair(sc, neighbors)) {
                int dir;
                struct place *neighbor;
                pointer cell;

                cell = scm_car(sc, neighbors);
                neighbors = scm_cdr(sc, neighbors);
                
                if (unpack(sc, &cell, "pd", &neighbor, &dir)) {
                        load_err("kern-mk-place %s: error in neighbor list", 
                                 place->tag);
                        return -1;
                }
                
                switch(dir) {
                case UP:
                        place->above = neighbor;
                        neighbor->below = place;
                        break;
                case DOWN:
                        place->below = neighbor;
                        neighbor->above = place;
                        break;
                default:
                        load_err("kern-mk-place %s: invalid direction in "\
                                 "neighbor list: %s", place->tag, 
                                 directionToString(dir));
                        return -1;
                        break;
                }
        }

        return 0;
}

static int kern_place_load_contents(scheme *sc, pointer *args, 
                                    struct place *place)
{
        pointer contents;

        if (! scm_is_pair(sc, *args)) {
                load_err("kern-mk-place %s: missing the contents list",
                         place->tag);
                return -1;
        }

        contents = scm_car(sc, *args);
        *args = scm_cdr(sc, *args);

        while (scm_is_pair(sc, contents)) {

                class Object *obj = 0;
                int x, y;
                pointer cell;

                cell = scm_car(sc, contents);
                contents = scm_cdr(sc, contents);

                if (unpack(sc, &cell, "pdd", &obj, &x, &y)) {
                        load_err("kern-mk-place %s: bad arg in content list",
                                 place->tag);
                        return -1;
                }

                if (!obj) {
                        load_err("kern-mk-place %s: null obj in content list",
                                 place->tag);
                        return -1;
                }

                obj->relocate(place, x, y, true);
        }

        return 0;
}

static int kern_place_load_hooks(scheme *sc, pointer *args, 
                                    struct place *place)
{
        pointer contents;
        pointer pre_entry_proc;

        if (! scm_is_pair(sc, *args)) {
                load_err("kern-mk-place %s: missing the hooks list",
                         place->tag);
                return -1;
        }

        contents = scm_car(sc, *args);
        *args = scm_cdr(sc, *args);

        if (scm_is_pair(sc, contents)) {
                if (unpack(sc, &contents, "c", &pre_entry_proc)) {
                        load_err("kern-mk-place %s: bad arg in hook list",
                                 place->tag);
                        return -1;
                }

                place->pre_entry_hook = closure_new(sc, pre_entry_proc);

        }

        return 0;
}

static int kern_place_load_entrances(scheme *sc, pointer *args, 
                                     struct place *place)
{
        pointer entrances;

        if (! scm_is_pair(sc, *args)) {
                load_err("kern-mk-place %s: missing the entrances list",
                         place->tag);
                return -1;
        }

        entrances = scm_car(sc, *args);
        *args = scm_cdr(sc, *args);

        while (scm_is_pair(sc, entrances)) {

                int dir, x, y;
                pointer cell;

                cell = scm_car(sc, entrances);
                entrances = scm_cdr(sc, entrances);

                if (unpack(sc, &cell, "ddd", &dir, &x, &y)) {
                        load_err("kern-mk-place %s: bad arg in entrances list",
                                 place->tag);
                        return -1;
                }

                if (place_set_edge_entrance(place, dir, x, y)) {
                        load_err("kern-mk-place %s: failed to set entrance for "\
                                 "direction %d to [%d %d]", place->tag, dir, x, y);
                        return -1;
                }
        }

        return 0;
}

KERN_API_CALL(kern_mk_place)
{
        int wild, wraps, underground, combat, argno = 1;
        struct terrain_map *map;
        struct place *place;
        struct sprite *sprite;
        char *tag = TAG_UNK, *name;
        pointer contents;
        pointer subplaces;
        pointer ret;

        if (unpack(sc, &args, "ysppbbbb", &tag, &name, &sprite, &map,
                   &wraps, &underground, &wild, &combat)) {
                load_err("kern-mk-place %s: bad args", tag);
                return sc->NIL;
        }

        if ( ! map->handle) {
                load_err("kern-mk-place %s: map %s has no session handle; "\
                         "is it already being used in another place?", tag, 
                         map->tag);
                return sc->NIL;
        }

        session_rm(Session, map->handle);
        map->handle = 0;
        place = place_new(tag, name, sprite, map, wraps, underground, wild, 
                          combat);


        if (kern_place_load_subplaces(sc, &args, place) ||
            kern_place_load_neighbors(sc, &args, place) ||
            kern_place_load_contents(sc, &args, place)  ||
            kern_place_load_hooks(sc, &args, place)     ||
            kern_place_load_entrances(sc, &args, place))
                goto abort;

 done:
        place->handle = session_add(Session, place, place_dtor, place_save, place_start);
        ret = scm_mk_ptr(sc, place);
        scm_define(sc, tag, ret);
        return ret;

 abort:
        place_del(place);
        return sc->NIL;
}

static pointer kern_mk_party_type(scheme *sc, pointer args)
{
        class PartyType *party;
        struct sprite *sprite;
        char *tag = TAG_UNK, *name;
        struct formation *formation;
        pointer groups;
        pointer ret;

        if (unpack(sc, &args, "yspp", &tag, &name, &sprite, &formation)) {
                load_err("kern-mk-party %s: bad args", tag);
                return sc->NIL;
        }

        party = new PartyType(tag, name, sprite);
        party->formation = formation;

        groups = scm_car(sc, args);
        if (! scm_is_pair(sc, groups)) {
                load_err("kern-mk-party %s: no groups", tag);
                goto abort;
        }

        while (scm_is_pair(sc, groups)) {

                struct species *species;
                struct occ *occ;
                char *dice;
                pointer group;
                pointer ai;
                struct closure *ai_clx;

                group = scm_car(sc, groups);
                groups = scm_cdr(sc, groups);

                if (unpack(sc, &group, "pppsc", &species, &occ, &sprite, &dice, &ai)) {
                        load_err("kern-mk-party %s: error in group list", tag);
                        goto abort;
                }

                if (!dice_valid(dice)) {
                        load_err("kern-mk-party %s: bad dice format '%s'", dice);
                        goto abort;
                }

                if (ai == sc->NIL)
                        ai_clx = NULL;
                else {
                        ai_clx = closure_new(sc, ai);
                }

                party->addGroup(species, occ, sprite, dice, ai_clx);
        }
        
        session_add(Session, party, party_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, party);
        scm_define(sc, tag, ret);
        return ret;

 abort:
        delete party;
        return sc->NIL;
}

static bool more_args(scheme *sc, pointer args, char *func, char *tag, 
                     int argno)
{
        if (! scm_is_pair(sc, args)) {
                load_err("%s %s arg %d: arg list too short", 
                         func, tag, argno);
                return false;
        }
        return true;
}

static pointer kern_mk_species(scheme *sc, pointer args)
{
        struct species *species;
        int str, intl, dex, spd, vr, hpmod, hpmult, argno = 1;
        int mpmod, mpmult, visible, n_slots, n_spells, i;
        struct sprite *sleep_sprite;
        class ArmsType *weapon;
        char *tag = TAG_UNK, *name;
        sound_t *damage_sound, *walking_sound;
        pointer slots;
        pointer spells;
        pointer ret;
        struct mmode *mmode;
        pointer on_death;

        if (unpack(sc, &args, "ysdddddpddddppbppc", &tag, &name, &str, 
                   &intl, &dex, &spd, &vr, &mmode, &hpmod, &hpmult, &mpmod, 
                   &mpmult, &sleep_sprite, &weapon, 
                   &visible, &damage_sound, &walking_sound, &on_death)) {
                load_err("kern-mk-species %s: bad args", tag);
                return sc->NIL;
        }

        if (scm_len(sc, args) < 2) {
                load_err("kern-mk-species %s: arg list too short", tag,
                         argno++);
                return sc->NIL;
        }

        if (! mmode) {
                load_err("kern-mk-species %s: null mmode", tag);
                return sc->NIL;
        }
        

        /* get the list of slots */
        slots = scm_car(sc, args);
        args = scm_cdr(sc, args);

        /* get the list of spells */
        spells = scm_car(sc, args);
        args = scm_cdr(sc, args);

        /* get the sizes of the various lists */
        n_slots = scm_len(sc, slots);
        n_spells = scm_len(sc, spells);

        species = species_new(tag, name, damage_sound, walking_sound, str,
                              intl, dex, spd, vr, hpmod, hpmult,
                              mpmod, mpmult, visible,
                              n_slots, n_spells);
        species->weapon = weapon;
        species->sleep_sprite = sleep_sprite;
        species->mmode = mmode;

        /* Check if an on-death procedure was specified. */
        if (on_death != sc->NIL) {
                species->on_death = closure_new(sc, on_death);
                closure_ref(species->on_death);
        }

        /* Load the list of slots. */
        i = 0;
        while (scm_is_pair(sc, slots)) {
                if (unpack(sc, &slots, "d", &species->slots[i++]))
                        goto abort;
        }

        /* Load the list of spells */
        i = 0;
        while (scm_is_pair(sc, spells)) {
                char *code;
                if (unpack(sc, &spells, "s", &code))
                        goto abort;
                species->spells[i] = strdup(code);
                assert(species->spells[i]);
                i++;
        }

        session_add(Session, species, species_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, species);
        scm_define(sc, tag, ret);
        return ret;

 abort:
        species_del(species);
        return sc->NIL;
}

static pointer kern_mk_arms_type(scheme *sc, pointer args)
{
        class ArmsType *arms;
        char *tag = TAG_UNK, *name;
        sound_t *fire_sound;
        int slots, hands, range, weight;
        char *hit, *defend, *damage, *armor;
        int rap, thrown, ubiq, argno = 1;
        struct sprite *sprite;
        class ArmsType *missile;
        pointer gifc;
        pointer ret;
        int gifc_cap;

        if (unpack(sc, &args, "yspssssddddpbbdpdo", &tag, &name, &sprite, 
                   &hit, &damage, &defend, &armor, &slots, &hands, 
                   &range, &rap, &missile, &thrown, &ubiq, &weight, 
                   &fire_sound, &gifc_cap, &gifc)) {
                load_err("kern-mk-arms-type %s: bad args", tag);
                return sc->NIL;
        }

        if (! dice_valid(hit)) {
                load_err("kern-mk-arms-type %s: bad dice format '%s'",
                         tag, hit);
                return sc->NIL;
        }

        if (! dice_valid(defend)) {
                load_err("kern-mk-arms-type %s: bad dice format '%s'", 
                         tag, defend);
                return sc->NIL;
        }

        if (! dice_valid(damage)) {
                load_err("kern-mk-arms-type %s: bad dice format '%s'", 
                         tag, damage);
                return sc->NIL;
        }

        if (! dice_valid(armor)) {
                load_err("kern-mk-arms-type %s: bad dice format '%s'", 
                         tag, armor);
                return sc->NIL;
        }

        arms = new ArmsType(tag, name, sprite, slots, hit, defend, hands, 
                            range,
                            weight, damage, armor, rap, thrown, ubiq,
                            fire_sound, missile);

        if (gifc != sc->NIL) {
                /* arms->get_handler = closure_new(sc, get_handler); */
                arms->setGifc(closure_new(sc, gifc), gifc_cap);
        }

        session_add(Session, arms, arms_type_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, arms);
        scm_define(sc, tag, ret);
        return ret;
}

static pointer kern_mk_field_type(scheme *sc, pointer args)
{
        class FieldType *field;
        char *tag = TAG_UNK, *name;
        struct sprite *sprite;
        int light, duration, pclass;
        closure_t *clx = NULL;
        pointer func = sc->NIL;
        pointer ret;        

        if (unpack(sc, &args, "yspdddc", &tag, &name, &sprite, &light, 
                   &duration, &pclass, &func)) {
                load_err("kern-mk-field-type %s: bad args", tag);
                return sc->NIL;
        }
        
        if (func != sc->NIL) {
                clx = closure_new(sc, func);
        }

        field = new FieldType(tag, name, sprite, light, duration, pclass, clx);
        session_add(Session, field, field_type_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, field);
        scm_define(sc, tag, ret);
        return ret;
}

static void mmode_dtor(void *ptr)
{
        mmode_del((struct mmode*)ptr);
}

KERN_API_CALL(kern_mk_mmode)
{
        char *tag, *name;
        int index;
        struct mmode *mmode;
        pointer ret;

        if (unpack(sc, &args, "ysd", &tag, &name, &index)) {
                load_err("kern-mk-mmode: bad args");
                return sc->NIL;
        }

        mmode = mmode_new(tag, name, index);
        session_add(Session, mmode, mmode_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, mmode);
        scm_define(sc, tag, ret);
        return ret;
}

static pointer kern_mk_obj_type(scheme *sc, pointer args)
{
        class ObjectType *type;
        char *tag = TAG_UNK, *name;
        enum layer layer;
        struct sprite *sprite;
        pointer ret;
        pointer gifc;
        int gifc_cap;

        if (unpack(sc, &args, "yspddo", &tag, &name, &sprite, &layer, 
                   &gifc_cap, &gifc)) {
                load_err("kern-mk-obj-type %s: bad args", tag);
                return sc->NIL;
        }

        type = new ObjectType(tag, name, sprite, layer);
        assert(type);

        if (gifc != sc->NIL) {
                type->setGifc(closure_new(sc, gifc), gifc_cap);
        }

        session_add(Session, type, obj_type_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, type);
        scm_define(sc, tag, ret);
        return ret;
}

static pointer kern_mk_occ(scheme *sc, pointer args)
{
        struct occ *occ;
        int hpmod, hpmult, argno = 1;
        int mpmod, mpmult, hit, def, dam, arm, i;
        char *tag = TAG_UNK, *name;
        float magic;
        class ObjectType *container;
        pointer traps;
        pointer arms;
        pointer items;
        pointer ret;

        /* Basic args */
        if (unpack(sc, &args, "ysrddddddddp",
                   &tag, &name, &magic, &hpmod, &hpmult, &mpmod, &mpmult, &hit,
                   &def, &dam, &arm, &container)) {
                load_err("kern-mk-occ %s: bad args", tag);
                return sc->NIL;
        }

        if (scm_len(sc, args) < 3) {
                load_err("kern-mk-occ %s: arg list too short", tag,
                         argno++);
                return sc->NIL;
        }

        traps = scm_car(sc, args);
        arms = scm_car(sc, scm_cdr(sc, args));
        items =  scm_car(sc, scm_cdr(sc, scm_cdr(sc, args)));

        occ = occ_new(tag, name, magic, hpmod, hpmult, mpmod, mpmult, hit, def,
                      dam, arm, scm_len(sc, arms), scm_len(sc, items), 
                      scm_len(sc, traps));
        occ->container = container;

        /* Traps */
        i = 0;
        while (scm_is_pair(sc, traps)) {
                pointer func;
                if (unpack(sc, &traps, "c", &func)) {
                        load_err("kern-mk-occ %s: error in trap list", tag);
                        goto abort;
                }
                closure_init(&occ->traps[i++], sc, func);
        }

        /* Arms */

        i = 0;
        while (scm_is_pair(sc, arms)) {
                class ArmsType *dummy;
                if (unpack(sc, &arms, "p", &dummy /*&occ->arms[i++]*/)) {
                        load_err("kern-mk-occ %s: error in arms list", tag);
                        goto abort;
                }
        }

        /* Items */
        i = 0;
        while (scm_is_pair(sc, items)) {
                pointer item = scm_car(sc, items);
                if (unpack(sc, &item, "pdd",  
                           &occ->items[i].type,
                           &occ->items[i].prob, 
                           &occ->items[i].n_max)) {
                        load_err("kern-mk-occ %s: error in items list", tag);
                        goto abort;
                }
                items = scm_cdr(sc, items);
                i++;
        }

        session_add(Session, occ, occ_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, occ);
        scm_define(sc, tag, ret);
        return ret;

 abort:
        occ_del(occ);
        return sc->NIL;
}

static int kern_load_hstack(scheme *sc, pointer args, 
                            void (*restore)(int, int, void *),
                            char *name, void *context)
{
        int val, handle;
        pointer pair;
                
        while (scm_is_pair(sc, args)) {
                
                pair = scm_car(sc, args);
                args = scm_cdr(sc, args);

                if (unpack(sc, &pair, "dd", &handle, &val)) {
                        load_err("%s: bad stack node", name);
                        return -1;
                }
                
                restore(handle, val, context);
        }        

        return 0;
}

static pointer kern_mk_char(scheme *sc, pointer args)
{
        class Character *character;
        int str, intl, dex, hpmod, hpmult;
        int mpmod, mpmult, hp, xp, mp, lvl;
        char *tag = TAG_UNK, *name;
        struct species *species;
        struct occ *occ;
        struct sprite *sprite;
        pointer conv;
        pointer readied;
        pointer hook_tbl;
        pointer ret;
        pointer ai;
        int base_faction;
        struct sched *sched;
        pointer factions;
        class Container *inventory;

        if (unpack(sc, &args, "yspppddddddddddddcpcp",
                   &tag, &name, &species, &occ, 
                   &sprite, &base_faction, &str,
                   &intl, &dex, &hpmod, &hpmult, &mpmod, &mpmult, 
                   &hp, &xp, &mp, &lvl, 
                   &conv, &sched, &ai, &inventory)) {
                load_err("kern-mk-char %s: bad args", tag);
                return sc->NIL;
        }

        if (! scm_is_pair(sc, args)) {
                load_err("kern-mk-char %s: no readied arms list", tag);
                return sc->NIL;
        }

        readied = scm_car(sc, args);
        args = scm_cdr(sc, args);

        character = new class Character(tag, name, sprite,species, occ,
                                        str, intl, dex, hpmod, hpmult, mpmod, 
                                        mpmult,
                                        hp, xp, mp, lvl);
        assert(character);
        character->setBaseFaction(base_faction);
        character->setSchedule(sched);
        character->setInventoryContainer(inventory);

        if (conv != sc->NIL)
                character->setConversation(closure_new(sc, conv));

        if (ai != sc->NIL) {
                character->ai = closure_new(sc, ai);
                closure_ref(character->ai);
        }

        /* Load the list of arms. */
        while (scm_is_pair(sc, readied)) {
                class ArmsType *arms;
                if (unpack(sc, &readied, "p", &arms)) {
                        load_err("kern-mk-char %s: error in arms list", tag);
                        goto abort;
                }
                character->ready(arms);
        }

        /* Load the hooks. */
        hook_tbl = scm_car(sc, args);
        args = scm_cdr(sc, args);
        while (scm_is_pair(sc, hook_tbl)) {

                struct effect *effect;
                int flags;
                pointer gobcell;
                pointer hook_entry;
                struct gob *gob = NULL;
                clock_alarm_t clk;

                hook_entry = scm_car(sc, hook_tbl);
                hook_tbl = scm_cdr(sc, hook_tbl);

                if (unpack(sc, &hook_entry, "pldd", &effect, &gobcell, &flags, 
                           &clk)) {
                        load_err("kern-mk-char %s: bad hook entry", tag);
                        goto abort;
                }

                /* Note: even if gobcell is sc->NIL we want to wrap it. I once
                 * tried to use a NULL gob instead but if we pass that back
                 * into scheme as an arg and the gc tries to mark it we'll
                 * crash. */
                gob = gob_new(sc, gobcell);
                gob->flags |= GOB_SAVECAR;
                
                character->restoreEffect(effect, gob, flags, clk);
        }

        ret = scm_mk_ptr(sc, character);
        
        /* If the character is tagged then it's not "anonymous", and we'll
         * assign it to a scheme variable named after the tag. */
        if (tag) {
                scm_define(sc, tag, ret);
        }

        return ret;

 abort:
        delete character;
        return sc->NIL;
}

static void obj_dtor(void *val)
{
        delete (class Object*)val;
}

static pointer kern_mk_obj(scheme *sc, pointer args)
{
        class Object *obj;
        class ObjectType *type = 0;
        int count;

        if (unpack(sc, &args, "pd", &type, &count)) {
                load_err("kern-mk-obj: bad args");
                return sc->NIL;
        }

        if (!type) {
                load_err("kern-mk-obj: null type");
                return sc->NIL;
        }

        // Fixme: we need a MAGIC number field, but it won't work because
        // 'type' is a c++ object. If the script hands us something that is not
        // really an ObjectType we'll crash sooner or later.

        obj = type->createInstance();        
        assert(obj);
        obj->setCount(count);

        // Objects aren't added to the session the way types are. Every object
        // will end up in some type of container; objects should always be
        // loaded as part of a container's contents; and the container should
        // always save the objects it contains.
        //
        // There's nothing to prevent a script from creating a "dangling"
        // object which is never put anywhere. Although weird (and probably a
        // bug in the script), it should be benign. Memory leaks are prevented
        // by deinitializing the interpreter every time we load a new session.
        // Furthermore, such orphan objects will never be saved, so they won't
        // propogate.

        return scm_mk_ptr(sc, obj);
}

static pointer kern_mk_field(scheme *sc, pointer args)
{
        class Field *obj;
        class FieldType *type = 0;
        int duration;

        if (unpack(sc, &args, "pd", &type, &duration)) {
                load_err("kern-mk-obj: bad args");
                return sc->NIL;
        }

        if (!type) {
                load_err("kern-mk-obj: null type");
                return sc->NIL;
        }

        obj = new Field(type, duration);
        assert(obj);

        // Objects aren't added to the session the way types are. Every object
        // will end up in some type of container; objects should always be
        // loaded as part of a container's contents; and the container should
        // always save the objects it contains.

        return scm_mk_ptr(sc, obj);
}

static pointer kern_mk_party(scheme *sc, pointer args)
{
        class Party *obj;
        class PartyType *type = 0;
        int faction;
        class Vehicle *vehicle;

        if (unpack(sc, &args, "pdp", &type, &faction, &vehicle)) {
                load_err("kern-mk-party: bad args");
                return sc->NIL;
        }

        if (!type) {
                load_err("kern-mk-party: null type");
                return sc->NIL;
        }

        obj = new Party(type, faction, vehicle);
        assert(obj);

        return scm_mk_ptr(sc, obj);
}

static pointer kern_obj_put_at(scheme *sc, pointer args)
{
        class Object *obj;
        struct place *place;
        pointer loc;
        int x, y;

        if (unpack(sc, &args, "p", &obj)) {
                rt_err("kern-obj-put-at: bad args");
                return sc->NIL;
        }

        if (!obj) {
                rt_err("kern-obj-put-at: null obj");
                return sc->NIL;
        }

        loc = scm_car(sc, args);
        if (! scm_is_pair(sc, loc)) {
                rt_err("kern-obj-put-at: invalid location");
                return sc->NIL;
        }

        if (unpack(sc, &loc, "pdd", &place, &x, &y)) {
                rt_err("kern-obj-put-at: bad location args");
                return sc->NIL;
        }

        if (!place) {
                rt_err("kern-obj-put-at: null place");
                return sc->NIL;
        }

        obj->relocate(place, x, y, true);
        return sc->NIL;
}

static pointer kern_obj_relocate(scheme *sc, pointer args)
{
        class Object *obj;
        struct place *place;
        pointer loc;
        pointer cutscene;
        int x, y;
        struct closure *clx = NULL;

        obj = unpack_obj(sc, &args, "kern-obj-relocate");
        if (!obj)
                return sc->NIL;

        if (unpack_loc(sc, &args, &place, &x, &y, "kern-obj-relocate"))
                return sc->NIL;

        if (unpack(sc, &args, "o", &cutscene)) {
                rt_err("kern-obj-relocate: bad args");
                return sc->NIL;
        }

        if (cutscene != sc->NIL) {
                clx = closure_new(sc, cutscene);
        }

        obj->relocate(place, x, y, true, clx);

        if (clx)
                closure_del(clx);

        return sc->NIL;
}

static pointer kern_obj_get_location(scheme *sc, pointer args)
{
        class Object *obj;
        struct place *place;
        int x, y;

        if (!(obj = unpack_obj(sc, &args, "kern-obj-get-location"))) {
                assert(false);
                return sc->NIL;
        }

        place = obj->getPlace();
        x = obj->getX();
        y = obj->getY();

        return pack(sc, "pdd", place, x, y);
}

static int kern_append_effect(struct hook_entry *entry, void *data)
{
        pointer cell;
        struct kern_append_info *info;

        info = (struct kern_append_info *)data;

        cell = scm_mk_ptr(info->sc, entry->effect);
        cell = _cons(info->sc, cell, info->sc->NIL, 0);

        if (info->head == info->sc->NIL) {
                info->head = cell;
                info->tail = cell;
        } else {
                info->tail->_object._cons._cdr = cell;
                info->tail = cell;
        }

        return 0;
}

KERN_API_CALL(kern_obj_get_effects)
{
        class Object *obj;
        struct place *place;
        int i;
        struct kern_append_info info;

        if (!(obj = unpack_obj(sc, &args, "kern-obj-get-effects"))) {
                return sc->NIL;
        }

        /* initialize the context used by the callback to append objects */
        info.sc = sc;
        info.head = sc->NIL;
        info.tail = sc->NIL;
        info.filter = NULL;

        /* for each effect hook on the object */
        for (i = 0; i < OBJ_NUM_HOOKS; i++) {

                /* build a scheme list of the attached effects */
                obj->hookForEach(i, kern_append_effect, &info);
        }

        return info.head;
}

static pointer kern_obj_get_vision_radius(scheme *sc, pointer args)
{
        class Object *obj;

        if (!(obj = unpack_obj(sc, &args, "kern-obj-get-location"))) {
                return sc->NIL;
        }

        return scm_mk_integer(sc, obj->getVisionRadius());
}

static pointer kern_obj_put_into(scheme *sc, pointer args)
{
        class Object *obj;
        class Object *container;

        if (unpack(sc, &args, "pp", &obj,  &container)) {
                load_err("kern-obj-put: bad args");
                return sc->NIL;
        }

        if (!obj) {
                rt_err("kern-obj-put: null obj");
                return sc->NIL;
        }

        if (!container) {
                rt_err("kern-obj-put: null container");
                return sc->NIL;
        }

        if (container->add(obj->getObjectType(), obj->getCount())) {
                delete obj;
        } else {
                rt_err("kern-obj-put-into: '%s' not a container type", 
                       container->getName());
        }

        return sc->NIL;
}

static pointer kern_obj_remove(scheme *sc, pointer args)
{
        class Object *obj;

        if (!(obj=unpack_obj(sc, &args, "kern-obj-remove"))) {
                return sc->NIL;
        }

        place_remove_object(obj->getPlace(), obj);

        return sc->NIL;
}

static pointer kern_obj_destroy(scheme *sc, pointer args)
{
        class Object *obj;

        if (!(obj=unpack_obj(sc, &args, "kern-obj-destroy"))) {
                return sc->NIL;
        }

        delete obj;

        return sc->NIL;
}

static pointer kern_obj_add_food(scheme *sc, pointer args)
{
        class Object *obj;
        int quantity;

        if (unpack(sc, &args, "pd", &obj, &quantity)) {
                load_err("kern-obj-add-food: bad args");
                return sc->NIL;
        }

        if (!obj) {
                rt_err("kern-obj-add-food: null obj");
                return sc->NIL;
        }

        if (! obj->addFood(quantity)) {
                rt_err("kern-obj-add-food: '%s' does not use food",
                       obj->getName());
        }

        return sc->NIL;
}

static pointer kern_mk_container(scheme *sc, pointer args)
{
        class Container *container;
        class ObjectType *type;
        pointer trap;
        pointer contents;

        if (unpack(sc, &args, "pc", &type, &trap)) {
                load_err("kern-mk-container: bad args");
                return sc->NIL;
        }

        // The container used as the player party's inventory does not have a
        // type.
        if (type)
                container = new Container(type);
        else
                container = new Container();

        if (trap != sc->NIL) {
                container->setTrap(closure_new(sc, trap));
        }

        contents = scm_car(sc, args);

        while (contents != sc->NIL) {

                int num;
                class ObjectType *type;
                pointer entry;

                entry = scm_car(sc, contents);
                contents = scm_cdr(sc, contents);

                if (! scm_is_pair(sc, entry)) {
                        load_err("kern-mk-container: error in inv list "\
                                 "(not a pair)");
                        goto abort;
                }

                if (unpack(sc, &entry, "dp", &num, &type)) {
                        load_err("kern-mk-container: error in inv list");
                        goto abort;
                }

                container->add(type, num);
        }

        return scm_mk_ptr(sc, container);
        
 abort:
        delete container;
        return sc->NIL;
}

KERN_API_CALL(kern_mk_player)
{
        int food, gold, ttnrc, ttnm;
        char *mv_desc, *tag;
        sound_t *mv_sound;
        struct sprite *sprite;
        struct terrain_map *campsite;
        struct formation *form, *camp_form;
        class Container *inventory;
        Vehicle *vehicle;
        pointer members;
        pointer ret;

        // --------------------------------------------------------------------
        // FIXME: the global player_party should be per-session. That way we
        // can postpone destroying the current player party until we have
        // successfully loaded the new session.
        // --------------------------------------------------------------------
        
        /* The player party is a special global object that is created on
         * startup. This is legacy, and needs to be addressed eventually. For
         * now reset the party by destroying it and recreating it. Do NOT call
         * player_init again because it sets up work queue jobs (which right
         * now I have no way of canceling). */

        if (player_party) {
                if (player_party->isOnMap()) // hack!
                        player_party->remove(); // hack!
                while (player_party->refcount) // hack!
                        obj_dec_ref(player_party); // hack!
                delete player_party;
        }

        if (unpack(sc, &args, "ypspddddppppp", 
                   &tag,
                   &sprite,
                   &mv_desc, &mv_sound,
                   &food, &gold, &ttnm, &ttnrc,
                   &form, &campsite, &camp_form, 
                   &vehicle, 
                   &inventory)) {
                load_err("kern-mk-player: bad args");
                return sc->NIL;
        }

        if (! inventory) {
                load_err("kern-mk-player: nil inventory container");
                return sc->NIL;
        }

        //members = scm_car(sc, scm_cdr(sc, args));
        members = scm_car(sc, args);

        player_party = new class player_party(tag, sprite, 
                                              mv_desc, 
                                              mv_sound,
                                              food, gold, form, 
                                              campsite, 
                                              camp_form);
        player_party->inventory = inventory;
        player_party->setTurnsToNextMeal(ttnm);
        player_party->setTurnsToNextRestCredit(ttnrc);

        /* Load the members. */
        while (scm_is_pair(sc, members)) {

                class Character *ch;

                if (unpack(sc, &members, "p", &ch)) {
                        load_err("kern-mk-player: error in member list");
                        goto abort;
                }

                if (!ch) {
                        load_err("kern-mk-player: null member object");
                        goto abort;
                }

                /* fixme: looks like a hack below */
                ch->setRestCredits(MAX_USEFUL_REST_HOURS_PER_DAY);

                if (! player_party->addMember(ch)) {
                        load_err("kern-mk-player: failed to add %s to player "
                                 "party", ch->getName());
                        goto abort;
                }
        }

        /* Board the vehicle */

        if (vehicle) {
                player_party->vehicle = vehicle;
                vehicle->occupant = player_party;
        }

        session_add_obj(Session, player_party, player_dtor, player_save, NULL);
        ret = scm_mk_ptr(sc, player_party);
        scm_define(sc, tag, ret);
        return ret;

 abort:
        delete player_party;
        player_party = 0;
        return sc->NIL;
}

static void sched_dtor(void *data)
{
        sched_del((struct sched *)data);
}

static void effect_dtor(void *data)
{
        effect_del((struct effect *)data);
}

static pointer kern_mk_sched(scheme *sc, pointer args)
{
        struct sched *sched;
        char *tag;
        char *activity;
        int n_appts;
        int i;
        pointer ret;

        /* unpack the tag */
        if (unpack(sc, &args, "y", &tag)) {
                load_err("kern-mk-sched: bad args");
                return sc->NIL;
        }

        /* count the number of appointments */
        n_appts = scm_len(sc, args);

        /* alloc the schedule */
        sched = sched_new(tag, n_appts);
        
        /* loop, adding the appointments to the schedule */
        for (i = 0; i < n_appts; i++) {
                struct appt *appt = &sched->appts[i];
                pointer p = scm_car(sc, args);
                pointer rect;
                args = scm_cdr(sc, args);

                if (unpack(sc, &p, "dd", &appt->hr, &appt->min, &activity)) {
                        load_err("kern-mk-sched %s: bad args in appt %d time",
                                 tag, i);
                        goto abort;
                }

                rect = scm_car(sc, p);
                p = scm_cdr(sc, p);

                if (unpack(sc, &rect, "dddd", &appt->x, &appt->y, &appt->w, 
                           &appt->h)) {
                        load_err("kern-mk-sched %s: bad args in appt %d rect",
                                 tag, i);
                        goto abort;
                }

                if (unpack(sc, &p, "s", &activity)) {
                        load_err("kern-mk-sched %s: bad args in appt %d activity",
                                 tag, i);
                        goto abort;
                }

                appt->act = sched_name_to_activity(activity);
                if (appt->act < 0) {
                        load_err("kern-mk-sched %d: unknown activity name %s",
                                 tag, activity);
                        goto abort;
                }
        }

        session_add(Session, sched, sched_dtor, 0, NULL);
        ret = scm_mk_ptr(sc, sched);
        scm_define(sc, tag, ret);
        return ret;

 abort:
        sched_del(sched);
        return sc->NIL;
}

static pointer kern_interp_error(scheme *sc, pointer args)
{
        load_err("interpreter error");
        return sc->NIL;
}

static pointer kern_include(scheme *sc, pointer args)
{
        char *fname;

        if (unpack(sc, &args, "s", &fname)) {
                load_err("kern-include: bad args");
                return sc->NIL;
        }

        session_add(Session, strdup(fname), incfile_dtor, incfile_save, NULL);
        return sc->NIL;
}


static pointer kern_set_crosshair(scheme *sc, pointer args)
{
        if (unpack(sc, &args, "p", &Session->crosshair_type)) {
                load_err("kern-set-crosshair: bad args");
        }
        return sc->NIL;
}


static pointer kern_set_cursor(scheme *sc, pointer args)
{
        if (unpack(sc, &args, "p", &Session->cursor_sprite)) {
                load_err("kern-set-cursor: bad args");
        }
        return sc->NIL;
}


static pointer kern_set_frame(scheme *sc, pointer args)
{
        if (unpack(sc, &args, "ppppppppppppp", 
                   &Session->frame.ulc,
                   &Session->frame.urc,
                   &Session->frame.llc,
                   &Session->frame.lrc,
                   &Session->frame.td,
                   &Session->frame.tu,
                   &Session->frame.tl,
                   &Session->frame.tr,
                   &Session->frame.tx,
                   &Session->frame.horz,
                   &Session->frame.vert,
                   &Session->frame.endl,
                   &Session->frame.endr)) {
                load_err("kern-set-frame: bad args");
        }
        return sc->NIL;
}


static pointer kern_set_ascii(scheme *sc, pointer args)
{
        if (unpack(sc, &args, "pd", 
                   &Session->ascii.images,
                   &Session->ascii.offset)) {
                load_err("kern-set-ascii: bad args");
                return sc->NIL;
        }

	if ((Session->ascii.images->w != ASCII_W) ||
            (Session->ascii.images->h != ASCII_H) ||
            (Session->ascii.offset >
             ((Session->ascii.images->cols * Session->ascii.images->rows) -
              (128 - ' ')))) {
                load_err("kern-set-ascii: ascii images are the wrong size "\
                         "or number");
        }

        return sc->NIL;
}


static pointer kern_set_clock(scheme *sc, pointer args)
{
        if (unpack(sc, &args, "dddddd",
                   &Session->clock.year,
                   &Session->clock.month,
                   &Session->clock.week,
                   &Session->clock.day,
                   &Session->clock.hour,
                   &Session->clock.min)) {
                load_err("kern-set-clock: bad args");
                return sc->NIL;
        }

        if (Session->clock.month  >= MONTHS_PER_YEAR ||
            Session->clock.week   >= WEEKS_PER_MONTH ||
            Session->clock.day    >= DAYS_PER_WEEK   ||
            Session->clock.hour   >= HOURS_PER_DAY   ||
            Session->clock.min    >= MINUTES_PER_HOUR) {
                load_err("kern-set-clock: invalid time");
                return sc->NIL;
        }

        Session->clock.total_minutes = 
                Session->clock.min +
                Session->clock.hour * MINUTES_PER_HOUR +
                Session->clock.day * MINUTES_PER_DAY +
                Session->clock.week * MINUTES_PER_WEEK +
                Session->clock.month * MINUTES_PER_MONTH;
        Session->clock.tick_to_change_time = CLOCK_TICKS_PER_MINUTE;
        Session->clock.set = 1;

        return sc->NIL;
}

static pointer kern_obj_apply_damage(scheme *sc, pointer args)
{
        class Object *obj;
        char *desc;
        int amount;

        if (unpack(sc, &args, "psd", &obj, &desc, &amount)) {
                rt_err("kern-obj-apply-damage: bad args");
                return sc->NIL;
        }

        if (!obj) {
                rt_err("kern-obj-apply-damage: null object");
                return sc->NIL;
        }

        obj->damage(amount);

        return sc->NIL;
}

static pointer kern_obj_add_effect(scheme *sc, pointer args)
{
        char *hook_name;
        class Object *obj;
        struct effect *effect = NULL;
        pointer result = sc->F;
        pointer gobcell;
        struct gob *gob = NULL;

        if (unpack(sc, &args, "ppl", &obj, &effect, &gobcell)) {
                rt_err("kern-obj-add-effect: bad args");
                return result;
        }

        if (!obj) {
                rt_err("kern-obj-add-effect: null object");
                return sc->NIL;
        }

        if (! is_effect(effect)) {
                rt_err("kern-obj-remove-effect: wrong type for effect!");
                return sc->NIL;
        }

        /* Note: even if gobcell is sc->NIL we want to wrap it. I once tried to
         * use a NULL gob instead but if we pass that back into scheme as an
         * arg and the gc tries to mark it we'll crash. */
        gob = gob_new(sc, gobcell);
        gob->flags |= GOB_SAVECAR;

        result = obj->addEffect(effect, gob) ? sc->T : sc->F;

        return sc->NIL;
}

static pointer kern_obj_remove_effect(scheme *sc, pointer args)
{
        class Object *obj;
        struct list *hook_list;
        struct effect *effect;

        if (unpack(sc, &args, "pp", &obj, &effect)) {
                load_err("kern-obj-remove-effect: bad args");
                return sc->NIL;
        }

        if (!obj) {
                rt_err("kern-obj-remove-effect: null object");
                return sc->NIL;
        }

        if (! is_effect(effect)) {
                rt_err("kern-obj-remove-effect: wrong type for effect!");
                return sc->NIL;
        }

        /* Just remove one per call */
        obj->removeEffect(effect);

        return sc->NIL;
}

static pointer kern_print(scheme *sc,  pointer args)
{
        while (scm_is_pair(sc, args)) {

                pointer val = scm_car(sc, args);
                args = scm_cdr(sc, args);

                if (scm_is_str(sc, val)) {
                        consolePrint(scm_str_val(sc, val));
                } else if (scm_is_int(sc, val)) {
                        consolePrint("%d", scm_int_val(sc, val));
                } else if (scm_is_real(sc, val)) {
                        consolePrint("%f", scm_real_val(sc, val));
                } else {
                        rt_err("kern-print: bad args");
                }
        }

        return sc->NIL;
}

static pointer kern_log_msg(scheme *sc,  pointer args)
{
        log_begin(NULL);

        while (scm_is_pair(sc, args)) {

                pointer val = scm_car(sc, args);
                args = scm_cdr(sc, args);

                if (scm_is_str(sc, val)) {
                        log_continue(scm_str_val(sc, val));
                } else if (scm_is_int(sc, val)) {
                        log_continue("%d", scm_int_val(sc, val));
                } else if (scm_is_real(sc, val)) {
                        log_continue("%f", scm_real_val(sc, val));
                } else {
                        rt_err("kern-print: bad args");
                }
        }

        log_end(NULL);

        return sc->NIL;
}

KERN_API_CALL(kern_log_enable)
{
        int val;

        if (unpack(sc, &args, "b", &val)) {
                rt_err("kern_log_enable: bad args");
                return sc->F;
        }

        if (val)
                log_enable();
        else
                log_disable();

        return sc->T;
}

static pointer kern_conv_say(scheme *sc,  pointer args)
{
        char *msg;
        Object *speaker;
        pointer msgs;
        int first = 1;

        if (unpack(sc, &args, "p", &speaker)) {
                rt_err("kern-print: bad args");
                return sc->NIL;
        }

        if (speaker == NULL) {
                rt_err("kern-conv-say: null speaker");
                return sc->NIL;
        }

        msgs = scm_car(sc, args);
        args = scm_cdr(sc, args);

        while (scm_is_pair(sc, msgs)) {
                
                if (unpack(sc, &msgs, "s", &msg)) {
                        rt_err("kern-conv-say: bad args");
                        return sc->NIL;
                }

                if (first) {
                        log_begin("%s: %s", speaker->getName(), msg);
                        first = 0;
                } else {
                        log_continue("%s", msg);
                }

        }

        if (! first)
                log_end(NULL);

        return sc->NIL;
}

static pointer kern_conv_get_yes_no(scheme *sc,  pointer args)
{
        Object *pc = unpack_obj(sc, &args, "kern-conv-get-yes-no?");
        if (NULL == pc)
                return sc->F;
        return ui_get_yes_no(pc->getName()) ? sc->T : sc->F;
}

static pointer kern_conv_get_reply(scheme *sc,  pointer args)
{
        char buf[32];

        Object *pc = unpack_obj(sc, &args, "kern-conv-get-line");
        if (NULL == pc)
                return sc->F;

        ui_getline(buf, sizeof(buf));
        log_msg("%s: %s", pc->getName(), buf);

        /* Return only the first four characters, to be consistent with the
         * usual keyword/reply scheme. */
        buf[4] = 0;

        return scm_mk_symbol(sc, buf);
}

static pointer kern_conv_trade(scheme *sc, pointer args)
{
        Object *npc;
        Object *pc;
        struct merchant merch;
        int i;

        if (unpack(sc, &args, "pp", &npc, &pc)) {
                rt_err("kern-conv-trade: bad args");
                return sc->NIL;
        }
        if (! npc || ! pc) {
                rt_err("kern-conv-trade: null kernel object(s)");
                return sc->NIL;
        }
        
        /* setup the merchant struct */
        merch.name = npc->getName();
        merch.n_trades = scm_len(sc, args);
        if (! merch.n_trades) {
                rt_err("kern-conv-trade: nothing in trade list");
                return sc->NIL;
        }
        merch.trades = (struct trade_info*)calloc(merch.n_trades, 
                                                 sizeof(struct trade_info));
        assert(merch.trades);

        /* fill out the merchant's item list */
        for (i = 0; i < merch.n_trades; i++) {

                ObjectType *type;
                pointer p = scm_car(sc, args);
                struct trade_info *trade = &merch.trades[i];
                args = scm_cdr(sc, args);

                if (unpack(sc, &p, "pd", &type, &trade->cost)) {
                        rt_err("kern-conv-trade: bad args in trade list %d", i);
                        goto abort;
                }

                if (! type) {
                        rt_err("kern-conv-trade: null object type in trade list %d", i);
                        goto abort;
                }

                trade->sprite = type->getSprite();
                trade->name = type->getName();
                trade->data = type;
                trade->show_sprite = 1;
        }

        ui_trade(&merch);

 abort:
        free(merch.trades);
        return sc->NIL;
}

static pointer kern_obj_get_activity(scheme *sc, pointer args)
{
        class Object *obj = unpack_obj(sc, &args, "kern-obj-get-activity");
        if (obj == NULL)
                return sc->NIL;

        return scm_mk_string(sc, 
                             sched_activity_to_name(obj->getActivity()));
}

static pointer kern_obj_set_sprite(scheme *sc, pointer args)
{
        class Object *obj;
        struct sprite *sprite;

        if (unpack(sc, &args, "pp", &obj, &sprite)) {
                rt_err("kern-obj-set-sprite: bad args");
                return sc->NIL;
        }

        if (!obj) {
                rt_err("kern-obj-set-sprite: null object");
                return sc->NIL;
        }

        obj->setSprite(sprite);

        return sc->NIL;
}

static pointer kern_obj_set_opacity(scheme *sc, pointer args)
{
        class Object *obj;
        int opacity;

        if (unpack(sc, &args, "pb", &obj, &opacity)) {
                rt_err("kern-obj-set-opacity: bad args");
                return sc->NIL;
        }

        if (!obj) {
                rt_err("kern-obj-set-opacity: null object");
                return sc->NIL;
        }

        obj->setOpacity(opacity != 0);

        return sc->NIL;
}

static pointer kern_obj_set_ap(scheme *sc, pointer args)
{
        class Object *obj;
        int ap;

        if (unpack(sc, &args, "pd", &obj, &ap)) {
                rt_err("kern-obj-set-ap: bad args");
                return sc->NIL;
        }

        if (!obj) {
                rt_err("kern-obj-set-ap: null object");
                return sc->NIL;
        }

        obj->setActionPoints(0);

        return sc->NIL;
}

static pointer kern_obj_set_visible(scheme *sc, pointer args)
{
        class Object *obj;
        int val;

        if (unpack(sc, &args, "pb", &obj, &val)) {
                rt_err("kern-obj-set-visible: bad args");
                return sc->NIL;
        }

        if (!obj) {
                rt_err("kern-obj-set-visible: null object");
                return sc->NIL;
        }

        obj->setVisible(val);

        return sc->NIL;
}

static pointer kern_obj_set_pclass(scheme *sc, pointer args)
{
        class Object *obj;
        int val;

        if (unpack(sc, &args, "pd", &obj, &val)) {
                rt_err("kern-obj-set-pclass: bad args");
                return sc->NIL;
        }

        if (!obj) {
                rt_err("kern-obj-set-pclass: null object");
                return sc->NIL;
        }

        obj->setPclass(val);

        return sc->NIL;
}

static pointer kern_obj_get_type(scheme *sc, pointer  args)
{
        Object *obj;

        if (!(obj = unpack_obj(sc, &args, "kern-obj-get-type")))
                return sc->NIL;

        return scm_mk_ptr(sc, obj->getObjectType());
}

static pointer kern_place_map(scheme *sc, pointer args)
{
        struct place *place;

        if (unpack(sc, &args, "p", &place)) {
                rt_err("kern-place-map: bad args");
                return sc->NIL;
        }

        if (! place || ! place->terrain_map) {
                rt_err("kern-place-map: null place or map");
                return sc->NIL;
        }

        return scm_mk_ptr(sc, place->terrain_map);
}

KERN_API_CALL(kern_place_synch)
{
        struct place *place;

        if (unpack(sc, &args, "p", &place)) {
                rt_err("kern-place-synch: bad args");
                return sc->NIL;
        }

        if (! place) {
                rt_err("kern-place-synch: null place");
                return sc->NIL;
        }
        place_synchronize(place);
        return sc->NIL;
}

static pointer kern_blit_map(scheme *sc, pointer args)
{
        struct terrain_map *src;
        struct terrain_map *dst;
        int dst_x;
        int dst_y;
        int src_x;
        int src_y;
        int w;
        int h;

        if (unpack(sc, &args, "pddpdddd", &dst, &dst_x, &dst_y, 
                   &src, &src_x, &src_y, &w, &h)) {
                rt_err("kern-blit-map: bad args");
                return sc->NIL;
        }

        if (! dst || ! src) {
                rt_err("kern-blit-map: null src or dst map");
                return sc->NIL;
        }

        terrain_map_blit(dst, dst_x, dst_y, src, src_x, src_y, w, h);

        /* Return the modified destination map */
        return scm_mk_ptr(sc, dst);
}

static pointer kern_map_rotate(scheme *sc, pointer args)
{
        struct terrain_map *map;
        int degree;

        if (unpack(sc, &args, "pd", &map, &degree)) {
                rt_err("kern-map-rotate: bad args");
                return sc->NIL;
        }

        if (! map) {
                rt_err("kern-map-rotate: null map");
                return sc->NIL;
        }

        terrain_map_rotate(map, degree);

        /* Return the modified map */
        return scm_mk_ptr(sc, map);
}

static pointer kern_tag(scheme *sc, pointer  args)
{
        char *tag;
        Object *obj;
        pointer p;

        if (unpack(sc, &args, "y", &tag)) {
                rt_err("kern-tag: bad args");
                return sc->NIL;
        }

        if (! scm_is_pair(sc, args)) {
                rt_err("kern-tag %s: no second arg", tag);
                return sc->NIL;
        }

        p = scm_car(sc, args);
        scm_define(sc, tag, p);

        if (unpack(sc, &args, "p", &obj)) {
                rt_err("kern-tag: bad object");
                return sc->NIL;
        }

        obj->tag = strdup(tag);
        assert(obj->tag);

        return p;
}

static pointer kern_obj_get_gob(scheme *sc, pointer  args)
{
        Object *obj;

        if (unpack(sc, &args, "p", &obj)) {
                rt_err("kern-obj-get-gob: bad args");
                return sc->NIL;
        }

        if (obj->getGob() == NULL) {
                rt_err("kern-obj-get-gob: no gob for %s", obj->getName());
                return sc->NIL;
        }

        // It's already a scheme pointer so just return it directly
        return obj->getGob()->p;
}

static pointer kern_obj_set_gob(scheme *sc, pointer  args)
{
        Object *obj;
        pointer gob;

        if (! (obj = unpack_obj(sc, &args, "kern-obj-set-gob"))) {
                return sc->NIL;
        }

        if (! scm_is_pair(sc, args)) {
               rt_err("kern-obj-set-gob: no gob specified");
               return sc->NIL;
        }

       obj->setGob(gob_new(sc, scm_car(sc, args)));

       return sc->NIL;
}

static pointer kern_astral_body_get_gob(scheme *sc, pointer  args)
{
        struct astral_body *astral_body;

        if (unpack(sc, &args, "p", &astral_body)) {
                rt_err("kern-astral-body-get-gob: bad args");
                return sc->NIL;
        }

        if (! astral_body) {
                rt_err("kern-astral-body-get-gob: null object");
                return sc->NIL;
        }

        if (astral_body->gob == NULL) {
                rt_err("kern-astral-body-get-gob: no gob for %s", 
                       astral_body->name);
                return sc->NIL;
        }

        // It's already a scheme pointer so just return it directly
        return astral_body->gob->p;
}

static pointer kern_astral_body_get_phase(scheme *sc, pointer  args)
{
        struct astral_body *astral_body;

        if (unpack(sc, &args, "p", &astral_body)) {
                rt_err("kern-astral-body-get-phase: bad args");
                return sc->NIL;
        }

        if (! astral_body) {
                rt_err("kern-astral-body-get-phase: null object");
                return sc->NIL;
        }

        return scm_mk_integer(sc, astral_body->phase);
}

static pointer kern_astral_body_set_gob(scheme *sc, pointer  args)
{
        struct astral_body *astral_body;

        if (unpack(sc, &args, "p", &astral_body)) {
                rt_err("kern-astral-body-set-gob: bad args");
                return sc->NIL;
        }

        if (! astral_body) {
                rt_err("kern-astral-body-set-gob: null object");
                return sc->NIL;
        }

        if (! scm_is_pair(sc, args)) {
               rt_err("kern-astral-body-set-gob: no gob specified");
               return sc->NIL;
        }

       astral_body->gob = (gob_new(sc, scm_car(sc, args)));

       return sc->NIL;
}

static pointer kern_type_get_gifc(scheme *sc, pointer  args)
{
        ObjectType *cptr;

        if (unpack(sc, &args, "p", &cptr)) {
                rt_err("kern-type-get-gifc: bad args");
                return sc->NIL;
        }

        if (cptr == NULL) {
                /* This is not necessarily an error. Some objects (like
                 * characters) have no type, which can result in us getting
                 * here. */
                return sc->NIL;
        }

        // The closure code field is already a scheme pointer so just return it
        // directly
        return cptr->getGifc()->code;
}

static pointer kern_add_tick_job(scheme *sc, pointer args)
{
        int tick;
        pointer proc;
        void *data;
        struct kjob *kjob;

        if (unpack(sc, &args, "dop", &tick, &proc, &data)) {
                rt_err("kern-add-tick-job: bad args");
                return sc->NIL;
        }

        wqCreateJob(&TickWorkQueue, Tick + tick, 0, 
                    kjob_new(data, closure_new(sc, proc)), 
                    kern_run_wq_job);

        return sc->NIL;
}

static pointer kern_ui_select_party_member(scheme *sc, pointer args)
{
        class Character *member;

        member = select_party_member();
        if (! member)
                return sc->NIL;
        return scm_mk_ptr(sc, member);
}

static pointer kern_conv_end(scheme *sc, pointer args)
{
        conv_end();
        return sc->T;
}

static pointer kern_map_set_dirty(scheme *sc, pointer args)
{
        mapSetDirty();
        return sc->T;
}

static pointer kern_mk_astral_body(scheme *sc, pointer args)
{
        struct astral_body *body;
        char *tag;
        char *name;
        int minutes_per_phase;
        int minutes_per_degree;
        int initial_arc;
        int initial_phase;
        int distance;
        pointer proc;
        pointer phases;
        pointer ret;
        int i;

        if (unpack(sc, &args, "ysdddddc", 
                   &tag,
                   &name,
                   &distance,
                   &minutes_per_phase,
                   &minutes_per_degree,
                   &initial_arc,
                   &initial_phase,
                   &proc)) {
                load_err("kern-mk-astral-body: bad args");
                return sc->NIL;
        }

        if (! scm_is_pair(sc, args)) {
                load_err("kern-mk-astral-body: null phase list");
                return sc->NIL;
        }

        phases = scm_car(sc, args);
        args = scm_cdr(sc, args);

        body = astral_body_new(tag, name, scm_len(sc, phases));
        body->distance = distance;
        body->minutes_per_phase = minutes_per_phase;
        body->minutes_per_degree = minutes_per_degree;
        body->initial_arc = initial_arc;
        body->initial_phase = initial_phase;
        body->arc = initial_arc;
        body->phase = initial_phase;

        if (proc != sc->NIL)
                body->gifc = closure_new(sc, proc);

        i = 0;
        while (scm_is_pair(sc, phases)) {
                pointer phase = scm_car(sc, phases);
                phases = scm_cdr(sc, phases);
                char *phase_name = NULL;

                if (unpack(sc, &phase, "pds", 
                           &body->phases[i].sprite,
                           &body->phases[i].maxlight,
                           &phase_name)) {
                        load_err("kern-mk-astral-body: bad args in phase "\
                                 "list at entry %d", i);
                        goto abort;
                }
                if (! phase_name) {
                        load_err("kern-mk-astral-body %s: null phase name",
                                 body->tag);
                        goto abort;
                }
                body->phases[i].name = strdup(phase_name);
                assert(body->phases[i].name);
                i++;
        }

        /* Like types, I define astral bodies in the script so they can be
         * referred to by their tags as script variables. I do this because a)
         * kern-obj-tag won't work on them, so I need some other way to tag
         * them, and b) they are unique enough that it won't hurt to just
         * automatically make them variables. */
        sky_add_astral_body(&Session->sky, body);        
        ret = scm_mk_ptr(sc, body);
        scm_define(sc, tag, ret);

        return ret;

 abort:
        astral_body_del(body);
        return sc->NIL;
        
}

KERN_API_CALL(kern_mk_vehicle_type)
{
        VehicleType *type;
        char *tag = TAG_UNK;
        char *name;
        struct sprite *sprite;
        struct terrain_map *map;
        ArmsType *ordnance;
        int vulnerable;
        int killsOccupants;
        int mustTurn;
        char *mv_desc;
        sound_t *mv_sound;
        int tailwind_penalty;
        int headwind_penalty;
        int crosswind_penalty;
        int max_hp;
        int speed;
        pointer ret;
        struct mmode *mmode;

        if (unpack(sc, &args, "yspppbbbspdddddp",
                   &tag,
                   &name,
                   &sprite,
                   &map,
                   &ordnance,
                   &vulnerable,
                   &killsOccupants,
                   &mustTurn,
                   &mv_desc,
                   &mv_sound,
                   &tailwind_penalty,
                   &headwind_penalty,
                   &crosswind_penalty,
                   &max_hp,
                   &speed,
                   &mmode
                    )) {
                load_err("kern-mk-vehicle-type %s: bad args", tag);
                return sc->NIL;
        }

        type = new VehicleType(tag,
                               name,
                               sprite,
                               map,
                               ordnance,
                               vulnerable,
                               killsOccupants,
                               mustTurn,
                               mv_desc,
                               mv_sound,
                               tailwind_penalty,
                               headwind_penalty,
                               crosswind_penalty,
                               max_hp,
                               speed
                               );
        assert(type);

        type->mmode = mmode;
        session_add(Session, type, vehicle_type_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, type);
        scm_define(sc, tag, ret);
        return ret;
}

KERN_API_CALL(kern_mk_vehicle)
{
        Vehicle *vehicle;
        VehicleType *type;
        int facing;
        int hp;

        if (unpack(sc, &args, "pdd", &type, &facing, &hp)) {
                load_err("kern-mk-vehicle: bad args");
                return sc->NIL;
        }

        if (!type) {
                load_err("kern-mk-vehicle-type: null type");
                return sc->NIL;
        }

        vehicle = new Vehicle(type, facing, hp);
        assert(vehicle);

        return scm_mk_ptr(sc, vehicle);
}

KERN_API_CALL(kern_obj_get_sprite)
{
        Object *obj = unpack_obj(sc, &args, "kern-obj-get-sprite");
        if (!obj)
                return sc->NIL;

        return scm_mk_ptr(sc, obj->getSprite());
}

KERN_API_CALL(kern_obj_get_light)
{
        Object *obj = unpack_obj(sc, &args, "kern-obj-get-light");
        if (!obj)
                return sc->NIL;

        return scm_mk_integer(sc, obj->getLight());
}

KERN_API_CALL(kern_obj_get_mmode)
{
        struct mmode *mmode;

        Object *obj = unpack_obj(sc, &args, "kern-obj-get-mmode");
        if (!obj)
                return sc->NIL;

        mmode = obj->getMovementMode();
        if (mmode)
                return scm_mk_ptr(sc, mmode);
        return sc->NIL;
}

KERN_API_CALL(kern_obj_get_name)
{
        struct mmode *mmode;

        Object *obj = unpack_obj(sc, &args, "kern-obj-get-name");
        if (!obj)
                return sc->NIL;

        return scm_mk_string(sc, obj->getName());
}

KERN_API_CALL(kern_obj_set_light)
{
        Object *obj;
        int light;

        if (unpack(sc, &args, "pd", &obj, &light)) {
                rt_err("kern-obj-set-light: bad args");
                return sc->NIL;
        }

        if (!obj) {
                rt_err("kern-obj-set-light: null obj");
                return sc->NIL;
        }

        obj->setLight(light);

        return sc->NIL;
}

KERN_API_CALL(kern_sleep)
{
        int msecs;

        if (unpack(sc, &args, "d", &msecs)) {
                rt_err("kern-sleep: bad args");
                return sc->F;
        }
        //usleep(MS_PER_TICK * msecs);
        SDL_Delay(msecs);
        return sc->T;
}

KERN_API_CALL(kern_map_view_create)
{
        return scm_mk_ptr(sc, mapCreateView());
}


KERN_API_CALL(kern_map_view_destroy)
{
        struct mview *v = kern_unpack_mview(sc, &args, 
                                            "kern-map-view-destroy");
        if (v)
                mapDestroyView(v);
        return sc->NIL;
}

KERN_API_CALL(kern_map_view_add)
{
        struct mview *v = kern_unpack_mview(sc, &args, "kern-map-view-add");
        if (v)
                mapAddView(v);
        return sc->NIL;
}

KERN_API_CALL(kern_map_view_rm)
{
        struct mview *v = kern_unpack_mview(sc, &args, "kern-map-view-rm");
        if (v)
                mapRmView(v);
        return sc->NIL;
}

KERN_API_CALL(kern_map_view_center)
{
        struct place *place;
        int x, y;

        struct mview *v = kern_unpack_mview(sc, &args, 
                                            "kern-map-view-center");
        if (!v)
                return sc->NIL;

        if (! kern_unpack_loc(sc, &args, &place, &x, &y,
                              "kern-map-view-center"))
                return sc->NIL;

        mapSetPlace(place);
        mapCenterView(v, x, y);

        return sc->NIL;
}

KERN_API_CALL(kern_map_center_camera)
{
        struct place *place;
        int x, y;

        if (! kern_unpack_loc(sc, &args, &place, &x, &y,
                              "kern-map-view-center"))
                return sc->NIL;

        mapCenterCamera(x, y);

        return sc->NIL;
}

KERN_API_CALL(kern_map_repaint)
{
        mapUpdate(0);
        return sc->NIL;
}

KERN_API_CALL(kern_map_flash)
{
        int msecs;
        if (unpack(sc, &args, "d", &msecs)) {
                rt_err("kern-map-flash: bad args");
                return sc->NIL;
        }
        mapFlash(msecs);
        return sc->NIL;
}

KERN_API_CALL(kern_sound_play)
{
        sound_t *sound;
        if (unpack(sc, &args, "p", &sound)) {
                rt_err("kern-sound-play: bad args");
                return sc->NIL;
        }
        sound_play(sound, SOUND_MAX_VOLUME);
        return sc->NIL;
}

KERN_API_CALL(kern_set_spell_words)
{
        int i = 0;
        pointer words;
        pointer word;

        words = args;

        for (i = 0; i < MAX_SPELL_WORDS; i++) {

                /* check for end-of-list */
                if (! scm_is_pair(sc, words))
                        break;

                word = scm_car(sc, words);
        
                /* type-check */
                if (! scm_is_str(sc, word)) {
                        load_err("kern-set-spell-words: entry %i not a string",
                                 i);
                        break;
                }

                /* copy the word into the global list of words */
                if (magic_add_word(&Session->magic, scm_str_val(sc, word))) {
                        load_err("kern-set-spell-words: error adding '%s'",
                                 scm_str_val(sc, word));
                }
                                
                words = scm_cdr(sc, words);
        }

        return sc->NIL;
}

KERN_API_CALL(kern_add_spell)
{
        char *code;
        ObjectType *type;
        struct spell *spell;
        pointer reagents;

        /* Unpack just as far as the word until we can verify that we can add
         * this spell. */
        if (unpack(sc, &args, "ps", &type, &code)) {
                load_err("kern-add-spell: bad args");
                return sc->NIL;
        }
        
        if (!(spell = magic_add_spell(&Session->magic, code))) {
                load_err("kern-add-spell: failed to add %s",
                         type->getName());
                return sc->NIL;
        }

        spell->type = type;

        /* NOTE: unlike other kernel data structures/objects, if we fail we
         * don't have to deallocate the spell structure. Nor do we need to
         * explicitly add it to the session for teardown later. After
         * magic_add_spell() returns the spell structure has already been added
         * to the spell tree associated with the session, and will be
         * automatically deallocated at end-of-session. */

        /* unpack remaining fields (other than the reagent list) directly into
         * the spell structure. */
        if (unpack(sc, &args, "dddddd", &spell->level, &spell->cost,
                   &spell->context, &spell->flags, &spell->range,
                   &spell->action_points)) {
                load_err("kern-add-spell: bad args");
                return sc->NIL;
        }

        if (! scm_is_pair(sc, args)) {
                load_err("kern-add-spell: no reagents listed");
                return sc->NIL;
        }

        reagents = scm_car(sc, args);
        args = scm_cdr(sc, args);

        while (scm_is_pair(sc, reagents)) {
                ObjectType *reagent_type;
                if (unpack(sc, &reagents, "p", &reagent_type)) {
                        load_err("kern-add-spell %s: bad arg in reagent list",
                                 spell->type->getName());
                        return sc->NIL;
                }
                if (spell_add_reagent(spell, reagent_type)) {
                        load_err("kern-add-spell: failed to add reagent %s "\
                                 "to mixture for spell %s", 
                                 reagent_type->getName(), 
                                 spell->type->getName());
                        return sc->NIL;
                }
                
        }

        return sc->NIL;
}

KERN_API_CALL(kern_dice_roll)
{
        static char *dice;

        if (unpack(sc, &args, "s", &dice)) {
                rt_err("kern-dice-roll: bad args");
                return scm_mk_integer(sc, 0);
        }

        if (!dice_valid(dice)) {
                rt_err("kern-dice-roll: bad dice '%s'", dice);
                return scm_mk_integer(sc, 0);
        }

        return scm_mk_integer(sc, dice_roll(dice));
}

KERN_API_CALL(kern_char_set_sleep)
{
        class Character *ch;
        int val;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-set-sleep");
        if (!ch)
                return sc->F;

        if (unpack(sc, &args, "b", &val)) {
                rt_err("kern-char-set-sleep: bad args");
                return sc->F;
        }

        if (val)
                ch->sleep();
        else
                ch->awaken();

        return sc->T;
}

KERN_API_CALL(kern_char_attack)
{
        class Character *attacker, *defender;
        class ArmsType *weapon;

        if (unpack(sc, &args, "ppp", &attacker, &weapon, &defender)) {
                rt_err("kern-char-attack: bad args");
                return sc->F;
        }

        if (! attacker) {
                rt_err("kern-char-attack: null attacker");
                return sc->F;
        }

        if (! weapon) {
                rt_err("kern-char-attack: null weapon");
                return sc->F;
        }

        if (! defender) {
                rt_err("kern-char-attack: null defender");
                return sc->F;
        }

        ctrl_do_attack(attacker, weapon, defender);
        return sc->T;
}

KERN_API_CALL(kern_char_is_asleep)
{
        class Character *ch;
        int val;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-is-asleep");
        if (!ch)
                return sc->F;

        return ch->isAsleep() ? sc->T : sc->F;
}

KERN_API_CALL(kern_mk_effect)
{
        struct effect *effect;
        pointer exec_proc = sc->NIL;
        pointer apply_proc = sc->NIL;
        pointer rm_proc = sc->NIL;
        pointer ret;
        pointer pair; // dbg        
        char *name, *tag, *desc, *status_code_str, *hook_name;
        int hook_id;

        if (unpack(sc, &args, "ysscccs", &tag, &name, &desc, &exec_proc,
                   &apply_proc, &rm_proc, &hook_name)) {
                load_err("kern-mk-effect: bad args");
                return sc->NIL;
        }

        hook_id = Object::nameToHookId(hook_name);
        if (hook_id < 0) {
                load_err("kern-mk-effect: bad hook '%s'", hook_name);
                return sc->NIL;
        }

        if (exec_proc == sc->NIL)
                exec_proc = NULL;

        if (apply_proc == sc->NIL)
                apply_proc = NULL;

        if (rm_proc == sc->NIL)
                rm_proc = NULL;

        effect = effect_new(tag, sc, exec_proc, apply_proc, rm_proc, name, desc);

        effect->hook_id = hook_id;

        if (unpack(sc, &args, "sdpbd", &status_code_str, &effect->detect_dc,
                   &effect->sprite, &effect->cumulative, &effect->duration)) {
                load_err("kern-mk-effect: bad args");
                goto abort;
        }

        if (status_code_str)
                effect->status_code = status_code_str[0];
        
        session_add(Session, effect, effect_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, effect);
        scm_define(sc, tag, ret);

        return ret;

 abort:
        effect_del(effect);
        return sc->NIL;
}

KERN_API_CALL(kern_ui_target)
{
        struct place *place;
        int ox, oy, tx, ty, range;

        /* Unpack the origin */
        if (unpack_loc(sc, &args, &place, &ox, &oy, "kern-ui-target")) {
                return sc->NIL;
        }
        
        /* Unpack the range */
        if (unpack(sc, &args, "d", &range)) {
                rt_err("kern-ui-target: bad range arg");
                return sc->NIL;
        }

        /* Not sure if this is really the best place to do this... */
	cmdwin_print("-");

        /* Get the target coords from the user */
        tx = ox;
        ty = oy;
        if (select_target(ox, oy, &tx, &ty, range))
                return sc->NIL;
        
        /* Pack the target coords for return */
        return pack(sc, "pdd", place, tx, ty);
}

KERN_API_CALL(kern_fire_missile)
{
        ArmsType *missile_type;
        Missile *missile;
        struct place *oplace, *dplace;
        int ox, oy, dx, dy;
        Object *target;

        /* Unpack the missile type */
        if (unpack(sc, &args, "p", &missile_type)) {
                rt_err("kern-fire-missile: bad missile type arg");
                return sc->NIL;
        }
        if (! missile_type) {
                rt_err("kern-fire-missile: null missile type");
                return sc->NIL;
        }

        /* Unpack the origin */
        if (unpack_loc(sc, &args, &oplace, &ox, &oy, "kern-fire-missile"))
                return sc->NIL;

        /* Unpack the destination */
        if (unpack_loc(sc, &args, &dplace, &dx, &dy, "kern-fire-missile"))
                return sc->NIL;

        /* Create the missile */
        missile = new Missile(missile_type);
        assert(missile);

        /* Fire the missile */
        missile->setPlace(dplace);
        missile->animate(ox, oy, dx, dy, 0);
        if (missile->hitTarget()) {

                /* Run the missile's hit-loc procedure, if any */
                if (missile->getObjectType()->canHitLocation()) {
                        missile->getObjectType()->hitLocation(missile, 
                                                              dplace, 
                                                              dx, 
                                                              dy);
                }
        }

        delete missile;
        return sc->NIL;
}

KERN_API_CALL(kern_obj_inc_light)
{
        int light;

        Object *obj = unpack_obj(sc, &args, "kern-obj-inc-light");
        if (!obj)
                return sc->F;

        if (unpack(sc, &args, "d", &light)) {
                rt_err("kern-obj-inc-light: bad args");
                return sc->F;
        }

        obj->setLight(obj->getLight() + light);

        return sc->T;
}

KERN_API_CALL(kern_obj_dec_light)
{
        int light;

        Object *obj = unpack_obj(sc, &args, "kern-obj-dec-light");
        if (!obj)
                return sc->F;

        if (unpack(sc, &args, "d", &light)) {
                rt_err("kern-obj-dec-light: bad args");
                return sc->F;
        }

        obj->setLight(obj->getLight() - light);

        return sc->T;
}

KERN_API_CALL(kern_obj_dec_ap)
{
        int val;

        Object *obj = unpack_obj(sc, &args, "kern-obj-add-ap");
        if (!obj)
                return sc->F;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-obj-add-ap: bad args");
                return sc->F;
        }

        obj->decActionPoints(val);

        return sc->T;
}

KERN_API_CALL(kern_place_is_wilderness)
{
        struct place *place;

        if (unpack(sc, &args, "p", &place)) {
                rt_err("kern-place-is-wilderness: bad args");
                return sc->F;
        }

        if (!place) {
                rt_err("kern-place-is-wilderness: null place");
                return sc->F;
        }

        return place_is_wilderness(place) ? sc->T : sc->F;
}

KERN_API_CALL(kern_place_is_wrapping)
{
        struct place *place;

        if (unpack(sc, &args, "p", &place)) {
                rt_err("kern-place-is-wrapping: bad args");
                return sc->F;
        }

        if (!place) {
                rt_err("kern-place-is-wrapping: null place");
                return sc->F;
        }

        return place_is_wrapping(place) ? sc->T : sc->F;
}

KERN_API_CALL(kern_obj_heal)
{
        Object *obj;
        int val;

        obj = unpack_obj(sc, &args, "kern-obj-heal");
        if (!obj)
                return sc->NIL;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-obj-heal: bad args");
                return sc->NIL;
        }
        
        obj->heal(val);

        return sc->NIL;
}

static int kern_in_los_internal(struct place *p1, int x1, int y1,
                                struct place *p2, int x2, int y2)
{
        char *vmask;
        int x3, y3;

        /* Get the vmask for the origin (viewer's coords) */
        vmask = vmask_get(p1, x1, y1);

        /* Translate (x2, y2) into vmask coordinates. Notationally, let:

              a = vector from place origin to (x1, y1)
              b = vector from place origin to (x2, y2)
              o = vector from place origin to vmask origin

           We already know a, b and (a - o) = (VMASK_W/2, VMASK_H/2). We need
           to solve for (b - o) and then wrap if the place supports wrapping.

              b - a + (a - o) = b - o
        */

        x3 = place_wrap_x(p1, x2 - x1 + VMASK_W/2);
        y3 = place_wrap_y(p1, y2 - y1 + VMASK_H/2);

        if (x3 < 0 ||
            y3 < 0 ||
            x3 >= VMASK_W ||
            y3 >= VMASK_H)
                return 0;

        return vmask[x3 + y3 * VMASK_W];
}

static int kern_filter_being(Object *obj, struct kern_append_info *info)
{
        return (obj->getLayer() == being_layer);
}

static int kern_filter_visible_hostile(Object *obj, 
                                       struct kern_append_info *info)
{
        class Being *subj;

        /* Extract a pointer to the subject looking for hostiles */
        subj = (class Being *)info->data;

        /* Filter out non-beings */
        if (obj->getLayer() != being_layer)
                return 0;

        /* Filter out non-hostiles */
        if (! are_hostile(subj, (class Being*)obj))
                return 0;

        /* Filter out objects not in los of the subject */
        if (! kern_in_los_internal(subj->getPlace(),subj->getX(),subj->getY(),
                                   obj->getPlace(),obj->getX(),obj->getY()))
                return 0;

        /* Filter out object not in the vision radius of the subject */
        if (place_flying_distance(subj->getPlace(),subj->getX(),subj->getY(),
                                  obj->getX(),obj->getY())
            > subj->getVisionRadius())
                return 0;

        /* Filter out invisible objects */
        if (! obj->isVisible())
                return 0;

        return 1;
}

static void kern_append_object(Object *obj, void *data)
{
        pointer cell;
        struct kern_append_info *info;

        info = (struct kern_append_info *)data;

        /* If there is a filter then use it */
        if (info->filter != NULL)

                /* If the filter rejects the object then don't append it */
                if (! info->filter(obj, info))
                        return;

        cell = scm_mk_ptr(info->sc, obj);
        cell = _cons(info->sc, cell, info->sc->NIL, 0);

        if (info->head == info->sc->NIL) {
                info->head = cell;
                info->tail = cell;

                /* Protect the list from gc until we can return to scheme */
                scm_protect(info->sc, cell);
        } else {
                info->tail->_object._cons._cdr = cell;
                info->tail = cell;
        }
}

static pointer scm_mk_loc(scheme *sc, struct place *place, int x, int y)
{
        return pack(sc, "pdd", place, x, y);

#if 0
        pointer pcell, xcell, ycell;

        pcell = scm_protect(scm_mk_ptr(sc, place));
        xcell = scm_protect(scm_mk_integer(sc, x));
        ycell = scm_protect(scm_mk_integer(sc, y));

        return _cons(sc, pcell, 
                     _cons(sc, xcell, 
                           _cons(sc, ycell, sc->NIL, 0), 
                           0), 
                     0);
#endif
}

static pointer 
kern_place_for_each_object_at(scheme *sc, struct place *place, int x, int y,
                              int (*filter)(Object *, 
                                            struct kern_append_info *),
                              void *data)
{
        struct kern_append_info info;

        /* initialize the context used by the callback to append objects */
        info.sc = sc;
        info.head = sc->NIL;
        info.tail = sc->NIL;
        info.filter = filter;
        info.data = data;

        /* build a scheme list of the objects at that location */
        place_for_each_object_at(place, x, y, kern_append_object, &info);

        /* unprotect the list prior to return */
        if (info.head != sc->NIL)
                scm_unprotect(sc, info.head);

        /* return the scheme list */
        return info.head;

}

static pointer 
kern_place_for_each_object(scheme *sc, struct place *place, 
                           int (*filter)(Object *, struct kern_append_info *),
                           void *data)
{
        struct kern_append_info info;

        /* initialize the context used by the callback to append objects */
        info.sc = sc;
        info.head = sc->NIL;
        info.tail = sc->NIL;
        info.filter = filter;
        info.data = data;

        /* build a scheme list of the objects at that location */
        place_for_each_object(place, kern_append_object, &info);

        /* unprotect the list prior to return */
        if (info.head != sc->NIL)
                scm_unprotect(sc, info.head);

        /* return the scheme list */
        return info.head;

}


KERN_API_CALL(kern_get_objects_at)
{
        struct place *place;
        int x, y;

        /* unpack the location */
        if (unpack_loc(sc, &args, &place, &x, &y, "kern-get-objects-at"))
                return sc->NIL;

        /* get all objects with no filtering */
        return kern_place_for_each_object_at(sc, place, x, y, NULL, NULL);
}

KERN_API_CALL(kern_obj_is_char)
{
        Object *obj;

        obj = unpack_obj(sc, &args, "kern-obj-is-char?");
        if (!obj)
                return sc->F;

        return (obj->getLayer() == being_layer) ? sc->T : sc->F;
}

KERN_API_CALL(kern_obj_is_visible)
{
        Object *obj;

        obj = unpack_obj(sc, &args, "kern-obj-is-visible?");
        if (!obj)
                return sc->F;

        return obj->isVisible() ? sc->T : sc->F;
}

KERN_API_CALL(kern_char_set_fleeing)
{
        int val;
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-set-fleeing");
        if (!ch)
                return sc->NIL;

        if (unpack(sc, &args, "b", &val)) {
                rt_err("kern-char-set-fleeing: bad args");
                return sc->NIL;
        }
        
        ch->setFleeing(val);

        return sc->NIL;

}

KERN_API_CALL(kern_char_get_species)
{
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-get-species");
        if (!ch)
                return sc->NIL;

        return scm_mk_ptr(sc, ch->species);
}

KERN_API_CALL(kern_char_get_mana)
{
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-get-mana");
        if (!ch)
                return sc->NIL;

        return scm_mk_integer(sc, ch->getMana());
}

KERN_API_CALL(kern_place_get_beings)
{
        struct place *place;

        /* unpack the place */
        if (unpack(sc, &args, "p", &place)) {
                rt_err("kern-place-get-beings: bad args");
                return sc->NIL;
        }
        if (! place) {
                rt_err("kern-place-get-beings: null place");
                return sc->NIL;
        }

        return kern_place_for_each_object(sc, place, kern_filter_being, NULL);
}

KERN_API_CALL(kern_being_get_visible_hostiles)
{
        Object *subj;

        /* Unpack the subject */
        subj = unpack_obj(sc, &args, "kern-place-get-visible-hostiles");
        if (!subj)
                return sc->NIL;

        if (! subj->getPlace()) {
                rt_err("kern-place-get-visible-hostiles: null place");
                return sc->NIL;
        }

        return kern_place_for_each_object(sc, subj->getPlace(), 
                                          kern_filter_visible_hostile,
                                          subj);
}

KERN_API_CALL(kern_place_get_objects)
{
        struct place *place;

        /* unpack the place */
        if (unpack(sc, &args, "p", &place)) {
                rt_err("kern-place-get-objects: bad args");
                return sc->NIL;
        }
        if (! place) {
                rt_err("kern-place-get-objects: null place");
                return sc->NIL;
        }

        return kern_place_for_each_object(sc, place, NULL, NULL);
}


/* struct kern_place_get_objects_in_los_info { */
/*         struct kern_append_info ap_info; */
/*         struct place *place; */
/*         int ox;  /\* looker's x *\/ */
/*         int oy;  /\* looker's y *\/ */
/*         int rad; /\* looker's rad *\/ */
/*         int vx;  /\* vmask ulc x *\/ */
/*         int vy;  /\* vmask ulc y *\/ */
/*         char *vmask; */
/* }; */

/* static void kern_place_get_objects_in_los_cb(Object *obj, void *data) */
/* { */
/*         struct kern_place_get_objects_in_los_info *info; */
/*         int x, y; */

/*         info = (struct kern_place_get_objects_in_los_info *)data; */

/*         /\* check if the object is within vision radius *\/ */
/*         if (place_flying_distance(info->place, */
/*                                   info->ox, */
/*                                   info->oy, */
/*                                   obj->getX(), */
/*                                   obj->getY()) */
/*             > info->rad) { */
/*                 return; */
/*         } */

/*         /\* translate the object's coordinates into coordinates offset from the */
/*          * upper left corner of the vmask region *\/ */
/*         x = obj->getX() - info->vx; */
/*         y = obj->getY() - info->vy; */

/*         /\* check if the object is outside the vmask *\/ */
/*         if (x < 0 || */
/*             y < 0 || */
/*             x >= VMASK_W || */
/*             y >= VMASK_H) */
/*                 return; */

/*         /\* if the object's tile is marked as visible then add it to the list *\/ */
/*         if (info->vmask[y * VMASK_W + x]) */
/*                 kern_append_object(obj, &info->ap_info); */
/* } */

/* KERN_API_CALL(kern_place_get_objects_in_los) */
/* { */
/*         class Object *obj; */
/*         struct kern_place_get_objects_in_los_info info; */

/*         obj = unpack_obj(sc, &args, "kern-place-get-objects-in-los"); */
/*         if (! obj) */
/*                 return sc->NIL; */

/*         if (! obj->getPlace()) { */
/*                 rt_err("kern-place-get-object-in-los: obj has null place"); */
/*                 return sc->NIL; */
/*         } */

/*         /\* initialize the context used by the callback to append objects *\/ */
/*         info.ap_info.sc   = sc; */
/*         info.ap_info.head = sc->NIL; */
/*         info.ap_info.tail = sc->NIL; */
/*         info.place        = obj->getPlace(); */
/*         info.ox           = obj->getX(); */
/*         info.oy           = obj->getY(); */
/*         info.rad          = obj->getVisionRadius(); */
/*         info.vx           = obj->getX() - VMASK_W / 2; */
/*         info.vy           = obj->getY() - VMASK_H / 2; */
/*         info.vmask        = vmask_get(obj->getPlace(), */
/*                                       obj->getX(),  */
/*                                       obj->getY()); */

/*         /\* build a scheme list of the objects *\/ */
/*         place_for_each_object(obj->getPlace(),  */
/*                               kern_place_get_objects_in_los_cb, &info); */

/*         /\* return the scheme list *\/ */
/*         return info.ap_info.head; */
/* } */

KERN_API_CALL(kern_place_get_name)
{
        struct place *place;

        /* unpack the place */
        if (unpack(sc, &args, "p", &place)) {
                rt_err("kern-place-get-objects: bad args");
                return sc->NIL;
        }
        if (! place) {
                rt_err("kern-place-get-objects: null place");
                return sc->NIL;
        }

        return scm_mk_string(sc, place->name);
}

KERN_API_CALL(kern_mk_stock_char)
{
        class Character *character;
        struct species *species;
        struct occ *occ;
        struct sprite *sprite;
        char *name;
        pointer ai;

        if (unpack(sc, &args, "pppsc", &species, &occ, &sprite, &name, 
                   &ai)) {
                rt_err("kern-mk-stock-char: bad args");
                return sc->NIL;
        }

        character = new class Character();
        assert(character);
        character->initStock(species, occ, sprite, name, 0);
        if (ai != sc->NIL) {
                character->ai = closure_new(sc, ai);
                closure_ref(character->ai);
        }

        return scm_mk_ptr(sc, character);
}

KERN_API_CALL(kern_place_is_passable)
{
        struct place *place;
        int x, y, pmask;
        class Object *obj;

        if (unpack_loc(sc, &args, &place, &x, &y, "kern-place-is-passable"))
                return sc->F;

        obj = unpack_obj(sc, &args, "kern-place-is-passable");
        if (!obj)
                return sc->F;

        return place_is_passable(place, x, y, obj, 0) ? sc->T : sc->F;
}

KERN_API_CALL(kern_place_is_hazardous)
{
        struct place *place;
        int x, y, pmask;
        class Object *obj;

        if (unpack_loc(sc, &args, &place, &x, &y, "kern-place-is-hazardous"))
                return sc->F;

        obj = unpack_obj(sc, &args, "kern-place-is-hazardous");
        if (!obj)
                return sc->F;

        return place_is_hazardous(place, x, y) ? sc->T : sc->F;
}

KERN_API_CALL(kern_place_set_terrain)
{
        struct place *place;
        int x, y;
        struct terrain *terrain;

        if (unpack_loc(sc, &args, &place, &x, &y, "kern-place-set-terrain"))
                return sc->F;

        if (unpack(sc, &args, "p", &terrain)) {
                rt_err("kern-place-set-terrain: bad args");
                return sc->F;
        }

        place_set_terrain(place, x, y, terrain);

        /* Often changing the terrain requires us to recalculate LOS in the
         * surrounding area. */
        vmask_invalidate(place, x, y, 1, 1);

        /* And that means the map usually needs repainting, too */
        mapSetDirty();

        return sc->T;
}

KERN_API_CALL(kern_place_get_terrain)
{
        struct place *place;
        int x, y;
        struct terrain *terrain;

        if (unpack_loc(sc, &args, &place, &x, &y, "kern-place-get-terrain"))
                return sc->F;

        terrain = place_get_terrain(place, x, y);

        return terrain ? scm_mk_ptr(sc, terrain) : sc->NIL;
}

KERN_API_CALL(kern_obj_set_temporary)
{
        class Object *obj;
        int val;

        obj = unpack_obj(sc, &args, "kern-obj-set-temporary");
        if (!obj)
                return sc->NIL;

        if (unpack(sc, &args, "b", &val)) {
                rt_err("kern-obj-set-temporary: bad value arg");
                return scm_mk_ptr(sc, obj);
        }

        obj->setTemporary(val);

        return scm_mk_ptr(sc, obj);
}

KERN_API_CALL(kern_obj_wander)
{
        class Object *obj;

        obj = unpack_obj(sc, &args, "kern-obj-wander");
        if (!obj)
                return sc->NIL;

        ctrl_wander(obj);

        return sc->NIL;
}

KERN_API_CALL(kern_obj_clone)
{
        class Character *obj, *clone;
        int val;

        obj = (class Character*)unpack_obj(sc, &args, "kern-char-clone");
        if (!obj)
                return sc->NIL;

        clone = (class Character*)obj->clone();
        assert(clone);
        clone->setTemporary(1);

        return scm_mk_ptr(sc, clone);
}

KERN_API_CALL(kern_set_wind)
{
        int dur, dir;

        if (unpack(sc, &args, "dd", &dir, &dur)) {
                rt_err("kern-set-wind: bad args");
                return sc->F;
        }

        windSetDirection(dir, dur);
        return sc->T;
}

KERN_API_CALL(kern_ui_direction)
{
        int dir = ui_get_direction();

        if (dir == CANCEL)
                return sc->NIL;

        return scm_mk_integer(sc, dir);
}

KERN_API_CALL(kern_place_get_neighbor)
{
        struct place *place;
        struct place *neighbor;
        int dir;

        if (unpack(sc, &args, "pd", &place, &dir)) {
                rt_err("kern-place-get-neighbor: bad args");
                return sc->NIL;
        }

        switch (dir) {
        case UP:
                neighbor = place->above;
                break;
        case DOWN:
                neighbor = place->below;
                break;
        default:
                /* If you want to use this call to get east/west/north/south
                 * neighboring places (in the event that I someday implement
                 * Sam's request) then carefully revisit the usage in the
                 * script. */
                neighbor = NULL;
                break;
        }

        if (neighbor)
                return scm_mk_ptr(sc, neighbor);
        else
                return sc->NIL;
}

KERN_API_CALL(kern_char_get_party)
{
        class Character *ch;
        class Party *party;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-get-party");
        if (!ch)
                return sc->NIL;

        party = ch->getParty();
        if (party)
                return scm_mk_ptr(sc, party);
        else
                return sc->NIL;
}

KERN_API_CALL(kern_char_add_defense)
{
        int val;
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-add-defense");
        if (!ch)
                return sc->NIL;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-char-add-defense: bad args");
                return sc->NIL;
        }
        
        ch->addDefense(val);

        return sc->NIL;
}

KERN_API_CALL(kern_add_magic_negated)
{
        int val;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-add-magic-negated: bad args");
                return sc->F;
        }

        add_magic_negated(val);
        return sc->T;
}

KERN_API_CALL(kern_add_quicken)
{
        int val;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-add-quicken: bad args");
                return sc->F;
        }

        add_quicken(val);
        return sc->T;
}

KERN_API_CALL(kern_add_reveal)
{
        int val;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-add-reveal: bad args");
                return sc->F;
        }

        add_reveal(val);
        return sc->T;
}

KERN_API_CALL(kern_add_time_stop)
{
        int val;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-add-time-stop: bad args");
                return sc->F;
        }

        add_time_stop(val);
        return sc->T;
}

KERN_API_CALL(kern_char_is_hostile)
{
        class Character *one, *another;

        if (unpack(sc, &args, "pp", &one, &another)) {
                rt_err("kern-char-is-hostile: bad args");
                return sc->F;
        }

        if (! one || ! another) {
                rt_err("kern-char-is-hostile: null character");
                return sc->F;                
        }

        return are_hostile(one, another) ? sc->T : sc->F;
}

KERN_API_CALL(kern_add_xray_vision)
{
        int val;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-add-xray-vision: bad args");
                return sc->F;
        }

        add_xray(val);
        return sc->T;
}

KERN_API_CALL(kern_char_charm)
{
        int val;
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-charm");
        if (!ch)
                return sc->NIL;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-char-charm: bad args");
                return sc->NIL;
        }
        
        ch->charm(val);

        return sc->NIL;
}

KERN_API_CALL(kern_char_uncharm)
{
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-charm");
        if (!ch)
                return sc->NIL;

        ch->unCharm();

        return sc->NIL;
}

KERN_API_CALL(kern_map_set_jitter)
{
        int val;

        if (unpack(sc, &args, "b", &val)) {
                rt_err("kern-map-set-jitter: bad args");
                return sc->F;
        }

        mapJitter(val);
        return sc->T;
}

KERN_API_CALL(kern_char_kill)
{
        int val;
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-kill");
        if (!ch)
                return sc->NIL;

        ch->kill();

        return sc->NIL;
}

KERN_API_CALL(kern_char_resurrect)
{
        int val;
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-resurrect");
        if (!ch)
                return sc->NIL;

        ch->resurrect();

        return sc->NIL;
}

KERN_API_CALL(kern_is_valid_location)
{
        struct place *place;
        int x, y;

        if (unpack_loc(sc, &args, &place, &x, &y, "kern-is-valid-location?"))
                return sc->F;

        if (place->wraps)
                return sc->T;

        if (x < 0 || x >= place_w(place) ||
            y < 0 || y >= place_h(place))
                return sc->F;

        return sc->T;
}

KERN_API_CALL(kern_terrain_get_pclass)
{
        struct terrain *terrain;

        if (unpack(sc, &args, "p", &terrain)) {
                rt_err("kern-terrain-get-pclass: bad args");
                return sc->NIL;
        }

        if(! terrain) {
                rt_err("kern-terrain-get-pclass: null terrain");
                return sc->NIL;
        }

        return scm_mk_integer(sc, terrain_pclass(terrain));
}

KERN_API_CALL(kern_place_get_width)
{
        struct place *place;

        if (unpack(sc, &args, "p", &place)) {
                rt_err("kern-place-get-width: bad args");
                return sc->NIL;
        }

        if (!place) {
                rt_err("kern-place-get-width: null place");
                return sc->NIL;
        }

        return scm_mk_integer(sc, place_w(place));
}

KERN_API_CALL(kern_place_get_height)
{
        struct place *place;

        if (unpack(sc, &args, "p", &place)) {
                rt_err("kern-place-get-height: bad args");
                return sc->NIL;
        }

        if (!place) {
                rt_err("kern-place-get-height: null place");
                return sc->NIL;
        }

        return scm_mk_integer(sc, place_h(place));
}

KERN_API_CALL(kern_get_distance)
{
        struct place *p1, *p2;
        int x1, x2, y1, y2;

        if (unpack_loc(sc, &args, &p1, &x1, &y1, "kern-get-distance") ||
            unpack_loc(sc, &args, &p2, &x2, &y2, "kern-get-distance"))
                return sc->NIL;

        /* warn("p1=%s x1=%d y1=%d x2=%d y2=%d\n", p1->name, x1, y1, x2, y2); */

        if (p1 != p2) {
                rt_err("kern-get-distance: place %s different from %s",
                       p1->tag, p2->tag);
                return sc->NIL;
        }

        return scm_mk_integer(sc, place_flying_distance(p1, x1, y1, x2, y2));
}

KERN_API_CALL(kern_in_los)
{
        struct place *p1, *p2;
        int x1, x2, y1, y2, x3, y3;
        char *vmask;

        if (unpack_loc(sc, &args, &p1, &x1, &y1, "kern-in-los?") ||
            unpack_loc(sc, &args, &p2, &x2, &y2, "kern-in-los?"))
                return sc->F;

        if (p1 != p2) {
                rt_err("kern-in-los?: place %s different from %s",
                       p1->tag, p2->tag);
                return sc->F;
        }

        return kern_in_los_internal(p1, x1, y1, p2, x2, y2) ? sc->T : sc->F;
}

KERN_API_CALL(kern_map_set_peering)
{
        int val;
        if (unpack(sc, &args, "b", &val)) {
                rt_err("kern-map-set-peering: bad args");
                return sc->NIL;
        }
        mapPeer(val);
        return sc->NIL;
}

KERN_API_CALL(kern_ui_waitkey)
{
        int key;
        getkey(&key, anykey);
        return sc->NIL;
}

KERN_API_CALL(kern_char_dec_mana)
{
        int val;
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-dec-mana");
        if (!ch)
                return sc->NIL;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-char-dec-mana: bad args");
                return sc->NIL;
        }
        
        ch->addMana(0 - val);

        return sc->NIL;        
}

KERN_API_CALL(kern_test_recursion)
{
        pointer func;

        if (unpack(sc, &args, "o", &func)) {
                rt_err("kern-test-recursion: bad args");
                return sc->NIL;
        }

        scheme_call(sc, func, sc->NIL);

        return sc->NIL;
}

static int kern_kh_cb(struct KeyHandler * kh, int key, int keymod)
{
        char *key_name = NULL;
        char buf[2];
        pointer sym;

        /* map the key to a string name */
        if (isprint(key)) {
                sprintf(buf, "%c", key);
                key_name = buf;
        } else {
                /* Note: KEY_NORTH and KEY_UP have the same value. If you put
                 * clauses in for both the compiler will warn about duplicate
                 * case statements. The same goes for the other three primary
                 * directions. */
                switch (key) {
                case KEY_NORTHWEST:
                        break;
                case KEY_NORTH:
                        key_name = "up";
                        break;
                case KEY_NORTHEAST:
                        key_name = "northeast";
                        break;
                case KEY_WEST:
                        key_name = "left";
                        break;
                case KEY_HERE:
                        key_name = "here";
                        break;
                case KEY_EAST:
                        key_name = "right";
                        break;
                case KEY_SOUTHWEST:
                        key_name = "southwest";
                        break;
                case KEY_SOUTH:
                        key_name = "down";
                        break;
                case KEY_SOUTHEAST:
                        key_name = "southeast";
                        break;
                case KEY_SHIFT_NORTH:
                        key_name = "shift-north";
                        break;
                case KEY_SHIFT_SOUTH:
                        key_name = "shift-south";
                        break;
                case KEY_SHIFT_EAST:
                        key_name = "shift-east";
                        break;
                case KEY_SHIFT_WEST:
                        key_name = "shift-west";                        
                        break;
                default:
                        key_name = "unknown";
                }
        }

        /* invoke the script's key handler */
        return closure_exec((struct closure*)kh->data, "y", key_name);
}

KERN_API_CALL(kern_ui_handle_events)
{
        struct KeyHandler kh;
        pointer func;

        if (unpack(sc, &args, "o", &func)) {
                rt_err("kern-ui-handle-events: bad args");
                return sc->F;
        }

        kh.fx = kern_kh_cb;
        kh.data = closure_new(sc, func);
        eventPushKeyHandler(&kh);
        eventHandle();
        eventPopKeyHandler();
        return sc->T;
}

KERN_API_CALL(kern_ui_select_from_list)
{
        struct KeyHandler kh;
	struct ScrollerContext data;
        char **strings;
        int list_sz;
        int i = 0;
        enum StatusMode omode;
        char *selection = NULL;

        list_sz = scm_len(sc, args);
        if (! list_sz)
                return sc->NIL;

        strings = (char**)calloc(list_sz, sizeof(strings[0]));
        assert(strings);

        while (scm_is_pair(sc, args)) {
                if (unpack(sc, &args, "s", &strings[i])) {
                        rt_err("kern-ui-select-from-list: bad args");
                        goto done;
                }
                i++;
        }

        omode = statusGetMode();
        statusSetStringList(list_sz, strings);
        statusSetMode(StringList);

        data.selection = NULL;
        data.selector  = String;
        kh.fx   = scroller;
        kh.data = &data;
	eventPushKeyHandler(&kh);
	eventHandle();
	eventPopKeyHandler();

        statusSetMode(omode);
        
        selection = (char*)data.selection;

 done:
        if (strings)
                free(strings);

        if (selection)
                return scm_mk_string(sc, selection);

        return sc->NIL;

}

KERN_API_CALL(kern_ui_page_text)
{
        struct KeyHandler kh;
        char *title;
        char *text = NULL;   
        int len = 0;
        int lines = 0;

        if (unpack(sc, &args, "s", &title)) {
                rt_err("kern-ui-status-page-text: bad title");
                return sc->NIL;
        }

        while (scm_is_pair(sc, args)) {

                char *line;

                if (unpack(sc, &args, "s", &line)) {
                        rt_err("kern-ui-status-page-text: bad text line");                        
                        goto done;
                }
                
                len += strlen(line);
                len++; /* for \n */
                text = (char*)realloc(text, len + 1 /* for \0 */);
                if (lines == 0) {
                        text[0] = 0;
                }
                strcat(text, line);
                strcat(text, "\n");
                lines++;
        }

        statusSetPageText(title, text);
        statusSetMode(Page);
        consolePrint("[Hit ESC to continue]\n");

        kh.fx = scroller;
        kh.data = NULL;
	eventPushKeyHandler(&kh);
	eventHandle();
	eventPopKeyHandler();

        statusSetMode(ShowParty);

 done:
        if (text)
                free(text);

        return sc->NIL;
}

KERN_API_CALL(kern_obj_remove_from_inventory)
{
        class Object *obj;
        class ObjectType *type;
        int amount;

        if (unpack(sc, &args, "ppd", &obj, &type, &amount)) {
                rt_err("kern-obj-remove-from-inventory: bad args");
                return sc->NIL;
        }

        obj->takeOut(type, amount);
        return sc->NIL;
}

KERN_API_CALL(kern_obj_add_to_inventory)
{
        class Object *obj;
        class ObjectType *type;
        int amount;

        if (unpack(sc, &args, "ppd", &obj, &type, &amount)) {
                rt_err("kern-obj-add-to-inventory: bad args");
                return sc->NIL;
        }

        obj->add(type, amount);
        return sc->NIL;
}

KERN_API_CALL(kern_mk_ptable)
{
        int n_mmode;
        int n_pclass;
        int pclass;
        struct ptable *ptable;
        pointer row;
        pointer col;

        /* The ptable table is a list of lists. Each row corresponds to a
         * passability class (a property of terrain, and objects which affect
         * passability onto a tile). Each column corresponds to a movement
         * mode. */

        if (! scm_is_pair(sc, args)) {
                load_err("kern-mk-ptable: arg 0 not a list");
                return sc->NIL;
        }

        /* count the number of passability classes and movement modes given in
         * the table */
        row = args;
        col = scm_car(sc, args);

        n_pclass = scm_len(sc, row);
        n_mmode = scm_len(sc, col);

        if (n_pclass <= 0) {
                load_err("kern-mk-ptable: 0 rows given");
                return sc->NIL;
        }

        if (n_mmode <= 0) {
                load_err("kern-mk-ptable: row 0 has no columns");
                return sc->NIL;
        }

        /* allocate the kernel passability table */
        ptable = ptable_new(n_mmode, n_pclass);
        
        /* for each row (passability class) */
        for (pclass = 0; pclass < n_pclass; pclass++) {

                int mmode;

                col = scm_car(sc, row);
                row = scm_cdr(sc, row);

                if (scm_len(sc, col) < n_mmode) {
                        load_err("kern-mk-ptable: row %d has only %d columns",
                                 pclass, scm_len(sc, col));
                        goto abort;
                }

                /* for each column (movement mode) */
                for (mmode = 0; mmode < n_mmode; mmode++) {

                        int mcost;

                        /* get the movement cost */
                        if (unpack(sc, &col, "d", &mcost)) {
                                load_err("kern-mk-ptable: row %d col %d bad arg",
                                         pclass, mmode);
                                goto abort;
                        }

                        /* insert it into the passability table */
                        ptable_set(ptable, mmode, pclass, mcost);
                }
        }


        /* associate the session with the new table */
        if (Session->ptable) {
                ptable_del(Session->ptable);
        }
        Session->ptable = ptable;

        return sc->NIL;

 abort:
        ptable_del(ptable);
        return sc->NIL;
}

static void kern_dtable_restore_faction(int handle, int level, void *context)
{
        int *args = (int*)context;
        dtable_restore((struct dtable*)args[0], args[1], args[2], handle, 
                       level);
}

KERN_API_CALL(kern_mk_dtable)
{
        int n_factions;
        int r_faction;
        struct dtable *dtable;
        pointer rows;
        pointer row;

        /* The dtable table is a list of lists. Each row corresponds to a
         * passability class (a property of terrain, and objects which affect
         * passability onto a tile). Each column corresponds to a movement
         * mode. */

        if (! scm_is_pair(sc, args)) {
                load_err("kern-mk-dtable: arg 0 not a list");
                return sc->NIL;
        }

        /* count the number of factions given in the table */
        rows = args;
        row  = scm_car(sc, rows);
        n_factions = scm_len(sc, rows);
        if (n_factions != scm_len(sc, row)) {
                load_err("kern-mk-dtable: # of rows and columns must be same");
                return sc->NIL;
        }
        if (n_factions <= 0) {
                load_err("kern-mk-dtable: 0 factions given");
                return sc->NIL;
        }

        /* allocate the kernel table */
        dtable = dtable_new(n_factions);

        /* for each row */
        for (r_faction = 0; r_faction < n_factions; r_faction++) {

                int c_faction;

                row  = scm_car(sc, rows);
                rows = scm_cdr(sc, rows);

                if (scm_len(sc, row) < n_factions) {
                        load_err("kern-mk-dtable: row %d has only %d columns "
                                 "(expected %d)",
                                 r_faction, scm_len(sc, row),
                                 n_factions);
                        goto abort;
                }

                /* for each column up to the limit */
                for (c_faction = 0; c_faction < n_factions; c_faction++) {

                        pointer col;
                        int parms[3];

                        col = scm_car(sc, row);
                        row = scm_cdr(sc, row);

                        /* make sure each column stack has at least one
                         * entry */
                        if (scm_len(sc, col) == 0) {
                                load_err("kern-mk-dtable: row %d col %d stack "
                                         "empty", r_faction, c_faction);
                                goto abort;
                        }

                        /* each column is a stack of pairs of numbers */
                        parms[0] = (int)dtable;
                        parms[1] = r_faction;
                        parms[2] = c_faction;
                        if (kern_load_hstack(sc, col, 
                                             kern_dtable_restore_faction,
                                             "kern-mk-dtable", &parms))
                                goto abort;
#if 0
                        while (scm_is_pair(sc, col)) {

                                int level, handle;
                                pointer pair;

                                pair = scm_car(sc, col);
                                col = scm_cdr(sc, col);

                                /* get the handle/level pair */
                                if (unpack(sc, &pair, "dd", &handle, &level)) {
                                        load_err("kern-mk-dtable: row %d col "
                                                 "%d bad arg",
                                                 r_faction, c_faction);
                                        goto abort;
                                }

                                /* restore it into the diplomacy table */
                                dtable_restore(dtable, r_faction, c_faction, 
                                               handle, level);
                        }
#endif
                }
        }

        /* associate the session with the new table */
        if (Session->dtable) {
                dtable_del(Session->dtable);
        }
        Session->dtable = dtable;

        return scm_mk_ptr(sc, dtable);

 abort:
        dtable_del(dtable);
        return sc->NIL;
}

#define DTABLE_SET    0x81
#define DTABLE_PUSH   0x82
#define DTABLE_CHANGE 0x83
#define DTABLE_POP    0x04
#define DTABLE_GET    0x05

#define DTABLE_FX_USES_LEVEL(fx) ((fx) & 0x80)

static pointer kern_dtable_aux(scheme *sc, pointer args, char *name, int fx)
{
        int f1, f2, level;
        char *errstr = NULL;

        if (! session_dtable()) {
                errstr = "no dtable";
                goto abort;
        }

        if (unpack(sc, &args, "dd", &f1, &f2)) {
                errstr = "bad faction args";
                goto abort;
        }

        if (DTABLE_FX_USES_LEVEL(fx)) {
                if (unpack(sc, &args, "d", &level)) {
                        errstr = "bad level arg";
                        goto abort;
                }
        }

        switch (fx) {
        case DTABLE_SET:
                dtable_set(session_dtable(), f1, f2, level);
                break;
        case DTABLE_CHANGE:
                dtable_change(session_dtable(), f1, f2, level);
                break;
        case DTABLE_PUSH:
                dtable_push(session_dtable(), f1, f2, level);
                break;
        case DTABLE_POP:
                dtable_pop(session_dtable(), f1, f2);
                break;
        case DTABLE_GET:
                level = dtable_get(session_dtable(), f1, f2);
                return scm_mk_integer(sc, level);
                break;
        default:
                assert(0);
                break;
        }

        return sc->T;
abort:
        rt_err("%s: %s", name, errstr);
        return sc->F;

}

KERN_API_CALL(kern_dtable_set)
{
        return kern_dtable_aux(sc, args, "kern_dtable_set", DTABLE_SET);
}

KERN_API_CALL(kern_dtable_change)
{
        return kern_dtable_aux(sc, args, "kern_dtable_change", DTABLE_CHANGE);
}

KERN_API_CALL(kern_dtable_push)
{
        return kern_dtable_aux(sc, args, "kern_dtable_push", DTABLE_PUSH);
}

KERN_API_CALL(kern_dtable_pop)
{
        return kern_dtable_aux(sc, args, "kern_dtable_pop", DTABLE_POP);
}

KERN_API_CALL(kern_dtable_get)
{
        return kern_dtable_aux(sc, args, "kern_dtable_get", DTABLE_GET);
}

KERN_API_CALL(kern_party_add_member)
{
        class Party *party;
        class Character *new_member;

        party = (Party*)unpack_obj(sc, &args, "kern_party_add_member:<party>");
        if (!party)
                return sc->NIL;

        new_member = (class Character*)unpack_obj(sc, &args, 
                                                  "kern_party_add_member:<member>");
        if (!new_member)
                return sc->NIL;

        if (party->addMember(new_member))
                return sc->T;

        return sc->F;
}

KERN_API_CALL(kern_being_set_base_faction)
{
        class Being *being;
        int faction;

        being = (class Character*)unpack_obj(sc, &args, "kern-being-set-base-faction");
        if (!being)
                goto done;

        if (unpack(sc, &args, "d", &faction)) {
                rt_err("kern-being-set-base-faction:<faction>: bad arg");
                goto done;
        }

        being->setBaseFaction(faction);
 done:
        return scm_mk_ptr(sc, being);

}

KERN_API_CALL(kern_being_get_current_faction)
{
        class Being *being;
        int faction = INVALID_FACTION;

        being = (Being*)unpack_obj(sc, &args, "kern-being-get-current-faction");
        if (!being)
                goto done;

        faction = being->getCurrentFaction();
 done:
        return scm_mk_integer(sc, faction);
}

KERN_API_CALL(kern_being_get_base_faction)
{
        class Being *being;
        int faction = INVALID_FACTION;

        being = (Being*)unpack_obj(sc, &args, "kern-being-get-base-faction");
        if (!being)
                goto done;

        faction = being->getBaseFaction();
 done:
        return scm_mk_integer(sc, faction);
}

KERN_API_CALL(kern_set_start_proc)
{
        pointer proc;

        if (unpack(sc, &args, "o", &proc)) {
                rt_err("kern-set-start-proc");
                return sc->NIL;
        }

        session_set_start_proc(Session, closure_new(sc, proc));

        return proc;
}

KERN_API_CALL(kern_player_set_follow_mode)
{
        player_party->enableFollowMode();
        return sc->NIL;
}

KERN_API_CALL(kern_obj_has)
{
        class Object *object;
        class ObjectType *type;
        int has = 0;

        object = (Object*)unpack_obj(sc, &args, "kern-obj-has");
        if (!object)
                goto done;

        if (unpack(sc, &args, "p", &type)) {
                rt_err("kern-obj-has?");
                goto done;
        }

        has = object->hasInInventory(type);
 done:
        return has ? sc->T : sc->F;
}

static pointer kern_astar_path_to_scheme_list(scheme *sc, struct astar_node *path)
{
        pointer head;
        pointer cell;

        /* base case - end of path */
        if (!path)
                return sc->NIL;

        /* create a scheme pair (x, y) */
        cell = _cons(sc, 
                     scm_mk_integer(sc, path->x), 
                     scm_mk_integer(sc, path->y),
                     0);

        /* recursively build a scheme list of pairs */
        head = _cons(sc, 
                     cell, 
                     kern_astar_path_to_scheme_list(sc, path->next), 
                     0);

        /* cleanup the node */
        astar_node_destroy(path);

        return head;
}

KERN_API_CALL(kern_obj_find_path)
{
        class Object *object;
        struct place *place;
        int x, y;
        struct astar_node *path = NULL;
        struct astar_search_info as_info;
        pointer sc_path;

        memset(&as_info, 0, sizeof (as_info));

        object = (Object*)unpack_obj(sc, &args, "kern-obj-find-path");
        if (!object)
                return sc->NIL;

        if (unpack_loc(sc, &args, &place, &as_info.x1, &as_info.y1, 
                       "kern-obj-find-path")) {
                return sc->NIL;
        }

        /* can't pathfind between places */
        if (object->getPlace() != place)
                return sc->NIL;

        /* find the path */
        as_info.x0 = object->getX();
        as_info.y0 = object->getY();
        printf("%d: begin pathfinding\n", SDL_GetTicks());
        path = place_find_path(place, &as_info, object);
        printf("%d: end pathfinding\n", SDL_GetTicks());
        if (! path)
                return sc->NIL;

        /* convert the path to a scheme list */
        sc_path = kern_astar_path_to_scheme_list(sc, path);
        return sc_path;
}

static pointer kern_build_weapon_list(scheme *sc, 
                                      class Character *character, 
                                      class ArmsType *weapon)
{
        /* base case */
        if (! weapon)
                return sc->NIL;
        
        /* recursive case */
        return _cons(sc, 
                     scm_mk_ptr(sc, weapon), 
                     kern_build_weapon_list(sc, 
                                            character, 
                                            character->getNextWeapon()), 
                     0);
}

KERN_API_CALL(kern_char_get_weapons)
{
        class Character *character;
        pointer lst;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, "kern-char-get-weapons");
        if (!character)
                return sc->NIL;

        /* recursively enumerate the character's available weapons into a
         * scheme list */
        return kern_build_weapon_list(sc, 
                                      character, 
                                      character->enumerateWeapons());
}

static pointer kern_build_container_list(scheme *sc,
                                         class Container *container,
                                         struct inv_entry *ie)
{
        pointer cell;

        /* base case */
        if (! ie)
                return sc->NIL;
        
        /* make a type/count pair */
        cell = _cons(sc, 
                     scm_mk_ptr(sc, ie->type),
                     scm_mk_integer(sc, ie->count),
                     0);

        /* recursively build a list of such pairs */
        return _cons(sc, 
                     cell,
                     kern_build_container_list(sc, 
                                               container, 
                                               container->next(ie, NULL)), 
                     0);
}

KERN_API_CALL(kern_char_get_inventory)
{
        class Container *container;
        class Character *character;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, "kern-char-get-hp");
        if (!character)
                return sc->NIL;

        /* grab it's inventory container */
        container = character->getInventoryContainer();
        if (!container)
                return sc->NIL;

        /* enumerate its contents into a scheme list */
        return kern_build_container_list(sc, container, container->first(NULL));
}

KERN_API_CALL(kern_char_get_hp)
{
        class Character *character;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, "kern-char-get-hp");
        if (!character)
                return sc->NIL;

        return scm_mk_integer(sc, character->getHp());
}

KERN_API_CALL(kern_char_get_strength)
{
        class Character *character;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, "kern-char-get-strength");
        if (!character)
                return sc->NIL;

        return scm_mk_integer(sc, character->getStrength());
}

KERN_API_CALL(kern_obj_get_ap)
{
        class Object *object;

        /* unpack the object */
        object = (class Object*)unpack_obj(sc, &args, "kern-obj-get-ap");
        if (!object)
                return sc->NIL;

        return scm_mk_integer(sc, object->getActionPoints());
}

KERN_API_CALL(kern_obj_get_count)
{
        class Object *object;

        /* unpack the object */
        object = (class Object*)unpack_obj(sc, &args, "kern-obj-get-count");
        if (!object)
                return sc->NIL;

        return scm_mk_integer(sc, object->getCount());
}

KERN_API_CALL(kern_arms_type_get_range)
{
        class ArmsType *type;

        /* unpack the type (should be an arms type, but no way to safely
         * tell) */
        if (unpack(sc, &args, "p", &type)) {
                rt_err("kern-char-arms-type");
                return sc->NIL;
        }

        /* get the range */
        return scm_mk_integer(sc, type->getRange());
}

KERN_API_CALL(kern_arms_type_get_ammo_type)
{
        class ArmsType *type, *ammo;

        /* unpack the type (should be an arms type, but no way to safely
         * tell) */
        if (unpack(sc, &args, "p", &type)) {
                rt_err("kern-char-arms-type");
                return sc->NIL;
        }

        /* get the ammo type */
        ammo = type->getAmmoType();

        /* return it, if any */
        return ammo ? scm_mk_ptr(sc, ammo) : sc->NIL;
}

KERN_API_CALL(kern_obj_move)
{
        class Object *object;
        int dx, dy;
        enum MoveResult result;

        object = (Object*)unpack_obj(sc, &args, "kern-obj-move");
        if (!object)
                return sc->F;

        if (unpack(sc, &args, "dd", &dx, &dy)) {
                rt_err("kern-obj-move: bad args");
                return sc->F;
        }
        
        result = object->move(dx, dy);

        switch (result) {
        case MovedOk:
        case ExitedMap:
        case SwitchedOccupants:
                return sc->T;
                break;
        default:
                return sc->F;
        }
}

KERN_API_CALL(kern_get_ticks)
{
        return scm_mk_integer(sc, SDL_GetTicks());
}

static int kern_obj_is_type(class Object *obj, struct kern_append_info *info)
{
        return (obj->getObjectType() == (class ObjectType*)info->data);
}

static void kern_append_loc(Object *obj, void *data)
{
        pointer cell;
        struct kern_append_info *info;

        info = (struct kern_append_info *)data;

        /* If there is a filter then use it */
        if (info->filter != NULL)

                /* If the filter rejects the object then don't append it */
                if (! info->filter(obj, info))
                        return;

        cell = scm_mk_loc(info->sc, obj->getPlace(), obj->getX(), obj->getY());
        cell = _cons(info->sc, cell, info->sc->NIL, 0);

        if (info->head == info->sc->NIL) {
                info->head = cell;
                info->tail = cell;
                scm_protect(info->sc, cell);
        } else {
                info->tail->_object._cons._cdr = cell;
                info->tail = cell;
        }
}

KERN_API_CALL(kern_search_rect)
{
        struct place *place;
        int ulc_x, ulc_y, w, h, lrc_x, lrc_y, x, y;
        struct terrain *ter;
        class ObjectType *objtype;
        struct kern_append_info info;

        /* unpack the args */
        if (unpack(sc, &args, "pddddpp", &place, &ulc_x, &ulc_y, &w, &h, 
                   &ter, &objtype)) {
                rt_err("kern-search-rect: bad args");
                return sc->NIL;
        }

        /* check the place */
        if (! place) {
                rt_err("kern-search-rect: null place");
                return sc->NIL;
        }

        /* clip the rectangle */
        lrc_x = ulc_x + w;
        lrc_y = ulc_y + h;
        place_clip_to_map(place, &ulc_x, &ulc_y);
        place_clip_to_map(place, &lrc_x, &lrc_y);

        /* prepare to search */
        info.sc = sc;
        info.head = sc->NIL;
        info.tail = sc->NIL;
        info.filter = kern_obj_is_type;
        info.data = objtype;

        /* iterate over the tiles */
        for (y = ulc_y; y < lrc_y; y++) {
                for (x = ulc_x; x < lrc_x; x++) {

                        /* check if terrain matches */
                        if (place_get_terrain(place, x, y) == ter) {
                                
                                pointer cell = scm_mk_loc(info.sc, 
                                                          place, x, y);
                                cell = _cons(info.sc, cell, info.sc->NIL, 0);
                                
                                if (info.head == info.sc->NIL) {
                                        info.head = cell;
                                        info.tail = cell;
                                        scm_protect(sc, cell);
                                } else {
                                        info.tail->_object._cons._cdr = cell;
                                        info.tail = cell;
                                }
                                

                        } else {

                                /* check for an object match */
                                place_for_each_object_at(place, x, y, 
                                                         kern_append_loc, 
                                                         &info);
                        }
                }
        }

        /* unprotect the list prior to returning */
        if (info.head != sc->NIL)
                scm_unprotect(sc, info.head);

        return info.head;
}

KERN_API_CALL(kern_search_rect_for_obj_type)
{
        struct place *place;
        int ulc_x, ulc_y, w, h, lrc_x, lrc_y, x, y;
        class ObjectType *objtype;
        struct kern_append_info info;

        /* unpack the args */
        if (unpack(sc, &args, "pddddp", &place, &ulc_x, &ulc_y, &w, &h, 
                   &objtype)) {
                rt_err("kern-search-rect-for-obj-type: bad args");
                return sc->NIL;
        }

        /* check the place */
        if (! place) {
                rt_err("kern-search-rect-for-obj-type: null place");
                return sc->NIL;
        }

        /* clip the rectangle */
        lrc_x = ulc_x + w;
        lrc_y = ulc_y + h;
        place_clip_to_map(place, &ulc_x, &ulc_y);
        place_clip_to_map(place, &lrc_x, &lrc_y);

        /* prepare to search */
        info.sc = sc;
        info.head = sc->NIL;
        info.tail = sc->NIL;
        info.filter = kern_obj_is_type;
        info.data = objtype;

        /* iterate over the tiles */
        for (y = ulc_y; y < lrc_y; y++) {
                for (x = ulc_x; x < lrc_x; x++) {

                        /* check for an object match */
                        place_for_each_object_at(place, x, y, 
                                                 kern_append_loc, 
                                                 &info);
                }
        }

        /* unprotect the list prior to returning */
        if (info.head != sc->NIL)
                scm_unprotect(sc, info.head);

        return info.head;
}

KERN_API_CALL(kern_search_rect_for_terrain)
{
        struct place *place;
        int ulc_x, ulc_y, w, h, lrc_x, lrc_y, x, y;
        struct terrain *ter;
        pointer cell;
        struct kern_append_info info;

        /* unpack the args */
        if (unpack(sc, &args, "pddddp", &place, &ulc_x, &ulc_y, &w, &h, 
                   &ter)) {
                rt_err("kern-search-rect-for-terrain: bad args");
                return sc->NIL;
        }

        /* check the place */
        if (! place) {
                rt_err("kern-search-rect-for-terrain: null place");
                return sc->NIL;
        }

        /* clip the rectangle */
        lrc_x = ulc_x + w;
        lrc_y = ulc_y + h;
        place_clip_to_map(place, &ulc_x, &ulc_y);
        place_clip_to_map(place, &lrc_x, &lrc_y);

        /* prepare to search */
        info.sc = sc;
        info.head = sc->NIL;
        info.tail = sc->NIL;
        info.filter = NULL;

        /* iterate over the tiles */
        for (y = ulc_y; y < lrc_y; y++) {
                for (x = ulc_x; x < lrc_x; x++) {

                        /* check if terrain matches */
                        if (place_get_terrain(place, x, y) != ter)
                                continue;
                                
                        /* make a scheme-style loc */
                        cell = scm_mk_loc(info.sc, place, x, y);

                        /* make it a list element */
                        cell = _cons(info.sc, cell, info.sc->NIL, 0);
                        
                        /* append it to the list */
                        if (info.head == info.sc->NIL) {
                                info.head = cell;
                                info.tail = cell;
                        } else {
                                info.tail->_object._cons._cdr = cell;
                                info.tail = cell;
                        }
                }
        }

        /* unprotect the list prior to returning */
        if (info.head != sc->NIL)
                scm_unprotect(sc, info.head);

        /* return the list of locations */
        return info.head;
}


KERN_API_CALL(kern_fold_rect)
{
        struct place *place;
        int ulc_x, ulc_y, w, h, lrc_x, lrc_y, x, y;
        struct terrain *ter;
        pointer proc;
        pointer val;

        /* unpack the args */
        if (unpack(sc, &args, "pdddd", &place, &ulc_x, &ulc_y, &w, &h)) { 
                rt_err("kern-fold-rect: bad args");
                return sc->NIL;
        }

        /* check the place */
        if (! place) {
                rt_err("kern-fold-rect: null place");
                return sc->NIL;
        }

        /* get a ptr to the procedure */
        if (! scm_is_pair(sc, args)) {
                rt_err("kern-fold-rect: no proc arg");
                return sc->NIL;
        }
        proc = scm_car(sc, args);
        args = scm_cdr(sc, args);

        /* get a ptr to the initial value */
        if (! scm_is_pair(sc, args)) {
                rt_err("kern-fold-rect: no proc arg");
                return sc->NIL;
        }
        val = scm_car(sc, args);
        args = scm_cdr(sc, args);

        /* clip the rectangle */
        lrc_x = ulc_x + w;
        lrc_y = ulc_y + h;
        place_clip_to_map(place, &ulc_x, &ulc_y);
        place_clip_to_map(place, &lrc_x, &lrc_y);

        /* iterate over the tiles */
        for (y = ulc_y; y < lrc_y; y++) {
                for (x = ulc_x; x < lrc_x; x++) {

                        /* val may be unreferenced by the script, so protect it
                         * while we allocate cells (I don't think it matters if
                         * val is immutable, but we could always test that) */
                        scm_protect(sc, val);

                        /* make the location for the closure callback */
                        pointer loc = scm_mk_loc(sc, place, x, y);

                        /* NOTE: don't need to protect the loc, the args to
                         * _cons are always protected within it */

                        /* make the arg list (val, loc) */
                        pointer pargs = _cons(sc, val, 
                                              _cons(sc, loc, sc->NIL, 0), 0);

                        /* done with allocations, so val does not need
                         * protectiong any more */
                        scm_unprotect(sc, val);

                        /* call the procedure, storing the return val for
                         * later */
                        val = scheme_call(sc, proc, pargs);
                }
        }

        return val;
}

KERN_API_CALL(kern_player_get_gold)
{
        return scm_mk_integer(sc, player_party->gold);
}

KERN_API_CALL(kern_player_set_gold)
{
        int val;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-player-set-gold: bad args");
                return sc->F;
        }

        player_party->gold = val;
        foogodRepaint();
        return sc->T;
}

#if 0
KERN_API_CALL(kern_los_invalidate)
{
        struct place *place;
        int x;
        int y;
        int w;
        int h;

        if (unpack_loc(sc, &args, &place, &x, &y, "kern-los-invalidate")) {
                return sc->NIL;
        }

        if (unpack(sc, &args, "dd", &w, &h)) {
                rt_err("kern-los-invalidate: bad args");
                return sc->NIL;
        }
        
        vmask_invalidate(place, x, y, w, h);
        return sc->NIL;
}
#endif

scheme *kern_init(void)
{        
        scheme *sc;

        sc = (scheme*)malloc(sizeof(*sc));
        if (!sc) {
                warn("error: could not allocate interpreter");
                return 0;
        }

        if(!scheme_init(sc)) {
                warn("load error: could not initialize script interpreter\n");
                free(sc);
                return 0;
        }

        /* Setup the script-to-kernel API */

        /* kern-mk api */
        API_DECL(sc, "kern-mk-arms-type", kern_mk_arms_type);
        API_DECL(sc, "kern-mk-astral-body", kern_mk_astral_body);
        API_DECL(sc, "kern-mk-char", kern_mk_char);
        API_DECL(sc, "kern-mk-container", kern_mk_container);
        API_DECL(sc, "kern-mk-effect", kern_mk_effect);
        API_DECL(sc, "kern-mk-field", kern_mk_field);
        API_DECL(sc, "kern-mk-field-type", kern_mk_field_type);
        API_DECL(sc, "kern-mk-map", kern_mk_map);
        API_DECL(sc, "kern-mk-mmode", kern_mk_mmode);
        API_DECL(sc, "kern-mk-obj", kern_mk_obj);
        API_DECL(sc, "kern-mk-obj-type", kern_mk_obj_type);
        API_DECL(sc, "kern-mk-occ", kern_mk_occ);
        API_DECL(sc, "kern-mk-palette", kern_mk_palette);
        API_DECL(sc, "kern-mk-party", kern_mk_party);
        API_DECL(sc, "kern-mk-party-type", kern_mk_party_type);
        API_DECL(sc, "kern-mk-place", kern_mk_place);
        API_DECL(sc, "kern-mk-player", kern_mk_player);
        API_DECL(sc, "kern-mk-ptable", kern_mk_ptable);
        API_DECL(sc, "kern-mk-sched", kern_mk_sched);
        API_DECL(sc, "kern-mk-sound", kern_mk_sound);
        API_DECL(sc, "kern-mk-species", kern_mk_species);
        API_DECL(sc, "kern-mk-sprite", kern_mk_sprite);
        API_DECL(sc, "kern-mk-sprite-set", kern_mk_sprite_set);
        API_DECL(sc, "kern-mk-stock-char", kern_mk_stock_char);
        API_DECL(sc, "kern-mk-terrain", kern_mk_terrain);
        API_DECL(sc, "kern-mk-vehicle", kern_mk_vehicle);
        API_DECL(sc, "kern-mk-vehicle-type", kern_mk_vehicle_type);

        /* kern-arms-type api */
        API_DECL(sc, "kern-arms-type-get-ammo-type", kern_arms_type_get_ammo_type);
        API_DECL(sc, "kern-arms-type-get-range",     kern_arms_type_get_range);

        /* kern-set api */
        API_DECL(sc, "kern-set-crosshair", kern_set_crosshair);
        API_DECL(sc, "kern-set-cursor", kern_set_cursor);
        API_DECL(sc, "kern-set-frame", kern_set_frame);
        API_DECL(sc, "kern-set-ascii", kern_set_ascii);
        API_DECL(sc, "kern-set-clock", kern_set_clock);

        /* kern-terrain api */
        API_DECL(sc, "kern-terrain-get-pclass", kern_terrain_get_pclass);

        /* kern-obj api */
        API_DECL(sc, "kern-obj-add-food", kern_obj_add_food);
        API_DECL(sc, "kern-obj-add-effect", kern_obj_add_effect);
        API_DECL(sc, "kern-obj-add-to-inventory", kern_obj_add_to_inventory);
        API_DECL(sc, "kern-obj-apply-damage", kern_obj_apply_damage);
        API_DECL(sc, "kern-obj-clone", kern_obj_clone);
        API_DECL(sc, "kern-obj-dec-ap", kern_obj_dec_ap);
        API_DECL(sc, "kern-obj-dec-light", kern_obj_dec_light);
        API_DECL(sc, "kern-obj-destroy", kern_obj_destroy);
        API_DECL(sc, "kern-obj-find-path", kern_obj_find_path);
        API_DECL(sc, "kern-obj-get-activity", kern_obj_get_activity);
        API_DECL(sc, "kern-obj-get-ap", kern_obj_get_ap);
        API_DECL(sc, "kern-obj-get-count", kern_obj_get_count);
        API_DECL(sc, "kern-obj-get-effects", kern_obj_get_effects);
        API_DECL(sc, "kern-obj-get-gob", kern_obj_get_gob);
        API_DECL(sc, "kern-obj-get-light", kern_obj_get_light);
        API_DECL(sc, "kern-obj-get-location", kern_obj_get_location);
        API_DECL(sc, "kern-obj-get-mmode", kern_obj_get_mmode);
        API_DECL(sc, "kern-obj-get-name", kern_obj_get_name);
        API_DECL(sc, "kern-obj-get-sprite", kern_obj_get_sprite);
        API_DECL(sc, "kern-obj-get-type", kern_obj_get_type);
        API_DECL(sc, "kern-obj-get-vision-radius", kern_obj_get_vision_radius);
        API_DECL(sc, "kern-obj-has?", kern_obj_has);
        API_DECL(sc, "kern-obj-heal", kern_obj_heal);
        API_DECL(sc, "kern-obj-inc-light", kern_obj_inc_light);
        API_DECL(sc, "kern-obj-is-char?", kern_obj_is_char);
        API_DECL(sc, "kern-obj-is-visible?", kern_obj_is_visible);
        API_DECL(sc, "kern-obj-move", kern_obj_move);
        API_DECL(sc, "kern-obj-put-at", kern_obj_put_at);
        API_DECL(sc, "kern-obj-put-into", kern_obj_put_into);
        API_DECL(sc, "kern-obj-relocate", kern_obj_relocate);
        API_DECL(sc, "kern-obj-remove", kern_obj_remove);
        API_DECL(sc, "kern-obj-remove-effect", kern_obj_remove_effect);
        API_DECL(sc, "kern-obj-remove-from-inventory", kern_obj_remove_from_inventory);
        API_DECL(sc, "kern-obj-set-ap", kern_obj_set_ap);
        API_DECL(sc, "kern-obj-set-gob", kern_obj_set_gob);
        API_DECL(sc, "kern-obj-set-light", kern_obj_set_light);
        API_DECL(sc, "kern-obj-set-opacity", kern_obj_set_opacity);
        API_DECL(sc, "kern-obj-set-pclass", kern_obj_set_pclass);
        API_DECL(sc, "kern-obj-set-sprite", kern_obj_set_sprite);
        API_DECL(sc, "kern-obj-set-temporary", kern_obj_set_temporary);
        API_DECL(sc, "kern-obj-set-visible", kern_obj_set_visible);
        API_DECL(sc, "kern-obj-wander", kern_obj_wander);

        /* kern-char api */
        API_DECL(sc, "kern-char-add-defense", kern_char_add_defense);
        API_DECL(sc, "kern-char-attack", kern_char_attack);
        API_DECL(sc, "kern-char-dec-mana", kern_char_dec_mana);
        API_DECL(sc, "kern-char-charm", kern_char_charm);
        API_DECL(sc, "kern-char-get-hp", kern_char_get_hp);
        API_DECL(sc, "kern-char-get-inventory", kern_char_get_inventory);
        API_DECL(sc, "kern-char-get-mana", kern_char_get_mana);
        API_DECL(sc, "kern-char-get-party", kern_char_get_party);
        API_DECL(sc, "kern-char-get-species", kern_char_get_species);
        API_DECL(sc, "kern-char-get-strength", kern_char_get_strength);
        API_DECL(sc, "kern-char-get-weapons", kern_char_get_weapons);
        API_DECL(sc, "kern-char-kill", kern_char_kill);
        API_DECL(sc, "kern-char-resurrect", kern_char_resurrect);
        API_DECL(sc, "kern-char-set-sleep", kern_char_set_sleep);
        API_DECL(sc, "kern-char-set-fleeing", kern_char_set_fleeing);
        API_DECL(sc, "kern-char-is-asleep?", kern_char_is_asleep);
        API_DECL(sc, "kern-char-is-hostile?", kern_char_is_hostile);
        API_DECL(sc, "kern-char-uncharm", kern_char_uncharm);

        /* kern-astral-body api */
        API_DECL(sc, "kern-astral-body-get-gob", kern_astral_body_get_gob);
        API_DECL(sc, "kern-astral-body-get-phase", kern_astral_body_get_phase);
        API_DECL(sc, "kern-astral-body-set-gob", kern_astral_body_set_gob);

        /* kern-place api */
        API_DECL(sc, "kern-place-get-beings", kern_place_get_beings);
        API_DECL(sc, "kern-place-get-height", kern_place_get_height);
        API_DECL(sc, "kern-place-get-name", kern_place_get_name);
        API_DECL(sc, "kern-place-get-neighbor", kern_place_get_neighbor);
        API_DECL(sc, "kern-place-get-objects", kern_place_get_objects);
        API_DECL(sc, "kern-place-get-terrain", kern_place_get_terrain);
        API_DECL(sc, "kern-place-get-width", kern_place_get_width);
        API_DECL(sc, "kern-place-is-passable", kern_place_is_passable);
        API_DECL(sc, "kern-place-is-hazardous", kern_place_is_hazardous);
        API_DECL(sc, "kern-place-is-wrapping?", kern_place_is_wrapping);
        API_DECL(sc, "kern-place-is-wilderness?", kern_place_is_wilderness);
        API_DECL(sc, "kern-place-map", kern_place_map);
        API_DECL(sc, "kern-place-set-terrain", kern_place_set_terrain);
        API_DECL(sc, "kern-place-synch", kern_place_synch);

        /* kern-type api */
        API_DECL(sc, "kern-type-get-gifc", kern_type_get_gifc);

        /* map api */
        API_DECL(sc, "kern-map-rotate", kern_map_rotate);

        /* player api */
        API_DECL(sc, "kern-player-get-gold", kern_player_get_gold);
        API_DECL(sc, "kern-player-set-follow-mode", 
                 kern_player_set_follow_mode);
        API_DECL(sc, "kern-player-set-gold", kern_player_set_gold);

        /* misc api */
        API_DECL(sc, "kern-add-magic-negated", kern_add_magic_negated);
        API_DECL(sc, "kern-add-quicken", kern_add_quicken);
        API_DECL(sc, "kern-add-reveal", kern_add_reveal);
        API_DECL(sc, "kern-add-spell", kern_add_spell);
        API_DECL(sc, "kern-add-tick-job", kern_add_tick_job);
        API_DECL(sc, "kern-add-time-stop", kern_add_time_stop);
        API_DECL(sc, "kern-add-xray-vision", kern_add_xray_vision);
        API_DECL(sc, "kern-blit-map", kern_blit_map);
        API_DECL(sc, "kern-dice-roll", kern_dice_roll);
        API_DECL(sc, "kern-fire-missile", kern_fire_missile);
        API_DECL(sc, "kern-fold-rect", kern_fold_rect);
        API_DECL(sc, "kern-get-distance", kern_get_distance);
        API_DECL(sc, "kern-get-objects-at", kern_get_objects_at);
        API_DECL(sc, "kern-get-ticks", kern_get_ticks);
        API_DECL(sc, "kern-in-los?", kern_in_los);
        /*API_DECL(sc, "kern-los-invalidate", kern_los_invalidate);*/
        API_DECL(sc, "kern-include", kern_include);
        API_DECL(sc, "kern-interp-error", kern_interp_error);
        API_DECL(sc, "kern-is-valid-location?", kern_is_valid_location);
        API_DECL(sc, "kern-print", kern_print);
        API_DECL(sc, "kern-search-rect", kern_search_rect);
        API_DECL(sc, "kern-search-rect-for-terrain", 
                 kern_search_rect_for_terrain);
        API_DECL(sc, "kern-search-rect-for-obj-type", 
                 kern_search_rect_for_obj_type);
        API_DECL(sc, "kern-set-spell-words", kern_set_spell_words);
        API_DECL(sc, "kern-set-start-proc", kern_set_start_proc);
        API_DECL(sc, "kern-set-wind", kern_set_wind);
        API_DECL(sc, "kern-sleep", kern_sleep);
        API_DECL(sc, "kern-sound-play", kern_sound_play);
        API_DECL(sc, "kern-tag", kern_tag);
        API_DECL(sc, "kern-test-recursion", kern_test_recursion);
        
        /* ui api */
        API_DECL(sc, "kern-ui-direction", kern_ui_direction);
        API_DECL(sc, "kern-ui-select-party-member", 
                        kern_ui_select_party_member);
        API_DECL(sc, "kern-ui-target", kern_ui_target);
        API_DECL(sc, "kern-ui-waitkey", kern_ui_waitkey);
        API_DECL(sc, "kern-ui-page-text", kern_ui_page_text);
        API_DECL(sc, "kern-ui-select-from-list", kern_ui_select_from_list);

        /* conv api */
        API_DECL(sc, "kern-conv-end", kern_conv_end);
        API_DECL(sc, "kern-conv-say", kern_conv_say);
        API_DECL(sc, "kern-conv-get-yes-no?", kern_conv_get_yes_no);
        API_DECL(sc, "kern-conv-trade", kern_conv_trade);
        API_DECL(sc, "kern-conv-get-reply", kern_conv_get_reply);

        /* kern-map api */
        API_DECL(sc, "kern-map-center-camera", kern_map_center_camera);
        API_DECL(sc, "kern-map-flash", kern_map_flash);
        API_DECL(sc, "kern-map-repaint", kern_map_repaint);
        API_DECL(sc, "kern-map-set-dirty", kern_map_set_dirty);
        API_DECL(sc, "kern-map-set-jitter", kern_map_set_jitter);
        API_DECL(sc, "kern-map-set-peering", kern_map_set_peering);
        API_DECL(sc, "kern-map-view-create", kern_map_view_create);
        API_DECL(sc, "kern-map-view-destroy", kern_map_view_destroy);
        API_DECL(sc, "kern-map-view-center", kern_map_view_center);
        API_DECL(sc, "kern-map-view-add", kern_map_view_add);
        API_DECL(sc, "kern-map-view-rm", kern_map_view_rm);

        /* kern-log api */
        API_DECL(sc, "kern-log-msg", kern_log_msg);
        API_DECL(sc, "kern-log-enable", kern_log_enable);

        /* kern-dtable api */
        API_DECL(sc, "kern-mk-dtable", kern_mk_dtable);
        API_DECL(sc, "kern-dtable-change", kern_dtable_change);
        API_DECL(sc, "kern-dtable-get", kern_dtable_get);
        API_DECL(sc, "kern-dtable-pop", kern_dtable_pop);
        API_DECL(sc, "kern-dtable-push", kern_dtable_push);
        API_DECL(sc, "kern-dtable-set", kern_dtable_set);

        /* kern-party-api */
        API_DECL(sc, "kern-party-add-member", kern_party_add_member);
        
        /* kern-being-api */
        API_DECL(sc, "kern-being-get-base-faction", 
                 kern_being_get_base_faction);
        API_DECL(sc, "kern-being-get-current-faction", 
                 kern_being_get_current_faction);
        API_DECL(sc, "kern-being-get-visible-hostiles", 
                 kern_being_get_visible_hostiles);
        API_DECL(sc, "kern-being-set-base-faction", 
                 kern_being_set_base_faction);


        /* Revisit: probably want to provide some kind of custom port here. */
        scheme_set_output_port_file(sc, stderr);

        /* Implemented but untested:
           API_DECL(sc, "kern-ui-handle-events", kern_ui_handle_events);
        */

        return sc;
}
