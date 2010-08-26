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

#include "applet.h"
#include "blender.h"
#include "character.h"
#include "cmd.h"
#include "conv.h"
#include "config.h"
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
#include "cfg.h"
#include "kern_intvar.h"  // SAM
#include "menus.h"
#include "file.h"
#include "skill.h"
#include "skill_set.h"
#include "skill_set_entry.h"
#include "templ.h"
#include "macros.h"
#include "ztats.h"
#include "ztats_pane.h"
#include "../config.h" /* for USE_QUESTS */
// kern.c *doesnt* include kern.h?? wtf??

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

#define API_DECL(sc, sym, val) \
        scm_define(sc, sym, scm_mk_ptr(sc, val))

#define KERN_API_CALL(name) static pointer name(scheme *sc, pointer args)
#define KERN_OBSOLETE_CALL(name)                 \
static pointer name(scheme *sc, pointer args) {  \
        warn("warn: '" #name "' is obsolete\n"); \
        return sc->NIL;                          \
}

#define KERN_ALLOC(type) (type *)calloc(1, sizeof(type))
#define KERN_FREE(ptr) (free(ptr))

#define TAG_UNK "<tag?>"


struct kjob {
        void *data;
        closure_t *clx;
};

/* Struct used by callbacks which build scheme lists */
struct kern_append_info {
        scheme *sc;
        pointer head;
        pointer tail;
        int (*filter)(Object *, struct kern_append_info *);
        void *data;
};

struct kern_ui_target_info {
        struct place *place;
        int x, y, range;
        struct list suggest;
};

/* Redefine the session query macro to turn its arg into a string, then #include
 * the list of querys directly into an array of string pointers. */
#undef SESSION_DECL_QUERY
#define SESSION_DECL_QUERY(id) #id
static const char * query_to_id[] = {
#       include "session_queries.h"
};

/*****************************************************************************
 *
 * kjob - wrapper for work queue jobs
 *
 *****************************************************************************/
static struct kjob * kjob_new(void *data, closure_t *clx)
{
        struct kjob *kjob;
        kjob = (struct kjob *)malloc(sizeof(*kjob));
        assert(kjob);
        kjob->data = data;
        kjob->clx = clx;
        closure_ref(clx);
        return kjob;
}

static void kjob_del(struct kjob *kjob)
{
        closure_unref(kjob->clx);
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

static void image_dtor(void *val)
{
        images_del((struct images*)val);
}

static void sprite_dtor(void *val)
{
        sprite_del((struct sprite*)val);
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
        terrain_map_unref((struct terrain_map*)val);
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
        occ_unref((struct occ*)val);
}

static void arms_type_dtor(void *val)
{
        delete (class ArmsType *)val;
}

static void missile_type_dtor(void *val)
{
        delete (class MissileType *)val;
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

static void blender_dtor(void *val)
{
        blender_t *blender=(blender_t*)val;
        list_remove(&blender->list);
        free(blender);
}

static int unpack(scheme *sc, pointer *cell, const char *fmt, ...)
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
                case 'c': /* closure (actually, a symbol, possibly for a
                           * closure; this is misleading) */
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
                case 'f': /* float */
                        rval = va_arg(args, float*);
                        if (! scm_is_num(sc, car)) {
                                errs++;
                                load_err("arg %d not a number", count);
                        } else if (! scm_is_real(sc, car)) {
                                /* coerce it */
                                *rval = scm_int_val(sc, car);
                        } else {
                                *rval = scm_real_val(sc, car);
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
                case 'l': /* plain old cell, (eg a gob) */
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

static Object *unpack_obj(scheme *sc, pointer *args, const char *caller)
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
                      int *y, const char *func)
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

static int unpack_rect(scheme *sc, pointer *args, SDL_Rect *rect)
{
        pointer prect = scm_car(sc, *args);
        *args = scm_cdr(sc, *args);
        long x, y, w, h;

        /* Can't use the rect fields directly because they're only Uint16 */
        if (unpack(sc, &prect, "dddd",  &x, &y, &w, &h)) {
                load_err("%s: error unpacking rect elements", __FUNCTION__);
                return -1;
        }

        rect->x = x;
        rect->y = y;
        rect->w = w;
        rect->h = h;
        
        return 0;
}

pointer vpack(scheme *sc, const char *fmt, va_list ap)
{
        pointer head=sc->NIL;
        pointer tail=sc->NIL;
        pointer cell=sc->NIL;
        pointer arg=sc->NIL;
        void *ptr;
        int ival;
        char *strval;

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
                    if (!arg) {
                        arg = sc->NIL;
                    }
                    break;
                default:
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

struct mview * kern_unpack_mview(scheme *sc, pointer *args, const char *caller)
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
                           int *x, int *y, const char *caller)
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

static pointer pack(scheme *sc, const char *fmt, ...)
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
        int width, height, rows, cols, offx, offy;
        char *fname;
        struct images *image = NULL;
        const char *tag = TAG_UNK;
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
        int n_frames, index, facings, wave;
        struct sprite *sprite;
        const char *tag = TAG_UNK;
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

static pointer kern_mk_terrain(scheme *sc, pointer args)
{
        int alpha, light;
        void *sprite;
        struct terrain *terrain;
        const char *tag = TAG_UNK, *name;
        pointer ret;
        int pclass;
        pointer proc = NULL;

        /* Revisit: ignore effects for now */

        if (unpack(sc, &args, "ysdpddc", &tag, &name, &pclass, &sprite, 
                   &alpha, &light, &proc)) {
                load_err("kern-mk-terrain %s: bad args", tag);
                return sc->NIL;
        }

        terrain = terrain_new(tag, name, (struct sprite*)sprite, pclass, 
                              alpha, light);

        if (proc != sc->NIL) {
                terrain->effect = closure_new_ref(sc, proc);
        }
        terrain->renderCombat = NULL;

        list_add(&Session->terrains, &terrain->session_list);
        ret = scm_mk_ptr(sc, terrain);
        scm_define(sc, tag, ret);

        return ret;
}

static pointer kern_mk_sound(scheme *sc, pointer args)
{
        sound_t *sound;
        const char *tag = TAG_UNK, *name;
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
         const char *tag = TAG_UNK;
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
        const char *tag = TAG_UNK;
        struct terrain_map *map;
        pointer ret;
        struct list *elem;

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

        /* run all registered terrain blenders on the new map */
        list_for_each(&Session->blenders, elem) {
                blender_t *blender=outcast(elem, blender_t, list);
                terrain_map_blend(map, blender->inf, blender->n_nonsup,
                                  blender->nonsup, blender->range);
        }
        

        map->handle = session_add(Session, map, terrain_map_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, map);

        /* Embedded maps (those defined within and used exclusively for) place
         * constructors may not have and do not need tags. */
        if (tag)
                scm_define(sc, tag, ret);

        return ret;

 abort:
        terrain_map_unref(map);
        return sc->NIL;
}



static pointer kern_mk_composite_map(scheme *sc, pointer args)
{
        int width, height, x = 0, y = 0, i = 0;
        const char *tag = TAG_UNK;
        struct terrain_map *map=NULL, *submap=NULL;
        pointer ret;        
        struct list *elem;

        /* parse supermap tag and dimensions */
        if (unpack(sc, &args, "ydd", &tag, &width, &height)) {
                load_err("kern-mk-composite-map %s: bad args", tag);
                return sc->NIL;
        }

        /* unpack the first submap */
        if (unpack(sc, &args, "p", &submap)) {
                load_err("kern-mk-composite-map %s: first submap invalid", tag);
                goto abort;
        }
        
        /* create the supermap, inferring the submap dimensions and palette
         * from the first submap */
        map = terrain_map_new(tag, width * submap->w, height * submap->h, 
                              submap->palette);

        /* set the supermap info */
        map->submap_w = submap->w;
        map->submap_h = submap->h;
        map->composite = 1;
        
        /* blit the first submap onto the supermap in the upper left-hand
         * corner */
        terrain_map_blit(map, 0, 0, submap, 0, 0, submap->w, submap->h);

        /* for each remaining submap in the list... */
        for (y = 0; y < height; y++) {

                for (x = 0; x < width; x++) {

                        /* except the first one... */
                        if (x == 0 && y == 0)
                                continue;

                        /* unpack it */
                        if (unpack(sc, &args, "p", &submap)) {
                                load_err("kern-mk-composite-map %s: submap "\
                                         "%d invalid", tag, i);
                                goto abort;
                        }
                        
                        /* check its palette and dimensions */
                        if (map->palette != submap->palette) {
                                load_err("kern-mk-composite-map %s: submap %d "\
                                         "palette doesn't match first submap "\
                                         "palette", tag, i);
                                goto abort;
                        }
                        if (map->submap_w != submap->w) {
                                load_err("kern-mk-composite-map %s: submap %d "\
                                         "width doesn't match first submap "\
                                         "width", tag, i);
                                goto abort;
                        }
                        if (map->submap_h != submap->h) {
                                load_err("kern-mk-composite-map %s: submap %d "\
                                         "height doesn't match first submap "\
                                         "height", tag, i);
                                goto abort;
                        }
                        
                        /* blit the submap onto the supermap */
                        terrain_map_blit(map, x * map->submap_w, y * map->submap_h, 
                                         submap, 0, 0, 
                                         map->submap_w, map->submap_h);
                }
        }

        /* run all registered terrain blenders on the new map */
        list_for_each(&Session->blenders, elem) {
                blender_t *blender=outcast(elem, blender_t, list);
                terrain_map_blend(map, blender->inf, blender->n_nonsup,
                                  blender->nonsup, blender->range);
        }

        /* add it to the session */
        map->handle = session_add(Session, map, terrain_map_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, map);

        /* define its tag (if specified) */
        if (tag)
                scm_define(sc, tag, ret);

        return ret;

 abort:
        terrain_map_unref(map);
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
                int dir, opdir;
                struct place *neighbor, *tmp;
                pointer cell;

                cell = scm_car(sc, neighbors);
                neighbors = scm_cdr(sc, neighbors);
                
                if (unpack(sc, &cell, "pd", &neighbor, &dir)) {
                        load_err("kern-mk-place %s: error in neighbor list", 
                                 place->tag);
                        return -1;
                }

                if (! IS_LEGAL_DIRECTION(dir)) {
                        load_err("kern-mk-place %s: invalid direction for "\
                                 "neighbor: %d\n", place->tag, dir);
                        return -1;
                }
                
                opdir = directionToOpposite(dir);

                /* check for existing neighbors */
                if ((tmp = place_get_neighbor(place, dir))) {
                        load_err("kern-mk-place %s: already has %s as a "\
                                 "neighbor in direction %d\n",
                                 place->tag, tmp->tag, dir);
                        return -1;
                }

                if ((tmp = place_get_neighbor(neighbor, opdir))) {
                        load_err("kern-mk-place %s: already has %s as a "\
                                 "neighbor in direction %d\n",
                                 neighbor->tag, 
                                 tmp->tag, opdir);
                        return -1;
                }

                /* finally, hook them up */
                place_set_neighbor(place, dir, neighbor);
        }

        return 0;
}

KERN_API_CALL(kern_place_set_neighbor)
{
        int dir;
        struct place *place, *neighbor;

        if (unpack(sc, &args, "dpp", &dir, &place, &neighbor)) {
                rt_err("kern-place-set-neighbor: bad args");
                return sc->F;
        }

        // neighbor == null is allowed (it unlinks current neighbor)
        if (! place) {
                rt_err("kern-place-set-neighbor: null place");
                return sc->F;                
        }
        
        if (! IS_LEGAL_DIRECTION(dir)) {
                rt_err("kern-place-set-neighbor: bad direction %d", dir);
                return sc->F;                
        }

        /* link (works both ways) */
	     place_set_neighbor(place, dir, neighbor);
        
        return sc->T;
}

KERN_API_CALL(kern_place_apply_tile_effects)
{
        struct place *place;
        class Object *obj;

        if (unpack(sc, &args, "p", &place)) {
                rt_err("kern-place-apply-tile-effects: bad place arg");
                return sc->NIL;
        }

        obj = unpack_obj(sc, &args, "kern-obj-relocate");
        if (!obj)
                return sc->NIL;

        place_apply_tile_effects(place, obj);
        return sc->NIL;
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

                obj->relocate(place, x, y, REL_NOTRIG);
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

        while (scm_is_pair(sc, contents)) {

                if (unpack(sc, &contents, "c", &pre_entry_proc)) {
                        load_err("kern-mk-place %s: bad arg in hook list",
                                 place->tag);
                        return -1;
                }

                place_add_on_entry_hook(place,
                                        closure_new_ref(sc, pre_entry_proc));
        }

        return 0;
}

KERN_API_CALL(kern_place_add_on_entry_hook)
{
        struct place *place; 
        pointer proc;

       if (unpack(sc, &args, "pc", &place, &proc)) {
                rt_err("kern-place-add-on-entry-hook: bad args");
                return sc->NIL;
        }

        if (! place) {
                rt_err("kern-place-add-on-entry-hook: null place");
                return sc->NIL;
        }

        place_add_on_entry_hook(place,
                                closure_new_ref(sc, proc));
        return sc->NIL;
}

KERN_API_CALL(kern_mk_place)
{
        int wild, wraps, underground, combat;
        struct terrain_map *map;
        struct place *place;
        struct sprite *sprite;
        const char *tag = TAG_UNK, *name;
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

        place->handle = session_add(Session, place, place_dtor, place_save, place_start);
        ret = scm_mk_ptr(sc, place);
        scm_define(sc, tag, ret);
        return ret;

 abort:
        place_del(place);
        return sc->NIL;
}

static pointer kern_mk_species(scheme *sc, pointer args)
{
        struct species *species;
        int str, intl, dex, spd, vr, hpmod, hpmult, argno = 1;
        int mpmod, mpmult, visible, n_slots, n_spells, i, xpval;
        int stationary=0;
        struct sprite *sleep_sprite;
        class ArmsType *weapon;
        const char *tag = TAG_UNK, *name, *armor_dice;
        sound_t *damage_sound, *walking_sound;
        pointer slots;
        pointer spells;
        pointer ret;
        struct mmode *mmode;
        

        if (unpack(sc, &args, "ysdddddpddddppbppdbs", &tag, &name, &str, 
                   &intl, &dex, &spd, &vr, &mmode, &hpmod, &hpmult, &mpmod, 
                   &mpmult, &sleep_sprite, &weapon, 
                   &visible, &damage_sound, &walking_sound,
                   &xpval, &stationary, &armor_dice)) {
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
        species->xpval = xpval;
        species->stationary = stationary;

        /* Optional armor dice */
        if (armor_dice) {
                species->armor_dice = strdup(armor_dice);
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
        const char *tag = TAG_UNK, *name;
        sound_t *fire_sound;
        int slots, hands, range, weight;
        char *hit, *defend, *damage, *armor;
        int rap, AP_mod, thrown, ubiq;
        struct sprite *sprite;
        class MissileType *missile;
        class ObjectType *ammo;
        pointer gifc;
        pointer ret;
        int gifc_cap;
		int str_attack_mod;
		int dex_attack_mod;
		int char_damage_mod;
		float char_avoid_mod;
		  struct mmode *mmode;
		

        if (unpack(sc, &args, "yspssssdddddppbbdpdodddrp",
        					&tag, 
        					&name, 
        					&sprite, 
                   	&hit, &damage, &armor, &defend,
		   &slots, &hands, &range, &rap, &AP_mod, 
                   	&missile, &ammo,
                   	&thrown, &ubiq,
                   	&weight, 
                   	&fire_sound,
                   	&gifc_cap,
                   	&gifc,
				   		&str_attack_mod, &dex_attack_mod, &char_damage_mod,
				   		&char_avoid_mod,
				   		&mmode)) {
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
                            weight, damage, armor, rap, AP_mod, thrown, ubiq,
                            fire_sound, missile, ammo, str_attack_mod, dex_attack_mod,
							char_damage_mod, char_avoid_mod, false);
													
			arms->setMovementMode(mmode);
			
        if (gifc != sc->NIL) {
                /* arms->get_handler = closure_new(sc, get_handler); */
                arms->setGifc(closure_new(sc, gifc), gifc_cap);
        }

        session_add(Session, arms, arms_type_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, arms);
        scm_define(sc, tag, ret);
        return ret;
}

static pointer kern_mk_missile_type(scheme *sc, pointer args)
{
	class MissileType *arms;
	const char *tag = TAG_UNK, *name;
	struct sprite *sprite;
	pointer gifc;
	pointer ret;
	int gifc_cap;
	struct mmode *mmode;
	int beam;
	int fixedrange;
	
	
	if (unpack(sc, &args, "yspdopbb",
			&tag, 
			&name,
			&sprite,
			&gifc_cap,
			&gifc,
			&mmode,
			&beam,
			&fixedrange))
	{
		load_err("kern-mk-projectile-type %s: bad args", tag);
		return sc->NIL;
	}
	
	arms = new MissileType(tag, name, sprite, beam, fixedrange, mmode);
	
	if (gifc != sc->NIL)
	{
		/* arms->get_handler = closure_new(sc, get_handler); */
		arms->setGifc(closure_new(sc, gifc), gifc_cap);
	}
	
	session_add(Session, arms, missile_type_dtor, NULL, NULL);
	ret = scm_mk_ptr(sc, arms);
	scm_define(sc, tag, ret);
	
	return ret;
}

static pointer kern_mk_field_type(scheme *sc, pointer args)
{
        class FieldType *field;
        const char *tag = TAG_UNK, *name;
        struct sprite *sprite;
        int light, duration, pclass;
        closure_t *clx = NULL;
        pointer func = sc->NIL;
        pointer ret;
        struct mmode *mmode = NULL;      

        if (unpack(sc, &args, "yspdddcp", &tag, &name, &sprite, &light, 
                   &duration, &pclass, &func, &mmode)) {
                load_err("kern-mk-field-type %s: bad args", tag);
                return sc->NIL;
        }
        
        if (func != sc->NIL) {
                clx = closure_new(sc, func);
        }

        field = new FieldType(tag, name, sprite, light, duration, pclass, clx);
        field->setMovementMode(mmode);
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
        const char *tag = TAG_UNK, *name;
        char *pluralName=NULL;
        enum layer layer;
        struct sprite *sprite;
        pointer ret;
        pointer gifc;
        int gifc_cap;
        struct mmode *mmode;

        /* unpack the tag */
        if (unpack(sc, &args, "y", &tag)) {
                load_err("kern-mk-obj-type %s: bad args (did you mean to use "\
                         "kern-mk-obj instead?)", tag);
                return sc->NIL;
        }

        /* probe the name to see if it is a list, if so then use the car as the
         * name and the cadr as the pluralName */
        if (scm_is_pair(sc, scm_car(sc, args))) {
                pointer list = scm_car(sc, args);
                args=scm_cdr(sc, args);
                if (unpack(sc, &list, "ss", &name, &pluralName)) {
                        load_err("kern-mk-obj-type %s: bad name arg", tag);
                        return sc->NIL;
                }
        } else if (unpack(sc, &args, "s", &name)) {
                load_err("kern-mk-obj-type %s: bad name arg", tag);
                return sc->NIL;
        }

        /* continue unpacking the rest of the args */
        if (unpack(sc, &args, "pddop", &sprite, &layer, &gifc_cap, &gifc, &mmode)) {
                load_err("kern-mk-obj-type %s: bad args (did you mean to use "\
                         "kern-mk-obj instead?)", tag);
                return sc->NIL;
        }

        type = new ObjectType(tag, name, sprite, layer);
        assert(type);

        if (gifc != sc->NIL) {
                type->setGifc(closure_new(sc, gifc), gifc_cap);
        }

        if (pluralName) {
                type->setPluralName(pluralName);
        }
        
        type->setMovementMode(mmode);
        session_add(Session, type, obj_type_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, type);
        scm_define(sc, tag, ret);
        return ret;
}

static pointer kern_mk_occ(scheme *sc, pointer args)
{
        struct occ *occ;
        int hpmod, hpmult;
        int mpmod, mpmult, hit, def, dam, arm, xpval;
        const char *tag = TAG_UNK, *name;
        struct skill_set *skset;
        float magic;
        pointer ret;

        /* Basic args */
        if (unpack(sc, &args, "ysrdddddddddp",
                   &tag, &name, &magic, &hpmod, &hpmult, &mpmod, &mpmult, &hit,
                   &def, &dam, &arm, &xpval, &skset)) {
                load_err("kern-mk-occ %s: bad args", tag);
                return sc->NIL;
        }

        occ = occ_new(tag, name, magic, hpmod, hpmult, mpmod, mpmult, hit, def,
                      dam, arm);
        occ_ref(occ);
        occ->xpval = xpval;
        occ_set_skills(occ, skset);

        session_add(Session, occ, occ_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, occ);
        scm_define(sc, tag, ret);
        return ret;
}

static int kern_load_hooks(scheme *sc, pointer hook_tbl, Object *obj)
{
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
                        return -1;
                }

                /* Note: even if gobcell is sc->NIL we want to wrap it. I once
                 * tried to use a NULL gob instead but if we pass that back
                 * into scheme as an arg and the gc tries to mark it we'll
                 * crash. */
                gob = gob_new(sc, gobcell);
                gob->flags |= GOB_SAVECAR;
                
                obj->restoreEffect(effect, gob, flags, clk);
        }

        return 0;
}

static void kern_load_conv(scheme *sc, pointer sym, Object *obj)
{
        struct conv *conv;
        struct closure *proc;

        if (sym == sc->NIL) {
            return;
        }

        if (! (proc = closure_new_ref(sc, sym))) {
                load_err("%s: closure_new failed", __FUNCTION__);
                return;
        }

        if (!(conv = conv_new(proc))) {
                load_err("%s: conv_new failed", __FUNCTION__);
                goto done2;
        }

        obj->setConversation(conv);
        conv_unref(conv);
 done2:
        closure_unref(proc);
}

static pointer kern_mk_char(scheme *sc, pointer args)
{
        class Character *character;
        int str, intl, dex, hpmod, hpmult;
        int mpmod, mpmult, hp, xp, mp, AP_per_round, lvl, dead;
        const char *tag = TAG_UNK, *name;
        struct species *species;
        struct occ *occ;
        struct sprite *sprite;
        pointer conv;
        pointer readied;
        pointer ret;
        pointer ai;
        pointer hook_tbl;
        int base_faction;
        struct sched *sched;
        class Container *inventory;

        if (unpack(sc, &args, "yspppdddddddddddddbcpcp",
                   &tag, &name, &species, &occ, 
                   &sprite, &base_faction, &str,
                   &intl, &dex, &hpmod, &hpmult, &mpmod, &mpmult, 
                   &hp, &xp, &mp, &AP_per_round, &lvl, &dead,
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
                                        hp, xp, mp, AP_per_round, lvl);
        assert(character);
        character->setBaseFaction(base_faction);
        character->setSchedule(sched);
        character->setInventoryContainer(inventory);
        character->setDead(dead);

        kern_load_conv(sc, conv, character);

        if (ai != sc->NIL) {
                character->setAI(closure_new(sc, ai));
        }

        /* Load the list of arms. */
        while (scm_is_pair(sc, readied)) {
                class ArmsType *arms;
                if (unpack(sc, &readied, "p", &arms)) {
                        load_err("kern-mk-char %s: error in arms list", tag);
                        goto abort;
                }
                /*character->add(arms, 1);*/
                character->ready(arms);
        }

        /* Load the hooks. */
        hook_tbl = scm_car(sc, args);
        args = scm_cdr(sc, args);
        if (kern_load_hooks(sc, hook_tbl, character)) {
                load_err("kern-mk-char %s: bad hook entry", tag);
                goto abort;
        }


        ret = scm_mk_ptr(sc, character);
        
        /* If the character is tagged then it's not "anonymous", and we'll
         * assign it to a scheme variable named after the tag. */
        if (tag) {
                scm_define(sc, tag, ret);

                /* Tagged objects may be referred to in the script by their
                 * tag. If the object is destroyed, the scheme variable that
                 * refers to the object is still valid (in Scheme, it isn't
                 * really possible to undefine variables). To prevent crashes
                 * on dereferencing this variable we'll bump the refcount. To
                 * ensure the object is destroyed on session teardown, we'll
                 * mark it for custom finalization, which will decrement the
                 * extra refcount. */
                obj_inc_ref(character);
                scm_set_cust_fin(sc, ret);
        }

        return ret;

 abort:
        delete character;
        return sc->NIL;
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

        if (kern_load_hooks(sc, scm_car(sc, args), obj)) {
                load_err("kern-mk-obj: error in hook list");
                goto abort;
        }
        args = scm_cdr(sc, args);

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
        
 abort:
        delete obj;
        return sc->NIL;
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

        obj = new Party();
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

        obj->relocate(place, x, y, REL_NOSTEP);
        return sc->NIL;
}

static pointer kern_obj_relocate(scheme *sc, pointer args)
{
        class Object *obj;
        struct place *place;
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
                clx = closure_new_ref(sc, cutscene);
        }

        obj->relocate(place, x, y, REL_NOSTEP, clx);

        closure_unref_safe(clx);

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

        if ((place = obj->getPlace())) {
                x = obj->getX();
                y = obj->getY();
                return pack(sc, "pdd", place, x, y);
        }

        return sc->NIL;
}

static pointer kern_obj_get_dir(scheme *sc, pointer args)
{
        class Object *obj;
        int dx, dy;

        if (!(obj = unpack_obj(sc, &args, "kern-obj-get-dir"))) {
                assert(false);
                return sc->NIL;
        }

        dx = obj->getDx();
        dy = obj->getDy();

        return pack(sc, "dd", dx, dy);
}

static pointer kern_place_get_location(scheme *sc, pointer args)
{
        struct place *place;

        if (unpack(sc, &args, "p", &place)) {
                rt_err("kern-place-get-location: bad args");
                return sc->NIL;
        }

        if (! place) {
                rt_err("kern-place-get-location: null place");
                return sc->NIL;
        }

        if (! place->location.place)
                return sc->NIL;

        return pack(sc, "pdd", 
                    place->location.place, 
                    place->location.x, 
                    place->location.y);
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
                return sc->F;
        }

        if (!obj) {
                rt_err("kern-obj-put: null obj");
                return sc->F;
        }

        if (!container) {
                rt_err("kern-obj-put: null container");
                return sc->F;
        }

        return container->add(obj->getObjectType(), 
                              obj->getCount()) ? sc->T : sc->F;

}

/*
 * kern_obj_remove - remove an object from the map. Note that this implicitly
 * destroys most objects automatically, unless the object has another reference
 * count. Use kern_obj_inc_ref to prevent destruction during this call.
 */
static pointer kern_obj_remove(scheme *sc, pointer args)
{
        class Object *obj;

        if (!(obj=unpack_obj(sc, &args, "kern-obj-remove"))) {
                return sc->NIL;
        }

        /* Bugfix: don't use place_remove_object() because it doesn't call
           setOnMap(false). */
        //place_remove_object(obj->getPlace(), obj);
        obj->remove();

        return sc->NIL;
}

#if 0
/*
 * kern_obj_destroy - obsolete explicit destructor. Try to use kern_obj_dec_ref
 * instead, wait and see if we really need this.
 */
static pointer kern_obj_destroy(scheme *sc, pointer args)
{
        class Object *obj;

        if (!(obj=unpack_obj(sc, &args, "kern-obj-destroy"))) {
                return sc->NIL;
        }

        delete obj;

        return sc->NIL;
}
#endif

static pointer kern_obj_inc_ref(scheme *sc, pointer args)
{
        class Object *obj;

        if (!(obj=unpack_obj(sc, &args, "kern-obj-inc-ref"))) {
                return sc->NIL;
        }

        obj_inc_ref(obj);

        return scm_mk_ptr(sc, obj);
}

static pointer kern_obj_dec_ref(scheme *sc, pointer args)
{
        class Object *obj;
        int refcount;

        if (!(obj=unpack_obj(sc, &args, "kern-obj-dec-ref"))) {
                return sc->NIL;
        }

        refcount = obj->refcount;
        obj_dec_ref(obj);
        
        if (refcount > 1)
                /* object was not destroyed - return it */
                return scm_mk_ptr(sc, obj);

        /* object was destroyed - return NIL */
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

static pointer kern_obj_add_gold(scheme *sc, pointer args)
{
        class Object *obj;
        int quantity;

        if (unpack(sc, &args, "pd", &obj, &quantity)) {
                load_err("kern-obj-add-gold: bad args");
                return sc->NIL;
        }

        if (!obj) {
                rt_err("kern-obj-add-gold: null obj");
                return sc->NIL;
        }

        if (! obj->addGold(quantity)) {
                /* NPC's can't add gold (not even if they have containers)
                 * because gold is not an object! Gold coins are, but that's
                 * not what gets passed in here, is it? */
                rt_err("kern-obj-add-gold: '%s' does not use gold",
                       obj->getName());
        }

        return sc->NIL;
}

static pointer kern_mk_inventory(scheme *sc, pointer args)
{
        class Container *container;
        pointer contents;

        container = new Container();

        /* contents */
        contents = scm_car(sc, args);
        args = scm_cdr(sc, args);
        while (contents != sc->NIL) {

                int num;
                class ObjectType *type;
                pointer entry;

                entry = scm_car(sc, contents);
                contents = scm_cdr(sc, contents);

                if (! scm_is_pair(sc, entry)) {
                        load_err("kern-mk-inventory: error in inv list "\
                                 "(not a pair)");
                        goto abort;
                }

                if (unpack(sc, &entry, "dp", &num, &type)) {
                        load_err("kern-mk-inventory: error in inv list");
                        goto abort;
                }

                container->add(type, num);
        }

        /* hooks */
        if (kern_load_hooks(sc, scm_car(sc, args), container)) {
                load_err("kern-mk-inventory: error in hook list");
                goto abort;
        }
        args = scm_cdr(sc, args);

        return scm_mk_ptr(sc, container);
        
 abort:
        delete container;
        return sc->NIL;
}

KERN_API_CALL(kern_mk_player)
{
        int food, gold, ttnm;
        char *mv_desc, *tag;
        sound_t *mv_sound;
        struct sprite *sprite;
        struct terrain_map *campsite;
        struct formation *form, *camp_form;
        class Container *inventory;
        Vehicle *vehicle;
        pointer members;
        pointer ret;

        if (unpack(sc, &args, "ypspdddppppp", 
                   &tag,
                   &sprite,
                   &mv_desc, &mv_sound,
                   &food, &gold, &ttnm,
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

        player_party = new class PlayerParty(tag, sprite, 
                                              mv_desc, 
                                              mv_sound,
                                              food, gold, form, 
                                              campsite, 
                                              camp_form);
        player_party->setInventoryContainer(inventory);
        player_party->setTurnsToNextMeal(ttnm);

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

                if (! player_party->addMember(ch)) {
                        load_err("kern-mk-player: failed to add %s to player "
                                 "party", ch->getName());
                        goto abort;
                }
        }

        /* Board the vehicle */
        if (vehicle) {

                /* This sets the vehicle's occupant, too. */
                player_party->setVehicle(vehicle);

                /* bugfix: party unrefs vehicle when it disembarks; needs to
                 * keep a refcount while boarded */
                obj_inc_ref(vehicle);
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
        sched->sc = sc;
        
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

                if (unpack(sc, &rect, "cdddd", &appt->place_sym, 
                           &appt->x, &appt->y, 
                           &appt->w, &appt->h)) {
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
        Session->num_kern_includes++;
        return sc->NIL;
}


static pointer kern_set_crosshair(scheme *sc, pointer args)
{
        if (unpack(sc, &args, "p", &Session->crosshair_type)) {
                load_err("kern-set-crosshair: bad args");
        }
        return sc->NIL;
}


static pointer kern_set_damage_sprite(scheme *sc, pointer args)
{
        if (unpack(sc, &args, "p", &Session->damage_sprite)) {
                load_err("kern-set-damage-sprite: bad args");
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
                rt_err("kern-obj-inflict-damage: bad args");
                return sc->NIL;
        }

        if (!obj) {
                rt_err("kern-obj-inflict-damage: null object");
                return sc->NIL;
        }

        obj->damage(amount);

        return sc->NIL;
}

static pointer kern_obj_inflict_damage(scheme *sc, pointer args)
{
        class Object *obj;
        char *desc;
        int amount;
		class Character *attacker;

        if (unpack(sc, &args, "psdp", &obj, &desc, &amount, &attacker)) {
                rt_err("kern-obj-apply-damage: bad args");
                return sc->NIL;
        }

        if (!obj) {
                rt_err("kern-obj-apply-damage: null object");
                return sc->NIL;
        }

        obj->inflictDamage(amount,attacker);

        return sc->NIL;
}

static pointer kern_obj_add_effect(scheme *sc, pointer args)
{
        class Object *obj;
        struct effect *effect = NULL;
        pointer gobcell;
        struct gob *gob = NULL;

        if (unpack(sc, &args, "ppl", &obj, &effect, &gobcell)) {
                rt_err("kern-obj-add-effect: bad args");
                return sc->F;
        }

        if (!obj) {
                rt_err("kern-obj-add-effect: null object");
                return sc->F;
        }

        if (! is_effect(effect)) {
                rt_err("kern-obj-remove-effect: wrong type for effect!");
                return sc->F;
        }

        /* Note: even if gobcell is sc->NIL we want to wrap it. I once tried to
         * use a NULL gob instead but if we pass that back into scheme as an
         * arg and the gc tries to mark it we'll crash. */
        gob = gob_new(sc, gobcell);
        gob->flags |= GOB_SAVECAR;

        return obj->addEffect(effect, gob) ? sc->T : sc->F;
}

static pointer kern_obj_remove_effect(scheme *sc, pointer args)
{
        class Object *obj;
        struct effect *effect;

        if (unpack(sc, &args, "pp", &obj, &effect)) {
                load_err("kern-obj-remove-effect: bad args");
                return sc->F;
        }

        if (!obj) {
                rt_err("kern-obj-remove-effect: null object");
                return sc->F;
        }

        if (! is_effect(effect)) {
                rt_err("kern-obj-remove-effect: wrong type for effect!");
                return sc->F;
        }

        /* Just remove one per call */
        return obj->removeEffect(effect) ? sc->T : sc->F;
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
                        rt_err("kern-log-msg: bad args");
                }
        }

        log_end(NULL);

        return sc->NIL;
}

KERN_API_CALL(kern_log_begin)
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
                        rt_err("kern-log-begin: bad args");
                }
        }

        return sc->NIL;
}

KERN_API_CALL(kern_log_end)
{
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
                        rt_err("kern-log-end: bad args");
                }
        }

        log_end(NULL);

        return sc->NIL;
}

static pointer kern_log_continue(scheme *sc,  pointer args)
{
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
                        rt_err("kern-log-continue: bad args");
                }
        }

        return sc->NIL;
}

KERN_API_CALL(kern_log_flush)
{
        log_flush();
        return sc->T;
}

static pointer kern_stdout_msg(scheme *sc,  pointer args)
{

        while (scm_is_pair(sc, args)) {

                pointer val = scm_car(sc, args);
                args = scm_cdr(sc, args);

                if (scm_is_str(sc, val)) {
                        fprintf(stdout,"%s",scm_str_val(sc, val));
                } else if (scm_is_int(sc, val)) {
                        fprintf(stdout,"%ld",scm_int_val(sc, val));
                } else if (scm_is_real(sc, val)) {
                        fprintf(stdout,"%f",scm_real_val(sc, val));
                } else {
                        rt_err("kern-print: bad args");
                }
        }
		fprintf(stdout,"\n");
		
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
        class Character *speaker;
        struct conv *conv;

        if (unpack(sc, &args, "p", &speaker)) {
                rt_err("kern-conv-say: bad args");
                return sc->NIL;
        }

        if (speaker == NULL) {
                rt_err("kern-conv-say: null speaker");
                return sc->NIL;
        }

        if (!(conv = speaker->getConversation())) {
                rt_err("%s() no conv for %s", __FUNCTION__, speaker->getName());
                return sc->NIL;
        }
        
        if (speaker->isKnown()) {
                log_begin("^c+%c%s:^c- ", CONV_NPC_COLOR, speaker->getName());
        } else {
                log_begin("^c+%c", CONV_NPC_COLOR);
                speaker->describe();
                log_continue(":^c- ");
        }

        args = scm_car(sc, args);

        while (scm_is_pair(sc, args)) {

                pointer val = scm_car(sc, args);
                args = scm_cdr(sc, args);
                if (scm_is_str(sc, val)) {
                        char *beg, *end, *text = scm_str_val(sc, val);
                        while (text) {
                                if (! conv_get_word(text, &beg, &end)) {
                                        log_continue(text);
                                        text = NULL;
                                } else {
                                        int keyword = conv_is_keyword(conv, beg);
                                        if (text<beg) {
                                                do {
                                                        log_continue("%c", *text);
                                                        text++;
                                                } while (text<beg);
                                        }
                                        if (keyword) {
                                                char color = (keyword & CONV_IS_MARKED) ? 'G' : 'm';
                                                log_continue("^c+%c", color);
                                        }
                                        while (beg<end) {
                                                log_continue("%c", *beg);
                                                beg++;
                                        }
                                        if (keyword) {
                                                log_continue("^c-");
                                        }
                                        text = end;
                                }
                        }
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

static pointer kern_conv_get_yes_no(scheme *sc,  pointer args)
{
        Object *pc = unpack_obj(sc, &args, "kern-conv-get-yes-no?");
        if (NULL == pc)
                return sc->F;
        return ui_get_yes_no(pc->getName()) ? sc->T : sc->F;
}

static pointer kern_conv_get_amount(scheme *sc,  pointer args)
{
        cmdwin_clear();
        cmdwin_spush("How much");
        return scm_mk_integer(sc, ui_get_quantity(-1));
}

static pointer kern_conv_get_reply(scheme *sc,  pointer args)
{
        char buf[32];

        Object *pc = unpack_obj(sc, &args, "kern-conv-get-reply");
        if (NULL == pc)
                return sc->F;

        ui_getline(buf, sizeof(buf));
        log_msg("^c+%c%s:^c- %s", CONV_PC_COLOR, pc->getName(), buf);

        /* Return only the first four characters, to be consistent with the
         * usual keyword/reply scheme. */
        buf[4] = 0;

        return scm_mk_symbol(sc, buf);
}

static pointer kern_conv_get_string(scheme *sc,  pointer args)
{
        char buf[32];

        Object *pc = unpack_obj(sc, &args, "kern-conv-get-string");
        if (NULL == pc)
                return sc->F;

        ui_getline(buf, sizeof(buf));
        log_msg("%s: %s", pc->getName(), buf);

        return scm_mk_string(sc, buf);
}

static pointer kern_conv_trade(scheme *sc, pointer args)
{
        Object *npc;
        Object *pc;
        struct merchant merch;
        int i, traded = 0;
        char *menu = 0;
        pointer catalog = sc->NIL;

        if (unpack(sc, &args, "pps", &npc, &pc, &menu)) {
                rt_err("kern-conv-trade: bad args");
                return sc->NIL;
        }

        if (! npc || ! pc) {
                rt_err("kern-conv-trade: null kernel object(s)");
                return sc->NIL;
        }

        /* Get the catalog */
        if (! scm_is_pair(sc, args)) {
                rt_err("kern-conv-trade: no catalog!");
                return sc->NIL;
        }
        catalog = scm_car(sc, args);
        args = scm_cdr(sc, args);
        if (! scm_is_pair(sc, catalog)) {
                rt_err("kern-conv-trade: catalog is not a list");
                return sc->NIL;
        }
        
        /* setup the merchant struct */
        merch.name = npc->getName();
        merch.n_trades = scm_len(sc, catalog);
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
                pointer p = scm_car(sc, catalog);
                struct trade_info *trade = &merch.trades[i];
                catalog = scm_cdr(sc, catalog);

                if (unpack(sc, &p, "pds", &type, &trade->cost, &trade->sales_pitch)) {
                        rt_err("kern-conv-trade: bad args in trade list %d", i);
                        goto abort;
                }

                if (! type) {
                        rt_err("kern-conv-trade: null object type in trade list %d", i);
                        goto abort;
                }

                /* This is kind of dumb. We should just point to the ObjectType
                 * and be done with it. */
                trade->sprite = type->getSprite();
                trade->name = type->getName();
                trade->data = type;
                trade->quantity = player_party->inventory->numAvail(type);
                trade->show_sprite = 1;
                trade->show_quantity = 1;
        }

        if (! strcmp(menu, "buy")) {
                traded = ui_buy(&merch);
        } else if (! strcmp(menu, "sell")) {
                traded = ui_sell(&merch);
        } else {
                traded = ui_trade(&merch);
        }

 abort:
        free(merch.trades);
        return traded ? sc->T : sc->F;
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

        obj->setActionPoints(ap);

        return sc->NIL;
}

static pointer kern_obj_set_facing(scheme *sc, pointer args)
{
        class Object *obj;
        int facing;

        if (unpack(sc, &args, "pd", &obj, &facing)) {
                rt_err("kern-obj-set-facing: bad args");
                return sc->NIL;
        }

        if (!obj) {
                rt_err("kern-obj-set-facing: null object");
                return sc->NIL;
        }

        obj->setFacing(facing);

        return sc->NIL;
}

static pointer kern_obj_get_facing(scheme *sc, pointer args)
{
        class Object *obj;

        if (unpack(sc, &args, "p", &obj)) {
                rt_err("kern-obj-get-facing: bad args");
                return sc->NIL;
        }

        if (!obj) {
                rt_err("kern-obj-get-facing: null object");
                return sc->NIL;
        }

        return scm_mk_integer(sc, obj->getFacing());
}

static pointer kern_obj_set_conv(scheme *sc, pointer args)
{
        class Object *obj;
        pointer conv;

        if (unpack(sc, &args, "pc", &obj, &conv)) {
                rt_err("kern-obj-set-conv: bad args");
                return sc->NIL;
        }

        if (conv == sc->NIL) {
                obj->setConversation(NULL);
        } else {
                kern_load_conv(sc, conv, obj);
        }

        return scm_mk_ptr(sc, obj);
}

static pointer kern_char_set_known(scheme *sc, pointer args)
{
        class Character *ch;
        int val;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-set-known");
        if (!ch)
                return sc->NIL;

        if (unpack(sc, &args, "b", &val)) {
                rt_err("kern-char-set-known: bad args");
                return sc->NIL;
        }

        ch->setKnown(val);
        return scm_mk_ptr(sc, ch);
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

        return scm_mk_ptr(sc, obj);
}

static pointer kern_obj_set_submerged(scheme *sc, pointer args)
{
        class Object *obj;
        int val;

        if (unpack(sc, &args, "pb", &obj, &val)) {
                rt_err("kern-obj-set-submerged: bad args");
                return sc->NIL;
        }

        if (!obj) {
                rt_err("kern-obj-set-submerged: null object");
                return sc->NIL;
        }

        obj->setSubmerged(val);

        return scm_mk_ptr(sc, obj);
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

KERN_API_CALL(kern_place_is_visible)
{
	struct place *place;
	int x, y;
	
	if (unpack_loc(sc, &args, &place, &x, &y, "kern_place_is_visible"))
		return sc->NIL;
	
	if (! place) {
		rt_err("kern_place_is_visible: null place");
		return sc->NIL;
	}
	
	// the player party doesnt wind up being in a temporary combat map,
	// but by its existance we can infer the player is there
	if (!place_is_wilderness_combat(place) && place != player_party->getPlace())
	{
		return sc->F;	
	}
	
	if (mapTileIsVisible(x,y) && (mapTileLightLevel(x,y) >= MIN_XAMINE_LIGHT_LEVEL))
	{
		return sc->T;	
	}
	else
	{
		return sc->F;	
	}
}

KERN_API_CALL(kern_place_is_combat_map)
{
	struct place *place;
	
  if (unpack(sc, &args, "p", &place)) {
       rt_err("kern_place_is_combat_map: bad args");
       return sc->NIL;
   }
   
	if (! place) {
		rt_err("kern_place_is_combat_map: null place");
		return sc->NIL;
	}
	
	if (place_is_wilderness_combat(place))
	{
		return sc->T;	
	}
	else
	{
		return sc->F;	
	}
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

        vmask_flush_all();


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


static pointer kern_map_flash_sprite(scheme *sc, pointer args)
{
	int x, y;
	struct sprite *sprite;
	
	if (unpack(sc, &args, "pdd", &sprite, &x, &y)) {
		rt_err("kern_map_flash_sprite: bad args");
		return sc->NIL;
	}
	
	if (!sprite) {
		rt_err("kern_map_flash_sprite: null sprite");
		return sc->NIL;
	}
	
	if (mapTileLightLevel(x,y) < MIN_XAMINE_LIGHT_LEVEL || (!mapTileIsVisible(x,y)))
	{
		return sc->NIL;   
	}
	
	mapFlashSprite(x, y, sprite);
	
	return sc->NIL;
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

        /* Tagged objects may be referred to in the script by their
         * tag. If the object is destroyed, the scheme variable that
         * refers to the object is still valid (in Scheme, it isn't
         * really possible to undefine variables). To prevent crashes
         * on dereferencing this variable we'll bump the refcount. To
         * ensure the object is destroyed on session teardown, we'll
         * mark it for custom finalization, which will decrement the
         * extra refcount. */
        obj_inc_ref(obj);
        scm_set_cust_fin(sc, p);

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

        if (!obj) {
                rt_err("kern-obj-get-gob: null obj");
                return sc->NIL;
        }

        if (obj->getGob() == NULL) {
                return sc->NIL;
        }

        // It's already a scheme pointer so just return it directly
        return obj->getGob()->p;
}

static pointer kern_obj_set_gob(scheme *sc, pointer  args)
{
        Object *obj;

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

static pointer kern_obj_set_ttl(scheme *sc, pointer  args)
{
        Object *obj;
        int ttl;

        if (! (obj = unpack_obj(sc, &args, "kern-obj-set-ttl"))) {
                goto done;
        }

        if (unpack(sc, &args, "d", &ttl)) {
               rt_err("kern-obj-set-ttl: bad ttl");
               goto done;
        }

        Object::setTTL(obj, ttl);
 done:
        return scm_mk_ptr(sc, obj);
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
        closure_t *gifc;

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

        gifc = cptr->getGifc();

        return gifc ? gifc->code : sc->NIL;
}

static pointer kern_type_get_name(scheme *sc, pointer  args)
{
        ObjectType *cptr;

        if (unpack(sc, &args, "p", &cptr)) {
                rt_err("kern-type-get-name: bad args");
                return sc->NIL;
        }

        if (cptr == NULL) {
                /* This is not necessarily an error. Some objects (like
                 * characters) have no type, which can result in us getting
                 * here. */
                return sc->NIL;
        }

        return scm_mk_string(sc, cptr->getName());
}

static pointer kern_type_describe(scheme *sc, pointer  args)
{
        ObjectType *cptr;

        if (unpack(sc, &args, "p", &cptr)) {
                rt_err("kern-type-get-name: bad args");
                return sc->NIL;
        }

        if (cptr == NULL) {
                /* This is not necessarily an error. Some objects (like
                 * characters) have no type, which can result in us getting
                 * here. */
                return sc->NIL;
        }

        cptr->describeType(1);
        return sc->NIL;
}

static pointer kern_add_tick_job(scheme *sc, pointer args)
{
        int tick;
        pointer proc;
        void *data;

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
        cmdwin_pop();
        if (! member) {
                return sc->NIL;
        }
        return scm_mk_ptr(sc, member);
}

static pointer kern_conv_end(scheme *sc, pointer args)
{
        conv_end();
        return sc->T;
}


static pointer kern_conv_begin(scheme *sc, pointer args)
{
        class Character *npc, *member;
        struct conv *conv;

        if (unpack(sc, &args, "p", &npc)) {
                rt_err("kern-conv-begin: bad args");
                return sc->F;
        }

        conv = npc->getConversation();
        if (! conv) {
                rt_err("kern-conv-begin: npc has no conv!");
                return sc->F;                
        }

        member = player_party->get_leader();
        if (! member) {
                rt_err("kern-conv-begin: no player party leader!");
                return sc->F;                
        }

	log_begin("You are accosted by ");
        Session->subject = player_party;
	npc->describe();
        Session->subject = NULL;
	log_end(".");

        conv_enter(npc, member, conv);

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
                body->gifc = closure_new_ref(sc, proc);

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
        const char *tag = TAG_UNK;
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
		  pointer proc;        

        if (unpack(sc, &args, "yspppbbbspdddddpo",
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
                   &mmode,
                   &proc
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
        
		if (proc != sc->NIL) {
			type->renderCombat = closure_new(sc, proc);
			closure_ref(type->renderCombat); //TODO clean up this nasty leaky hack
		}
		else
		{
			type->renderCombat=NULL;	
		}
        
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

KERN_API_CALL(kern_obj_get_movecost)
{
	     class Object *obj;
        int val;

        if (unpack(sc, &args, "pd", &obj, &val)) {
                rt_err("kern_obj_get_movecost: bad args");
                return sc->NIL;
        }

        if (!obj) {
                rt_err("kern_obj_get_movecost: null object");
                return sc->NIL;
        }
	
        val = obj->getMovementCost(val);
        return scm_mk_integer(sc,val);
}

KERN_API_CALL(kern_obj_get_name)
{
        Object *obj = unpack_obj(sc, &args, "kern-obj-get-name");
        if (!obj) {
                return sc->NIL;
        }

        if (!obj->getName()) {
                return sc->NIL;
        }

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

//refactor refactor refactor!
KERN_API_CALL(kern_sound_play_at)
{
	sound_t *sound;
	struct place *place, *foc_place;
	int x, foc_x;
	int y, foc_y;
	if (unpack(sc, &args, "p", &sound))\
	{
		rt_err("kern-sound-play-at: bad args");
		return sc->NIL;
	}
	if (unpack_loc(sc, &args, &place, &x, &y, "kern-sound-play-at: bad loc"))
	{
		return sc->NIL;
	}
	int volume = SOUND_MAX_VOLUME;
	int distance;
	mapGetCameraFocus(&foc_place, &foc_x, &foc_y);
	if (foc_place == place)
	{
		distance = place_flying_distance(foc_place, foc_x, foc_y, x, y);
		if (distance > 1)
			volume = (volume * (20 - distance))/20;
		if (volume > 0)
		{
			sound_play(sound, volume, false);
		}
	}
	return sc->NIL;
}

KERN_API_CALL(kern_sound_play_ambient)
{
	sound_t *sound;
	struct place *place, *foc_place;
	int x, foc_x;
	int y, foc_y;
	if (unpack(sc, &args, "p", &sound))
	{
	       rt_err("kern-sound-play-ambient: bad args");
	       return sc->NIL;
	}
	if (unpack_loc(sc, &args, &place, &x, &y, "kern-sound-play-ambient: bad loc"))
	{
	       return sc->NIL;
	}
	int volume = SOUND_MAX_VOLUME;
	int distance;
	mapGetCameraFocus(&foc_place, &foc_x, &foc_y);
	if (foc_place == place)
	{
		distance = place_flying_distance(foc_place, foc_x, foc_y, x, y);
		if (distance > 1)
			volume = (volume * (20 - distance))/20;
		if (volume > 0)
		{
			sound_play(sound, volume, true);
		}
	}
	return sc->NIL;
}

KERN_API_CALL(kern_music_play)
{
	char *file;
	if (unpack(sc, &args, "s", &file))
	{
	       rt_err("kern-music-play: bad args");
	       return sc->NIL;
	}
	music_load_track(file);
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

KERN_API_CALL(kern_mk_skill)
{
        char *name, *desc;
        pointer yuse, can_yuse, list;
        struct skill *skill;
        int wilderness_ok, passive;

        /* Unpack name and desc */
        if (unpack(sc, &args, "ss", &name, &desc)) {
                load_err("kern-mk-skill: bad args");
                return sc->NIL;
        }

        skill = skill_new();
        skill_set_name(skill, name);
        skill_set_desc(skill, desc);

        /* Unpack ap, mp and yusage procs */
        if (unpack(sc, &args, "ddbbcc", 
                   &skill->ap, 
                   &skill->mp, 
                   &wilderness_ok,
                   &passive,
                   &yuse, &can_yuse)) {
                load_err("kern-mk-skill %s: bad args", name);
                goto abort;
        }

        /* I used an int for the unpack since I don't trust the cast to work
         * portably on structure bit fields */
        skill->wilderness_ok = wilderness_ok;
        skill->passive = passive;

        /* yuse is mandatory for non-passive skills */
        if (! skill->passive 
            && yuse == sc->NIL) {
                load_err("kern-mk-skill %s: active but nil yuse proc", name);
                goto abort;
        }

        if (yuse != sc->NIL) {
                skill->yuse = closure_new_ref(sc, yuse);
        }

        /* can_yuse is optional */
        if (can_yuse != sc->NIL) {
                skill->can_yuse = closure_new_ref(sc, can_yuse);
        }

        /* list of tools */
        list = scm_car(sc, args);
        args = scm_cdr(sc, args);
        while (scm_is_pair(sc, list)) {
                void *objtype;
                if (unpack(sc, &list, "p", &objtype)) {
                        load_err("kern-mk-skill %s: bad tool arg", name);
                        goto abort;
                }
                skill_add_tool(skill, objtype);
        }

        /* list of materials: (objtype, int) pairs */
        list = scm_car(sc, args);
        args = scm_cdr(sc, args);
        while (scm_is_pair(sc, list)) {
                void *objtype;
                int quan;
                pointer pair = scm_car(sc, list);
                list = scm_cdr(sc, list);
                if (unpack(sc, &pair, "pd", &objtype, &quan)) {
                        load_err("kern-mk-skill %s: bad material arg", name);
                        goto abort;
                }
                skill_add_material(skill, objtype, quan);
        }

        list_add(&Session->skills, &skill->list);
        return scm_mk_ptr(sc, skill);

 abort:
        skill_unref(skill);
        return sc->NIL;
}

KERN_API_CALL(kern_mk_skill_set)
{
        char *name;
        struct skill_set *skset;
        pointer list;

        if (unpack(sc, &args, "s", &name)) {
                load_err("kern-mk-skill-set: bad name");
                return sc->NIL;
        }

        skset = skill_set_new();
        skill_set_set_name(skset, name);

        list = scm_car(sc, args);
        args = scm_cdr(sc, args);
        while (scm_is_pair(sc, list)) {
                pointer pair;
                int lvl;
                struct skill *skill;

                pair = scm_car(sc, list);
                list = scm_cdr(sc, list);
                if (unpack(sc, &pair, "dp", &lvl, &skill)) {
                        load_err("kern-mk-skill-set %s: bad skill list args", 
                                 name);
                        goto abort;
                }

                if (!skill) {
                        load_err("kern-mk-skill-set %s: nil skill", name);
                        goto abort;
                }

                skill_set_add_skill(skset, skill, lvl);
        }

        list_add(&Session->skill_sets, &skset->list);
        return scm_mk_ptr(sc, skset);

 abort:
        skill_set_unref(skset);
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
        if (unpack(sc, &args, "ddddp", &spell->level, &spell->cost,
                   &spell->context, &spell->action_points, &spell->sprite)) {
                load_err("kern-add-spell: bad args");
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

KERN_API_CALL(kern_init_random)
{
	/* This should have some timing randomness,
		since human interaction is required before scripts run */
	
	srand(clock());
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

KERN_API_CALL(kern_char_force_drop)
{
        class Character *ch;
        int val;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-force-drop");
        if (!ch)
                return sc->NIL;

        if (unpack(sc, &args, "b", &val)) {
                rt_err("kern-char-force-drop: bad args");
                goto done;
        }

        ch->setForceContainerDrop(val);
 done:
        return scm_mk_ptr(sc, ch);
}

KERN_API_CALL(kern_char_unready)
{
        class Character *ch;
        class ArmsType *type;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-unready");
        if (!ch)
                return sc->F;

        if (unpack(sc, &args, "p", &type)) {
                rt_err("kern-char-unready: bad args");
                return sc->F;
        }

        return ch->unready(type) ? sc->T : sc->F;
}

KERN_API_CALL(kern_char_get_readied_weapons)
{
        class Character *ch;
        class ArmsType *weapon;
        pointer head = sc->NIL;
        pointer tail = sc->NIL;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-unready");
        if (!ch)
                return sc->F;

		int armsIndex = 0;
        for (weapon = ch->enumerateWeapons(&armsIndex); weapon != NULL; 
             weapon = ch->getNextWeapon(&armsIndex)) {
                    
                /* skip "natural" weapons that are not really readied */
                if (ch->species &&
                    weapon == ch->species->weapon)
                        continue;

                pointer cell = scm_mk_ptr(sc, weapon);
                cell = _cons(sc, cell, sc->NIL, 0);

                if (head == sc->NIL) {
                        head = cell;
                        tail = cell;
                } else {
                        tail->_object._cons._cdr = cell;
                        tail = cell;
                }
        }

        return head;
}

KERN_API_CALL(kern_char_set_hp)
{
        class Character *ch;
        int val;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-set-hp");
        if (!ch)
                return sc->NIL;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-char-set-hp: bad args");
        } else {
                ch->setHp(val);
        }

        return scm_mk_ptr(sc, ch);;
}

KERN_API_CALL(kern_char_set_mana)
{
        class Character *ch;
        int val;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-set-mana");
        if (!ch)
                return sc->NIL;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-char-set-mana: bad args");
        } else {
                ch->setMana(val);
        }

        return scm_mk_ptr(sc, ch);;
}

KERN_API_CALL(kern_char_set_schedule)
{
        class Character *ch;
        struct sched *val;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-set-schedule");
        if (!ch)
                return sc->NIL;

        if (unpack(sc, &args, "p", &val)) {
                rt_err("kern-char-set-schedule: bad args");
        } else {
                ch->setSchedule(val);
        }

        return scm_mk_ptr(sc, ch);
}

/* 
 * kern_char_join_player -- wrapper for Character::joinPlayer
 */
KERN_API_CALL(kern_char_join_player)
{
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-join-player");
        if (!ch)
                return sc->F;

        if (ch->joinPlayer())
                return sc->T;
        return sc->F;
}

KERN_API_CALL(kern_char_is_known)
{
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-is-known");
        if (!ch) {
                return sc->F;
        }

        return ch->isKnown() ? sc->T : sc->F;
}

KERN_API_CALL(kern_char_leave_player)
{
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-leave-player");
        if (!ch)
                return sc->F;

        if (NULL==ch->getPlace()
            || place_is_wilderness(ch->getPlace()))
                return sc->F;

        ch->leavePlayer();

        return sc->T;
}

/* 
 * kern_char_set_ai -- change the AI for a Character object
 */
KERN_API_CALL(kern_char_set_ai)
{
        class Character *ch;
        pointer ai;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-set-ai");
        if (!ch)
                return sc->F;

        if (unpack(sc, &args, "c", &ai)) {
                rt_err("kern-char-set-ai: bad args");
                return sc->F;
        }

        if (ai == sc->NIL) {
                ch->setAI(NULL);
        } else {
                ch->setAI(closure_new(sc, ai));
        }

        return sc->T;
}

KERN_API_CALL(kern_char_task_abort)
{
    class Character *ch;

    ch = (class Character*)unpack_obj(sc, &args, "kern-char-task-abort");
    if (ch) {
        ch->taskAbort();
    }
    return sc->NIL;
}

KERN_API_CALL(kern_char_task_begin)
{
    char *taskname = NULL;
    class Character *ch;
    pointer taskproc, taskgob;
    struct closure *closure = NULL;
    struct gob *gob = NULL;

    ch = (class Character*)unpack_obj(sc, &args, "kern-char-task-begin");
    if (!ch) {
        return sc->F;
    }

    if (unpack(sc, &args, "scl", &taskname, &taskproc, &taskgob)) {
        rt_err("%s: bad args", __FUNCTION__);
        return sc->F;
    }

    if (taskproc == sc->NIL) {
        rt_err("%s: nil task procedure not allowed", __FUNCTION__);
        return sc->F;
    }

    /* For now, disallow starting tasks in the wilderness. Maybe later. */
    if (place_is_wilderness(ch->getPlace())) {
        return sc->F;
    }

    if (!(closure = closure_new_ref(sc, taskproc))) {
        return sc->F;
    }

    /* gob is optional */
    if (taskgob != sc->NIL) {
        if (!(gob = gob_new(sc, taskgob))) {
            closure_unref(closure);
            return sc->F;
        }
    }

    ch->taskBegin(taskname, closure, gob);
    closure_unref(closure);

    return sc->T;
}

KERN_API_CALL(kern_char_task_continue)
{
    char *taskname = NULL;
    class Character *ch;
    pointer taskproc, taskgob;
    struct closure *closure = NULL;
    struct gob *gob = NULL;

    ch = (class Character*)unpack_obj(sc, &args, "kern-char-task-continue");
    if (!ch) {
        return sc->F;
    }

    if (unpack(sc, &args, "scl", &taskname, &taskproc, &taskgob)) {
        rt_err("%s: bad args", __FUNCTION__);
        return sc->F;
    }

    if (taskproc == sc->NIL) {
        return sc->F;
    }
    
    if (!(closure = closure_new_ref(sc, taskproc))) {
        return sc->F;
    }

    /* gob is optional */
    if (taskgob != sc->NIL) {
        if (!(gob = gob_new(sc, taskgob))) {
            closure_unref(closure);
            return sc->F;
        }
    }

    ch->taskContinue(taskname, closure, gob);
    closure_unref(closure);

    return sc->T;
}

KERN_API_CALL(kern_char_task_end)
{
    class Character *ch;

    ch = (class Character*)unpack_obj(sc, &args, "kern-char-task-end");
    if (ch) {
        ch->taskEnd();
    }
    return sc->NIL;
}


KERN_API_CALL(kern_char_set_sched)
{
        class Character *ch;
        struct sched *sched = 0;

        if (unpack(sc, &args, "pp", &ch, &sched)) {
                rt_err("kern-char-set-sched: bad args");
                return sc->NIL;
        }

        if (!ch) {
                rt_err("kern-char-set-sched: null object");
                return sc->NIL;
        }

        ch->setSchedule(sched);

        return sc->T;
}

KERN_API_CALL(kern_char_set_control_mode)
{
        static struct { const char *str; enum control_mode mode; } tbl[] = {
                { "auto", CONTROL_MODE_AUTO },
                { "player", CONTROL_MODE_PLAYER },
                { "idle", CONTROL_MODE_IDLE },
                { "follow", CONTROL_MODE_FOLLOW }
        };
        class Character *ch;
        char *modestr = 0;
        int i;


        ch = (class Character*)unpack_obj(sc, &args, "kern-char-set-control-mode");
        if (!ch)
                return sc->NIL;

        if (unpack(sc, &args, "s", &modestr)) {
                rt_err("kern-char-set-control-mode: bad args");
                return sc->NIL;
        }

        for (i = 0; i < array_sz(tbl); i++) {
                if (! strcmp(tbl[i].str, modestr)) {
                        ch->setControlMode(tbl[i].mode);
                        return sc->NIL;
                }
        }

        return sc->NIL;
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

        ctrl_do_attack(attacker, weapon, defender, 
                       attacker->getToHitPenalty());
        
        attacker->decActionPoints(weapon->getRequiredActionPoints());
                       
        return sc->T;
}

KERN_API_CALL(kern_char_is_asleep)
{
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-is-asleep");
        if (!ch)
                return sc->F;

        return ch->isAsleep() ? sc->T : sc->F;
}

KERN_API_CALL(kern_char_is_dead)
{
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-is-dead");
        if (!ch)
                return sc->T;

        return ch->isDead() ? sc->T : sc->F;
}

KERN_API_CALL(kern_mk_effect)
{
        struct effect *effect;
        pointer exec_proc = sc->NIL;
        pointer apply_proc = sc->NIL;
        pointer rm_proc = sc->NIL;
        pointer restart_proc = sc->NIL;
        void *sprite;
        pointer ret;
        char *name, *tag = 0;
        int hook_id;

        if (unpack(sc, &args, "yspccccd", &tag, &name, &sprite, &exec_proc,
                   &apply_proc, &rm_proc, &restart_proc, &hook_id)) {
                load_err("kern-mk-effect %s: bad args", tag);
                return sc->NIL;
        }

        if (exec_proc == sc->NIL)
                exec_proc = NULL;

        if (apply_proc == sc->NIL)
                apply_proc = NULL;

        if (rm_proc == sc->NIL)
                rm_proc = NULL;

        if (restart_proc == sc->NIL)
                restart_proc = NULL;
				
        effect = effect_new(tag, sc, exec_proc, apply_proc, rm_proc, 
                            restart_proc, name);

        effect->hook_id = hook_id;
        effect->sprite = (struct sprite*)sprite;

        if (unpack(sc, &args, "dbd", &effect->detect_dc,
                   &effect->cumulative, &effect->duration)) {
                load_err("kern-mk-effect %s: bad args", tag);
                goto abort;
        }

        session_add(Session, effect, effect_dtor, NULL, NULL);
        ret = scm_mk_ptr(sc, effect);
        scm_define(sc, tag, ret);

        return ret;

 abort:
        effect_del(effect);
        return sc->NIL;
}

/*  kern_ui_target_visitor - build a suggested list of targets from all beings
 *  in range. */
static void kern_ui_target_visitor(class Object *obj, void *data)
{
        struct kern_ui_target_info *info = (struct kern_ui_target_info*)data;
        class Character *npc = 0;
        int dist = 0;
        struct location_list *entry = 0;

        if (being_layer!=obj->getLayer())
                return;

        npc = (class Character*)obj;

        if (! npc->isVisible() && ! Reveal)
                return;
        
        dist = place_flying_distance(info->place,
                                     info->x,
                                     info->y,
                                     obj->getX(),
                                     obj->getY());
        if (dist > info->range)
                return;

        /* Add it to the list */
        entry = (struct location_list*)malloc(sizeof(*entry));
        assert(entry);
        entry->x = obj->getX();
        entry->y = obj->getY();
        list_add_tail(&info->suggest, &entry->list);
}

/* kern_ui_target_cleanup_info - free the suggest list. */
static void kern_ui_target_cleanup_info(struct kern_ui_target_info *info)
{
        struct list *head = &info->suggest;
        struct list *entry = head->next;
        while (entry != head) {
                struct location_list *tmp = 
                        (struct location_list*)entry;
                entry = entry->next;
                list_remove(&tmp->list);
                free(tmp);
        }
}

KERN_API_CALL(kern_ui_target)
{
        struct place *place;
        int ox, oy, tx, ty, range;
        struct kern_ui_target_info info;
        pointer ret;

        /* Unpack the origin */
        if (unpack_loc(sc, &args, &place, &ox, &oy, "kern-ui-target")) {
                return sc->NIL;
        }
        
        /* Unpack the range */
        if (unpack(sc, &args, "d", &range)) {
                rt_err("kern-ui-target: bad range arg");
                return sc->NIL;
        }

        /* Build a list of suggested targets. */
        memset(&info, 0, sizeof(info));
        info.place = Place;
        info.x = ox;
        info.y = oy;
        info.range = range;
        list_init(&info.suggest);
        place_for_each_object(Place,
                              kern_ui_target_visitor,
                              &info);
        

        /* Get the target coords from the user */
        tx = ox;
        ty = oy;
        if (select_target(ox, oy, &tx, &ty, range, &info.suggest)) {
                ret = sc->NIL;
        }
        
        /* Pack the target coords for return */
        else { 
                ret = pack(sc, "pdd", place, tx, ty);
        }

        kern_ui_target_cleanup_info(&info);
        return ret;
}

static int kern_mk_templ_visitor(struct templ *templ, int x, int y, void *data)
{
        closure_t *check = (closure_t*)data;
        
        /* if the check proc returns #f then turn off this location in the
         * template */
        if (! closure_exec(check, "dd", x, y)) {
                templ_set(templ, x, y, 0);
        }

        return 0;
}

KERN_API_CALL(kern_mk_templ)
{
        int rad, x, y;
        struct place *place;
        pointer checkptr;
        struct templ *templ;
        closure_t *checkproc;

        /* origin */
        if (unpack_loc(sc, &args, &place, &x, &y, "kern-mk-templ")) {
                return sc->NIL;
        }

        /* radius, check-proc and gob */
        if (unpack(sc, &args, "dc", &rad, &checkptr)) {
                rt_err("kern-mk-templ: bad args");
                return sc->NIL;
        }

        /* create the templ and set its origin */
        templ = templ_new_from_range(rad);
        templ_set_origin(templ, x, y);

        /* run the check procedure on each location covered by the templ */
        checkproc = closure_new_ref(sc, checkptr);
        templ_for_each(templ, kern_mk_templ_visitor, checkproc);
        closure_unref(checkproc);

        return scm_mk_ptr(sc, templ);
}

KERN_API_CALL(kern_ui_target_generic)
{
        ui_select_target_req_t req;
        pointer move_cb, select_cb, gob, dummy;

        ui_select_target_req_init(&req);

        /* Unpack the origin */
        if (unpack_loc(sc, &args, &req.place, &req.x1, &req.y1, 
                       "kern-ui-target-generic")) {
                return sc->NIL;
        }

        /* Unpack the initial cursor loc */
        if (unpack_loc(sc, &args, &req.place, &req.x2, &req.y2, 
                       "kern-ui-target-generic")) {
                return sc->NIL;
        }
        
        /* Unpack the template */
        if (unpack(sc, &args, "p", &req.tiles)) {
                rt_err("kern-ui-target-generic: bad template arg");
                return sc->NIL;
        }

        /* fixme: unpack the suggested list */
        if (unpack(sc, &args, "p", &dummy)) {
                rt_err("kern-ui-target-generic: bad template arg");
                return sc->NIL;
        }

        /* unpack the move-cb, select-cb and gob */
        if (unpack(sc, &args, "ccc", &move_cb, &select_cb, &gob)) {
                rt_err("kern-ui-target-generic: bad callback or gob arg");
                return sc->NIL;
        }
        
        /* fixme: convert the cb procs into closures (will have to clean them
         * up at the bottom, too) */

        /* Prompt the player; returns when player has made selection */
        if (ui_select_target_generic(&req)) {
                return sc->NIL;
        }

        /* Kind of a hack: manually unref the templ here, assuming the caller
         * is done with it, which is going to be the usual case. If I encounter
         * an unusual case then I'll need to add kern-templ-ref/unref so the
         * script can protect it. */
        if (req.tiles) {
                templ_unref(req.tiles);
        }
        
        /* Pack the target coords for return */
        return pack(sc, "pdd", req.place, req.x2, req.y2);
}

KERN_API_CALL(kern_fire_missile)
{
        MissileType *missile_type;
        Missile *missile;
        struct place *oplace, *dplace;
        int ox, oy, dx, dy, hitTarget = 0;

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
        missile->animate(ox, oy, &dx, &dy, 0, 0);
        hitTarget = missile->hitTarget();
        missile->fireHitLoc(NULL, NULL, oplace,dx,dy,-1);
        delete missile;
        return hitTarget ? sc->T : sc->F;
}


KERN_API_CALL(kern_fire_missile_to_max)
{
        MissileType *missile_type;
        Missile *missile;
        struct place *oplace, *dplace;
        int ox, oy, dx, dy, hitTarget = 0;
        int range;

        /* Unpack the missile type */
        if (unpack(sc, &args, "pd", &missile_type, &range)) {
                rt_err("kern-fire-missile-to-max: bad missile type arg");
                return sc->NIL;
        }
        if (! missile_type) {
                rt_err("kern-fire-missile-to-max: null missile type");
                return sc->NIL;
        }

        /* Unpack the origin */
        if (unpack_loc(sc, &args, &oplace, &ox, &oy, "kern-fire-missile-to-max"))
                return sc->NIL;

        /* Unpack the destination */
        if (unpack_loc(sc, &args, &dplace, &dx, &dy, "kern-fire-missile-to-max"))
                return sc->NIL;

        /* Create the missile */
        missile = new Missile(missile_type);
        assert(missile);

        /* Fire the missile */
        missile->setPlace(dplace);
        missile->animate(ox, oy, &dx, &dy, 0, range);
        hitTarget = missile->hitTarget();
        missile->fireHitLoc(NULL, NULL, oplace,dx,dy,-1);
        delete missile;
        return hitTarget ? sc->T : sc->F;
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

        return sc->T;
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
        if (! place_in_los(subj->getPlace(),subj->getX(),subj->getY(),
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

static int kern_filter_visible_allies(Object *obj, 
                                      struct kern_append_info *info)
{
        class Being *subj;

        /* Extract a pointer to the subject looking for hostiles */
        subj = (class Being *)info->data;

        /* Filter out non-beings */
        if (obj->getLayer() != being_layer)
                return 0;

        /* Filter out non-allies */
        if (! are_allies(subj, (class Being*)obj))
                return 0;

        /* Filter out objects not in los of the subject */
        if (! place_in_los(subj->getPlace(),subj->getX(),subj->getY(),
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
        /* OBSOLETE! Use kern-obj-is-being */
        Object *obj;

        obj = unpack_obj(sc, &args, "kern-obj-is-char?");
        if (!obj)
                return sc->F;

        return (obj->getLayer() == being_layer) ? sc->T : sc->F;
}

KERN_API_CALL(kern_obj_is_container)
{
        Object *obj;

        obj = unpack_obj(sc, &args, "kern-obj-is-container?");
        if (!obj)
                return sc->F;

        return (obj->getLayer() == container_layer) ? sc->T : sc->F;
}

KERN_API_CALL(kern_obj_is_field)
{
        Object *obj;

        obj = unpack_obj(sc, &args, "kern-obj-is-field?");
        if (!obj)
                return sc->F;

        return (obj->getLayer() == field_layer) ? sc->T : sc->F;
}

KERN_API_CALL(kern_obj_is_being)
{
        Object *obj;

        obj = unpack_obj(sc, &args, "kern-obj-is-being?");
        if (!obj)
                return sc->F;

        return (obj->getLayer() == being_layer) ? sc->T : sc->F;
}

KERN_API_CALL(kern_obj_is_mech)
{
        Object *obj;

        obj = unpack_obj(sc, &args, "kern-obj-is-mech?");
        if (!obj)
                return sc->F;

        return (obj->getLayer() == mech_layer) ? sc->T : sc->F;
}

KERN_API_CALL(kern_obj_is_visible)
{
        Object *obj;

        obj = unpack_obj(sc, &args, "kern-obj-is-visible?");
        if (!obj)
                return sc->F;

        return obj->isVisible() ? sc->T : sc->F;
}

KERN_API_CALL(kern_obj_is_submerged)
{
        Object *obj;

        obj = unpack_obj(sc, &args, "kern-obj-is-submerged?");
        if (!obj)
                return sc->F;

        return obj->isSubmerged() ? sc->T : sc->F;
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

KERN_API_CALL(kern_char_set_player_controlled)
{
        int val;
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-set-player-controlled");
        if (!ch)
                return sc->NIL;

        if (unpack(sc, &args, "b", &val)) {
                rt_err("kern-char-set-player-controlled: bad args");
                return sc->NIL;
        }
        
        ch->setPlayerControlled(val);

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

KERN_API_CALL(kern_char_get_occ)
{
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-get-occ");
        if (!ch || ! ch->occ)
                return sc->NIL;

        return scm_mk_ptr(sc, ch->occ);
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

KERN_API_CALL(kern_being_get_visible_allies)
{
        Object *subj;

        /* Unpack the subject */
        subj = unpack_obj(sc, &args, "kern-place-get-visible-allies");
        if (!subj)
                return sc->NIL;

        if (! subj->getPlace()) {
                rt_err("kern-place-get-visible-allies: null place");
                return sc->NIL;
        }

        return kern_place_for_each_object(sc, subj->getPlace(), 
                                          kern_filter_visible_allies,
                                          subj);
}

KERN_API_CALL(kern_being_get_visible_tiles)
{
        Object *subj;
        struct place *place;
        int ox, oy, vr;
        pointer head = sc->NIL;
        pointer tail = sc->NIL;
        pointer cell;

        /* Unpack the subject */
        subj = unpack_obj(sc, &args, "kern-being-get-visible-tiles");
        if (!subj)
                return sc->NIL;

        place = subj->getPlace();
        if (! place) {
                rt_err("kern-being-get-visible-tiles: null place");
                return sc->NIL;
        }

        ox = subj->getX();
        oy = subj->getY();
        vr = subj->getVisionRadius();
        
        for (int y = 0; y < place_h(place); y++) {
                for (int x = 0; x < place_w(place); x++) {

                        /* Filter out tiles not in los of the subject */
                        if (! place_in_los(place, ox, oy, place, x, y))
                                continue;
                        
                        /* Filter out tiles not in the vision radius of the
                         * subject */
                        if (place_flying_distance(place, ox, oy, x, y) > vr)
                                continue;
                        
                        /* else append this location to the list */
                        cell = scm_mk_loc(sc, place, x, y);
                        cell = _cons(sc, cell, sc->NIL, 0);

                        if (head == sc->NIL) {
                                head = cell;
                                tail = cell;
                                scm_protect(sc, cell);
                        } else {
                                tail->_object._cons._cdr = cell;
                                tail = cell;
                        }
                }
        }

        /* unprotect the list prior to returning */
        if (head != sc->NIL)
                scm_unprotect(sc, head);

        return head;
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
/*         int rad; /\* looke r's rad *\/ */
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
                rt_err("kern-place-get-name: bad args");
                return sc->NIL;
        }
        if (! place) {
                rt_err("kern-place-get-name: null place");
                return sc->NIL;
        }

        return scm_mk_string(sc, place->name);
}

KERN_API_CALL(kern_place_is_passable)
{
        struct place *place;
        int x, y;
        class Object *obj;

        if (unpack_loc(sc, &args, &place, &x, &y, "kern-place-is-passable"))
                return sc->F;

        obj = unpack_obj(sc, &args, "kern-place-is-passable");
        if (!obj)
                return sc->F;

        return place_is_passable(place, x, y, obj, 0) ? sc->T : sc->F;
}

KERN_API_CALL(kern_place_move_is_passable)
{
        struct place *fplace, *tplace;
        int fx, fy, tx, ty;
        class Object *obj;

        if (unpack_loc(sc, &args, &fplace, &fx, &fy, 
                       "kern-place-move-is-passable"))
                return sc->F;

        if (unpack_loc(sc, &args, &tplace, &tx, &ty,
                       "kern-place-move-is-passable"))
                return sc->F;

        obj = unpack_obj(sc, &args, "kern-place-move-is-passable");
        if (!obj)
                return sc->F;

        return place_move_is_passable(fplace, fx, fy, tx, ty, obj, 0) ? sc->T 
                : sc->F;
}

KERN_API_CALL(kern_place_is_hazardous)
{
        struct place *place;
        int x, y;
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

        if (! terrain) {
                rt_err("kern-place-set-terrain: nil terrain");
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

KERN_API_CALL(kern_place_set_subplace)
{
        struct place *place, *subplace;
        int x, y;

        if (unpack(sc, &args, "p", &subplace)) {
                rt_err("kern-place-set-subplace: bad args");
                return sc->NIL;
        }

        if (! subplace) {
                rt_err("kern-place-set-subplace: nil subplace");
                return sc->NIL;
        }

        if (unpack_loc(sc, &args, &place, &x, &y, "kern-place-set-subplace"))
                return sc->NIL;


        place_add_subplace(place, subplace, x, y);

        return scm_mk_ptr(sc, subplace);
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

KERN_API_CALL(kern_place_get_movement_cost)
{
        struct place *place;
        int x, y, cost=0;
        class Object *obj;
        
        if (unpack_loc(sc, &args, &place, &x, &y, "kern-place-get-movement-cost"))
                goto done;

        obj = unpack_obj(sc, &args, "kern-place-get-movement-cost");
        if (!obj)
                goto done;

        cost = place_get_movement_cost(place, x, y, obj,0);

 done:
        return scm_mk_integer(sc, cost);
}

KERN_API_CALL(kern_place_get_light)
{
        struct place *place;
        int x, y;

        if (unpack_loc(sc, &args, &place, &x, &y, "kern-place-get-light"))
                return sc->F;

        return scm_mk_integer(sc, place_get_light(place, x, y));
}

KERN_API_CALL(kern_place_get_terrain_map)
{
        struct place *place;
        struct terrain_map *map;

        if (unpack(sc, &args, "p", &place)) {
                rt_err("kern-place-get-terrain-map: bad args");
                return sc->NIL;
        }

        map = place_get_terrain_map(place);
        if (!map)
                return sc->NIL;
        return scm_mk_ptr(sc, map);
}

KERN_API_CALL(kern_place_set_terrain_map)
{
        struct place *place;
        struct terrain_map *map;

        if (unpack(sc, &args, "pp", &place, &map)) {
                rt_err("kern-place-set-terrain-map: bad args");
                return sc->NIL;
        }

        place_set_terrain_map(place, map);
        return scm_mk_ptr(sc, place);
}

KERN_API_CALL(kern_place_blocks_los)
{
        struct place *place;
        int x, y;

        if (unpack_loc(sc, &args, &place, &x, &y, "kern-place-blocks-los?"))
                return sc->F;

        return place_visibility(place, x, y) ? sc->F : sc->T;
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

KERN_API_CALL(kern_obj_set_ignore_time_stop)
{
        class Object *obj;
        int val;

        obj = unpack_obj(sc, &args, "kern-obj-set-ignore-time-stop");
        if (!obj)
                return sc->NIL;

        if (unpack(sc, &args, "b", &val)) {
                rt_err("kern-obj-set-ignore-time-stop: bad value arg");
                return scm_mk_ptr(sc, obj);
        }

        obj->setIgnoreTimeStop(val);

        return scm_mk_ptr(sc, obj);
}

KERN_API_CALL(kern_obj_wander)
{
	class Object *obj;
	
	obj = unpack_obj(sc, &args, "kern-obj-wander");
	if (!obj)
	return sc->NIL;
	
	//moves can have nasty consequences,
	//so keep our own ref to the object for a bit
	obj_inc_ref(obj);
	ctrl_wander(obj);
	obj_dec_ref(obj);
	
	return sc->NIL;
}

KERN_API_CALL(kern_obj_clone)
{
        class Object *obj, *clone;

        obj = unpack_obj(sc, &args, "kern-char-clone");
        if (!obj)
                return sc->NIL;

        clone = obj->clone();
        assert(clone);
        clone->setTemporary(1);

        return scm_mk_ptr(sc, clone);
}

KERN_API_CALL(kern_obj_freeze)
{
        class Object *obj;
		char* key;
		int x,y;

        obj = unpack_obj(sc, &args, "kern-obj-freeze");
        if (!obj)
                return sc->NIL;

        if (unpack(sc, &args, "sdd", &key, &x, &y)) {
                rt_err("kern-obj-freeze: bad args");
                return sc->NIL;
        }

        obj_inc_ref(obj);
        freezer_freezeObject(key, x, y, obj);

        return sc->NIL;
}
		
KERN_API_CALL(kern_obj_thaw)
{
        class Object *obj;
		struct place *place;
		char* key;
		int x,y;

        if (unpack(sc, &args, "sp", &key, &place)) {
                rt_err("kern-obj-thaw-at: bad args");
                return sc->NIL;
        }

        obj = freezer_thawObject(key, &x, &y);

		if (obj)
		{
			obj->relocate(place, x, y, REL_NOTRIG);
			scm_mk_ptr(sc, obj);
			obj_dec_ref(obj);
			return scm_mk_ptr(sc, obj);
		}
		
        return sc->NIL;
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

KERN_API_CALL(kern_get_wind)
{
        return scm_mk_integer(sc, windGetDirection());
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

        /* lookup neighbor */
        neighbor = place_get_neighbor(place, dir);
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

KERN_API_CALL(kern_char_add_experience)
{
        int val;
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, 
                                          "kern-char-add-experience");
        if (!ch)
                return sc->NIL;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-char-add-experience: bad args");
                goto done;
        }
        
        ch->addExperience(val);
 done:
        return scm_mk_ptr(sc, ch);
}

KERN_API_CALL(kern_add_magic_negated)
{
        int val;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-add-magic-negated: bad args");
                return sc->F;
        }

        add_magic_negated(val);
        foogodRepaint();
        return sc->T;
}

KERN_API_CALL(kern_get_magic_negated)
{
        return scm_mk_integer(sc, MagicNegated);
}

KERN_API_CALL(kern_add_quicken)
{
        int val;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-add-quicken: bad args");
                return sc->F;
        }

        add_quicken(val);
        foogodRepaint();
        return sc->T;
}

KERN_API_CALL(kern_set_time_accel)
{
        float val;

        if (unpack(sc, &args, "f", &val)) {
                rt_err("kern-set-time-accel: bad args");
                return sc->F;
        }

        session_set_time_accel(val);
        foogodRepaint();
        return sc->T;
}

KERN_API_CALL(kern_set_turn_count)
{
        int val;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-set-turn-count: bad args");
                return sc->F;
        }

        session_set_turn_count(val);
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
        foogodRepaint();
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
        foogodRepaint();
        return sc->T;
}

KERN_API_CALL(kern_being_is_hostile)
{
        class Being *one, *another;

        if (unpack(sc, &args, "pp", &one, &another)) {
                rt_err("kern-being-is-hostile: bad args");
                return sc->F;
        }

        if (! one || ! another) {
                rt_err("kern-being-is-hostile: null character");
                return sc->F;                
        }

        return are_hostile(one, another) ? sc->T : sc->F;
}

KERN_API_CALL(kern_being_is_ally)
{
        class Being *one, *another;

        if (unpack(sc, &args, "pp", &one, &another)) {
                rt_err("kern-being-is-ally: bad args");
                return sc->F;
        }

        if (! one || ! another) {
                rt_err("kern-being-is-ally: null character");
                return sc->F;                
        }

        return are_allies(one, another) ? sc->T : sc->F;
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

KERN_API_CALL(kern_image_load)
{
        char *fname, *path;
        SDL_Surface *image = 0;

        if (unpack(sc, &args, "s", &fname)) {
                rt_err("kern-image-load: bad args");
                return sc->NIL;
        }

        path = file_mkpath(cfg_get("include-dirname"), fname);
        if (! path) {
                rt_err("kern-image-load: %s", file_get_error());
                return sc->NIL;
        }

        image = IMG_Load(path);
        if (! image) {
                rt_err("kern-image-load: %s", SDL_GetError());
        }
        free(path);

        return scm_mk_ptr(sc, image);
}

KERN_API_CALL(kern_image_free)
{
        SDL_Surface *image;

        if (unpack(sc, &args, "p", &image)) {
                rt_err("kern-image-free: bad args");
                return sc->NIL;
        }

        SDL_FreeSurface(image);
        return sc->NIL;
}

KERN_API_CALL(kern_map_set_image)
{
        SDL_Surface *image;

        if (unpack(sc, &args, "p", &image)) {
                rt_err("kern-map-set-image: bad args");
                return sc->NIL;
        }

        if (!image) {
                mapClearImage();
                mapUpdate(0);
                return sc->NIL;
        }

        mapSetImage(image);
        return sc->NIL;
}

KERN_API_CALL(kern_map_blit_image)
{
        SDL_Surface *image;
        Uint32 x, y;

        if (unpack(sc, &args, "pdd", &image, &x, &y)) {
                rt_err("kern-map-blit-image: bad args");
                return sc->NIL;
        }

        if (!image) {
                return sc->NIL;
        }

        mapBlitImage(image, x, y);
        return sc->NIL;
}

KERN_API_CALL(kern_map_get_width)
{
        struct terrain_map *map;

        if (unpack(sc, &args, "p", &map)) {
                rt_err("kern-map-get-width: bad args");
                return sc->NIL;
        }

        if (!map) {
                rt_err("kern-map-get-width: null map");
                return sc->NIL;
        }

        return scm_mk_integer(sc, map->w);
}


KERN_API_CALL(kern_map_get_height)
{
        struct terrain_map *map;

        if (unpack(sc, &args, "p", &map)) {
                rt_err("kern-map-get-width: bad args");
                return sc->NIL;
        }

        if (!map) {
                rt_err("kern-map-get-width: null map");
                return sc->NIL;
        }

        return scm_mk_integer(sc, map->h);
}

KERN_API_CALL(kern_char_kill)
{
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-kill");
        if (!ch)
                return sc->NIL;

        ch->kill();

        return sc->NIL;
}

KERN_API_CALL(kern_char_resurrect)
{
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, "kern-char-resurrect");
        if (!ch)
                return sc->NIL;

        ch->resurrect();

        return scm_mk_ptr(sc, ch);
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

KERN_API_CALL(kern_terrain_blocks_los)
{
        struct terrain *terrain;

        if (unpack(sc, &args, "p", &terrain)) {
                rt_err("kern-terrain-blocks-los?: bad args");
                return sc->NIL;
        }

        if(! terrain) {
                rt_err("kern-terrain-blocks-los?: null terrain");
                return sc->NIL;
        }

        return terrain->alpha ? sc->T : sc->F;
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

KERN_API_CALL(kern_terrain_set_combat_map)
{
        struct terrain *terrain;
        struct terrain_map *map;

        if (unpack(sc, &args, "pp", &terrain, &map)) {
                rt_err("kern-terrain-set-combat-map: bad args");
                return sc->NIL;
        }

        if(! terrain) {
                rt_err("kern-terrain-set-combat-map: null terrain");
                return sc->NIL;
        }

        terrain->combat_map = map;

        return scm_mk_ptr(sc, terrain);
}

KERN_API_CALL(kern_terrain_set_combat_handler)
{
	struct terrain *terrain;
	pointer proc;
	
	if (unpack(sc, &args, "po", &terrain, &proc)) {
		rt_err("kern-terrain-set-combat-handler: bad args");
		return sc->NIL;
	}
	
	if(!terrain) {
		rt_err("kern-terrain-set-combat-handler: null terrain");
		return sc->NIL;
	}
	
	if (proc != sc->NIL) {
		terrain->renderCombat = closure_new(sc, proc);
		closure_ref(terrain->renderCombat); //TODO clean up this nasty leaky hack
	}
	
	return scm_mk_ptr(sc, terrain);
}

KERN_API_CALL(kern_terrain_map_inc_ref)
{
        struct terrain_map *map;

        if (unpack(sc, &args, "p", &map)) {
                rt_err("kern-terrain-map-inc-ref: bad args");
                return sc->NIL;
        }

        terrain_map_ref(map);
        return scm_mk_ptr(sc, map);
}

KERN_API_CALL(kern_terrain_map_dec_ref)
{
        struct terrain_map *map;

        if (unpack(sc, &args, "p", &map)) {
                rt_err("kern-terrain-map-dec-ref: bad args");
                return sc->NIL;
        }

        terrain_map_ref(map);
        return sc->NIL;
}

KERN_API_CALL(kern_mk_blender)
{
        blender_t *blender;
        pointer rlist;
        int i = 0;

        blender = (blender_t*)calloc(1, sizeof(*blender));
        list_init(&blender->list);

        if (unpack(sc, &args, "p", &blender->inf)) {
                rt_err("kern-terrain-map-blend: bad args");
                goto abort;
        }

        /* list of not-superior terrains */
        rlist = scm_car(sc, args);
        args = scm_cdr(sc,  args);

        if (! scm_is_pair(sc, rlist)) {
                rt_err("kern-terrain-map-blend: missing non-superior list");
                goto abort;
        }

        while (scm_is_pair(sc, rlist) 
               && blender->n_nonsup < BLENDER_MAX_NONSUP) {
                if (unpack(sc, &rlist, "p", &blender->nonsup[blender->n_nonsup])) {
                        rt_err("kern-terrain-map-blend: non-superior terrain %d bad", i);
                        goto abort;
                }
                blender->n_nonsup++;
        }
        
        if (scm_is_pair(sc, rlist)) {
                warn("kern-terrain-map-blend: at most %d non-superior "\
                     "terrains may be used, the rest will be ignored",
                     BLENDER_MAX_NONSUP);
        }

        /* list of target (range) terrains */
        i = 0;
        rlist = scm_car(sc, args);
        args = scm_cdr(sc,  args);

        if (! scm_is_pair(sc, rlist)) {
                rt_err("kern-terrain-map-blend: missing range list");
                goto abort;
        }

        while (scm_is_pair(sc, rlist) 
               && i < BLENDER_N_RANGE) {

                if (unpack(sc, &rlist, "p", &blender->range[i])) {
                        rt_err("kern-terrain-map-blend: range %d bad", i);
                        return sc->NIL;
                }

                i++;
        }

        if (i < BLENDER_N_RANGE) {
                rt_err("kern-terrain-map-blend: expected %d ranges, got %d", 
                       BLENDER_N_RANGE, i);
                goto abort;
        }

        session_add(Session, blender, blender_dtor, NULL, NULL);
        list_add(&Session->blenders, &blender->list);

        return sc->T;

 abort:
        free(blender);
        return sc->F;
}

KERN_API_CALL(kern_terrain_map_blend)
{
        struct terrain_map *map;
        struct terrain *inf, *nonsup[32], *range[16];
        pointer rlist;
        int i = 0;
        int n_nonsup = 0;

        if (unpack(sc, &args, "pp", &map,  &inf)) {
                rt_err("kern-terrain-map-blend: bad args");
                return sc->NIL;
        }

        /* list of not-superior terrains */
        rlist = scm_car(sc, args);
        args = scm_cdr(sc,  args);

        if (! scm_is_pair(sc, rlist)) {
                rt_err("kern-terrain-map-blend: missing non-superior list");
                return sc->NIL;
        }

        while (scm_is_pair(sc, rlist) && n_nonsup < array_sz(nonsup)) {

                if (unpack(sc, &rlist, "p", &nonsup[n_nonsup])) {
                        rt_err("kern-terrain-map-blend: non-superior terrain %d bad", i);
                        return sc->NIL;
                }

                n_nonsup++;
        }
        
        if (scm_is_pair(sc, rlist)) {
                warn("kern-terrain-map-blend: at most %d non-superior "\
                     "terrains may be used, the rest will be ignored",
                     array_sz(nonsup));
        }

        /* list of target (range) terrains */
        i = 0;
        rlist = scm_car(sc, args);
        args = scm_cdr(sc,  args);

        if (! scm_is_pair(sc, rlist)) {
                rt_err("kern-terrain-map-blend: missing range list");
                return sc->NIL;
        }

        while (scm_is_pair(sc, rlist) && i < 16) {

                if (unpack(sc, &rlist, "p", &range[i])) {
                        rt_err("kern-terrain-map-blend: range %d bad", i);
                        return sc->NIL;
                }

                i++;
        }

        if (i < 16) {
                rt_err("kern-terrain-map-blend: expected 16 ranges, got %d", i);
                return sc->NIL;
        }

        terrain_map_blend(map, inf, n_nonsup, nonsup, range);

        return scm_mk_ptr(sc, map);
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

KERN_API_CALL(kern_place_get_vehicle)
{
        struct place *place;
        int x, y;
        class Vehicle *veh;

        if (unpack_loc(sc, &args, &place, &x, &y, "kern-place-get-vehicle"))
                return sc->NIL;

        if (!place) {
                rt_err("kern-place-get-vehicle: null place");
                return sc->NIL;
        }

        veh = place_get_vehicle(place, x, y);
        
        return veh ? scm_mk_ptr(sc, veh) : sc->NIL;
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
        int x1, x2, y1, y2;

        if (unpack_loc(sc, &args, &p1, &x1, &y1, "kern-in-los?") ||
            unpack_loc(sc, &args, &p2, &x2, &y2, "kern-in-los?"))
                return sc->F;

        if (p1 != p2) {
                /* happens sometimes when player exits a place and NPC's
                 * looking for him in the same round */
                warn("kern-in-los?: place %s different from %s\n",
                     p1->tag, p2->tag);
                return sc->F;
        }

        return place_in_los(p1, x1, y1, p2, x2, y2) ? sc->T : sc->F;
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
        return scm_mk_integer(sc, key);
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

KERN_API_CALL(kern_ui_select_from_list)
{
        struct KeyHandler kh;
	struct ScrollerContext data;
        const char **strings;
        int list_sz;
        int i = 0;
        enum StatusMode omode;
        char *selection = NULL;

        list_sz = scm_len(sc, args);
        if (! list_sz)
                return sc->NIL;

        strings = (const char**)calloc(list_sz, sizeof(strings[0]));
        assert(strings);

        while (scm_is_pair(sc, args)) {
                if (unpack(sc, &args, "s", &strings[i])) {
                        rt_err("kern-ui-select-from-list: bad args");
                        goto done;
                }
                i++;
        }

        foogodSetHintText(SCROLLER_HINT);
        foogodSetMode(FOOGOD_HINT);
        omode = statusGetMode();
        statusSetStringList("Select", list_sz, strings);
        statusSetMode(StringList);

        data.selection = NULL;
        data.selector  = String;
        kh.fx   = scroller;
        kh.data = &data;
	eventPushKeyHandler(&kh);
	eventHandle();
	eventPopKeyHandler();

        statusSetMode(omode);
        foogodSetMode(FOOGOD_DEFAULT);
        
        selection = (char*)data.selection;

 done:
        if (strings)
                free(strings);

        if (selection)
                return scm_mk_string(sc, selection);

        return sc->NIL;

}

KERN_API_CALL(kern_ui_select_item)
{
        enum StatusMode omode;
        struct inv_entry *ie;
        class Character *ch;

        ch = (class Character*)unpack_obj(sc, &args, "kern-ui-select-item");
        if (!ch || !ch->getInventoryContainer())
                return sc->NIL;

        omode = statusGetMode();
        statusBrowseContainer(ch->getInventoryContainer(), "Select");
        ie = ui_select_item();
        statusSetMode(omode);

        if (!ie) {
                return sc->NIL;
        }

        return scm_mk_ptr(sc, ie->type);
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

        foogodSetHintText(PAGER_HINT);
        foogodSetMode(FOOGOD_HINT);
        statusSetPageText(title, text);
        statusSetMode(Page);
        consolePrint("[Hit ESC to continue]\n");

        kh.fx = scroller;
        kh.data = NULL;
	eventPushKeyHandler(&kh);
	eventHandle();
	eventPopKeyHandler();

        statusSetMode(ShowParty);
        foogodSetMode(FOOGOD_DEFAULT);

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

        if (! obj->takeOut(type, amount)) {
                rt_err("kern-obj-remove-from-inventory: failed! "\
                       "(is quantity > amount available to take out?)");
        }
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

                        int val;

                        /* unpack the value */
                        if (unpack(sc, &row, "d", &val)) {
                                load_err("kern-mk-dtable: row %d column %d "
                                         "is a bad entry", r_faction, 
                                         c_faction);
                                goto abort;
                        }
                        
                        /* poke it into the table */
                        dtable_set(dtable, r_faction, c_faction, val);
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
#define DTABLE_GET    0x05
#define DTABLE_INC    0x06
#define DTABLE_DEC    0x07

#define DTABLE_FX_USES_LEVEL(fx) ((fx) & 0x80)

static pointer kern_dtable_aux(scheme *sc, pointer args, const char *name, int fx)
{
        int f1, f2, level;
        const char *errstr = NULL;

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
        case DTABLE_GET:
                level = dtable_get(session_dtable(), f1, f2);
                return scm_mk_integer(sc, level);
                break;
        case DTABLE_INC:
                dtable_inc(session_dtable(), f1, f2);
                break;
        case DTABLE_DEC:
                dtable_dec(session_dtable(), f1, f2);
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

KERN_API_CALL(kern_dtable_get)
{
        return kern_dtable_aux(sc, args, "kern_dtable_get", DTABLE_GET);
}

KERN_API_CALL(kern_dtable_inc)
{
        return kern_dtable_aux(sc, args, "kern_dtable_inc", DTABLE_INC);
}

KERN_API_CALL(kern_dtable_dec)
{
        return kern_dtable_aux(sc, args, "kern_dtable_dec", DTABLE_DEC);
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

KERN_API_CALL(kern_party_set_vehicle)
{
        class Party *party;
        class Vehicle *vehicle;

        party = (Party*)unpack_obj(sc, &args, "kern-party-set-vehicle");
        if (!party)
                return sc->NIL;

        vehicle = (Vehicle*)unpack_obj(sc, &args, "kern-party-set-vehicle");
        party->setVehicle(vehicle);

        return scm_mk_ptr(sc, party);
}

KERN_API_CALL(kern_party_get_vehicle)
{
        class Party *party;
        class Vehicle *vehicle;

        party = (Party*)unpack_obj(sc, &args, "kern-party-get-vehicle");
        if (!party)
                return sc->NIL;

        vehicle = party->getVehicle();
        if (vehicle)
                return scm_mk_ptr(sc, vehicle);
        return sc->NIL;
}

static bool wrap_kern_append_obj(class Character *c, void *v)
{
        kern_append_object(c, v);
        return false;
}

KERN_API_CALL(kern_get_time)
{
        pointer head, tail, cell;

        /* have to do everything in forward order so that our cells remain
         * protected from gc until the list is built */
        head = _cons(sc, scm_mk_integer(sc, clock_year()), sc->NIL, 0);
        tail = head;
        scm_protect(sc, head);

        cell = _cons(sc, scm_mk_integer(sc, clock_month()), sc->NIL, 0);
        tail->_object._cons._cdr = cell;
        tail = cell;

        cell = _cons(sc, scm_mk_integer(sc, clock_week()), sc->NIL, 0);
        tail->_object._cons._cdr = cell;
        tail = cell;

        cell = _cons(sc, scm_mk_integer(sc, clock_day()), sc->NIL, 0);
        tail->_object._cons._cdr = cell;
        tail = cell;

        cell = _cons(sc, scm_mk_integer(sc, clock_hour()), sc->NIL, 0);
        tail->_object._cons._cdr = cell;
        tail = cell;

        cell = _cons(sc, scm_mk_integer(sc, clock_minute()), sc->NIL, 0);
        tail->_object._cons._cdr = cell;
        tail = cell;

        scm_unprotect(sc, head);
        return head;
}

KERN_API_CALL(kern_get_time_remainder)
{
        return scm_mk_integer(sc, clock_tick());
}


KERN_API_CALL(kern_get_total_minutes)
{
 		return scm_mk_integer(sc, clock_time());
}


KERN_API_CALL(kern_party_get_members)
{
        class Party *party;
        struct kern_append_info info;

        if (unpack(sc, &args, "p", &party)) {
                rt_err("kern-party-get-members: bad args");
                return sc->NIL;
        }

        /* initialize the context used by the callback to append objects */
        info.sc = sc;
        info.head = sc->NIL;
        info.tail = sc->NIL;
        info.filter = NULL;
        info.data = NULL;

        /* build a scheme list of the objects at that location */
        party->forEachMember(wrap_kern_append_obj, &info);

        /* unprotect the list prior to return */
        if (info.head != sc->NIL)
                scm_unprotect(sc, info.head);

        /* return the scheme list */
        return info.head;
}

KERN_API_CALL(kern_being_set_base_faction)
{
        class Being *being;
        int faction;

        being = (class Being*)unpack_obj(sc, &args, "kern-being-set-base-faction");
        if (!being)
                goto done;

        if (unpack(sc, &args, "d", &faction)) {
                rt_err("kern-being-set-base-faction: bad arg");
                goto done;
        }

        being->setBaseFaction(faction);
 done:
        return scm_mk_ptr(sc, being);

}

KERN_API_CALL(kern_being_set_current_faction)
{
        class Being *being;
        int faction;

        being = (class Being*)unpack_obj(sc, &args, "kern-being-set-current-faction");
        if (!being)
                goto done;

        if (unpack(sc, &args, "d", &faction)) {
                rt_err("kern-being-set-current-faction: bad arg");
                goto done;
        }

        being->setCurrentFaction(faction);
 done:
        return scm_mk_ptr(sc, being);

}

KERN_API_CALL(kern_being_set_name)
{
        class Being *being;
        char *val;

        being = (class Being*)unpack_obj(sc, &args, "kern-being-set-name");
        if (!being)
                goto done;

        if (unpack(sc, &args, "s", &val)) {
                rt_err("kern-being-set-name: bad arg");
                goto done;
        }

        being->setName(val);
 done:
        return scm_mk_ptr(sc, being);

}

KERN_API_CALL(kern_vehicle_set_name)
{
        class Vehicle *vehicle;
        char *val;

        vehicle = (class Vehicle*)unpack_obj(sc, &args, "kern-vehicle-set-name");
        if (!vehicle)
                goto done;

        if (unpack(sc, &args, "s", &val)) {
                rt_err("kern-vehicle-set-name: bad arg");
                goto done;
        }

        vehicle->setName(val);
 done:
        return scm_mk_ptr(sc, vehicle);

}

KERN_API_CALL(kern_harm_relations)
{
        class Character *cha;
        class Character *chb;

        if (unpack(sc, &args, "pp", &cha, &chb)) {
                rt_err("kern-harm-relations: bad args");
                return sc->NIL;
        }

		harm_relations(cha,chb);

		return sc->NIL;
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

KERN_API_CALL(kern_add_hook)
{
        pointer pproc;
        char *hookstr;

        if(unpack(sc, &args, "yc", &hookstr, &pproc)) {
                load_err("%s: bad args", __FUNCTION__);
                return sc->NIL;
        }

////         printf("%s() %s: ", __FUNCTION__, hookstr);
////         if (scm_is_sym(sc, pproc)) {
////             printf("%s\n", scm_sym_val(sc, pproc));
////         } else {
////             printf("<raw code>\n");
////         }

        session_hook_id_t id = session_str_to_hook_id(hookstr);
        if (id >= NUM_HOOKS) {
            load_err("%s: bad hook id=%d (%s)", __FUNCTION__, id, hookstr);
            return sc->NIL;
        }

        pointer pargs = sc->NIL;
        if (scm_is_pair(sc, args)) {
            pargs = scm_car(sc, args);
        }
        void *ret = session_add_hook(Session, (session_hook_id_t)id, closure_new(sc, pproc), pargs);
        return ret ? scm_mk_ptr(sc, ret) : sc->NIL;
}

KERN_API_CALL(kern_rm_hook)
{
    pointer pproc;
    char *hookstr;
    
    if(unpack(sc, &args, "yc", &hookstr, &pproc)) {
        load_err("%s: bad args", __FUNCTION__);
        return sc->NIL;
    }
    
    session_hook_id_t id = session_str_to_hook_id(hookstr);
    if (id < NUM_HOOKS) {
        session_rm_hook(Session, (session_hook_id_t)id, pproc);
    }
    
    return sc->NIL;
}

KERN_API_CALL(kern_add_query)
{
        pointer pproc;
        char *str;
        int id = 0;

        if(unpack(sc, &args, "yo", &str, &pproc)) {
                load_err("%s: bad args", __FUNCTION__);
                return sc->F;
        }
        
        for (id = 0; id < NUM_HOOKS; id++) {
                if (! strcmp(query_to_id[id], str)) {
                        session_add_query(Session, (session_query_id_t)id, closure_new(sc, pproc));
                        return pproc;
                }
        }

        return sc->F;
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
        path = place_find_path(place, &as_info, object);
        if (! path)
                return sc->NIL;

        /* convert the path to a scheme list */
        sc_path = kern_astar_path_to_scheme_list(sc, path);
        return sc_path;
}

static pointer kern_build_weapon_list(scheme *sc, 
                                      class Character *character, 
                                      class ArmsType *weapon,
									  int *armsIndex)
{
        /* base case */
        if (! weapon)
                return sc->NIL;
        
        /* recursive case */
        return _cons(sc, 
                     scm_mk_ptr(sc, weapon), 
                     kern_build_weapon_list(sc, 
                                            character, 
                                            character->getNextWeapon(armsIndex),
											armsIndex), 
                     0);
}

KERN_API_CALL(kern_char_get_weapons)
{
        class Character *character;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, 
                                                 "kern-char-get-weapons");
        if (!character)
                return sc->NIL;

        /* recursively enumerate the character's available weapons into a
         * scheme list */
		int armsIndex = 0;
        return kern_build_weapon_list(sc, 
                                      character, 
                                      character->enumerateWeapons(&armsIndex),
									  &armsIndex);
}

/**
 * A generic append-to-scheme-list function.
 */
static void kern_list_append(struct kern_append_info *info, void *data)
{
        /* alloc a cell */
        pointer cell = scm_mk_ptr(info->sc, data);

        /* make it a list element */
        cell = _cons(info->sc, cell, info->sc->NIL, 0);
        
        /* add it to the list */
        if (info->head == info->sc->NIL) {
                info->head = cell;
                info->tail = cell;

                /* Protect the head from garbage collection. As long as the
                 * head is protected the entire list is protected. The caller
                 * must unprotect the head just before returning the list back
                 * to scheme, so the collector will clean it up when the script
                 * no longer needs it. */
                scm_protect(info->sc, cell);

        } else {
                info->tail->_object._cons._cdr = cell;
                info->tail = cell;
        }  
}

/**
 * Used by kern-char-get-skills to add all the skills in a skill set to the
 * list.
 */
static void kern_add_skill_set(struct kern_append_info *info, int pclvl, 
                               struct skill_set *skset)
{
        struct list *elem;

        /* for each skill in the skill set */
        list_for_each(&skset->skills, elem) {

                struct skill_set_entry *ssent;
                ssent = list_entry(elem, struct skill_set_entry, list);

                /* is the character of sufficient level? */
                if (pclvl < ssent->level) {
                        continue;
                }

                kern_list_append(info, ssent->skill);
        }
}

/**
 * A generic append-to-scheme-list function.
 */
static void kern_list_append_pointer(struct kern_append_info *info, pointer cell)
{
        /* make it a list element */
        cell = _cons(info->sc, cell, info->sc->NIL, 0);
        
        /* add it to the list */
        if (info->head == info->sc->NIL) {
                info->head = cell;
                info->tail = cell;

                /* Protect the head from garbage collection. As long as the
                 * head is protected the entire list is protected. The caller
                 * must unprotect the head just before returning the list back
                 * to scheme, so the collector will clean it up when the script
                 * no longer needs it. */
                scm_protect(info->sc, cell);

        } else {
                info->tail->_object._cons._cdr = cell;
                info->tail = cell;
        }  
}

/**
 * A generic pagination function.
 * Copies one line of text to output.
 * output should be pre-allocated, and large enough to glom all of input if necessary
 * returns a pointer offset to the location the search finished at
 */
static char* kern_paginate_text(char *input, char *output)
{
	int curlength = 0;
	char* endinput = NULL;
	char* endoutput = output;
	while (*input != 0)
	{
		if (isspace(*input))
		{
			endinput = input;
			endoutput = output;
			//printf("[ ]");
		}
		else if (*input == '^') // handle colour codes
		{
			//printf("[col:");
			*output=*input;
			input++;
			output++;
			if (*input == 0) break;
			if (*input != 'c')
			{
				//printf("?%c]",*input);
				*output=*input;
				input++;
				output++;
				continue;
			}
			*output=*input;
			input++;
			output++;
			if (*input == 0) break;
			if (*input == '+')
			{
				//printf("+");
				*output=*input;
				input++;
				output++;				
				if (*input == 0) break;
			}
			//printf("%c]",*input);
			*output=*input;
			input++;
			output++;
			continue;
		}
		else
		{
			//printf("%c",*input);	
		}
		if (curlength >= STAT_CHARS_PER_LINE)
		{		
			//printf("[LEN]\n");
			if (endinput != NULL)
			{
				*endoutput = '\0';
				return (endinput + 1);
			}
			else
			{
				*output = '\0';
				return input;
			}
		}
		*output=*input;
		curlength++;
		input++;
		output++;
	}
	//printf("[EOS]\n");
	*output='\0';
	return input;
}

KERN_API_CALL(kern_ui_paginate_text)
{
	struct kern_append_info info;
	
	/* initialize the context used by the callback to append objects */
	info.sc = sc;
	info.head = sc->NIL;
	info.tail = sc->NIL;
	info.filter = NULL;
	info.data = NULL;
	
	while (scm_is_pair(sc, args))
	{
		char *line;	
		if (unpack(sc, &args, "s", &line))
		{
			rt_err("kern-ui-paginate-text: bad text line");
			break;
		}
		// shortcut empty strings
		if (*line == '\0')
		{
			pointer paginated_string_element=scm_mk_string(sc,line);
			kern_list_append_pointer(&info, paginated_string_element);
			continue;
		}
		
		char *buffer;
		int totallen = strlen(line);
		buffer = (char *)malloc((1+totallen)*sizeof(char));
		assert(buffer);
		assert(line[totallen]==0);	
		
		char* seek=line;
		while (*seek != 0)
		{
			*buffer='\0';
			seek = kern_paginate_text(seek,buffer);
			pointer paginated_string_element=scm_mk_string(sc,buffer);
			kern_list_append_pointer(&info, paginated_string_element);
		}
		
		free(buffer);
	}
	
	/* unprotect the list prior to return */
	if (info.head != sc->NIL)
	    scm_unprotect(sc, info.head);

	return info.head;
}


KERN_API_CALL(kern_char_get_skills)
{
        class Character *subj;
        struct kern_append_info info;

        /* unpack the character */
        subj = (class Character*)unpack_obj(sc, &args, 
                                                 "kern-char-get-skills");
        if (!subj) {
                return sc->NIL;
        }
        
        /* initialize the relevant list-building info */
        memset(&info, 0, sizeof(info));
        info.sc = sc;
        info.head = sc->NIL;
        info.tail = sc->NIL;

        /* add species skills */
        if (subj->species
            && subj->species->skills) {
                kern_add_skill_set(&info, subj->getLevel(), 
                                   subj->species->skills);
        }

        /* add occupation skills */
        if (subj->occ
            && subj->occ->skills) {
                kern_add_skill_set(&info, subj->getLevel(), 
                                   subj->occ->skills);
        }
        
        /* allow the list to be gc'd when the script is done with it */
        if (info.head != sc->NIL) {
                scm_unprotect(sc, info.head);
        }

        return info.head;
}

static pointer kern_build_arm_list(scheme *sc, 
                                   class Character *character, 
                                   class ArmsType *arm,
                                   int *armsIndex)
{
        /* base case */
        if (! arm)
                return sc->NIL;
        
        /* recursive case */
        return _cons(sc, 
                     scm_mk_ptr(sc, arm), 
                     kern_build_arm_list(sc, 
                                         character, 
                                         character->getNextArms(armsIndex),
                                         armsIndex), 
                     0);
}

KERN_API_CALL(kern_char_get_arms)
{
        class Character *character;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, 
                                                 "kern-char-get-arms");
        if (!character)
                return sc->NIL;

        /* recursively enumerate the character's available arms into a
         * scheme list */
		int armsIndex=0;
        return kern_build_arm_list(sc, 
                                      character, 
                                      character->enumerateArms(&armsIndex),
									  &armsIndex);
}

KERN_API_CALL(kern_char_arm_self)
{
        class Character *character;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, 
                                                 "kern-char-get-weapons");
        if (!character)
                return sc->NIL;

        /* recursively enumerate the character's available weapons into a
         * scheme list */
        character->armThyself();

        return scm_mk_ptr(sc, character);
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
        character = (class Character*)unpack_obj(sc, &args, 
                                                 "kern-char-get-inventory");
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
        character = (class Character*)unpack_obj(sc, &args, 
                                                 "kern-char-get-hp");
        if (!character)
                return sc->NIL;

        return scm_mk_integer(sc, character->getHp());
}

KERN_API_CALL(kern_obj_get_hp)
{
        class Object *kobj;

        /* unpack the character */
        kobj = (class Object*)unpack_obj(sc, &args, 
                                                 "kern-obj-get-hp");
        if (!kobj)
                return sc->NIL;

        return scm_mk_integer(sc, kobj->getHp());
}

KERN_API_CALL(kern_char_get_max_hp)
{
        class Character *character;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, 
                                                 "kern-char-get-max-hp");
        if (!character)
                return sc->NIL;

        return scm_mk_integer(sc, character->getMaxHp());
}

KERN_API_CALL(kern_char_get_max_mana)
{
        class Character *character;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, 
                                                 "kern-char-get-max-mana");
        if (!character)
                return sc->NIL;

        return scm_mk_integer(sc, character->getMaxMana());
}

KERN_API_CALL(kern_char_get_level)
{
        class Character *character;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, "kern-char-get-level");
        if (!character)
                return sc->NIL;

        return scm_mk_integer(sc, character->getLevel());
}


KERN_API_CALL(kern_char_get_experience_value)
{
        class Character *character;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, "kern-char-get-level");
        if (!character)
                return sc->NIL;

        return scm_mk_integer(sc, character->getExperienceValue());
}

KERN_API_CALL(kern_char_set_level)
{
        class Character *character;
        int val = 0;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, "kern-char-set-level");
        if (!character)
                return sc->NIL;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-char-set-level: bad args");
                return sc->NIL;
        }

        character->setLevel(val);
        return scm_mk_ptr(sc, character);
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

KERN_API_CALL(kern_char_get_dexterity)
{
        class Character *character;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, "kern-char-get-dexterity");
        if (!character)
                return sc->NIL;

        return scm_mk_integer(sc, character->getDexterity());
}

KERN_API_CALL(kern_char_get_intelligence)
{
        class Character *character;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, "kern-char-get-intelligence");
        if (!character)
                return sc->NIL;

        return scm_mk_integer(sc, character->getIntelligence());

}

KERN_API_CALL(kern_char_get_base_strength)
{
        class Character *character;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, "kern-char-get-strength");
        if (!character)
                return sc->NIL;

        return scm_mk_integer(sc, character->getBaseStrength());
}

KERN_API_CALL(kern_char_get_base_dexterity)
{
        class Character *character;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, "kern-char-get-dexterity");
        if (!character)
                return sc->NIL;

        return scm_mk_integer(sc, character->getBaseDexterity());
}

KERN_API_CALL(kern_char_get_base_intelligence)
{
        class Character *character;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, "kern-char-get-intelligence");
        if (!character)
                return sc->NIL;

        return scm_mk_integer(sc, character->getBaseIntelligence());

}

KERN_API_CALL(kern_char_set_strength)
{
        class Character *character;
        int val = 0;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, "kern-char-set-strength");
        if (!character)
                return sc->NIL;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-char-set-strength: bad args");
                return sc->NIL;
        }

	character->setStrength(val);
        return scm_mk_ptr(sc, character);
}

KERN_API_CALL(kern_char_set_dexterity)
{
        class Character *character;
        int val = 0;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, "kern-char-set-dexterity");
        if (!character)
                return sc->NIL;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-char-set-dexterity: bad args");
                return sc->NIL;
        }

	character->setDexterity(val);
        return scm_mk_ptr(sc, character);
}

KERN_API_CALL(kern_char_set_intelligence)
{
        class Character *character;
        int val = 0;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, "kern-char-set-intelligence");
        if (!character)
                return sc->NIL;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-char-set-intelligence: bad args");
                return sc->NIL;
        }

	character->setIntelligence(val);
        return scm_mk_ptr(sc, character);
}

KERN_API_CALL(kern_char_get_speed)
{
        class Character *character;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, "kern-char-get-speed");
        if (!character)
                return sc->NIL;

        return scm_mk_integer(sc, character->getSpeed());
}

KERN_API_CALL(kern_char_set_speed)
{
        class Character *character;
        int val = 0;

        /* unpack the character */
        character = (class Character*)unpack_obj(sc, &args, "kern-char-set-speed");
        if (!character)
                return sc->NIL;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-char-set-speed: bad args");
                return sc->NIL;
        }

	character->setSpeed(val);
        return scm_mk_ptr(sc, character);
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
                rt_err("kern-arms-type-get-range");
                return scm_mk_integer(sc, 0);
        }

        if (! type) {
                rt_err("kern-arms-type-get-range: null type");
                return scm_mk_integer(sc, 0);
        }

        /* get the range */
        return scm_mk_integer(sc, type->getRange());
}

KERN_API_CALL(kern_arms_type_get_ammo_type)
{
        class ArmsType *type;
        class ObjectType *ammo;

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

KERN_API_CALL(kern_arms_type_set_mmode)
{
        class ArmsType *type;
        struct mmode *mmode;
        
        /* unpack the type (should be an arms type, but no way to safely
         * tell) */
        if (unpack(sc, &args, "pp", &type, &mmode)) {
                rt_err("kern-arms-type-set-mmode");
                return sc->NIL;
        }

        if (! type) {
                rt_err("kern-arms-type-set-mmode: null type");
                return sc->NIL;
        }

        type->setMovementMode(mmode);

        return sc->NIL;
}


KERN_API_CALL(kern_arms_type_fire_in_direction)
{
	class ArmsType *type;
	struct place *place;
	int startx, starty;
	int dx,dy;
	
	/* unpack the type (should be an arms type, but no way to safely
	* tell) */
	if (unpack(sc, &args, "p", &type)) {
		rt_err("kern_arms_type_fire_in_direction");
		return sc->NIL;
	}
	
	if (unpack_loc(sc, &args, &place, &startx,&starty, "kern_arms_type_fire_in_direction"))
		return sc->NIL;
	
	if (unpack(sc, &args, "dd", &dx,&dy)) {
		rt_err("kern_arms_type_fire_in_direction");
		return sc->NIL;
	}
		
	if (! type) {
		rt_err("kern_arms_type_fire_in_direction: null type");
		return sc->NIL;
	}
	
	if (! place) {
		rt_err("kern_arms_type_fire_in_direction: null place");
		return sc->NIL;
	}
	
	type->fireInDirection(place, startx, starty, dx, dy, NULL);
	
	return sc->NIL;
}

KERN_API_CALL(kern_obj_move)
{
	class Object *object;
	int dx, dy;
	enum MoveResult result;
	
	object = (Object*)unpack_obj(sc, &args, "kern-obj-move");
	if (!object)
		return sc->F;
	
	if (unpack(sc, &args, "dd", &dx, &dy))
	{
		rt_err("kern-obj-move: bad args");
		return sc->F;
	}
	
	//moves can have nasty consequences,
	//so keep our own ref to the object for a bit
	obj_inc_ref(object);
	result = object->move(dx, dy);
	obj_dec_ref(object);
	
	switch (result)
	{
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

KERN_API_CALL(kern_ticks_per_turn)
{
        return scm_mk_integer(sc, session_ticks_per_turn());
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
                         * protection any more */
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

KERN_API_CALL(kern_player_get_food)
{
        return scm_mk_integer(sc, player_party->food);
}

KERN_API_CALL(kern_player_set_food)
{
        int val;

        if (unpack(sc, &args, "d", &val)) {
                rt_err("kern-player-set-food: bad args");
                return sc->F;
        }

        player_party->food = val;
        foogodRepaint();
        return sc->T;
}

KERN_API_CALL(kern_begin_combat)
{
        struct move_info info;
        struct combat_info cinfo;
        class Party *party;
        struct place *place;
        int x, y;

        if (unpack_loc(sc, &args, &place, &x, &y, "kern-begin-combat")) {
                return sc->NIL;
        }

        party = (class Party*)unpack_obj(sc, &args, "kern-begin-combat");
        if (!party)
                return sc->NIL;

        /* Combat expects the npc party to have valid coords, whereas I don't
         * expect the script to always put the npc party on the map before
         * calling this function, so force the location of the npc party to the
         * location specified. */
        party->setPlace(place);
        party->setX(x);
        party->setY(y);

        memset(&info, 0, sizeof(info));
        info.place = place;
        info.x = x;
        info.y = y;
        info.dx = party->getDx();
        info.dy = party->getDy();
        info.px = player_party->getX();
        info.py = player_party->getY();
        info.npc_party = party;
                
        /* If the npc party has a null or invalid direction vector (this is the
         * case with an ambush) then use the opposite of the player's direction
         * vector. */
        if ((!info.dx && !info.dy) ||
            (info.dx && info.dy)) {
                info.dx = - player_party->getDx();
                info.dy = - player_party->getDy();
        }

        memset(&cinfo, 0, sizeof(cinfo));
        cinfo.defend = true;
        cinfo.move = &info;
                
        combat_enter(&cinfo);
        return sc->T;
}

KERN_API_CALL(kern_ambush_while_camping)
{
        class Party *party;
        int dx, dy;
        struct place *place;

        /* we need to be in town or wilderness combat for this to work; this
         * will leak memory if it fails and the caller does not destroy the
         * party */
        if (place_is_wilderness(Place)) {
                rt_err("kern-ambush-while-camping: not in combat");
                return sc->F;
        }

        /* unpack the npc party */
        party = (class Party*)unpack_obj(sc, &args, "kern-ambush-while-camping");
        if (!party)
                return sc->F;
        
        if (unpack(sc, &args, "p", &place)) {
                rt_err("kern-ambush-while-camping: bad args");
                return sc->F;
        }

        if (! place) {
                rt_err("kern-ambush-while-camping: null place");
                return sc->F;
        }

        /* Workaround for 1808708: if both the player and npc party are in
         * vehicles then don't do the normal ambush routine (the combat map
         * will be wrong). Instead, let's just wake the player up. */
        if (party->getVehicle()
            && player_party->getVehicle()) {

                log_begin(0);
                Session->subject = player_party;
                party->describe();
                log_continue(" approaches!");
                log_end(0);

                player_party->endCamping();
                player_party->removeMembers();
                return sc->T;
        }

        /* If the npc party has a null or invalid direction vector then
         * generate a random one. */
        dx = party->getDx();
        dy = party->getDy();
        while (! dx && ! dy) {
                /* gmcnutt: the following is not random. Was there a reason for
                 * it? */
                //dx = - player_party->getDx();
                //dy = - player_party->getDy();
                dx = (rand() % 3) - 1;
                dy = (rand() % 3) - 1;
        }

        /* Partial bugfix for 1612006: If the player is on impassable terrain,
         * and the npc is in a vehicle, disembark first, and move them directly
         * over the player's location. This prevents ships from coming ashore
         * onto your camping map (and bringing part of the ocean with them). */
        if (! place_is_passable(player_party->getPlace(), 
                                player_party->getX(), 
                                player_party->getY(), 
                                party, 0)
            && party->getVehicle()) {
                int newx = player_party->getX();
                int newy = player_party->getY();
                party->disembark();
                place_move_object(party->getPlace(), party, newx, newy);
                party->setX(newx);
                party->setY(newy);
        }

        if (combat_add_party(party, dx, dy, 0, place, 0, 0)) {
                player_party->ambushWhileCamping();
                return sc->T;
        }
        return sc->F;
}

/*
 * kern_being_pathfind_to -- wrapper for Being::pathfindTo
 */
KERN_API_CALL(kern_being_pathfind_to)
{
	class Being *being;
	struct place *place;
	int x, y;
	
	/* unpack being */
	being = (class Being*)unpack_obj(sc, &args, "kern-being-pathfind-to");
	if (! being)
		return sc->F;
	
	/* unpack destination */
	if (unpack_loc(sc, &args, &place, &x, &y, "kern-being-pathfind-to"))
		return sc->F;
	
	//moves can have nasty consequences,
	//so keep our own ref to the object for a bit
	obj_inc_ref(being);
		
		
	/* pathfind */
	if (being->pathfindTo(place, x, y))
	{
		obj_dec_ref(being);
		return sc->T;
	}
	obj_dec_ref(being);
	return sc->F;
}

KERN_API_CALL(kern_get_player)
{
        return scm_mk_ptr(sc, player_party);
}

KERN_API_CALL(kern_species_get_hp_mod)
{
        struct species *species;

        if (unpack(sc, &args, "p", &species)) {
                rt_err("kern-species-get-hp-mod: bad args");
                return scm_mk_integer(sc, 0);
        }

        if (! species) {
                rt_err("kern-species-get-hp-mod: null species");
                return scm_mk_integer(sc, 0);
        }

        return scm_mk_integer(sc, species->hp_mod);
}

KERN_API_CALL(kern_species_get_hp_mult)
{
        struct species *species;

        if (unpack(sc, &args, "p", &species)) {
                rt_err("kern-species-get-hp-mult: bad args");
                return scm_mk_integer(sc, 0);
        }

        if (! species) {
                rt_err("kern-species-get-hp-mult: null species");
                return scm_mk_integer(sc, 0);
        }

        return scm_mk_integer(sc, species->hp_mult);
}

KERN_API_CALL(kern_species_get_mp_mod)
{
        struct species *species;

        if (unpack(sc, &args, "p", &species)) {
                rt_err("kern-species-get-mp-mod: bad args");
                return scm_mk_integer(sc, 0);
        }

        if (! species) {
                rt_err("kern-species-get-mp-mod: null species");
                return scm_mk_integer(sc, 0);
        }

        return scm_mk_integer(sc, species->mp_mod);
}

KERN_API_CALL(kern_species_get_mp_mult)
{
        struct species *species;

        if (unpack(sc, &args, "p", &species)) {
                rt_err("kern-species-get-mp-mult: bad args");
                return scm_mk_integer(sc, 0);
        }

        if (! species) {
                rt_err("kern-species-get-mp-mult: null species");
                return scm_mk_integer(sc, 0);
        }

        return scm_mk_integer(sc, species->mp_mult);
}

KERN_API_CALL(kern_occ_get_hp_mod)
{
        struct occ *occ;

        if (unpack(sc, &args, "p", &occ)) {
                rt_err("kern-occ-get-hp-mod: bad args");
                return scm_mk_integer(sc, 0);
        }

        if (! occ) {
                rt_err("kern-occ-get-hp-mod: null occ");
                return scm_mk_integer(sc, 0);
        }

        return scm_mk_integer(sc, occ->hp_mod);
}

KERN_API_CALL(kern_occ_get_hp_mult)
{
        struct occ *occ;

        if (unpack(sc, &args, "p", &occ)) {
                rt_err("kern-occ-get-hp-mult: bad args");
                return scm_mk_integer(sc, 0);
        }

        if (! occ) {
                rt_err("kern-occ-get-hp-mult: null occ");
                return scm_mk_integer(sc, 0);
        }

        return scm_mk_integer(sc, occ->hp_mult);
}

KERN_API_CALL(kern_occ_get_mp_mod)
{
        struct occ *occ;

        if (unpack(sc, &args, "p", &occ)) {
                rt_err("kern-occ-get-mp-mod: bad args");
                return scm_mk_integer(sc, 0);
        }

        if (! occ) {
                rt_err("kern-occ-get-mp-mod: null occ");
                return scm_mk_integer(sc, 0);
        }

        return scm_mk_integer(sc, occ->mp_mod);
}

KERN_API_CALL(kern_occ_get_mp_mult)
{
        struct occ *occ;

        if (unpack(sc, &args, "p", &occ)) {
                rt_err("kern-occ-get-mp-mult: bad args");
                return scm_mk_integer(sc, 0);
        }

        if (! occ) {
                rt_err("kern-occ-get-mp-mult: null occ");
                return scm_mk_integer(sc, 0);
        }

        return scm_mk_integer(sc, occ->mp_mult);
}

KERN_API_CALL(kern_occ_get_gob)
{
        struct occ *occ;

        if (unpack(sc, &args, "p", &occ)) {
                rt_err("kern-occ-get-gob: bad args");
                return sc->NIL;
        }

        if (! occ) {
                rt_err("kern-occ-get-gob: null occ");
                return sc->NIL;
        }

		if (occ->gob == NULL)
		{
			return sc->NIL;
		}

        // It's already a scheme pointer so just return it directly
        return occ->gob->p;
}

KERN_API_CALL(kern_occ_set_gob)
{

        struct occ *occ;
		
        if (unpack(sc, &args, "p", &occ)) {
                rt_err("kern-occ-set-gob: bad args");
                return sc->NIL;
        }
		
        if (! scm_is_pair(sc, args)) {
               rt_err("kern-occ-set-gob: no gob specified");
               return sc->NIL;
        }

       occ->gob = gob_new(sc, scm_car(sc, args));

       return sc->NIL;
}

KERN_API_CALL(kern_end_game)
{
        Quit = true;
        return sc->NIL;
}

KERN_API_CALL(kern_sprite_clone)
{
        struct sprite *orig, *clone;
        char *tag;

        if (unpack(sc, &args, "py", &orig, &tag)) {
                rt_err("kern-sprite-clone: bad args");
                return sc->NIL;
        }
        clone = sprite_clone(orig, tag);
        if (clone) {
                pointer ret = scm_mk_ptr(sc, clone);
                session_add(Session, clone, sprite_dtor, NULL, NULL);

                /* Tags are optional on clones, but only clones with tags will
                 * be assigned to scheme variables. */
                if (tag) {
                        scm_define(sc, tag, ret);
                }
                return ret;
        }
        return sc->NIL;
}

KERN_API_CALL(kern_sprite_append_decoration)
{
        struct sprite *orig, *decor;

        if (unpack(sc, &args, "pp", &orig, &decor)) {
                rt_err("kern-sprite-append-decoration: bad args");
                return sc->NIL;
        }
        if (!orig || ! decor) {
                rt_err("kern-sprite-append-decoration: null arg");
                return sc->NIL;
        }
        sprite_append_decoration(orig, decor);
        return scm_mk_ptr(sc, orig);
}

KERN_API_CALL(kern_sprite_blit_over)
{
        struct sprite *orig, *decor;

        if (unpack(sc, &args, "pp", &orig, &decor)) {
                rt_err("kern-sprite-append-decoration: bad args");
                return sc->NIL;
        }
        if (!orig || ! decor) {
                rt_err("kern-sprite-append-decoration: null arg");
                return sc->NIL;
        }
        sprite_blit_over(orig, decor);
        return scm_mk_ptr(sc, orig);
}

KERN_API_CALL(kern_sprite_strip_decorations)
{
        struct sprite *orig;

        if (unpack(sc, &args, "p", &orig)) {
                rt_err("kern-sprite-strip-decoration: bad args");
                return sc->NIL;
        }
        sprite_strip_decorations(orig);
        return scm_mk_ptr(sc, orig);
}

KERN_API_CALL(kern_sprite_apply_matrix)
{
        struct sprite *sprite;
        float matrix[4][3];
        int row;
        pointer pcol;

        /* unpack the sprite */
        if (unpack(sc, &args, "p", &sprite)) {
                load_err("kern-sprite-apply-matrix: bad args");
                return sc->NIL;
        }

        if (!scm_is_pair(sc, args)) {
                load_err("kern-sprite-apply-matrix: no matrix!");
                goto abort;
        }
        args = scm_car(sc, args);

        /* unpack the matrix */
        for (row = 0; row < 4; row++) {
                if (! scm_is_pair(sc, args)) {
                        load_err("kern-sprite-apply-matrix: only %d of 4 rows!", row);
                        goto abort;
                }
                pcol = scm_car(sc, args);
                args = scm_cdr(sc, args);
                if (unpack(sc, &pcol, "fff", &matrix[row][0],
                           &matrix[row][1],
                           &matrix[row][2])) {
                        load_err("kern-sprite-apply-matrix: bad args in row %d!", row);
                        goto abort;
                }
        }

        sprite_apply_matrix(sprite, matrix);
 abort:
        return scm_mk_ptr(sc, sprite);
}


KERN_API_CALL(kern_los_invalidate)
{
        vmask_flush_all();
        return sc->T;
}

KERN_API_CALL(kern_cfg_set)
{
        char *key, *val;

        while (scm_is_pair(sc, args)) {
                if (unpack(sc, &args, "ss", &key, &val)) {
                        rt_err("kern-cfg-set: bad args");
                        return sc->NIL;
                }
                cfg_set(key, val);
        }
        return sc->NIL;
}

KERN_API_CALL(kern_cfg_get)
{
        char *key, *val;

        if (unpack(sc, &args, "s", &key)) {
                rt_err("kern-cfg-get: bad args");
                return sc->NIL;
        }
        val = cfg_get(key);
        if (!val)
                return sc->NIL;
        return scm_mk_string(sc, cfg_get(key));
}

KERN_API_CALL(kern_set_kern_intvar)
{
        char *key;
	int   value;

        while (scm_is_pair(sc, args)) {
                if (unpack(sc, &args, "sd", &key, &value)) {
                        rt_err("kern-set-kern-intvar: bad args");
                        return sc->NIL;
                }
		kern_intvar_set(key, value);
        }
        return sc->NIL;
}

KERN_API_CALL(kern_get_kern_intvar)
{
    char *key;
    int   value;

        if (unpack(sc, &args, "s", &key)) {
                rt_err("kern_get_kern_intvar: bad args");
                return sc->NIL;
        }
        value = kern_intvar_get(key);

        return scm_mk_integer(sc, value);
}

KERN_API_CALL(kern_add_save_game)
{
        char *fname;

        while (scm_is_pair(sc, args)) {
                if (unpack(sc, &args, "s", &fname)) {
                        rt_err("kern-add-save-game: bad args");
                }
                menu_add_saved_game(fname);
        }

        return sc->NIL;
}

KERN_API_CALL(kern_type_set_gob)
{
        ObjectType *type = 0;

        if (unpack(sc, &args, "p", &type)) {
                rt_err("kern-type-set-gob: bad args");
                return sc->NIL;
        }

        if (! scm_is_pair(sc, args)) {
               rt_err("kern-type-set-gob: no gob specified");
        } else {
                type->setGob(gob_new(sc, scm_car(sc, args)));
        }

       return scm_mk_ptr(sc, type);
}

KERN_API_CALL(kern_type_set_quest_item_flag)
{
        ObjectType *type = 0;
        int val = 0;

        if (unpack(sc, &args, "pb", &type, &val)) {
                rt_err("kern-type-set-quest-item-flag: bad args");
                return sc->NIL;
        }

        type->setQuestItemFlag(val);
        return sc->NIL;
}

KERN_API_CALL(kern_type_get_gob)
{
        ObjectType *type = 0;

        if (unpack(sc, &args, "p", &type)) {
                rt_err("kern-type-get-gob: bad args");
                return sc->NIL;
        }

        if (!type) {
                rt_err("kern-type-get-gob: null obj");
                return sc->NIL;
        }

        if (! type->getGob()) {
                return sc->NIL;
        }

        return type->getGob()->p;
}

KERN_API_CALL(kern_set_quicken_sprite)
{
        struct sprite *sprite;
        if (unpack(sc, &args, "p", &sprite)) {
                load_err("kern-set-quicken-sprite: bad args");
                return sc->F;
        }
        quicken_effect_sprite() = sprite;
        return sc->T;
}

KERN_API_CALL(kern_set_magic_negated_sprite)
{
        struct sprite *sprite;
        if (unpack(sc, &args, "p", &sprite)) {
                load_err("kern-set-magic-negated-sprite: bad args");
                return sc->F;
        }
        magic_negated_effect_sprite() = sprite;
        return sc->T;
}

KERN_API_CALL(kern_set_reveal_sprite)
{
        struct sprite *sprite;
        if (unpack(sc, &args, "p", &sprite)) {
                load_err("kern-set-reveal-sprite: bad args");
                return sc->F;
        }
        reveal_effect_sprite() = sprite;
        return sc->T;
}

KERN_API_CALL(kern_set_xray_vision_sprite)
{
        struct sprite *sprite;
        if (unpack(sc, &args, "p", &sprite)) {
                load_err("kern-set-xray-vision-sprite: bad args");
                return sc->F;
        }
        xray_vision_effect_sprite() = sprite;
        return sc->T;
}

KERN_API_CALL(kern_set_time_stop_sprite)
{
        struct sprite *sprite;
        if (unpack(sc, &args, "p", &sprite)) {
                load_err("kern-set-time_stop-sprite: bad args");
                return sc->F;
        }
        time_stop_effect_sprite() = sprite;
        return sc->T;
}

KERN_API_CALL(kern_obj_set_mmode)
{
        Object *obj;
        struct mmode *mmode;

        if (!(obj = unpack_obj(sc, &args, "kern-obj-set-mmode")))
                return sc->NIL;

        if (unpack(sc, &args, "p", &mmode)) {
                rt_err("kern-obj-set-mmode: bad args");
                return sc->NIL;
        }

        obj->setMovementMode(mmode);

        return scm_mk_ptr(sc, obj);
        
}

KERN_API_CALL(kern_progress_bar_start)
{
        char *title = 0;
        unsigned int max_steps = 0;

        if (unpack(sc, &args, "sd", &title, &max_steps)) {
                load_err("kern-progress-bar-start: bad args");
                return sc->NIL;
        }

        foogod_progress_bar_set_title(title);
        foogod_progress_bar_set_max_steps(max_steps);
        foogodSetMode(FOOGOD_PROGRESS_BAR);

        return sc->NIL;
}

KERN_API_CALL(kern_progress_bar_advance)
{
        unsigned int steps = 0;

        if (unpack(sc, &args, "d", &steps)) {
                load_err("kern-progress-bar-advance: bad args");
                return sc->NIL;
        }

        foogod_progress_bar_advance(steps);
        foogodRepaint();

        return sc->NIL;
}

KERN_API_CALL(kern_progress_bar_finish)
{
        foogod_progress_bar_finish();
        foogodRepaint();

        return sc->NIL;
}

/**
 * (kern-ztats-add-pane <enter> <scroll> <paint> <gob>)
 *
 * <enter> is (enter <gob> <kparty> <dir> <x> <y> <w> <h>)
 * <scroll> is (scroll <gob> <dir>), returning #t iff the scroll was handled
 * <paint> is (paint <gob>)
 * <gob> is script info
 */
struct kern_ztats_pane {
        struct ztats_pane base;
        struct closure *enter, *scroll, *paint, *select;
        struct gob *gob;
        scheme *sc;
        unsigned char added:1;
};

static pointer pack_rect(scheme *sc, SDL_Rect *rect)
{
        return pack(sc, "dddd", rect->x, rect->y, rect->w, rect->h);
}

static void kern_ztats_pane_enter(struct ztats_pane *pane, class Party *party, enum StatusScrollDir via, 
                           SDL_Rect *dims)
{
        struct kern_ztats_pane *kzp = (struct kern_ztats_pane*)pane;
        pointer prect = pack_rect(kzp->sc, dims);
        scm_protect(kzp->sc, prect);
        closure_exec(kzp->enter, "lpdl", kzp->gob->p, party, via, prect);
        scm_unprotect(kzp->sc, prect);
}

static int kern_ztats_pane_scroll(struct ztats_pane *pane, enum StatusScrollDir dir)
{
        struct kern_ztats_pane *kzp = (struct kern_ztats_pane*)pane;
        return closure_exec(kzp->scroll, "ld", kzp->gob->p, dir);
}

static void kern_ztats_pane_paint(struct ztats_pane *pane)
{
        struct kern_ztats_pane *kzp = (struct kern_ztats_pane*)pane;
        closure_exec(kzp->paint, "l", kzp->gob->p);
}

static void kern_ztats_pane_select(struct ztats_pane *pane)
{
        struct kern_ztats_pane *kzp = (struct kern_ztats_pane*)pane;
        closure_exec(kzp->select, "l", kzp->gob->p);
}

static void kern_ztats_pane_dtor(void *val)
{
        struct kern_ztats_pane *kzp = (struct kern_ztats_pane*)val;

        if (kzp->added) {
                ztats_rm_pane(&kzp->base);
        }

        if (kzp->gob) {
                gob_unref(kzp->gob);
        }
        if (kzp->paint) {
                closure_unref(kzp->paint);
        }
        if (kzp->scroll) {
                closure_unref(kzp->scroll);
        }
        if (kzp->enter) {
                closure_unref(kzp->enter);
        }
        if (kzp->select) {
                closure_unref(kzp->select);
        }
        free(kzp);
}

static struct ztats_pane_ops kern_ztats_pane_ops = {
        kern_ztats_pane_enter,
        kern_ztats_pane_scroll,
        kern_ztats_pane_paint,
        kern_ztats_pane_select
};

KERN_API_CALL(kern_ztats_add_pane)
{
        pointer penter, pscroll, ppaint, pselect, pgob;
        struct kern_ztats_pane *kzp;

#ifndef USE_QUESTS
        return sc->F;
#endif

        if (unpack(sc, &args, "ooool", &penter, &pscroll, &ppaint, &pselect, &pgob)) {
                load_err("kern-ztats-add-pane: bad args");
                return sc->NIL;
        }

        if (!(kzp = (struct kern_ztats_pane*)calloc(1, sizeof(*kzp)))) {
                load_err("alloc failed");
                return sc->NIL;
        }

        kzp->base.ops = &kern_ztats_pane_ops;
        kzp->sc = sc;
        if (! (kzp->enter = closure_new_ref(sc, penter))) {
                goto fail;
        }
        if (! (kzp->scroll = closure_new_ref(sc, pscroll))) {
                goto fail;
        }
        if (! (kzp->paint = closure_new_ref(sc, ppaint))) {
                goto fail;
        }
        if (! (kzp->select = closure_new_ref(sc, pselect))) {
                goto fail;
        }
        if (! (kzp->gob = gob_new(sc, pgob))) {
                goto fail;
        }
        gob_ref(kzp->gob);

        ztats_add_pane(&kzp->base);

        /* Mark that we've added it so that we remember to remove it in the
         * dtor. */
        kzp->added = 1;

        session_add(Session, kzp, kern_ztats_pane_dtor, NULL, NULL);
        return sc->T;

 fail:
        kern_ztats_pane_dtor(kzp);
        return sc->F;
}

KERN_API_CALL(kern_status_set_title)
{
        char *title;
        if (unpack(sc, &args, "s", &title)) {
                load_err("%s: bad args", __FUNCTION__);
                return sc->NIL;
        }

        status_set_title(title);
        return sc->T;
}

/**
 * (kern-screen-print (<x> <y> <w> <h>) <flags> <...>)
 *
 * <x> <y> <w> <h> are the rect (absolute screen coords) to print to
 * <flags> are the SP_* #defines in screen.h
 * <fmt> is std printf format plus the color tag extensions of ascii.h
 * <...> are the varargs
 */
KERN_API_CALL(kern_screen_print)
{
        static char buf[256];
        int room = sizeof(buf);
        char *ptr = buf;
        SDL_Rect rect;
        int flags = 0;

        if (unpack_rect(sc, &args, &rect)) {
                load_err("%s: error unpacking rect", __FUNCTION__);
                return sc->NIL;
        }

        if (unpack(sc, &args, "d", &flags)) {
                load_err("%s: error unpacking flags", __FUNCTION__);
                return sc->NIL;
        }
        
        while (scm_is_pair(sc, args) && (room > 1)) {

                pointer val = scm_car(sc, args);
                args = scm_cdr(sc, args);
                int n = 0;

                if (scm_is_str(sc, val)) {
                        n = snprintf(ptr, room, scm_str_val(sc, val));
                } else if (scm_is_int(sc, val)) {
                        n = snprintf(ptr, room, "%ld", scm_int_val(sc, val));
                } else if (scm_is_real(sc, val)) {
                        n = snprintf(ptr, room, "%f", scm_real_val(sc, val));
                } else {
                        rt_err("%s: unknown type", __FUNCTION__);
                }

                ptr += n;
                room -= n;
        }

        screenPrint(&rect, flags, buf);

        return sc->NIL;
}

/**
 * (kern-screen-draw-sprite (<x> <y> <w> <h>) <flags> <sprite>)
 *
 * <x> <y> <w> <h> are the rect (absolute screen coords) to print to
 * <flags> are the SP_* #defines in screen.h
 * <fmt> is std printf format plus the color tag extensions of ascii.h
 * <sprite> is the sprite to draw
 */
KERN_API_CALL(kern_screen_draw_sprite)
{
        SDL_Rect rect;
        int flags = 0;
		struct sprite *toblit;

        if (unpack_rect(sc, &args, &rect)) {
                load_err("%s: error unpacking rect", __FUNCTION__);
                return sc->NIL;
        }

        if (unpack(sc, &args, "d", &flags)) {
                load_err("%s: error unpacking flags", __FUNCTION__);
                return sc->NIL;
        }
        
        if (unpack(sc, &args, "p", &toblit)) {
                rt_err("kern-sprite-clone: bad args");
                return sc->NIL;
        }
        
        sprite_paint_direct(toblit, 0, &rect);
        
        //screenBlit(toblit->rsurf->surf, &toblit->frames[0], &rect);

        return sc->NIL;
}

/**
 * (kern-screen-shade <rect> <amount>)
 *
 * <rect> specifies the area of the screen to shade
 * <amount> is a value from 0 (transparent) to 255 (opaque black)
 */
KERN_API_CALL(kern_screen_shade)
{
        SDL_Rect rect;
        int amount;

        if (unpack_rect(sc, &args, &rect)) {
                load_err("%s: error unpacking 'screenrect' arg", __FUNCTION__);
                return sc->NIL;
        }

        if (unpack(sc, &args, "d", &amount)) {
                load_err("%s: error unpacking 'amount' arg", __FUNCTION__);
                return sc->NIL;
        }
        
        screenShade(&rect, amount);
        return sc->NIL;
}

KERN_API_CALL(kern_screen_erase)
{
        SDL_Rect rect;

        if (unpack_rect(sc, &args, &rect)) {
                load_err("%s: error unpacking 'screenrect' arg", __FUNCTION__);
                return sc->NIL;
        }

        screenErase(&rect);
        return sc->T;
}

KERN_API_CALL(kern_screen_update)
{
        SDL_Rect rect;

        if (unpack_rect(sc, &args, &rect)) {
                load_err("%s: error unpacking 'screenrect' arg", __FUNCTION__);
                return sc->NIL;
        }

        screenUpdate(&rect);
        return sc->T;
}



/**
 * (kern-event-push-keyhandler <keyh>)
 *
 * <keyh> is a proc of the form (keyh key keymod)
 */
static int kern_keyh_fx(struct KeyHandler *keyh, int key, int keymod)
{
        DECL_CAST(struct closure, proc, keyh->data);
        return closure_exec(proc, "dd", key, keymod) ? 1 : 0;
}

KERN_API_CALL(kern_event_run_keyhandler)
{
        pointer pclos;
        struct closure *proc;

        if (unpack(sc, &args, "o", &pclos)) {
                load_err("%s: error in arg", __FUNCTION__);
                return sc->F;
        }

        if (sc->NIL == pclos) {
                load_err("%s: NIL closure arg", __FUNCTION__);
                return sc->F;
        }

        if (! (proc =closure_new_ref(sc, pclos))) {
                load_err("%s: closure_new failed", __FUNCTION__);
                return sc->F;
        }

        eventRunKeyHandler(kern_keyh_fx, proc);
        closure_unref(proc);
        return sc->T;
}

/**
 * (kern-applet-run <run> <paint>)
 *
 * <run> is a proc of form (run <dims>), dims being the screen rect.
 * <paint> is a proc of form (paint)
 */
struct kern_applet {
        struct applet base;
        struct closure *run, *paint;
        struct gob *gob;
        scheme *sc;
};

static void kern_applet_run(struct applet *applet, SDL_Rect *dims, struct session *session)
{
        DECL_CAST(struct kern_applet, ka, applet);
        pointer prect = pack_rect(ka->sc, dims);
        closure_exec(ka->run, "ll", ka->gob->p, prect);
}

static void kern_applet_paint(struct applet *applet)
{
        DECL_CAST(struct kern_applet, ka, applet);
        closure_exec(ka->paint, "l", ka->gob->p);
}

static void kern_applet_dtor(void *val)
{
        struct kern_applet *ka = (struct kern_applet*)val;
        if (ka->gob) {
                gob_unref(ka->gob);
        }
        if (ka->paint) {
                closure_unref(ka->paint);
        }
        if (ka->run) {
                closure_unref(ka->run);
        }
        KERN_FREE(ka);
}

static struct applet_ops kern_applet_ops = {
        kern_applet_run,
        kern_applet_paint
};

KERN_API_CALL(kern_applet_run)
{
        pointer prun, ppaint, pgob;
        struct kern_applet *ka;

        if (unpack(sc, &args, "ool", &prun, &ppaint, &pgob)) {
                load_err("%s: bad args", __FUNCTION__);
                return sc->NIL;
        }

        if (!(ka = KERN_ALLOC(struct kern_applet))) {
                load_err("%s: alloc failed", __FUNCTION__);
                return sc->NIL;
        }

        ka->base.ops = &kern_applet_ops;
        ka->sc = sc;
        if (! (ka->run = closure_new_ref(sc, prun))) {
                goto fail;
        }
        if (! (ka->paint = closure_new_ref(sc, ppaint))) {
                goto fail;
        }
        if (! (ka->gob = gob_new(sc, pgob))) {
                goto fail;
        }
        gob_ref(ka->gob);

        statusRunApplet(&ka->base);
        kern_applet_dtor(ka);

        return sc->T;

 fail:
        kern_applet_dtor(ka);
        return sc->F;

        
}

/**
 * (kern-define <symbol> <value>)
 *
 * This is a way to define mutable scheme variables that persist across
 * sessions. In other words, you can declare a variable in scheme, change
 * something about it during the game, and rest assured that when the game
 * reloads the value will be the same. By way of contrast, normal statements
 * like (define foo (list 'a 6)) define variables for the current session only.
 */

struct kern_define_data {
        scheme *sc;
        char *sym;
        pointer cell;
};

static void kern_define_dtor(void *val)
{
        DECL_CAST(struct kern_define_data, data, val);
        free(data->sym);
        KERN_FREE(val);
}

static void kern_define_save(save_t *save, void *val)
{
        DECL_CAST(struct kern_define_data, data, val);
        save->write(save, "(kern-define '%s ", data->sym);
        scheme_serialize(data->sc, data->cell, save);
        save->write(save, ")\n");
}

KERN_API_CALL(kern_define)
{
        char *str;
        pointer pcell;
        struct kern_define_data *data;
        
        if (unpack(sc, &args, "yl", &str, &pcell)) {
                load_err("%s: bad args", __FUNCTION__);
                return sc->NIL;
        }
        
        if (!(data = KERN_ALLOC(struct kern_define_data))) {
                load_err("%s: alloc failed", __FUNCTION__);
                return sc->NIL;
        }

        if (!(data->sym = strdup(str))) {
                KERN_FREE(data);
                load_err("%s: strdup failed", __FUNCTION__);
                return sc->NIL;
        }

        data->sc = sc;
        data->cell = pcell;
        session_add(Session, data, kern_define_dtor, kern_define_save, NULL);
        scm_define(sc, str, pcell);
        return sc->NIL;
}

KERN_API_CALL(kern_obj_set_portrait)
{
        class Object *obj;
        struct sprite *sprite;

        if (unpack(sc, &args, "pp", &obj, &sprite)) {
                rt_err("%s: bad args", __FUNCTION__);
                return sc->NIL;
        }

        if (!obj) {
                rt_err("%s: null object", __FUNCTION__);
                return sc->NIL;
        }

        obj->setPortrait(sprite);

        return scm_mk_ptr(sc, obj);

}

KERN_API_CALL(kern_script_version)
{
        char *verstr = NULL;

        if (unpack(sc, &args, "s", &verstr)) {
	        char buffer[40]; // max length of unsigned int = 10 digits
	        sprintf(buffer,"%u.%u.%u",Session->major, Session->minor,Session->release);
	        return scm_mk_string(sc, buffer);
        }

        if (sscanf(verstr, "%u.%u.%u", &Session->major, &Session->minor, 
                   &Session->release) != 3) {
                load_err("%s: bad version string '%s'", __FUNCTION__, verstr);
                return sc->NIL;
        }

        return sc->NIL;
}

KERN_OBSOLETE_CALL(kern_set_ascii);
KERN_OBSOLETE_CALL(kern_set_frame);
KERN_OBSOLETE_CALL(kern_set_cursor);

static int fincount=0; /* for debug */
static void kern_finalize(scheme *sc, pointer pp)
{
        class Object *obj = (class Object*)pp;
        obj_dec_ref(obj);
        fincount++;
}

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

        fincount=0;
        scheme_set_custom_finalize(sc, kern_finalize);

        /* Setup the script-to-kernel API */

        /* kern-arms-type api */
        API_DECL(sc, "kern-arms-type-get-ammo-type", kern_arms_type_get_ammo_type);
        API_DECL(sc, "kern-arms-type-get-range",     kern_arms_type_get_range);
        API_DECL(sc, "kern-arms-type-set-mmode",     kern_arms_type_set_mmode);
        API_DECL(sc, "kern-arms-type-fire-in-direction", kern_arms_type_fire_in_direction);

        /* kern-astral-body api */
        API_DECL(sc, "kern-astral-body-get-gob", kern_astral_body_get_gob);
        API_DECL(sc, "kern-astral-body-get-phase", kern_astral_body_get_phase);
        API_DECL(sc, "kern-astral-body-set-gob", kern_astral_body_set_gob);

        /* kern-being-api */
        API_DECL(sc, "kern-being-get-base-faction", kern_being_get_base_faction);
        API_DECL(sc, "kern-being-get-current-faction", kern_being_get_current_faction);
        API_DECL(sc, "kern-being-get-visible-hostiles", kern_being_get_visible_hostiles);
        API_DECL(sc, "kern-being-get-visible-allies", kern_being_get_visible_allies);
        API_DECL(sc, "kern-being-get-visible-tiles", kern_being_get_visible_tiles);
        API_DECL(sc, "kern-being-is-hostile?", kern_being_is_hostile);
        API_DECL(sc, "kern-being-is-ally?", kern_being_is_ally);
        API_DECL(sc, "kern-being-set-name", kern_being_set_name);
        API_DECL(sc, "kern-being-pathfind-to", kern_being_pathfind_to);
        API_DECL(sc, "kern-being-set-base-faction", kern_being_set_base_faction);
        API_DECL(sc, "kern-being-set-current-faction", kern_being_set_current_faction);

        /* kern-char api */
        API_DECL(sc, "kern-char-add-defense", kern_char_add_defense);
        API_DECL(sc, "kern-char-add-experience", kern_char_add_experience);
        API_DECL(sc, "kern-char-arm-self", kern_char_arm_self);
        API_DECL(sc, "kern-char-attack", kern_char_attack);
        API_DECL(sc, "kern-char-dec-mana", kern_char_dec_mana);
        API_DECL(sc, "kern-char-charm", kern_char_charm);
        API_DECL(sc, "kern-char-force-drop", kern_char_force_drop);
        API_DECL(sc, "kern-char-get-arms", kern_char_get_arms);
        API_DECL(sc, "kern-char-get-experience-value", kern_char_get_experience_value);		
        API_DECL(sc, "kern-char-get-hp", kern_char_get_hp);
        API_DECL(sc, "kern-char-get-inventory", kern_char_get_inventory);
        API_DECL(sc, "kern-char-get-level", kern_char_get_level);
        API_DECL(sc, "kern-char-get-mana", kern_char_get_mana);
        API_DECL(sc, "kern-char-get-occ", kern_char_get_occ);
        API_DECL(sc, "kern-char-get-max-hp", kern_char_get_max_hp);
        API_DECL(sc, "kern-char-get-max-mana", kern_char_get_max_mana);
        API_DECL(sc, "kern-char-get-party", kern_char_get_party);
        API_DECL(sc, "kern-char-get-readied-weapons", kern_char_get_readied_weapons);
        API_DECL(sc, "kern-char-get-species", kern_char_get_species);
        API_DECL(sc, "kern-char-get-strength", kern_char_get_strength);
        API_DECL(sc, "kern-char-get-dexterity", kern_char_get_dexterity);
        API_DECL(sc, "kern-char-get-intelligence", kern_char_get_intelligence);
        API_DECL(sc, "kern-char-get-base-strength", kern_char_get_base_strength);
        API_DECL(sc, "kern-char-get-base-dexterity", kern_char_get_base_dexterity);
        API_DECL(sc, "kern-char-get-base-intelligence", kern_char_get_base_intelligence);
        API_DECL(sc, "kern-char-set-sched", kern_char_set_sched);
        API_DECL(sc, "kern-char-set-speed", kern_char_set_speed);
        API_DECL(sc, "kern-char-get-speed", kern_char_get_speed);
        API_DECL(sc, "kern-char-set-strength", kern_char_set_strength);
        API_DECL(sc, "kern-char-set-dexterity", kern_char_set_dexterity);
        API_DECL(sc, "kern-char-set-intelligence", kern_char_set_intelligence);
        API_DECL(sc, "kern-char-task-abort", kern_char_task_abort);
        API_DECL(sc, "kern-char-task-begin", kern_char_task_begin);
        API_DECL(sc, "kern-char-task-continue", kern_char_task_continue);
        API_DECL(sc, "kern-char-task-end", kern_char_task_end);
        API_DECL(sc, "kern-char-get-skills", kern_char_get_skills);
        API_DECL(sc, "kern-char-get-weapons", kern_char_get_weapons);
        API_DECL(sc, "kern-char-is-asleep?", kern_char_is_asleep);
        API_DECL(sc, "kern-char-is-dead?", kern_char_is_dead);
        API_DECL(sc, "kern-char-is-known?", kern_char_is_known);
        API_DECL(sc, "kern-char-join-player", kern_char_join_player);
        API_DECL(sc, "kern-char-kill", kern_char_kill);
        API_DECL(sc, "kern-char-leave-player", kern_char_leave_player);
        API_DECL(sc, "kern-char-resurrect", kern_char_resurrect);
        API_DECL(sc, "kern-char-set-ai", kern_char_set_ai);
        API_DECL(sc, "kern-char-set-control-mode", kern_char_set_control_mode);
        API_DECL(sc, "kern-char-set-fleeing", kern_char_set_fleeing);
        API_DECL(sc, "kern-char-set-hp", kern_char_set_hp);
        API_DECL(sc, "kern-char-set-known", kern_char_set_known);
        API_DECL(sc, "kern-char-set-level", kern_char_set_level);
        API_DECL(sc, "kern-char-set-mana", kern_char_set_mana);
        API_DECL(sc, "kern-char-set-player-controlled", kern_char_set_player_controlled);
        API_DECL(sc, "kern-char-set-schedule", kern_char_set_schedule);
        API_DECL(sc, "kern-char-set-sleep", kern_char_set_sleep);
        API_DECL(sc, "kern-char-uncharm", kern_char_uncharm);
        API_DECL(sc, "kern-char-unready", kern_char_unready);

        /* kern-event api */
        API_DECL(sc, "kern-event-run-keyhandler", kern_event_run_keyhandler);

        /* kern-map api */
        API_DECL(sc, "kern-map-rotate", kern_map_rotate);

        /* kern-mk api */
        API_DECL(sc, "kern-mk-arms-type", kern_mk_arms_type);
        API_DECL(sc, "kern-mk-astral-body", kern_mk_astral_body);
        API_DECL(sc, "kern-mk-blender", kern_mk_blender);
        API_DECL(sc, "kern-mk-char", kern_mk_char);
        API_DECL(sc, "kern-mk-inventory", kern_mk_inventory);
        API_DECL(sc, "kern-mk-effect", kern_mk_effect);
        API_DECL(sc, "kern-mk-field", kern_mk_field);
        API_DECL(sc, "kern-mk-field-type", kern_mk_field_type);
        API_DECL(sc, "kern-mk-map", kern_mk_map);
        API_DECL(sc, "kern-mk-missile-type", kern_mk_missile_type);
        API_DECL(sc, "kern-mk-composite-map", kern_mk_composite_map);
        API_DECL(sc, "kern-mk-mmode", kern_mk_mmode);
        API_DECL(sc, "kern-mk-obj", kern_mk_obj);
        API_DECL(sc, "kern-mk-obj-type", kern_mk_obj_type);
        API_DECL(sc, "kern-mk-occ", kern_mk_occ);
        API_DECL(sc, "kern-mk-palette", kern_mk_palette);
        API_DECL(sc, "kern-mk-party", kern_mk_party);
        API_DECL(sc, "kern-mk-place", kern_mk_place);
        API_DECL(sc, "kern-mk-player", kern_mk_player);
        API_DECL(sc, "kern-mk-ptable", kern_mk_ptable);
        API_DECL(sc, "kern-mk-sched", kern_mk_sched);
        API_DECL(sc, "kern-mk-skill", kern_mk_skill);
        API_DECL(sc, "kern-mk-skill-set", kern_mk_skill_set);
        API_DECL(sc, "kern-mk-sound", kern_mk_sound);
        API_DECL(sc, "kern-mk-species", kern_mk_species);
        API_DECL(sc, "kern-mk-sprite", kern_mk_sprite);
        API_DECL(sc, "kern-mk-sprite-set", kern_mk_sprite_set);
        API_DECL(sc, "kern-mk-templ", kern_mk_templ);
        API_DECL(sc, "kern-mk-terrain", kern_mk_terrain);
        API_DECL(sc, "kern-mk-vehicle", kern_mk_vehicle);
        API_DECL(sc, "kern-mk-vehicle-type", kern_mk_vehicle_type);

        /* kern-obj api */
        API_DECL(sc, "kern-obj-add-food", kern_obj_add_food);
        API_DECL(sc, "kern-obj-add-gold", kern_obj_add_gold);
        API_DECL(sc, "kern-obj-add-effect", kern_obj_add_effect);
        API_DECL(sc, "kern-obj-add-to-inventory", kern_obj_add_to_inventory);
        API_DECL(sc, "kern-obj-apply-damage", kern_obj_apply_damage);
        API_DECL(sc, "kern-obj-inflict-damage", kern_obj_inflict_damage);
        API_DECL(sc, "kern-obj-clone", kern_obj_clone);
        API_DECL(sc, "kern-obj-dec-ap", kern_obj_dec_ap);
        API_DECL(sc, "kern-obj-dec-light", kern_obj_dec_light);
        API_DECL(sc, "kern-obj-dec-ref", kern_obj_dec_ref);
        /*API_DECL(sc, "kern-obj-destroy", kern_obj_destroy);*/
        API_DECL(sc, "kern-obj-find-path", kern_obj_find_path);
        API_DECL(sc, "kern-obj-get-activity", kern_obj_get_activity);
        API_DECL(sc, "kern-obj-get-ap", kern_obj_get_ap);
        API_DECL(sc, "kern-obj-get-count", kern_obj_get_count);
        API_DECL(sc, "kern-obj-get-dir", kern_obj_get_dir);
        API_DECL(sc, "kern-obj-get-hp", kern_obj_get_hp);
        API_DECL(sc, "kern-obj-get-effects", kern_obj_get_effects);
        API_DECL(sc, "kern-obj-get-facing", kern_obj_get_facing);
        API_DECL(sc, "kern-obj-get-gob", kern_obj_get_gob);
        API_DECL(sc, "kern-obj-get-light", kern_obj_get_light);
        API_DECL(sc, "kern-obj-get-location", kern_obj_get_location);
        API_DECL(sc, "kern-obj-get-mmode", kern_obj_get_mmode);
        API_DECL(sc, "kern-obj-get-movecost", kern_obj_get_movecost);
        API_DECL(sc, "kern-obj-get-name", kern_obj_get_name);
        API_DECL(sc, "kern-obj-get-sprite", kern_obj_get_sprite);
        API_DECL(sc, "kern-obj-get-type", kern_obj_get_type);
        API_DECL(sc, "kern-obj-get-vision-radius", kern_obj_get_vision_radius);
        API_DECL(sc, "kern-obj-has?", kern_obj_has);
        API_DECL(sc, "kern-obj-heal", kern_obj_heal);
        API_DECL(sc, "kern-obj-inc-light", kern_obj_inc_light);
        API_DECL(sc, "kern-obj-inc-ref", kern_obj_inc_ref);
        API_DECL(sc, "kern-obj-is-being?", kern_obj_is_being);
        API_DECL(sc, "kern-obj-is-char?", kern_obj_is_char);
        API_DECL(sc, "kern-obj-is-container?", kern_obj_is_container);
        API_DECL(sc, "kern-obj-is-field?", kern_obj_is_field);
        API_DECL(sc, "kern-obj-is-mech?", kern_obj_is_mech);
        API_DECL(sc, "kern-obj-is-submerged?", kern_obj_is_submerged);
        API_DECL(sc, "kern-obj-is-visible?", kern_obj_is_visible);
        API_DECL(sc, "kern-obj-move", kern_obj_move);
        API_DECL(sc, "kern-obj-put-at", kern_obj_put_at);
        API_DECL(sc, "kern-obj-put-into", kern_obj_put_into);
        API_DECL(sc, "kern-obj-relocate", kern_obj_relocate);
        API_DECL(sc, "kern-obj-remove", kern_obj_remove);
        API_DECL(sc, "kern-obj-remove-effect", kern_obj_remove_effect);
        API_DECL(sc, "kern-obj-remove-from-inventory", kern_obj_remove_from_inventory);
        API_DECL(sc, "kern-obj-set-ap", kern_obj_set_ap);
        API_DECL(sc, "kern-obj-set-conv", kern_obj_set_conv);
        API_DECL(sc, "kern-obj-set-facing", kern_obj_set_facing);
        API_DECL(sc, "kern-obj-set-gob", kern_obj_set_gob);
        API_DECL(sc, "kern-obj-set-ignore-time-stop", kern_obj_set_ignore_time_stop);
        API_DECL(sc, "kern-obj-set-light", kern_obj_set_light);
        API_DECL(sc, "kern-obj-set-opacity", kern_obj_set_opacity);
        API_DECL(sc, "kern-obj-set-pclass", kern_obj_set_pclass);
        API_DECL(sc, "kern-obj-set-mmode", kern_obj_set_mmode);
        API_DECL(sc, "kern-obj-set-portrait", kern_obj_set_portrait);
        API_DECL(sc, "kern-obj-set-sprite", kern_obj_set_sprite);
        API_DECL(sc, "kern-obj-set-submerged", kern_obj_set_submerged);
        API_DECL(sc, "kern-obj-set-temporary", kern_obj_set_temporary);
        API_DECL(sc, "kern-obj-set-ttl", kern_obj_set_ttl);
        API_DECL(sc, "kern-obj-set-visible", kern_obj_set_visible);
        API_DECL(sc, "kern-obj-wander", kern_obj_wander);
		API_DECL(sc, "kern-obj-freeze", kern_obj_freeze);
		API_DECL(sc, "kern-obj-thaw", kern_obj_thaw);

        /* kern-occ api */
        API_DECL(sc, "kern-occ-get-hp-mod",  kern_occ_get_hp_mod);
        API_DECL(sc, "kern-occ-get-hp-mult", kern_occ_get_hp_mult);
        API_DECL(sc, "kern-occ-get-mp-mod",  kern_occ_get_mp_mod);
        API_DECL(sc, "kern-occ-get-mp-mult", kern_occ_get_mp_mult);
        API_DECL(sc, "kern-occ-get-gob", kern_occ_get_gob);
        API_DECL(sc, "kern-occ-set-gob", kern_occ_set_gob);

        /* kern-place api */
        API_DECL(sc, "kern-place-add-on-entry-hook", kern_place_add_on_entry_hook);
        API_DECL(sc, "kern-place-apply-tile-effects", kern_place_apply_tile_effects);
        API_DECL(sc, "kern-place-set-subplace", kern_place_set_subplace);
        API_DECL(sc, "kern-place-get-beings", kern_place_get_beings);
        API_DECL(sc, "kern-place-get-height", kern_place_get_height);
        API_DECL(sc, "kern-place-get-light", kern_place_get_light);
        API_DECL(sc, "kern-place-get-location", kern_place_get_location);
        API_DECL(sc, "kern-place-get-movement-cost", kern_place_get_movement_cost);
        API_DECL(sc, "kern-place-get-name", kern_place_get_name);
        API_DECL(sc, "kern-place-get-neighbor", kern_place_get_neighbor);
        API_DECL(sc, "kern-place-get-objects", kern_place_get_objects);
        API_DECL(sc, "kern-place-get-terrain", kern_place_get_terrain);
        API_DECL(sc, "kern-place-get-terrain-map", kern_place_get_terrain_map);
        API_DECL(sc, "kern-place-get-vehicle", kern_place_get_vehicle);       
        API_DECL(sc, "kern-place-get-width", kern_place_get_width);
        API_DECL(sc, "kern-place-is-passable", kern_place_is_passable);
        API_DECL(sc, "kern-place-is-hazardous", kern_place_is_hazardous);
        API_DECL(sc, "kern-place-is-wrapping?", kern_place_is_wrapping);
        API_DECL(sc, "kern-place-is-wilderness?", kern_place_is_wilderness);
        API_DECL(sc, "kern-place-blocks-los?", kern_place_blocks_los);
        API_DECL(sc, "kern-place-map", kern_place_map);
        API_DECL(sc, "kern-place-move-is-passable?", kern_place_move_is_passable);
        API_DECL(sc, "kern-place-set-neighbor", kern_place_set_neighbor);
        API_DECL(sc, "kern-place-set-terrain", kern_place_set_terrain);
        API_DECL(sc, "kern-place-set-terrain-map", kern_place_set_terrain_map);
        API_DECL(sc, "kern-place-synch", kern_place_synch);
        API_DECL(sc, "kern-place-is-visible?", kern_place_is_visible);        
        API_DECL(sc, "kern-place-is-combat-map?", kern_place_is_combat_map);        

        /* player api */
        API_DECL(sc, "kern-player-get-food", kern_player_get_food);
        API_DECL(sc, "kern-player-get-gold", kern_player_get_gold);
        API_DECL(sc, "kern-player-set-follow-mode", kern_player_set_follow_mode);
        API_DECL(sc, "kern-player-set-food", kern_player_set_food);
        API_DECL(sc, "kern-player-set-gold", kern_player_set_gold);

        /* screen api */
        API_DECL(sc, "kern-screen-erase", kern_screen_erase);
        API_DECL(sc, "kern-screen-print", kern_screen_print);
        API_DECL(sc, "kern-screen-shade", kern_screen_shade);
        API_DECL(sc, "kern-screen-update", kern_screen_update);
        API_DECL(sc, "kern-screen-draw-sprite", kern_screen_draw_sprite);

        /* kern-set api */
        API_DECL(sc, "kern-set-crosshair", kern_set_crosshair);
        API_DECL(sc, "kern-set-damage-sprite", kern_set_damage_sprite);
        API_DECL(sc, "kern-set-clock", kern_set_clock);

        /* kern-species api */
        API_DECL(sc, "kern-species-get-hp-mod",  kern_species_get_hp_mod);
        API_DECL(sc, "kern-species-get-hp-mult", kern_species_get_hp_mult);
        API_DECL(sc, "kern-species-get-mp-mod",  kern_species_get_mp_mod);
        API_DECL(sc, "kern-species-get-mp-mult", kern_species_get_mp_mult);

        /* kern-terrain api */
        API_DECL(sc, "kern-terrain-blocks-los?", kern_terrain_blocks_los);
        API_DECL(sc, "kern-terrain-get-pclass", kern_terrain_get_pclass);
        API_DECL(sc, "kern-terrain-set-combat-map", kern_terrain_set_combat_map);
        API_DECL(sc, "kern-terrain-set-combat-handler", kern_terrain_set_combat_handler);
        API_DECL(sc, "kern-terrain-map-inc-ref", kern_terrain_map_inc_ref);
        API_DECL(sc, "kern-terrain-map-dec-ref", kern_terrain_map_dec_ref);
        API_DECL(sc, "kern-terrain-map-blend", kern_terrain_map_blend);
        API_DECL(sc, "kern-terrainmap-get-width", kern_map_get_width);
        API_DECL(sc, "kern-terrainmap-get-height", kern_map_get_height);

        /* kern-type api */
        API_DECL(sc, "kern-type-describe", kern_type_describe);
        API_DECL(sc, "kern-type-get-gifc", kern_type_get_gifc);
        API_DECL(sc, "kern-type-get-gob", kern_type_get_gob);
        API_DECL(sc, "kern-type-get-name", kern_type_get_name);
        API_DECL(sc, "kern-type-set-gob", kern_type_set_gob);
        API_DECL(sc, "kern-type-set-quest-item-flag", kern_type_set_quest_item_flag);

        /* misc api */
        API_DECL(sc, "kern-add-magic-negated", kern_add_magic_negated);
        API_DECL(sc, "kern-add-quicken", kern_add_quicken);
        API_DECL(sc, "kern-add-reveal", kern_add_reveal);
        API_DECL(sc, "kern-add-save-game", kern_add_save_game);
        API_DECL(sc, "kern-add-spell", kern_add_spell);
        API_DECL(sc, "kern-add-tick-job", kern_add_tick_job);
        API_DECL(sc, "kern-add-time-stop", kern_add_time_stop);
        API_DECL(sc, "kern-ambush-while-camping", kern_ambush_while_camping);
        API_DECL(sc, "kern-add-xray-vision", kern_add_xray_vision);
        API_DECL(sc, "kern-begin-combat", kern_begin_combat);
        API_DECL(sc, "kern-blit-map", kern_blit_map);
        API_DECL(sc, "kern-init-random", kern_init_random);
        API_DECL(sc, "kern-define", kern_define);
        API_DECL(sc, "kern-dice-roll", kern_dice_roll);
        API_DECL(sc, "kern-end-game" , kern_end_game);
        API_DECL(sc, "kern-fire-missile", kern_fire_missile);
        API_DECL(sc, "kern-fire-missile-to-max", kern_fire_missile_to_max);
        API_DECL(sc, "kern-fold-rect", kern_fold_rect);
        API_DECL(sc, "kern-get-distance", kern_get_distance);
        API_DECL(sc, "kern-get-objects-at", kern_get_objects_at);
        API_DECL(sc, "kern-get-magic-negated", kern_get_magic_negated);
        API_DECL(sc, "kern-get-player", kern_get_player);
        API_DECL(sc, "kern-get-ticks", kern_get_ticks);
        API_DECL(sc, "kern-get-time", kern_get_time);
        API_DECL(sc, "kern-time-get-remainder", kern_get_time_remainder);
        API_DECL(sc, "kern-get-total-minutes", kern_get_total_minutes);
        API_DECL(sc, "kern-harm-relations", kern_harm_relations);		
        API_DECL(sc, "kern-in-los?", kern_in_los);
        API_DECL(sc, "kern-los-invalidate", kern_los_invalidate);
        API_DECL(sc, "kern-include", kern_include);
        API_DECL(sc, "kern-interp-error", kern_interp_error);
        API_DECL(sc, "kern-is-valid-location?", kern_is_valid_location);
        API_DECL(sc, "kern-print", kern_print);
        API_DECL(sc, "kern-search-rect", kern_search_rect);
        API_DECL(sc, "kern-search-rect-for-terrain", 
                 kern_search_rect_for_terrain);
        API_DECL(sc, "kern-search-rect-for-obj-type", 
                 kern_search_rect_for_obj_type);
        API_DECL(sc, "kern-add-hook", kern_add_hook);
        API_DECL(sc, "kern-add-query", kern_add_query);
        API_DECL(sc, "kern-rm-hook", kern_rm_hook);
        API_DECL(sc, "kern-set-quicken-sprite", kern_set_quicken_sprite);
        API_DECL(sc, "kern-set-time-stop-sprite", kern_set_time_stop_sprite);
        API_DECL(sc, "kern-set-magic-negated-sprite", kern_set_magic_negated_sprite);
        API_DECL(sc, "kern-set-reveal-sprite", kern_set_reveal_sprite);
        API_DECL(sc, "kern-set-xray-vision-sprite", kern_set_xray_vision_sprite);
        API_DECL(sc, "kern-set-spell-words", kern_set_spell_words);
        API_DECL(sc, "kern-set-wind", kern_set_wind);
        API_DECL(sc, "kern-get-wind", kern_get_wind);
        API_DECL(sc, "kern-set-time-accel", kern_set_time_accel);
        API_DECL(sc, "kern-sleep", kern_sleep);
        API_DECL(sc, "kern-music-play", kern_music_play);
        API_DECL(sc, "kern-sound-play", kern_sound_play);
        API_DECL(sc, "kern-sound-play-at", kern_sound_play_at);
        API_DECL(sc, "kern-sound-play-ambient", kern_sound_play_ambient);
        API_DECL(sc, "kern-tag", kern_tag);
        API_DECL(sc, "kern-test-recursion", kern_test_recursion);
        API_DECL(sc, "kern-ticks-per-turn", kern_ticks_per_turn);
        API_DECL(sc, "kern-set-turn-count", kern_set_turn_count);
        API_DECL(sc, "kern-map-flash-sprite", kern_map_flash_sprite); 
        API_DECL(sc, "kern-script-version", kern_script_version);
        
        /* ui api */
        API_DECL(sc, "kern-ui-direction", kern_ui_direction);
        API_DECL(sc, "kern-ui-paginate-text", kern_ui_paginate_text);
        API_DECL(sc, "kern-ui-page-text", kern_ui_page_text);
        API_DECL(sc, "kern-ui-select-from-list", kern_ui_select_from_list);
        API_DECL(sc, "kern-ui-select-item", kern_ui_select_item);
        API_DECL(sc, "kern-ui-select-party-member", kern_ui_select_party_member);
        API_DECL(sc, "kern-ui-target", kern_ui_target);
        API_DECL(sc, "kern-ui-target-generic", kern_ui_target_generic);
        API_DECL(sc, "kern-ui-waitkey", kern_ui_waitkey);
        API_DECL(sc, "kern-applet-run", kern_applet_run);

        /* conv api */
        API_DECL(sc, "kern-conv-begin", kern_conv_begin);
        API_DECL(sc, "kern-conv-end", kern_conv_end);
        API_DECL(sc, "kern-conv-say", kern_conv_say);
        API_DECL(sc, "kern-conv-get-amount", kern_conv_get_amount);
        API_DECL(sc, "kern-conv-get-yes-no?", kern_conv_get_yes_no);
        API_DECL(sc, "kern-conv-trade", kern_conv_trade);
        API_DECL(sc, "kern-conv-get-reply", kern_conv_get_reply);
        API_DECL(sc, "kern-conv-get-string", kern_conv_get_string);

        /* kern-map api */
        API_DECL(sc, "kern-map-blit-image", kern_map_blit_image);
        API_DECL(sc, "kern-map-center-camera", kern_map_center_camera);
        API_DECL(sc, "kern-map-flash", kern_map_flash);
        API_DECL(sc, "kern-map-repaint", kern_map_repaint);
        API_DECL(sc, "kern-map-set-dirty", kern_map_set_dirty);
        API_DECL(sc, "kern-map-set-image", kern_map_set_image);
        API_DECL(sc, "kern-map-set-jitter", kern_map_set_jitter);
        API_DECL(sc, "kern-map-set-peering", kern_map_set_peering);
        API_DECL(sc, "kern-map-view-create", kern_map_view_create);
        API_DECL(sc, "kern-map-view-destroy", kern_map_view_destroy);
        API_DECL(sc, "kern-map-view-center", kern_map_view_center);
        API_DECL(sc, "kern-map-view-add", kern_map_view_add);
        API_DECL(sc, "kern-map-view-rm", kern_map_view_rm);
        API_DECL(sc, "kern-map-view-add", kern_map_view_add);
        API_DECL(sc, "kern-map-view-rm", kern_map_view_rm);
                
        /* kern-log api */
        API_DECL(sc, "kern-log-begin", kern_log_begin);
        API_DECL(sc, "kern-log-continue", kern_log_continue);
        API_DECL(sc, "kern-log-end", kern_log_end);
        API_DECL(sc, "kern-log-enable", kern_log_enable);
        API_DECL(sc, "kern-log-flush", kern_log_flush);
        API_DECL(sc, "kern-log-msg", kern_log_msg);
        API_DECL(sc, "kern-stdout-msg", kern_stdout_msg);
		
        /* kern-dtable api */
        API_DECL(sc, "kern-mk-dtable", kern_mk_dtable);
        API_DECL(sc, "kern-dtable-get", kern_dtable_get);
        API_DECL(sc, "kern-dtable-set", kern_dtable_set);
        API_DECL(sc, "kern-dtable-inc", kern_dtable_inc);
        API_DECL(sc, "kern-dtable-dec", kern_dtable_dec);

        /* kern-party-api */
        API_DECL(sc, "kern-party-add-member", kern_party_add_member);
        API_DECL(sc, "kern-party-get-members", kern_party_get_members);
        API_DECL(sc, "kern-party-get-vehicle", kern_party_get_vehicle);
        API_DECL(sc, "kern-party-set-vehicle", kern_party_set_vehicle);

        /* kern-sprite api */
        API_DECL(sc, "kern-sprite-clone", kern_sprite_clone);
        API_DECL(sc, "kern-sprite-append-decoration", kern_sprite_append_decoration);
        API_DECL(sc, "kern-sprite-apply-matrix", kern_sprite_apply_matrix);
        API_DECL(sc, "kern-sprite-blit-over", kern_sprite_blit_over);
        API_DECL(sc, "kern-sprite-strip-decorations", kern_sprite_strip_decorations);


        /* kern-vehicle-api */
        API_DECL(sc, "kern-vehicle-set-name", kern_vehicle_set_name);

        /* kern-cfg api */
        API_DECL(sc, "kern-cfg-set", kern_cfg_set);
        API_DECL(sc, "kern-cfg-get", kern_cfg_get);

        API_DECL(sc, "kern-set-kern-intvar", kern_set_kern_intvar);
        API_DECL(sc, "kern-get-kern-intvar", kern_get_kern_intvar);


        /* kern-image api */
        API_DECL(sc, "kern-image-load", kern_image_load);
        API_DECL(sc, "kern-image-free", kern_image_free);

        /* kern-progress-bar api */
        API_DECL(sc, "kern-progress-bar-start", kern_progress_bar_start);
        API_DECL(sc, "kern-progress-bar-advance", kern_progress_bar_advance);
        API_DECL(sc, "kern-progress-bar-finish", kern_progress_bar_finish);

        /* kern-ztats api */
        API_DECL(sc, "kern-ztats-add-pane", kern_ztats_add_pane);
        API_DECL(sc, "kern-status-set-title", kern_status_set_title);

        /* obsolete (keep these until old save games are unlikely to use
         * them) */
        API_DECL(sc, "kern-set-frame", kern_set_frame);
        API_DECL(sc, "kern-set-cursor", kern_set_cursor);
        API_DECL(sc, "kern-set-ascii", kern_set_ascii);

        
        /* Revisit: probably want to provide some kind of custom port here. */
        scheme_set_output_port_file(sc, stderr);

        /* Shared constants */
        scm_define_int(sc, "kern-key-esc", SDLK_ESCAPE);
        scm_define_int(sc, "kern-key-space", SDLK_SPACE);
        scm_define_int(sc, "kern-key-return", '\n'); // remapped in event.c::mapKey
        scm_define_int(sc, "kern-key-enter", SDLK_KP_ENTER);
        scm_define_int(sc, "kern-key-up", SDLK_KP8); // also handles arrowkeys
        scm_define_int(sc, "kern-key-down", SDLK_KP2); // also handles arrowkeys
        scm_define_int(sc, "kern-key-kp-pgup", SDLK_KP9);
        scm_define_int(sc, "kern-key-kp-pgdn", SDLK_KP3);
        scm_define_int(sc, "kern-key-pgup", SDLK_PAGEUP);
        scm_define_int(sc, "kern-key-pgdn", SDLK_PAGEDOWN);
        scm_define_int(sc, "kern-sp-centered", SP_CENTERED);
        scm_define_int(sc, "kern-ascii-h", ASCII_H);
        return sc;
}
