/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2002, 2003, 2011 Gordon McNutt
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 *
 * Gordon McNutt
 * gmcnutt@users.sourceforge.net
 */

#include "glyph.h"
#include "list.h"
#include "mem.h"

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define GLYPH_NIL 0x0000
#define GLYPH_COLOR_STACK_DEPTH 32
//#include <string.h>


enum glyph_ctrl_states {
        GLYPH_STATE_DEF = 0,
        GLYPH_STATE_ESC,
        GLYPH_STATE_CLR,
        GLYPH_STATE_CLRPUSH
};

enum {
        START_OF_LINE,
        FIRST_WORD,
        BETWEEN_WORDS,
        PROBE_WORD
};

struct glyph_formatter {
        glyph_ctrl_states state;
        char color_stack[GLYPH_COLOR_STACK_DEPTH];
        int i_color;  /* index onto stack */
        char color; /* active color */
};

typedef struct glyph_buf {
        struct list list;
        glyph_t *start;
        glyph_t *pos;
        glyph_t *end;
        glyph_t *mark;
} glyph_buf_t;

typedef struct glyph_doc {
        struct list lines;
        struct list *iter;
} glyph_doc_t;

static void glyph_dbg(glyph_t gl)
{
        printf("%c", glyph_get_char(gl));
}

static void glyph_buf_dbg(glyph_buf_t *gbuf)
{
        glyph_t gl;
        for (gl = glyph_buf_first(gbuf); gl; gl = glyph_buf_next(gbuf)) {
                glyph_dbg(gl);
        }
}

static void glyph_doc_dbg(glyph_doc_t *gdoc)
{
        glyph_buf_t *gbuf;
        int i = 0;
        for (gbuf = glyph_doc_first(gdoc); gbuf; gbuf = glyph_doc_next(gdoc)) {
                printf("%d:'", i++);
                glyph_buf_dbg(gbuf);
                printf("'\n");
        }
}

static void glyph_buf_fin(void *arg)
{
        glyph_buf_t *gbuf = (glyph_buf_t*)arg;
        mem_deref(gbuf->start);
}

static glyph_buf_t *glyph_buf_alloc(int size)
{
        glyph_buf_t *gbuf = MEM_ALLOC_TYPE(glyph_buf_t, glyph_buf_fin);
        gbuf->start = MEM_ALLOC_NTYPE(glyph_t, size, NULL);
        gbuf->pos = gbuf->start;
        gbuf->end = gbuf->start + size;
        list_init(&gbuf->list);
        return gbuf;
}

static void glyph_buf_append(glyph_buf_t *gbuf, glyph_t gl)
{
        if (gbuf->pos < gbuf->end) {
                *gbuf->pos++ = gl;
        }
}

int glyph_buf_room(glyph_buf_t *gbuf)
{
        return gbuf->end - gbuf->pos;
}

static void glyph_buf_set_mark_to_pos(glyph_buf_t *gbuf)
{
        gbuf->mark = gbuf->pos;
}

static void glyph_buf_set_end_to_pos(glyph_buf_t *gbuf)
{
        gbuf->end = gbuf->pos;
}

static void glyph_buf_copy_from_mark(glyph_buf_t *src, glyph_buf_t *dst)
{
        glyph_t *gl = src->mark;
        while (gl < src->pos) {
                glyph_buf_append(dst, *gl++);
        }
}

static void glyph_buf_set_pos_to_mark(glyph_buf_t *gbuf)
{
        gbuf->pos = gbuf->mark;
}

static void glyph_buf_set_pos_to_start(glyph_buf_t *gbuf)
{
        gbuf->pos = gbuf->start;
}

glyph_t glyph_buf_first(glyph_buf_t *gbuf)
{
        glyph_buf_set_pos_to_start(gbuf);
        return glyph_buf_next(gbuf);
}

glyph_t glyph_buf_next(glyph_buf_t *gbuf)
{
        if (gbuf->pos < gbuf->end) {
                return *gbuf->pos++;
        }
        return GLYPH_NIL;
}

static void glyph_doc_append(glyph_doc_t *gdoc, glyph_buf_t *gbuf)
{
        list_add_tail(&gdoc->lines, &gbuf->list);
}

static void glyph_doc_fin(void *arg)
{
        glyph_doc_t *gdoc = (glyph_doc_t*)arg;
        gdoc->iter = gdoc->lines.next;
        while (gdoc->iter != &gdoc->lines) {
                glyph_buf_t *gbuf = list_entry(gdoc->iter, glyph_buf_t, list);
                gdoc->iter = gdoc->iter->next;
                mem_deref(gbuf);
        }
}

static void glyph_formatter_push_color(glyph_formatter_t *gf)
{
        assert(gf->i_color < GLYPH_COLOR_STACK_DEPTH);
        gf->color_stack[gf->i_color] = gf->color;
        gf->i_color++;
}

static void glyph_formatter_pop_color(glyph_formatter_t *gf)
{
        assert(gf->i_color > 0);
        gf->i_color--;
        gf->color = gf->color_stack[gf->i_color];
        return;
}

static void glyph_formatter_set_color(glyph_formatter_t *gf, char clr)
{
        /* Check for a pop. */
        switch (clr) {
        case '+': 
                glyph_formatter_push_color(gf);
                break;
        case '-':
                glyph_formatter_pop_color(gf);
                break;
        case '=':
                /* current color, nop */
                break;
        default:
                gf->color = clr;
                break;
        }
}


int glyph_strlen(const glyph_t *glyph)
{
        int len = 0;
        while (*glyph++) {
                len++;
        }
        return len;
}

glyph_formatter_t *glyph_formatter_alloc(void)
{
        glyph_formatter_t *gf = MEM_ALLOC_TYPE(glyph_formatter_t, NULL);
        if (!gf) {
                return gf;
        }

        gf->color = 'w';
        gf->state = GLYPH_STATE_DEF;
        return gf;
}

void glyph_formatter_deref(glyph_formatter_t *gf)
{
        free(gf);
}

glyph_buf_t *glyph_buf_alloc_and_format(glyph_formatter_t *gf, const char *str)
{
        glyph_buf_t *gbuf = glyph_buf_alloc(strlen(str));
        char c;

        while ((c = *str++)) {

                glyph_t gl = GLYPH_NIL;

                switch (gf->state) {

                case GLYPH_STATE_CLR:
                        glyph_formatter_set_color(gf, c);
                        gf->state = ('+' == c ? GLYPH_STATE_CLRPUSH : 
                                     GLYPH_STATE_DEF);
                        break;
                
                case GLYPH_STATE_CLRPUSH:
                        glyph_formatter_set_color(gf, c);
                        gf->state = GLYPH_STATE_DEF;
                        break;
                        
                case GLYPH_STATE_ESC:
                        if (c == 'c') {
                                gf->state = GLYPH_STATE_CLR;
                        }
                        break;
                
                case GLYPH_STATE_DEF:
                default:
                        if (c == '^') {
                                gf->state = GLYPH_STATE_ESC;
                        } else {
                                glyph_set_char(&gl, c);
                                glyph_set_color(&gl, gf->color);
                                glyph_buf_append(gbuf, gl);
                        }
                }
        }

        glyph_buf_set_end_to_pos(gbuf);
        glyph_buf_set_pos_to_start(gbuf);

        return gbuf;
}

glyph_doc_t *glyph_doc_alloc_and_layout(glyph_buf_t *gbuf, int width)
{
        glyph_doc_t *gdoc = MEM_ALLOC_TYPE(glyph_doc_t, glyph_doc_fin);
        list_init(&gdoc->lines);
        gdoc->iter = &gdoc->lines;

        glyph_buf_t *gline = glyph_buf_alloc(width);
        glyph_t gl;
        int state = START_OF_LINE;
        for (gl = glyph_buf_first(gbuf); gl; gl = glyph_buf_next(gbuf)) {

                char ch = glyph_get_char(gl);

                if (ch == '\n') {
                        goto newline;
                }

                switch (state) {
                case START_OF_LINE:
                        if (! isspace(ch)) {
                                glyph_buf_append(gline, gl);
                                state = FIRST_WORD;
                        }
                        break;
                case FIRST_WORD:
                        glyph_buf_append(gline, gl);
                        if (isspace(ch)) {
                                if (!glyph_buf_room(gline)) {
                                        goto newline;
                                } else {
                                        state = BETWEEN_WORDS;
                                }
                        } else {
                                if (!glyph_buf_room(gline)) {
                                        goto newline;
                                }
                        }
                        break;
                case BETWEEN_WORDS:
                        if (isspace(ch)) {
                                glyph_buf_append(gline, gl);
                                if (!glyph_buf_room(gline)) {
                                        goto newline;
                                }
                        } else {
                                glyph_buf_set_mark_to_pos(gline);
                                glyph_buf_append(gline, gl);
                                if (!glyph_buf_room(gline)) {
                                        goto copytonewline;
                                } else {
                                        state = PROBE_WORD;
                                }
                        }
                        break;
                case PROBE_WORD:
                        glyph_buf_append(gline, gl);
                        if (isspace(ch)) {
                                if (!glyph_buf_room(gline)) {
                                        goto newline;
                                } else {
                                        state = BETWEEN_WORDS;
                                }                                
                        } else {
                                if (!glyph_buf_room(gline)) {
                                        goto copytonewline;
                                }
                        }
                        break;
                }

                continue;

        newline:
                glyph_buf_set_end_to_pos(gline);
                glyph_buf_set_pos_to_start(gline);
                glyph_doc_append(gdoc, gline);
                gline = glyph_buf_alloc(width);
                state = START_OF_LINE;
                continue;

        copytonewline:
                glyph_buf_t *tmp = glyph_buf_alloc(width);
                glyph_buf_copy_from_mark(gline, tmp);
                glyph_buf_set_pos_to_mark(gline);
                glyph_buf_set_end_to_pos(gline);
                glyph_buf_set_pos_to_start(gline);
                glyph_doc_append(gdoc, gline);
                gline = tmp;
                state = FIRST_WORD;
        }

        glyph_buf_set_end_to_pos(gline);
        glyph_buf_set_pos_to_start(gline);
        glyph_doc_append(gdoc, gline);

        printf("glyph doc:\n");
        glyph_doc_dbg(gdoc);

        return gdoc;
}

glyph_buf_t *glyph_doc_first(glyph_doc_t *gdoc)
{
        gdoc->iter = gdoc->lines.next;
        return glyph_doc_next(gdoc);
}

glyph_buf_t *glyph_doc_next(glyph_doc_t *gdoc)
{
        if (gdoc->iter == &gdoc->lines) {
                return NULL;
        }

        glyph_buf_t *gbuf = list_entry(gdoc->iter, glyph_buf_t, list);
        gdoc->iter = gdoc->iter->next;
        return gbuf;
}

int glyph_doc_get_num_lines(glyph_doc_t *gdoc)
{
        return list_len(&gdoc->lines);
}

void glyph_doc_deref(glyph_doc_t *gdoc)
{
        mem_deref(gdoc);
}

char glyph_get_color(glyph_t gl) 
{ 
        return (gl >> 8) & 0xff; 
}

char glyph_get_char(glyph_t gl)
{
        return gl & 0xff;
}

void glyph_set_color(glyph_t *gl, char clr) 
{ 
        *gl &= 0x00ff;
        *gl |= (clr << 8);
}

void glyph_set_char(glyph_t *gl, char ch)
{
        *gl &= 0xff00;
        *gl |= ch;
}

void glyph_buf_deref(glyph_buf_t *gbuf)
{
        mem_deref(gbuf);
}
