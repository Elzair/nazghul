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

#ifndef glyph_h
#define glyph_h

/**
 * This module provides a way to support colored chars as "glyphs". (Attributes
 * other than foreground color may be added later.) It provides a function for
 * converting specially formatted strings (described below) into a colored
 * glyph string. It also provides a function for laying out a glyph string into
 * a document with fixed width, using linebreaks on whitespace.
 *
 * All objects allocated by this module support reference-counting.
 *
 * COLOR FORMATTING SYNTAX
 *
 * The glyph codes are either a normal letter or ^c followed by a command:
 *
 *       <SEQ> := ^c<CMD><COLOR>
 *     <COLOR> := B|w|r|g|b|c|y|m|G|!
 *       <CMD> := +|-|<NIL>
 *       <NIL> :=
 *
 * The + CMD pushes the current color before setting the new color. The -
 * CMD pops the last pushed color and makes it the new color.
 *
 * The color codes:
 *
 *   w White
 *   B Black
 *   r Red
 *   g Green
 *   b Blue
 *   y Yellow
 *   c Cyan
 *   m Magenta
 *   G Gray
 *
 */

typedef short int glyph_t;
struct glyph_buf;
struct glyph_buf_iter;
struct glyph_formatter;
struct glyph_doc;
struct glyph_doc_iter;

typedef struct glyph_buf glyph_buf_t;
typedef struct glyph_buf_iter glyph_buf_iter_t;
typedef struct glyph_formatter glyph_formatter_t;
typedef struct glyph_doc glyph_doc_t;
typedef struct glyph_doc_iter glyph_doc_iter_t;

char glyph_get_color(glyph_t gl);
char glyph_get_char(glyph_t gl);
void glyph_set_color(glyph_t *gl, char clr);
void glyph_set_char(glyph_t *gl, char ch);

glyph_formatter_t *glyph_formatter_alloc(void);
void glyph_formatter_deref(struct glyph_formatter *);

glyph_buf_t *glyph_buf_alloc_and_format(struct glyph_formatter *gfmt, const char *str);
int glyph_buf_room(const glyph_buf_t *gbuf);
int glyph_buf_len(const glyph_buf_t *gbuf);
void glyph_buf_deref(glyph_buf_t *gbuf);

glyph_buf_iter_t *glyph_buf_iter_alloc(glyph_buf_t *gbuf);
glyph_t glyph_buf_iter_next(glyph_buf_iter_t *gbi);
void glyph_buf_iter_deref(glyph_buf_iter_t *gbi);

glyph_doc_t *glyph_doc_alloc(int width);
glyph_doc_t *glyph_doc_alloc_and_layout(glyph_buf_t *gbuf, int width);
void glyph_doc_layout_and_append(glyph_doc_t *gdoc, glyph_buf_t *gbuf);
int glyph_doc_get_num_lines(const glyph_doc_t *gdoc); 
void glyph_doc_deref(glyph_doc_t *gdoc);

glyph_doc_iter_t *glyph_doc_iter_alloc(glyph_doc_t *gdoc);
void glyph_doc_iter_goto(glyph_doc_iter_t *gdi, int line);
glyph_buf_t *glyph_doc_iter_next(glyph_doc_iter_t *gbi);
void glyph_doc_iter_deref(glyph_doc_iter_t *gbi);

#endif
