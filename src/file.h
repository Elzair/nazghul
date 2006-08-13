/* $Id$
 *
 * Copyright (C) 2006 Gordon McNutt
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
 */

#ifndef file_h
#define file_h

#include <stdio.h>

/* Declare some common file and directory utitilites. */

/* This concatenates a bunch of directory names followed by a file name into a
 * single string and returns it. Use the stdlib free() function to delete the
 * string. */
extern char *file_mkpath(const char *dir, const char *fname);

/* The following all test for the existence of a file. The _include_dir and
 * _save_dir versions check the configured read-only include dir and read-write
 * save dir, resp. */
extern int file_exists(const char *fname);
extern int file_exists_in_dir(const char *dirname, const char *fname);
extern int file_exists_in_include_dir(const char *fname);
extern int file_exists_in_save_dir(const char *fname);

/* The following all try to open a file in the respective directories. To close
 * the file just use fclose(). The file_open_in_save_dir function will attempt
 * to create the configured save dir if it doesn't exist yet, except on win32
 * platforms. */
extern FILE *file_open(const char *fname, char *mode);
extern FILE *file_open_in_dir(const char *dirname, const char *fname, char *mode);
extern FILE *file_open_in_include_dir(const char *fname);
extern FILE *file_open_in_save_dir(const char *fname, char *mode);

/* The following all try to load a script file from the respective directories.
 * These are meant for loading the various config scripts. They are not
 * suitable for loading session save files, because they destroy the
 * interpreter context after the scripts have executed (session.c knows this
 * and loads save games "by hand" so it can keep the context). */
extern int file_load(const char *fname);
extern int file_load_from_dir(const char *dirname, const char *fname);
extern int file_load_from_include_dir(const char *fname);
extern int file_load_from_save_dir(const char *fname);

/* If any of the file_open_ functions return NULL then use the following to get
 * an error string description of what went wrong. Obviously this is not meant
 * for multi-threaded usage. */
extern const char *file_get_error();

#endif
