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

#include "file.h"

#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

static char *file_errstr = "Success";

char *file_mkpath(const char *dir, const char *fname)
{
	char *ret;
	size_t dl,fl;

	if (! dir)
		return strdup(fname);
	dl = strlen(dir);
	fl = strlen(fname);

	ret = (char*)malloc(dl+fl+2);
	if (! ret) {
                file_errstr = "Allocation failed";
		return ret;
        }
	memcpy(ret,dir,dl);
	ret[dl] = '/';
	memcpy(ret+dl+1,fname,fl);
	ret[dl+1+fl] = '\0';

	return ret;
}

int file_exists(const char *fname)
{
        return 0;

}

int file_exists_in_dir(const char *dirname, const char *fname)
{
        return 0;

}

int file_exists_in_include_dir(const char *fname)
{
        return 0;

}

int file_exists_in_save_dir(const char *fname)
{
        return 0;

}

FILE *file_open(const char *fname)
{
        return 0;

}

FILE *file_open_in_dir(const char *dirname, const char *fname)
{
        return 0;

}

FILE *file_open_in_include_dir(const char *fname)
{
        return 0;

}

FILE *file_open_in_save_dir(const char *fname)
{
        return 0;
}

const char *file_get_error()
{
        return file_errstr;
}
