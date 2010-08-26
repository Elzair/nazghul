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

#include "cfg.h"
#include "kern.h"
#include "scheme.h"

#include <assert.h>
#include <errno.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

static const char *file_errstr = "Success";

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
        FILE *file = fopen(fname, "r");
        if (file) {
                fclose(file);
                return 1;
        }
        return 0;
}

int file_exists_in_dir(const char *dirname, const char *fname)
{
        int ret = -1;
        char *path = file_mkpath(dirname, fname);
        if (path) {
                ret = file_exists(path);
                free(path);
        }
        return ret;
}

int file_exists_in_include_dir(const char *fname)
{
        return file_exists_in_dir(cfg_get("include-dirname"), fname);
}

int file_exists_in_save_dir(const char *fname)
{
        return file_exists_in_dir(cfg_get("saved-games-dirname"), fname);

}

FILE *file_open(const char *fname, const char *mode)
{
        FILE *file = NULL;

        /* Check args. */
        if (!fname) {
                file_errstr = "Null filename";
                return 0;
        }

        /* Open the file. */
        file = fopen(fname, mode);
        if (!file) {
                file_errstr = strerror(errno);
                return 0;
        }

        return file;
}

FILE *file_open_in_dir(const char *dirname, const char *fname, const char *mode)
{
        FILE *file = 0;
        char *path = file_mkpath(dirname, fname);
        if (path) {
                file = file_open(path, mode);
                free(path);
        }
        return file;
}

FILE *file_open_in_include_dir(const char *fname)
{
        return file_open_in_dir(cfg_get("include-dirname"), fname, "r");
}

FILE *file_open_in_save_dir(const char *fname, const char *mode)
{
        char *dir = cfg_get("saved-games-dirname");

#ifndef WIN32        
        /* FIXME: cygwin build fails, saying that mkdir below has too
         * many arguments. We don't use the save dir so not a
         * problem */
        if (dir
            && strchr(mode, 'w')
            && !file_exists(dir)) {
                        if (mkdir(dir, 0777)) {
                                file_errstr = strerror(errno);
                                return 0;
                        }
        }
#endif
        return file_open_in_dir(dir, fname, mode);
}

int file_load(const char *fname)
{
        scheme *sc = NULL;
        FILE *file = NULL;

        /* Open the load file. */
        file = file_open(fname, "r");
	if (! file) {
                return -1;
        }

        /* Create a new interpreter. */
        if (! (sc = kern_init())) {
                file_errstr = "Could not create interpreter";
                fclose(file);
                return -1;
        }

        /* Load the init file. */
        scheme_load_named_file(sc, file, fname);

        /* Cleanup interpreter. */
        scheme_deinit(sc);
        free(sc);

        /* close the file */
        fclose(file);

        return 0;
}

int file_load_from_dir(const char *dirname, const char *fname)
{
        int ret = -1;
        char *path = file_mkpath(dirname, fname);
        if (path) {
                ret = file_load(path);
                free(path);
        }
        return ret;
}

int file_load_from_include_dir(const char *fname)
{
        return file_load_from_dir(cfg_get("include-dirname"), fname);
}

int file_load_from_save_dir(const char *fname)
{
        return file_load_from_dir(cfg_get("saved-games-dirname"), fname);
}

const char *file_get_error()
{
        return file_errstr;
}
