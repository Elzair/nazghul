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
#include "util.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <stdio.h>
#include <ctype.h>

char *mmap_file(char *file, int *len)
{
	int fd;
	struct stat stat;
	char *ret;

	if ((fd = open(file, O_RDONLY)) < 0) {
		perror(file);
		return 0;
	}

	if (fstat(fd, &stat) < 0) {
		perror("fstat");
		return 0;
	}

	*len = stat.st_size;

	if ((ret =
	     (char *) mmap(0, stat.st_size, PROT_READ, MAP_PRIVATE, fd, 0)) < 0)
		perror("mmap");

	close(fd);

	return ret;
}

char SDL_keysym_to_ascii(SDL_keysym * keysym)
{
	/* This routine translates shift characters, etc. */

	if (keysym->mod & KMOD_SHIFT) {

		/* This is nasty, and possibly keyboard-specific, but I don't
		 * know a better way to do it. The ASCII characters which are
		 * accessed via the SHIFT key do not occupy a contiguous range
		 * on my keyboard, so I'm using a switch. */
		switch (keysym->sym) {
		case '`':
			return '~';
		case '1':
			return '!';
		case '2':
			return '@';
		case '3':
			return '#';
		case '4':
			return '$';
		case '5':
			return '%';
		case '6':
			return '^';
		case '7':
			return '&';
		case '8':
			return '*';
		case '9':
			return '(';
		case '0':
			return ')';
		case '-':
			return '_';
		case '=':
			return '+';
		case '[':
			return '{';
		case ']':
			return '}';
		case '\\':
			return '|';
		case ';':
			return ':';
		case '\'':
			return '\"';
		case ',':
			return '<';
		case '.':
			return '>';
		case '/':
			return '?';
		default:
			;
			/* fall through */
		}

		/* Now check for uppercase letters. */
		if ((keysym->sym >= 'a') && (keysym->sym <= 'z')) {
			return toupper(keysym->sym);
		}

		/* Otherwise we don't handle this key. */
		return 0;
	}

	/* Since we're here that means the shift key is not being held down.
	 * This is the normal case, where we just have to filter out keys we
	 * ignore. */
	if (keysym->sym > 127)
		return 0;

	/* For ASCII characters SDL sanely maps them to their ASCII
	 * equivalents. */
	return (keysym->sym);
}
