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
#ifndef loader_h
#define loader_h

struct lexer;

class Loader {
      public:
	Loader();
	bool getInt(int *);
	bool getBool(bool *);
	bool getWord(char **);
	bool getString(char **);
	bool getBitmask(int *);
	bool getToken(char *);
	bool getChar(char *);
	bool getFloat(float *);
	bool getRaw(char *, int maxlen);

	bool matchToken(char);
	bool matchWord(char *);

	bool getIntKeyValue(char *key, int *val);
	bool getWordKeyValue(char *key, char **val);
	bool getStringKeyValue(char *key, char **val);

	void advance();
	void setError(char *fmt, ...);

	void *(*lookupTag) (char *tag, int tid);
	struct lexer *lexer;
	char error[256];
};

#endif				// loader_h
