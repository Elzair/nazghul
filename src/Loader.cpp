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
#include "Loader.h"
#include "lexer.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>

Loader::Loader()
{
	memset(error, 0, sizeof(error));
}

bool Loader::getInt(int *val)
{
	if (lexer->token != lexer_INT) {
		snprintf(error, sizeof(error),
			 "Expected INT, got '%s'", lexer->lexeme);
		return false;
	}
	*val = atoi(lexer->lexeme);
	advance();
	return true;
}

bool Loader::getFloat(float *val)
{
	if (lexer->token != lexer_FLOAT) {
		snprintf(error, sizeof(error),
			 "Expected FLOAT, got '%s'", lexer->lexeme);
		return false;
	}
	*val = atof(lexer->lexeme);
	advance();
	return true;
}

bool Loader::getBool(bool * val)
{
	int tmp;
	if (!getInt(&tmp)) {
		snprintf(error, sizeof(error),
			 "Expected BOOL, got '%s'", lexer->lexeme);
		return false;
	}
	*val = !!tmp;
	return true;
}

bool Loader::getWord(char **val)
{
	if (lexer->token != lexer_WORD) {
		snprintf(error, sizeof(error),
			 "Expected WORD, got '%s'", lexer->lexeme);
		return false;
	}
	*val = strdup(lexer->lexeme);
	advance();
	if (!*val) {
		snprintf(error, sizeof(error),
			 "Insufficient memory to copy string");
		return false;
	}
	return true;
}

bool Loader::getString(char **val)
{
	if (lexer->token != lexer_STRING) {
		snprintf(error, sizeof(error),
			 "Expected STRING, got '%s'", lexer->lexeme);
		return false;
	}
	*val = strdup(lexer->lexeme);
	advance();
	if (!*val) {
		snprintf(error, sizeof(error),
			 "Insufficient memory to copy string");
		return false;
	}
	return true;
}

bool Loader::getBitmask(int *bmap)
{
	*bmap = 0;

	if (lexer->token != '(') {
		snprintf(error, sizeof(error), "Expected '(', got '%s'",
			 lexer->lexeme);
		return false;
	}

	advance();

	if (lexer->token == ')') {
		advance();
		return true;
	}

	for (;;) {

		if (lexer->token != lexer_INT) {
			snprintf(error, sizeof(error),
				 "Expected INT, got '%s'", lexer->lexeme);
			return false;
		}

		*bmap |= atoi(lexer->lexeme);
		advance();
		if (lexer->token == ')') {
			advance();
			return true;
		}
		if (lexer->token != '|') {
			snprintf(error, sizeof(error),
				 "Expected ')' or '|', got '%s'",
				 lexer->lexeme);
			return false;
		}
		advance();
	}
}

bool Loader::getToken(char *token)
{
	*token = (char) lexer->token;
	advance();
	return true;
}

bool Loader::getChar(char *ch)
{
	*ch = *lexer->lexeme;
	advance();
	return true;
}

bool Loader::matchToken(char token)
{
	if (lexer->token != token) {
		snprintf(error, sizeof(error), "Expected '%c', got '%s'",
			 token, lexer->lexeme);
		return false;
	}
	advance();
	return true;
}

bool Loader::matchWord(char *word)
{
	if (lexer->token != lexer_WORD) {
		snprintf(error, sizeof(error),
			 "Expected WORD, got '%s'", lexer->lexeme);
		return false;
	}
	if (strcmp(lexer->lexeme, word)) {
		snprintf(error, sizeof(error),
			 "Expected keyword '%s', got '%s'", word,
			 lexer->lexeme);
		return false;
	}
	advance();
	return true;
}

void Loader::advance()
{
	lexer_lex(lexer);
}

void Loader::setError(char *fmt, ...)
{
	va_list args;
	char *ptr;
	int room, n;

	room = sizeof(error);

	ptr = error;
	n = snprintf(error, room, "%d:", lexer->line);
	if (n < 0)
		return;

	ptr += n;
	room -= n;

	va_start(args, fmt);
	vsnprintf(ptr, room, fmt, args);
	va_end(args);
}

bool Loader::getIntKeyValue(char *key, int *val)
{
	return (matchWord(key) && getInt(val));
}

bool Loader::getWordKeyValue(char *key, char **val)
{
	return (matchWord(key) && getWord(val));
}

bool Loader::getStringKeyValue(char *key, char **val)
{
	return (matchWord(key) && getString(val));
}

bool Loader::getRaw(char *buf, int len)
{
	enum lex_mode omode;
	char *ptr = buf;

	omode = lexer->mode;
	lexer->mode = LEX_RAW;

	while (len) {		// && lexer_ && !isspace(*ptr)) {

		*ptr = (char) lexer->lexeme[0];

		if (isspace(*ptr)) {
			*ptr = 0;
			break;
		}

		len--;
		ptr++;

		if (!lexer_lex(lexer))
			break;

	}

	lexer->mode = omode;

	// advance based on the original mode
	if (lexer->token)
		lexer_lex(lexer);

	return true;
}
