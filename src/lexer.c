/* Copyright (c) 2002 Gordon McNutt */
#include "lexer.h"

#include <ctype.h>
#include <string.h>
#include <stdlib.h>

int lexer_lex(struct lexer *lex)
{
	int i = 0;
	memset(lex->lexeme, 0, lex->n_lexeme);

	if (lex->mode == LEX_RAW) {
		/* end of buffer */
		if (!lex->len) {
			lex->token = 0;
			return 0;
		}
		goto punctuation;
	}

      skip_whitespace:
	/* whitespace */
	while (lex->len && isspace(*lex->ptr) ||
	       (lex->ignore_semicolons && *lex->ptr == ';')) {
		if (*lex->ptr == '\n')
			lex->line++;
		lex->ptr++;
		lex->len--;
	}

	/* end of buffer */
	if (!lex->len) {
		lex->token = 0;
		return 0;
	}

	/* skip to raw character parsing for char mode */
	if (lex->mode == LEX_CHAR) {
		goto punctuation;
	}

	/* comment */
	if (*lex->ptr == lex->comment_char) {
		lex->ptr++;
		lex->len--;
		while (*lex->ptr != '\n' && lex->len) {
			lex->len--;
			lex->ptr++;
		}
		if (*lex->ptr == '\n' && lex->len) {
			lex->ptr++;
			lex->len--;
			lex->line++;
		}

		if (!lex->len) {
			lex->token = 0;
			return 0;
		}

		goto skip_whitespace;
	}

	/* string */
	if (*lex->ptr == '"') {
		lex->ptr++;
		lex->len--;
		while (*lex->ptr != '"' && i < lex->n_lexeme && lex->len) {
			lex->lexeme[i++] = *lex->ptr++;
			lex->len--;
		}
		if (*lex->ptr == '"' && lex->len) {
			lex->ptr++;
			lex->len--;
		}
		lex->token = lexer_STRING;
		return lexer_STRING;
	}

	/* word */
	if (isalpha(*lex->ptr) || *lex->ptr == '_') {
		do {
			lex->lexeme[i++] = *lex->ptr++;
			lex->len--;
		} while ((isalpha(*lex->ptr) ||
			  isdigit(*lex->ptr) ||
			  *lex->ptr == '_') && lex->len && i < lex->n_lexeme);
		lex->token = lexer_WORD;
		return lex->token;
	}

	/* number */
	if (isdigit(*lex->ptr) || *lex->ptr == '-' || *lex->ptr == '+') {
		do {
			lex->lexeme[i++] = *lex->ptr++;
			lex->len--;
		} while (isdigit(*lex->ptr) && lex->len && i < lex->n_lexeme);

		lex->token = lexer_INT;

		/* hex */
		if (*lex->ptr == 'x' && i == 1) {
			do {
				lex->lexeme[i++] = *lex->ptr++;
				lex->len--;
			} while (isxdigit(*lex->ptr) &&
				 lex->len && i < lex->n_lexeme);
		}

		/* float */
		else if (*lex->ptr == '.' || (*lex->ptr == 'x' && i == 1)) {
			do {
				lex->lexeme[i++] = *lex->ptr++;
				lex->len--;
			} while (isdigit(*lex->ptr) &&
				 lex->len && i < lex->n_lexeme);
			lex->token = lexer_FLOAT;
		}

		return lex->token;
	}

	/* punctuation */
      punctuation:
	lex->token = *lex->ptr;
	lex->lexeme[i++] = *lex->ptr++;
	lex->len--;
	return lex->token;
}

struct lexer *lexer_create(int bufsz)
{
	struct lexer *ret;

	if (!(ret = (struct lexer *) malloc(sizeof(struct lexer))))
		return 0;

	memset(ret, 0, sizeof(struct lexer));

	ret->n_lexeme = bufsz;
	if (!(ret->lexeme = (char *) malloc(bufsz + 1))) {
		free(ret);
		return 0;
	}

	return ret;
}

void lexer_destroy(struct lexer *lexer)
{
	free(lexer);
}

void lexer_init(struct lexer *lexer, char *ptr, int len)
{
	lexer->ptr = ptr;
	lexer->len = len;
	lexer->token = lexer_UNDEF;
	memset(lexer->lexeme, 0, lexer->n_lexeme + 1);
}
