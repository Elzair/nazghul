/* Copyright (c) 2002 Gordon McNutt */
#ifndef lexer_h
#define lexer_h

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Tokens:
 */
#define lexer_STRING      's'
#define lexer_WORD        'w'
#define lexer_INT         'i'
#define lexer_FLOAT       'f'
#define lexer_UNDEF       'u'

/**
 * Suggested internal buffer size if you're not sure.
 */
#define lexer_DEFBUFSZ    512

/**
 * Convenience macro for parsers:
 */
#define lexer_MATCH(lexer, sym) { \
        if (lexer((lexer)) != (sym)) \
                return 0; \
}

	enum lex_mode {
		LEX_NORMAL = 0,	/* consume one word unit at a time */
		LEX_CHAR,	/* consume one non-whitespace character at a
				 * time */
		LEX_RAW,	/* consume one character at a time (even
				 * whitespace) */
	};

	struct lexer {
		char *ptr;	/* pointer into source buffer */
		int len;	/* remaining length of source buffer */
		int n_lexeme;	/* max allowable lexeme size */
		char *lexeme;	/* lexeme of current symbol */
		int token;	/* token of current symbol */
		int line;
		char comment_char;	/* for rest-of-line comments */
		int ignore_semicolons:1;
		enum lex_mode mode;
	};

/**
 * Create a new lexer.
 * 'bufsz' specifies the max allowable lexeme size.
 * Returns the new lexer or NULL on error.
 */
	extern struct lexer *lexer_create(int bufsz);

/**
 * destroy an existing lexer.
 */
	extern void lexer_destroy(struct lexer *lexer);

/**
 * Setup a new source buffer.
 */
	extern void lexer_init(struct lexer *lexer, char *src, int len);

/**
 * Get the string value of the last symbol extracted.
 */
	static inline char *lexer_lexeme(struct lexer *lexer) {
		return lexer->lexeme;
	}
/**
 * Get the token for the last symbol extracted.
 */ static inline int lexer_token(struct lexer *lexer) {
		return lexer->token;
	}

/**
 * Extract the next symbol from the buffer. Sets the lexeme and token values
 * accordingly. Skips whitespace and comments.
 * Returns the token for the symbol. If there are none (end of buffer reached)
 * returns 0.
 */
	extern int lexer_lex(struct lexer *lexer);

#ifdef __cplusplus
}
#endif

#endif
