/* Copyright (c) 2002 Gordon McNutt */
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
