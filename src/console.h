/* Copyright (c) 2002 Gordon McNutt */
#ifndef console_h
#define console_h

#ifdef __cplusplus
extern "C" {
#endif

	extern void consoleInit(void);
	extern void consolePrint(char *fmt, ...);
	extern void consoleBackspace(int n);
	extern void consoleNewline(void);
	extern void consoleRepaint(void);
	extern void console_set_y(int y);
	extern int console_get_y(void);

#ifdef __cplusplus
}
#endif
#endif
