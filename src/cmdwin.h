/* Copyright (c) 2002 Gordon McNutt */

#ifndef cmdwin_h
#define cmdwin_h

extern int cmdwin_init(void);
extern void cmdwin_print(char *fmt, ...);
extern void cmdwin_backspace(int n);
extern void cmdwin_clear(void);
extern void cmdwin_repaint_cursor(void);
extern void cmdwin_repaint(void);
extern void cmdwin_mark(void);
extern void cmdwin_erase_back_to_mark(void);

#endif
