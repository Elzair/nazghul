/* Copyright (c) 2002 Gordon McNutt */
#ifndef wind_h
#define wind_h

#ifdef __cplusplus
extern "C" {
#endif
        
        extern void windInit(void);
        extern void windSetDirection(int dir, int duration);
        extern int windGetDirection(void);
        extern void windAdvanceTurns(void);
        extern void windRepaint(void);
#ifdef __cplusplus
}
#endif

#endif
