/* Copyright (c) 2002 Gordon McNutt */
/* Ripped this off from linux/include/linux/usb.h
 */
#ifndef debug_h
#define debug_h

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

#ifdef DEBUG
#ifdef __cplusplus
#define dbg(format, arg...) \
    do { \
        printf(__FILE__ " line %d: ", __LINE__); \
        printf(format "\n" , ## arg); \
    } while(0)
#else
#define dbg(format, arg...) \
        printf(__FUNCTION__ ": " format "\n" , ## arg)
#endif				/* __cplusplus */
#else
#define dbg(format, arg...) \
         do {} while (0)
#endif

#ifdef __cplusplus
// g++ does not support the __FUNCTION__ compiler keyword
#define err(format, arg...) \
        fprintf(stderr, format "\n" , ## arg)
#define info(format, arg...) \
        printf(format "\n" , ## arg)
#define warn(format, arg...) \
        printf(format "\n" , ## arg)
#else
#define err(format, arg...) \
        fprintf(stderr, __FUNCTION__ ": " format "\n" , ## arg)
#define info(format, arg...) \
        printf(__FUNCTION__ ": " format "\n" , ## arg)
#define warn(format, arg...) \
        printf(__FUNCTION__ ": " format "\n" , ## arg)
#endif

#ifdef __cplusplus
}
#endif
#endif
