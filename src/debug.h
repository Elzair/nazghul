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
