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
#ifndef dup_constants_h
#define dup_constants_h

// This section defines the mechanism events reserved by the game engine.
#define MECH_ATTACK       1
#define MECH_STEP         2
#define MECH_HANDLE       3
#define MECH_OPEN         4
#define MECH_CLOSE        5
#define MECH_LOCK         6
#define MECH_UNLOCK       7
#define MECH_MAGIC_LOCK   8
#define MECH_MAGIC_UNLOCK 9
#define MECH_TURN         10
#define MECH_FULL_MOON    11

// This value marks the beginning of the unreserved events. A game script can
// define its own events and number them starting from here.
#define MECH_FIRST_UNRESERVED_EVENT 100

/* Effects */
// gmcnutt: EFFECT_NATURAL was bit 30, and is intended to be used for natural
// abilities (like snakes spitting venom) so that they will not be affected by
// negate magic. But I ran into a problem: I needed to add EFFECT_RESTORE for
// mana restoration potions, but I found that using a constant with bit 31 set
// did not work out right. The parser uses atol(), which sees the high bit set
// and I think assumes it's an overflow, so it converts the value to LONG_MAX,
// which isn't what we intended. Since I'm basically out of bits something had
// to give, and I sacrificed EFFECT_NATURAL for EFFECT_RESTORE.
//
// During the 0.3.x development line we'll address this issue. A simple thing
// to do would be to start using multi-byte bitmaps and fix the parser. But the
// new effect system may make this a non-issue anyway, so let's wait and see.
//
#define EFFECT_NATURAL      0
#define EFFECT_NONE         0
#define EFFECT_POISON       1	/* 0 */
#define EFFECT_BURN         2	/* 1 */
#define EFFECT_SLEEP        4	/* 2 */
#define EFFECT_LIGHT        8	/* 3 */
#define EFFECT_CURE         16	/* 4 */
#define EFFECT_HEAL         32	/* 5 */
#define EFFECT_AWAKEN       64	/* 6 */
#define EFFECT_CHARM        128	/* 7 */
#define EFFECT_DAMAGE       256	/* 8 */
#define EFFECT_UNLOCK       512	/* 9 */
#define EFFECT_REPEL        1024	/* 10 */
#define EFFECT_LOCATE       2048	/* 11 */
#define EFFECT_SUMMON       4096	/* 12 */
#define EFFECT_WIND_CHANGE  8192	/* 13 */
#define EFFECT_TELEPORT     16384	/* 14 */
#define EFFECT_DESTROY      32768	/* 15 */
#define EFFECT_ARMOUR       65536	/* 16 */
#define EFFECT_REVEAL       131072	/* 17 */
#define EFFECT_QUICK        262144	/* 18 */
#define EFFECT_NEGATE       524288	/* 19 */
#define EFFECT_TREMOR       1048576	/* 20 */
#define EFFECT_CONFUSE      2097152	/* 21 */
#define EFFECT_SHOW_TERRAIN 4194304	/* 22 */
#define EFFECT_WIND         8388608	/* 23 */
#define EFFECT_PEER         16777216	/* 24 */
#define EFFECT_CLONE        33554432	/* 25 */
#define EFFECT_INVISIBLE    67108864	/* 26 */
#define EFFECT_TIME_STOP    134217728	/* 27 */
#define EFFECT_RESURRECT    268435456	/* 28 */
#define EFFECT_GATE_TRAVEL  536870912	/* 29 */
#define EFFECT_RESTORE      1073741824  /* 30 */
// WARNING: bit 31 will not work as intended! We are out of bits!

#endif
