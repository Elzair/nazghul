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

// This value marks the beginning of the unreserved events. A game script can
// define its own events and number them starting from here.
#define MECH_FIRST_UNRESERVED_EVENT 100

/* Effects */
#define EFFECT_NONE         0
#define EFFECT_POISON       1
#define EFFECT_BURN         2
#define EFFECT_SLEEP        4
#define EFFECT_LIGHT        8
#define EFFECT_CURE         16
#define EFFECT_HEAL         32
#define EFFECT_AWAKEN       64
#define EFFECT_CHARM        128
#define EFFECT_DAMAGE       256
#define EFFECT_UNLOCK       512
#define EFFECT_REPEL        1024
#define EFFECT_LOCATE       2048
#define EFFECT_SUMMON       4096
#define EFFECT_WIND_CHANGE  8192
#define EFFECT_TELEPORT     16384
#define EFFECT_DESTROY      32768
#define EFFECT_ARMOUR       65536
#define EFFECT_REVEAL       131072
#define EFFECT_QUICK        262144
#define EFFECT_NEGATE       524288
#define EFFECT_TREMOR       1048576
#define EFFECT_CONFUSE      2097152
#define EFFECT_SHOW_TERRAIN 4194304
#define EFFECT_WIND         8388608
#define EFFECT_PEER         16777216
#define EFFECT_CLONE        33554432
#define EFFECT_INVISIBLE    67108864
#define EFFECT_TIME_STOP    134217728
#define EFFECT_RESURRECT    268435456
#define EFFECT_GATE_TRAVEL  536870912
#define EFFECT_NATURAL      1073741824 /* bit 30 */

#endif

