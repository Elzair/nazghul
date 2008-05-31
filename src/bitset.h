/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2008 Gordon McNutt
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 *
 * Gordon McNutt
 * gmcnutt@users.sourceforge.net
 */

#ifndef bitset_h
#define bitset_h

/**
 * bitset - an arbitrary-sized bitfield
 */

#include <stdlib.h>
#include <string.h>

/**
 * A bitset is an array of unsigned ints. The size of the array is determined
 * when the bitset is allocated or declared, and is based on the number of
 * bits.
 */
typedef unsigned int bitset_t;

/**
 * The number of bytes in an array element.
 */
#define bitset_elem_bytes sizeof(bitset_t)

/**
 * The divisor used by bitset_index.
 */
#define bitset_div (bitset_elem_bytes*8)

/**
 * The mask used by bitset_shift.
 */
#define bitset_mask (bitset_div-1)

/**
 * Given a bit, find out which array element the bit is in.
 *
 * @param bitno is the number of the bit (0 is the first bit)
 * @returns the index of the array the bit is in.
 */
#define bitset_index(bitno) ((bitno)/bitset_div)

/**
 * Given a bit, find its position in the array element.
 *
 * @param bitno is the number of the bit (0 is the first bit)
 * @returns the position of the element the bit is in.
 */
#define bitset_shift(bitno) ((bitno)&bitset_mask)

/**
 * Given the number of bits in a bitset, find out how many elements are in its
 * array.
 *
 * @param nbits is the number of bits in the bitfield.
 * @returns the number of elements in the bitfield array.
 */
#define bitset_elems(nbits) (unsigned int)(((nbits)+bitset_mask)/bitset_div)

/**
 * Given the number of bits in a bitset, find out how many bytes it uses.
 *
 * @param nbits is the number of bits in the bitfield.
 * @returns the number of bytes in the bitfield array.
 */
#define bitset_bytes(nbits) (bitset_elems(nbits)*bitset_elem_bytes)

/**
 * Set a bit.
 *
 * @param bs is the bitset.
 * @param bitno is the number of the bit (0 is the first bit)
 */
#define bitset_set(bs,bitno) ((bs)[bitset_index(bitno)] |= (1<<bitset_shift(bitno)))

/**
 * Clear a bit.
 *
 * @param bs is the bitset.
 * @param bitno is the number of the bit (0 is the first bit)
 */
#define bitset_clr(bs,bitno) ((bs)[bitset_index(bitno)] &= ~(1<<bitset_shift(bitno)))

/**
 * Test if a bit is set.
 *
 * @param bs is the bitset.
 * @param bitno is the number of the bit (0 is the first bit)
 * @returns non-zero iff the bit is set.
 */
#define bitset_tst(bs,bitno) ((bs)[bitset_index(bitno)] & (1<<bitset_shift(bitno)))

/**
 * Allocate a bitset that can hold a given number of bits. All of the bits are
 * initialized to zero.
 *
 * @param n_bits is the number of bits it needs to hold.
 * @returns the bitset of NULL if the allocation failed.
 */
static inline bitset_t *bitset_alloc(int n_bits)
{
        return (bitset_t*)calloc(bitset_elems(n_bits), bitset_elem_bytes);
}

/**
 * Free the memory associated with a bitset.
 *
 * @param bs is the bitset.
 */
static inline void bitset_free(bitset_t *bs)
{
        free(bs);
}

#endif
