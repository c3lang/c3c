#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <stdint.h>
#include <stdbool.h>

static inline bool is_power_of_two(uint64_t x)
{
	return x != 0 && (x & (x - 1)) == 0;
}

static inline uint32_t nextHighestPowerOf2(uint32_t v)
{
	v--;
	v |= v >> 1u;
	v |= v >> 2u;
	v |= v >> 4u;
	v |= v >> 8u;
	v |= v >> 16u;
	v++;
	return v;
}



static inline bool is_lower(char c)
{
	return c >= 'a' && c <= 'z';
}

static inline bool is_upper(char c)
{
	return c >= 'A' && c <= 'Z';
}

static inline bool is_oct(char c)
{
	return c >= '0' && c <= '7';
}

static inline bool is_oct_or_(char c)
{
	switch (c)
	{
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '_':
			return true;
		default:
			return false;
	}
}

static inline bool is_binary(c)
{
	return c  == '0' || c == '1';
}

static inline bool is_binary_or_(c)
{
	switch (c)
	{
		case '0': case '1': case '_':
			return true;
		default:
			return false;
	}
}

static inline bool is_digit_or_(char c)
{
	switch (c)
	{
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
		case '_':
			return true;
		default:
			return false;
	}
}

static inline bool is_digit(char c)
{
	return c >= '0' && c <= '9';
}

static inline bool is_hex_or_(char c)
{
	switch (c)
	{
		case 'a': case 'b': case 'c': case 'd': case 'e':
		case 'f':
		case 'A': case 'B': case 'C': case 'D': case 'E':
		case 'F':
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
		case '_':
			return true;
		default:
			return false;
	}
}

static inline bool is_hex(char c)
{
	switch (c)
	{
		case 'a': case 'b': case 'c': case 'd': case 'e':
		case 'f':
		case 'A': case 'B': case 'C': case 'D': case 'E':
		case 'F':
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			return true;
		default:
			return false;
	}
}

static inline bool is_alphanum_(char c)
{
	switch (c)
	{
		case 'a': case 'b': case 'c': case 'd': case 'e':
		case 'f': case 'g': case 'h': case 'i': case 'j':
		case 'k': case 'l': case 'm': case 'n': case 'o':
		case 'p': case 'q': case 'r': case 's': case 't':
		case 'u': case 'v': case 'w': case 'x': case 'y':
		case 'z':
		case 'A': case 'B': case 'C': case 'D': case 'E':
		case 'F': case 'G': case 'H': case 'I': case 'J':
		case 'K': case 'L': case 'M': case 'N': case 'O':
		case 'P': case 'Q': case 'R': case 'S': case 'T':
		case 'U': case 'V': case 'W': case 'X': case 'Y':
		case 'Z':
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
		case '_':
			return true;
		default:
			return false;
	}
}

static inline bool is_letter(char c)
{
	switch (c)
	{
		case 'a': case 'b': case 'c': case 'd': case 'e':
		case 'f': case 'g': case 'h': case 'i': case 'j':
		case 'k': case 'l': case 'm': case 'n': case 'o':
		case 'p': case 'q': case 'r': case 's': case 't':
		case 'u': case 'v': case 'w': case 'x': case 'y':
		case 'z':
		case 'A': case 'B': case 'C': case 'D': case 'E':
		case 'F': case 'G': case 'H': case 'I': case 'J':
		case 'K': case 'L': case 'M': case 'N': case 'O':
		case 'P': case 'Q': case 'R': case 'S': case 'T':
		case 'U': case 'V': case 'W': case 'X': case 'Y':
		case 'Z':
			return true;
		default:
			return false;
	}
}


#define FNV1_PRIME 0x01000193u
#define FNV1_SEED 0x811C9DC5u
#define FNV1a(c, seed) ((uint32_t)((((unsigned)(c)) ^ (seed)) * FNV1_PRIME))

static inline uint32_t fnv1a(const char *key, uint32_t len)
{
	uint32_t hash = FNV1_SEED;
	for (int i = 0; i < len; i++)
	{
		hash = FNV1a(key[i], hash);
	}
	return hash;
}