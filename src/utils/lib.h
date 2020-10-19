#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "common.h"

const char* expand_path(const char* path);
const char* find_lib_dir(void);
char *read_file(const char *path, size_t *return_size);
int filename_to_module(const char *path, char buffer[MAX_IDENTIFIER_LENGTH + 1]);
void path_get_dir_and_filename_from_full(const char *full_path, char **filename, char **dir_path);
void file_find_top_dir();
void file_add_wildcard_files(const char ***files, const char *path, bool recursive);

void memory_init(void);
void *malloc_arena(unsigned long mem);
void free_arena(void);
void print_arena_status(void);

void run_arena_allocator_tests(void);

#define MALLOC(mem) malloc_arena(mem)
#define CALLOCS(type) ({ type *__x = malloc_arena(sizeof(type)); memset(__x, 0, sizeof(type)); __x; })
#define COPY(value) ({ typeof(value) __x = malloc_arena(sizeof(*value)); memcpy(__x, value, sizeof(*value)); __x; })

static inline bool is_power_of_two(uint64_t x)
{
	return x != 0 && (x & (x - 1)) == 0;
}

static inline uint32_t next_highest_power_of_2(uint32_t v)
{
	v--;
	v |= v >> 1U;
	v |= v >> 2U;
	v |= v >> 4U;
	v |= v >> 8U;
	v |= v >> 16U;
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

static inline bool is_binary(char c)
{
	return c  == '0' || c == '1';
}


static inline bool is_binary_or_(char c)
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

/**
 * Convert hex character to nibble
 * @param c
 * @return value or -1 if invalid.
 */
static inline int char_to_nibble(char c)
{
	char conv[256] = {
			['0'] = 1,
			['1'] = 2,
			['2'] = 3,
			['3'] = 4,
			['4'] = 5,
			['5'] = 6,
			['6'] = 7,
			['7'] = 8,
			['8'] = 9,
			['9'] = 10,
			['A'] = 11,
			['B'] = 12,
			['C'] = 13,
			['D'] = 14,
			['E'] = 15,
			['F'] = 16,
			['a'] = 11,
			['b'] = 12,
			['c'] = 13,
			['d'] = 14,
			['e'] = 15,
			['f'] = 16,
	};
	return conv[(unsigned)c] - 1;
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

static inline char is_valid_escape(char c)
{
	switch (c)
	{
		case 'a':
			return '\a';
		case 'b':
			return '\b';
		case 'e':
			return 0x1B;
		case 'f':
			return '\f';
		case 'n':
			return '\n';
		case 'r':
			return '\r';
		case 't':
			return '\t';
		case 'v':
			return '\v';
		case 'x':
			return 'x';
		case 'u':
			return 'u';
		case 'U':
			return 'U';
		case '\'':
			return '\'';
		case '"':
			return '"';
		case '\\':
			return '\\';
		case '0':
			return '\0';
		default:
			return -1;
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
	for (uint32_t i = 0; i < len; i++)
	{
		hash = FNV1a(key[i], hash);
	}
	return hash;
}

typedef struct
{
	unsigned size;
	unsigned capacity;
} _VHeader;

static inline _VHeader* _vec_new(size_t element_size, size_t capacity)
{
	_VHeader *header = malloc_arena(element_size * capacity + sizeof(_VHeader));
	header->size = 0;
	header->capacity = capacity;
	return header;
}

static inline unsigned vec_size(const void*vec)
{
	return vec ? (((_VHeader *)vec) - 1)->size : 0;
}

static inline void vec_resize(const void *vec, unsigned new_size)
{
	if (vec)
	{
		(((_VHeader *)vec) - 1)->size = new_size;
	}
}

static inline void vec_pop(const void *vec)
{
	assert(vec);
	assert(vec_size(vec) > 0);
	(((_VHeader *)vec) - 1)->size--;
}
static inline void* _expand(void *vec, size_t element_size)
{
	if (vec == NULL)
	{
		vec = _vec_new(element_size, 16) + 1;
	}
	_VHeader *header = ((_VHeader *)vec) - 1;
	header->size++;
	if (header->size == header->capacity)
	{
		_VHeader *new_array = _vec_new(element_size, header->capacity << 1U);
		memcpy(new_array, header, element_size * header->capacity + sizeof(_VHeader));
		header = new_array;
		new_array->capacity = header->capacity << 1U;
		vec = header + 1;
	}
	return vec;
}

#define VECEACH(_vec, _index) \
	for (unsigned _index = 0, __vecsize = vec_size(_vec); _index < __vecsize; _index++)


#define VECNEW(_type, _capacity) ((_type *)(_vec_new(sizeof(_type), _capacity) + 1))
#define VECADD(_vec, _value) \
	({ \
		typeof(_vec) __temp = (typeof(_vec))_expand((_vec), sizeof(typeof(*(_vec)))); \
		__temp[vec_size(__temp) - 1] = _value; \
		_vec = __temp; })
#define vec_add(_vec, _value) do { (_vec) = VECADD((_vec), _value); } while (0)

#define VECLAST(_vec) ({ unsigned _size = vec_size(_vec); _size ? (_vec)[_size - 1] : NULL; })


static inline bool is_all_upper(const char* string)
{
	char c;
	while ((c = *(string++)) != '\0')
	{
		if (is_lower(c)) return false;
	}
	return true;
}

static inline bool is_all_lower(const char* string)
{
	char c;
	while ((c = *(string++)) != '\0')
	{
		if (is_upper(c)) return false;
	}
	return true;
}

#ifndef __printflike
#define __printflike(x, y)
#endif

char *strcat_arena(const char *a, const char *b);
char *strformat(const char *var, ...) __printflike(1, 2);
char *strcopy(const char *start, size_t len);

#define MAX(_a, _b) ({ \
  typeof(_a) __a__ = (_a); \
  typeof(_b) __b__ = (_b); \
  __a__ > __b__ ? __a__ : __b__; })

#define MIN(_a, _b) ({ \
  typeof(_a) __a__ = (_a); \
  typeof(_b) __b__ = (_b); \
  __a__ < __b__ ? __a__ : __b__; })
