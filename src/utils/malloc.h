#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "vmem.h"
#define ARENA_DEF(name, type) \
extern Vmem name##_arena; \
typedef unsigned type##Id; \
static inline type *name##_alloc(void) { return (type *)vmem_alloc(&name##_arena, sizeof(type)); } \
static inline void name##_arena_free(void) { vmem_free(&name##_arena); } \
static inline type *name##_calloc(void) { \
	type *ptr = name##_alloc();  \
	memset(ptr, 0, sizeof(type)); \
	return ptr; } \
static inline type *name##ptr(type ## Id id) { return ((type *)name##_arena.ptr) + id; } \
static inline type##Id name##id(type *ptr) { return (unsigned) (ptr - ((type *)name##_arena.ptr)); } \
static inline type *name##_copy(type *ptr) { type *x = name##_alloc(); memcpy(x, ptr, sizeof(type)); return x; }
