#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "common.h"

void init_arena(void);
void *malloc_arena(unsigned long mem);
void free_arena(void);

void run_arena_allocator_tests(void);

#define MALLOC(mem) malloc_arena(mem)
#define MALLOCS(type) malloc_arena(sizeof(type))