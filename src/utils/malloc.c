// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "common.h"
#include "vmem.h"

#define KB 1024ul
// Use 1MB at a time.
#define MB (KB * 1024ul)

static int allocations_done;
static Vmem arena;
uintptr_t arena_zero;
static Vmem char_arena;

void memory_init(size_t max_mem)
{
	if (max_mem) vmem_set_max_limit(max_mem);
	vmem_init(&arena, 2048);
	vmem_init(&char_arena, 512);
	allocations_done = 0;
	arena_zero = (uintptr_t)arena.ptr;
	vmem_alloc(&arena, 16);
}

void memory_release()
{
	vmem_free(&arena);
	vmem_free(&char_arena);
}


void *calloc_string(size_t len)
{
	ASSERT0(len > 0);
	allocations_done++;
	return vmem_alloc(&char_arena, len);
}


// Simple bump allocator with buckets.
void *calloc_arena(size_t mem)
{
	ASSERT0(mem > 0);
	// Round to multiple of 16
	mem = (mem + 15U) & ~15ULL;
	allocations_done++;
	return vmem_alloc(&arena, mem);
}

void print_arena_status(void)
{
	printf("-- ARENA INFO -- \n");
	printf(" * Memory used:  %zu Kb\n", arena.allocated / 1024);
	printf(" * Allocations: %d\n", allocations_done);
	printf(" * String memory used:  %zu Kb\n", char_arena.allocated / 1024);
}

void free_arena(void)
{
	vmem_free(&arena);
}


void run_arena_allocator_tests(void)
{
	printf("Begin arena allocator testing.\n");
	bool was_init = arena.ptr != NULL;
	if (!was_init) memory_init(0);
	memory_release();
	memory_init(0);
	ASSERT(calloc_arena(10) != calloc_arena(10), "Expected different values...");
	printf("-- Tested basic allocation - OK.\n");
	ASSERT(arena.allocated == 48, "Expected allocations rounded to next 16 bytes");
	calloc_arena(1);
	ASSERT(arena.allocated == 64, "Expected allocations rounded to next 16 bytes");
	printf("-- Tested allocation alignment - OK.\n");
	ASSERT(calloc_arena(1024 * 1024) != NULL, "Expected allocation to work");
	free_arena();
	ASSERT(arena.allocated == 0, "Arena not freed?");
	printf("-- Test freeing arena - OK.\n");
	if (was_init) memory_init(0);
}

void *cmalloc(size_t size)
{
	void *ptr = malloc(size);
	if (!ptr) error_exit("Failed to malloc %d bytes.", size);
	return ptr;
}

void *ccalloc(size_t size, size_t elements)
{
	void *ptr = calloc(size, elements);
	if (!ptr) error_exit("Failed to calloc %d bytes.", size * elements);
	return ptr;
}
