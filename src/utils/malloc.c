// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "common.h"

#define KB 1024ul
// Use 1MB at a time.
#define MB (KB * 1024ul)
#define BUCKET_SIZE MB
#define STARTING_ARENA_BUCKETS 16

static uint8_t **arena_buckets;
static size_t arena_buckets_used;
static size_t arena_buckets_array_size;
static size_t current_use;
static void *current_arena;
static int allocations_done;
void memory_init(void)
{
	arena_buckets = malloc(STARTING_ARENA_BUCKETS * sizeof(void *));
	arena_buckets_used = 1;
	arena_buckets_array_size = STARTING_ARENA_BUCKETS;
	arena_buckets[0] = malloc(BUCKET_SIZE);
	allocations_done = 0;
	current_use = 0;
	current_arena = arena_buckets[0];
}

// Simple bump allocator with buckets.
void *malloc_arena(size_t mem)
{
	assert(mem > 0);
	// Round to multiple of 16
	size_t oldmem = mem;
	mem = (mem + 15u) & ~15ull;
	assert(mem >= oldmem);
	if (mem >= BUCKET_SIZE / 4)
	{
		void *ret = malloc(mem);
		ASSERT(ret, "Out of memory.");
		return malloc(mem);
	}
	if (current_use + mem > BUCKET_SIZE)
	{
		if (arena_buckets_used == arena_buckets_array_size)
		{
			arena_buckets_array_size *= 2;
			arena_buckets = realloc(arena_buckets, arena_buckets_array_size * sizeof(void *));
			ASSERT(arena_buckets, "Ran out of memory after allocating %ld KB", BUCKET_SIZE * arena_buckets_used / KB);
		}
		current_arena = malloc(BUCKET_SIZE);
		ASSERT(current_arena, "Ran out of memory after allocating %ld KB", BUCKET_SIZE * arena_buckets_used / KB);
		arena_buckets[arena_buckets_used++] = current_arena;
		current_use = 0;
	}
	uint8_t *ptr = current_arena + current_use;
	current_use += mem;
	allocations_done++;
	if (mem > 4096)
	{
		// printf("Allocated large chunk %llu\n", (unsigned long long)mem);
	}
	return (void *)ptr;

}

void print_arena_status(void)
{
	printf("-- ARENA INFO -- \n");
	printf(" * Memory used:  %ld Kb\n", ((arena_buckets_used - 1) * BUCKET_SIZE + current_use) / 1024);
	printf(" * Buckets used: %d\n", (int)arena_buckets_used);
	printf(" * Allocations: %d\n", allocations_done);
}

void free_arena(void)
{
	for (uint32_t i = 0; i < arena_buckets_used; i++)
	{
		free(arena_buckets[i]);
	}
	current_arena = NULL;
	arena_buckets_used = 0;
	arena_buckets = NULL;
	arena_buckets_array_size = 0;
	current_use = 0;
}


void run_arena_allocator_tests(void)
{
	printf("Begin arena allocator testing.\n");
	bool was_init = arena_buckets != NULL;
	if (!was_init) memory_init();
	free_arena();
	memory_init();
	ASSERT(malloc_arena(10) != malloc_arena(10), "Expected different values...");
	printf("-- Tested basic allocation - OK.\n");
	ASSERT(current_use == 32, "Expected allocations rounded to next 16 bytes");
	malloc_arena(1);
	ASSERT(current_use == 48, "Expected allocations rounded to next 16 bytes");
	printf("-- Tested allocation alignment - OK.\n");
	EXPECT("buckets in use", arena_buckets_used, 1);
	for (int i = 0; i < 8; i++)
	{
		ASSERT(malloc_arena(BUCKET_SIZE / 8), "Should be possible to allocate this");
	}
	EXPECT("buckets in use", arena_buckets_used, 2);
	for (int i = 0; i < 7; i++)
	{
		ASSERT(malloc_arena(BUCKET_SIZE / 8), "Should be possible to allocate this");
	}
	EXPECT("buckets in use", arena_buckets_used, 2);
	ASSERT(malloc_arena(BUCKET_SIZE / 8), "Expected alloc to pass");
	EXPECT("buckets in use", arena_buckets_used, 3);
	for (size_t i = 0; i < 8 * STARTING_ARENA_BUCKETS; i++)
	{
		ASSERT(malloc_arena(BUCKET_SIZE / 8), "Should be possible to allocate this");
	}
	EXPECT("buckets in use", arena_buckets_used, STARTING_ARENA_BUCKETS + 3);
	printf("-- Test switching buckets - OK.\n");
	free_arena();
	ASSERT(arena_buckets_array_size == 0, "Arena not freed?");
	printf("-- Test freeing arena - OK.\n");
	if (was_init) memory_init();
}
