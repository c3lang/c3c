// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.


#include "vmem.h"

#include "lib.h"

#if PLATFORM_POSIX
#include <sys/mman.h>
#include <errno.h>
#endif

#if PLATFORM_WINDOWS
#include <windows.h>
#endif

#define MB_SIZE ((size_t)1024 * 1024U)
// 4 MB at a time
#define COMMIT_PAGE_SIZE (MB_SIZE * 4U)

#if PLATFORM_LINUX
#define EXTRA_ALLOCATION (2 * MB_SIZE)
#else
#define EXTRA_ALLOCATION 0
#endif

static inline void mmap_init(Vmem *vmem, size_t size)
{
	assert(is_power_of_two(size) && "Size should be a power of 2");

#if PLATFORM_WINDOWS
	void* ptr = VirtualAlloc(0, size, MEM_RESERVE, PAGE_NOACCESS);
	if (!ptr)
	{
		FATAL_ERROR("Failed to map virtual memory block");
	}
	vmem->real_ptr = vmem->ptr = ptr;
#elif PLATFORM_POSIX
	void* ptr = NULL;
	if (size < COMMIT_PAGE_SIZE) size = COMMIT_PAGE_SIZE;
	size_t min_size = size / 512;
	if (min_size < COMMIT_PAGE_SIZE) min_size = COMMIT_PAGE_SIZE;
	while (size >= min_size)
	{
		ptr = mmap(0, size + EXTRA_ALLOCATION, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
		// It worked?
		if (ptr != MAP_FAILED) break;
		// Did it fail in a non-retriable way?
		if (errno != ENOMEM && errno != EOVERFLOW && errno != EAGAIN) break;
		// Try a smaller size
		size /= 2;
	}
	// Check if we ended on a failure.
	if (ptr == MAP_FAILED)
	{
		FATAL_ERROR("Failed to map a virtual memory block.");
	}
	vmem->real_ptr = ptr;
	#if PLATFORM_LINUX
		vmem->ptr = (void*)((((size_t)ptr + EXTRA_ALLOCATION - 1) / EXTRA_ALLOCATION) * EXTRA_ALLOCATION);
		#ifdef MADV_HUGEPAGE
			madvise(vmem->ptr, size, MADV_HUGEPAGE);
		#endif
	#else
		vmem->ptr = ptr;
	#endif
#else
	FATAL_ERROR("Unsupported platform.");
#endif
	vmem->size = size;
	vmem->allocated = 0;
	vmem->committed = 0;
}

static inline void* mmap_allocate(Vmem *vmem, size_t to_allocate)
{
	size_t allocated_after = to_allocate + vmem->allocated;
	size_t blocks_committed = vmem->committed / COMMIT_PAGE_SIZE;
	size_t end_block = (allocated_after + COMMIT_PAGE_SIZE - 1) / COMMIT_PAGE_SIZE;  // round up
	size_t blocks_to_allocate = end_block - blocks_committed;
	if (blocks_to_allocate > 0)
	{
		size_t to_commit = blocks_to_allocate * COMMIT_PAGE_SIZE;
		char *start_ptr = ((char*)vmem->ptr) + vmem->committed;
#if PLATFORM_POSIX
		bool success = mprotect(start_ptr, to_commit, PROT_READ | PROT_WRITE) == 0;
	#if PLATFORM_LINUX
		#ifdef MADV_POPULATE_WRITE
			if (success) madvise(start_ptr, to_commit, MADV_POPULATE_WRITE);
		#endif
	#endif
#elif PLATFORM_WINDOWS
		void *res = VirtualAlloc(start_ptr, to_commit, MEM_COMMIT, PAGE_READWRITE);
		bool success = res != NULL;
#else
		bool success = false;
		FATAL_ERROR("Unsupported platform.");
#endif
		if (!success)
		{
			if (to_allocate < 0x1000)
			{
				error_exit("⚠️Fatal Error! The compiler ran out of memory: more than %u MB was allocated from a single memory arena, "
					"exceeding the current maximum limit. Perhaps you called some recursive macro?",
					(unsigned)(allocated_after / MB_SIZE));
			}
			error_exit("⚠️Fatal Error! The compiler ran out of memory: more than %u MB was allocated from a single memory arena, "
				"exceeding the current maximum limit. The last allocation was for %llu bytes.",
				(unsigned)(allocated_after / MB_SIZE), (unsigned long long)to_allocate);
		}
		vmem->committed += to_commit;
	}
	void *ptr = ((uint8_t *)vmem->ptr) + vmem->allocated;
	vmem->allocated = allocated_after;
	if (vmem->size < allocated_after)
	{
		if (to_allocate < 0x1000)
		{
			error_exit("⚠️Fatal Error! The compiler ran out of memory: more than %u MB was allocated from a single memory arena, "
				"exceeding the current maximum limit. Perhaps you called some recursive macro?",
				(unsigned)(vmem->size / MB_SIZE));
		}
		error_exit("⚠️Fatal Error! The compiler ran out of memory: more than %u MB was allocated from a single memory arena, "
			"exceeding the current maximum limit. The last allocation was for %llu bytes.",
			(unsigned)(vmem->size / MB_SIZE), (unsigned long long)to_allocate);
	}
	return ptr;
}

size_t max = 0x10000000;

void vmem_set_max_limit(size_t size_in_mb)
{
	assert(is_power_of_two(size_in_mb) && "Max limit should be a power of 2");
	max = size_in_mb;
}

void vmem_init(Vmem *vmem, size_t size_in_mb)
{
	if (size_in_mb > max) size_in_mb = max;
	mmap_init(vmem, MB_SIZE * size_in_mb);
}

void *vmem_alloc(Vmem *vmem, size_t alloc)
{
	return mmap_allocate(vmem, alloc);
}

void vmem_free(Vmem *vmem)
{
	if (!vmem->real_ptr) return;
#if PLATFORM_WINDOWS
	VirtualFree(vmem->real_ptr, 0, MEM_RELEASE);
#elif PLATFORM_POSIX
	munmap(vmem->real_ptr, vmem->size + EXTRA_ALLOCATION);
#endif
	vmem->allocated = 0;
	vmem->real_ptr = 0;
	vmem->ptr = 0;
	vmem->size = 0;
	vmem->committed = 0;
}
