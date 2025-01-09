// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.


#include "vmem.h"

#if PLATFORM_POSIX
#include <sys/mman.h>
#include <errno.h>

#endif

#if PLATFORM_WINDOWS
#include <windows.h>
#define COMMIT_PAGE_SIZE 0x10000
#endif


static inline void mmap_init(Vmem *vmem, size_t size)
{
#if PLATFORM_WINDOWS
	void* ptr = VirtualAlloc(0, size, MEM_RESERVE, PAGE_NOACCESS);
	vmem->committed = 0;
	if (!ptr)
	{
		FATAL_ERROR("Failed to map virtual memory block");
	}
#elif PLATFORM_POSIX
	void* ptr = NULL;
	size_t min_size = size / 16;
	if (min_size < 1) min_size = size;
	while (size >= min_size)
	{
		ptr = mmap(0, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
		// It worked?
		if (ptr != MAP_FAILED && ptr) break;
		// Did it fail in a non-retriable way?
		if (errno != ENOMEM && errno != EOVERFLOW && errno != EAGAIN) break;
		// Try a smaller size
		size /= 2;
	}
	// Check if we ended on a failure.
	if ((ptr == MAP_FAILED) || !ptr)
	{
		FATAL_ERROR("Failed to map a virtual memory block.");
	}
	// Otherwise, record the size and we're fine!
#else
	FATAL_ERROR("Unsupported platform.");
#endif
	vmem->size = size;
	vmem->ptr = ptr;
	vmem->allocated = 0;
}

static inline void* mmap_allocate(Vmem *vmem, size_t to_allocate)
{
	size_t allocated_after = to_allocate + vmem->allocated;
#if PLATFORM_WINDOWS
	size_t blocks_committed = vmem->committed / COMMIT_PAGE_SIZE;
	size_t end_block = (allocated_after + COMMIT_PAGE_SIZE - 1) / COMMIT_PAGE_SIZE;  // round up
	size_t blocks_to_allocate = end_block - blocks_committed;
	if (blocks_to_allocate > 0)
	{
		size_t to_commit = blocks_to_allocate * COMMIT_PAGE_SIZE;
		void *res = VirtualAlloc(((char*)vmem->ptr) + vmem->committed, to_commit, MEM_COMMIT, PAGE_READWRITE);
		if (!res) FATAL_ERROR("Failed to allocate more memory.");
		vmem->committed += to_commit;
	}
#endif
	void *ptr = ((uint8_t *)vmem->ptr) + vmem->allocated;
	vmem->allocated = allocated_after;
	if (vmem->size < allocated_after)
	{
		error_exit("Error: The compiler ran out of memory! Over %u MB was allocated from a single memory arena, perhaps "
				   "you called some recursive macro?", (unsigned)(vmem->size / (1024 * 1204)));
	}
	return ptr;
}

size_t max = 0x10000000;
void vmem_set_max_limit(size_t size_in_mb)
{
	max = size_in_mb;
}

void vmem_init(Vmem *vmem, size_t size_in_mb)
{
	if (size_in_mb > max) size_in_mb = max;
	mmap_init(vmem, 1024 * 1024 * size_in_mb);
}

void *vmem_alloc(Vmem *vmem, size_t alloc)
{
	return mmap_allocate(vmem, alloc);
}

void vmem_free(Vmem *vmem)
{
	if (!vmem->ptr) return;
#if PLATFORM_WINDOWS
	VirtualFree(vmem->ptr, 0, MEM_RELEASE);
#elif PLATFORM_POSIX
	munmap(vmem->ptr, vmem->size);
#endif
	vmem->allocated = 0;
	vmem->ptr = 0;
	vmem->size = 0;
}