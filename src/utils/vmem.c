// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.


#include "vmem.h"

#if PLATFORM_POSIX
#include <sys/mman.h>
#endif

#if PLATFORM_WINDOWS
#include <windows.h>
#endif

#define COMMIT_PAGE_SIZE 0x10000

static inline void mmap_init(Vmem *vmem, size_t size)
{
	vmem->size = size;
#if PLATFORM_WINDOWS
	void* ptr = VirtualAlloc(0, size, MEM_RESERVE, PAGE_NOACCESS);
	vmem->committed = 0;
	if (!ptr)
	{
		FATAL_ERROR("Failed to map virtual memory block");
	}
#elif PLATFORM_POSIX
	void* ptr = mmap(0, size, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
	if ((ptr == MAP_FAILED) || !ptr)
	{
		FATAL_ERROR("Failed to map virtual memory block");
	}
#else
	FATAL_ERROR("Unsupported platform.");
#endif
	vmem->ptr = ptr;
	vmem->allocated = 0;
}

static inline void* mmap_allocate(Vmem *vmem, size_t to_allocate)
{
	assert(to_allocate != 0);
	size_t allocated_after = to_allocate + vmem->allocated;
	size_t blocks_committed = vmem->committed / COMMIT_PAGE_SIZE;
	size_t end_block = (allocated_after + COMMIT_PAGE_SIZE - 1) / COMMIT_PAGE_SIZE;  // round up
	size_t blocks_to_allocate = end_block - blocks_committed;
	if (blocks_to_allocate > 0)
	{
		size_t to_commit = blocks_to_allocate * COMMIT_PAGE_SIZE;
		void* ptr = (char*)vmem->ptr + vmem->committed;
#if PLATFORM_WINDOWS
		void *res = VirtualAlloc(ptr, to_commit, MEM_COMMIT, PAGE_READWRITE);
		if (!res) FATAL_ERROR("Failed to allocate more memory.");
#elif PLATFORM_POSIX
		int res = mprotect(ptr, to_commit, PROT_READ | PROT_WRITE);
		if (res) FATAL_ERROR("Failed to allocate more memory.");
#endif
		vmem->committed += to_commit;
	}
	void *ptr = ((uint8_t *)vmem->ptr) + vmem->allocated;
	vmem->allocated = allocated_after;
	assert(vmem->size > vmem->allocated);
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