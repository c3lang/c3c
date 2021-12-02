// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.


#include "vmem.h"

#if PLATFORM_POSIX
#include <sys/mman.h>
#endif

#if defined(WIN32)
#include <Windows.h>
#define COMMIT_PAGE_SIZE 0x10000
#endif


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
	void* ptr = mmap(0, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON | MAP_NORESERVE, -1, 0);
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
#ifndef NDEBUG
	for (size_t i = vmem->allocated; i < allocated_after; i++)
	{
		((uint8_t *)vmem->ptr)[i] = 0xAA;
	}
#endif
	vmem->allocated = allocated_after;
	return ptr;
}

void vmem_init(Vmem *vmem, size_t size_in_mb)
{
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