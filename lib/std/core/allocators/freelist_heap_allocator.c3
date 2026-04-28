// Copyright (c) 2021-2025 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

module std::core::mem::allocators;
import std::math;

<*
 The FreeListHeapAllocator implements a simple heap allocator on top of an allocator function.

 It uses the given allocator function to allocate memory from some source.
 This allocator is intended to be used in environments where there isn't any native libc malloc,
 and it has to be emulated from a memory region, or wrapping linear memory as is the case for plain WASM.
*>
struct FreeListHeapAllocator (Allocator)
{
	MemoryAllocFn alloc_fn;
	MemoryFreeFn free_fn;
	Header* free_list;
}

<*
 Initializes the allocator structure.

 @require alloc_fn != null  : "An underlying memory provider must be given"
 @require !self.free_list : "The allocator may not be already initialized"
*>
fn void FreeListHeapAllocator.init(&self, MemoryAllocFn alloc_fn, MemoryFreeFn free_fn = dummy_free_fn)
{
	self.alloc_fn = alloc_fn;
	self.free_fn = free_fn;
	self.free_list = null;
}

<*
 Frees resources of the allocator.
*>
fn void FreeListHeapAllocator.free(&self)
{
	if (@unlikely(!self.free_list)) return;

	Header* curr = self.free_list;

	while (curr)
	{
		Header* next = curr.next;
		self.free_fn(curr);
		curr = next;
	}

	self.free_list = null;
}

<*
 Implements the Allocator interface method.

 @require !alignment || math::is_power_of_2(alignment)
 @require alignment <= mem::MAX_MEMORY_ALIGNMENT : `alignment too big`
 @require size > 0
*>
fn void*? FreeListHeapAllocator.acquire(&self, sz size, AllocInitType init_type, sz alignment) @dynamic
{
	if (init_type == ZERO)
	{
		return alignment > 0 ? @aligned_alloc_fn(self, freelist_alloc_calloc, size, alignment) : freelist_alloc_calloc(self, size);
	}
	return alignment > 0 ? @aligned_alloc_fn(self, freelist_alloc_alloc, size, alignment) : freelist_alloc_alloc(self, size);
}

<*
 Implements the Allocator interface method.

 @require !alignment || math::is_power_of_2(alignment)
 @require alignment <= mem::MAX_MEMORY_ALIGNMENT : `alignment too big`
 @require old_pointer != null
 @require size > 0
 @return ? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY
*>
fn void*? FreeListHeapAllocator.resize(&self, void* old_pointer, sz size, sz alignment) @dynamic
{
	return alignment > 0
		? @aligned_realloc_fn(self, freelist_alloc_calloc, freelist_alloc_free, old_pointer, size, alignment)
		:  freelist_alloc_realloc(self, old_pointer, size);
}

<*
 Implements the Allocator interface method.

 @require ptr != null
*>
fn void FreeListHeapAllocator.release(&self, void* ptr, bool aligned) @dynamic
{
	if (aligned)
	{
		@aligned_free_fn(self, freelist_alloc_free, ptr)!!;
	}
	else
	{
		freelist_alloc_free(self, ptr);
	}
}

<*
 Implements reallocation of the provided pointer to new_size

 @require old_pointer != null : "Shouldn't be null"
 @require new_size > 0 : "Should be bigger than 0"
*>
fn void*? freelist_alloc_realloc(FreeListHeapAllocator* self, void* old_pointer, sz new_size) @local
{
	// Find the block header.
	Header* block = (Header*)old_pointer - 1;
	if (block.size >= new_size) return old_pointer;
	void* new = freelist_alloc_alloc(self, new_size)!;
	sz max_to_copy = math::min(block.size, new_size);
	mem::copy(new, old_pointer, max_to_copy);
	freelist_alloc_free(self, old_pointer);
	return new;
}

<*
 Allocates memory block and zeroes it out.

 @require bytes > 0 : "Must be bigger than 0"
*>
fn void*? freelist_alloc_calloc(FreeListHeapAllocator* self, sz bytes) @local
{
	void* data = freelist_alloc_alloc(self, bytes)!;
	mem::clear(data, bytes, mem::DEFAULT_MEM_ALIGNMENT);
	return data;
}

<*
 Finds appropriate memory block for requested amount of bytes.

 @require bytes > 0 : "Must be bigger than 0"
*>
fn void*? freelist_alloc_alloc(FreeListHeapAllocator* self, sz bytes) @local
{
	sz aligned_bytes = mem::aligned_offset(bytes, mem::DEFAULT_MEM_ALIGNMENT);
	if (!self.free_list)
	{
		freelist_alloc_add_block(self, aligned_bytes)!;
	}

	Header* current = self.free_list;
	Header* previous = current;
	while (current)
	{
		switch
		{
			case current.size >= aligned_bytes && current.size <= aligned_bytes + Header::size + 64:
				if (current == previous)
				{
					self.free_list = current.next;
				}
				else
				{
					previous.next = current.next;
				}
				current.next = null;
				return current + 1;
			case current.size > aligned_bytes:
				Header* unallocated = (Header*)((char*)current + aligned_bytes + Header::size);
				unallocated.size = current.size - aligned_bytes - Header::size;
				unallocated.next = current.next;
				if (current == self.free_list)
				{
					self.free_list = unallocated;
				}
				else
				{
					previous.next = unallocated;
				}
				current.size = aligned_bytes;
				current.next = null;
				return current + 1;
			default:
				previous = current;
				current = current.next;
		}
	}
	freelist_alloc_add_block(self, aligned_bytes)!;
	return freelist_alloc_alloc(self, aligned_bytes);
}

<*
 Allocates memory block and inserts it into the free list.

 @require aligned_bytes > 0 : "Must be bigger than 0"
*>
fn void? freelist_alloc_add_block(FreeListHeapAllocator* self, sz aligned_bytes) @local
{
	assert(mem::aligned_offset(aligned_bytes, mem::DEFAULT_MEM_ALIGNMENT) == aligned_bytes);
	char[] result = self.alloc_fn(aligned_bytes + Header::size)!;
	Header* new_block = (Header*)result.ptr;
	new_block.size = result.len - Header::size;
	new_block.next = null;
	freelist_alloc_free(self, new_block + 1);
}

<*
 Inserts free memory block into the free list.
*>
fn void freelist_alloc_free(FreeListHeapAllocator* self, void* ptr) @local
{
	// Empty ptr -> do nothing.
	if (!ptr) return;

	// Find the block header.
	Header* block = (Header*)ptr - 1;

	// No free list? Then just return self.
	if (!self.free_list)
	{
		self.free_list = block;
		return;
	}

	// Find where in the list it should be inserted.
	Header* current = self.free_list;
	Header* prev = current;
	while (current)
	{
		if (block < current)
		{
			// Between prev and current
			if (block > prev) break;
			// Before current
			if (current == prev) break;
		}
		prev = current;
		current = prev.next;
	}
	if (current)
	{
		// Insert after the current block.
		// Are the blocks adjacent?
		if (current == (Header*)((char*)(block + 1) + block.size))
		{
			// Merge
			block.size += current.size + Header::size;
			block.next = current.next;
		}
		else
		{
			// Chain to current
			block.next = current;
		}
	}
	if (prev == current)
	{
		// Swap new start of free list
		self.free_list = block;
	}
	else
	{
		// Prev adjacent?
		if (block == (Header*)((char*)(prev + 1) + prev.size))
		{
			prev.size += block.size + Header::size;
			prev.next = block.next;
		}
		else
		{
			// Link prev to block
			prev.next = block;
		}
	}
}

union Header @local
{
	struct
	{
		Header* next;
		sz size;
	}
	sz align;
}
