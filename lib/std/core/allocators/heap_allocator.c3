// Copyright (c) 2021-2025 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

module std::core::mem::allocator;
import std::math;

<*
 The SimpleHeapAllocator implements a simple heap allocator on top of an allocator function.

 It uses the given allocator function to allocate memory from some source, but never frees it.
 This allocator is intended to be used in environments where there isn't any native libc malloc,
 and it has to be emulated from a memory region, or wrapping linear memory as is the case for plain WASM.
*>
struct SimpleHeapAllocator (Allocator)
{
	MemoryAllocFn alloc_fn;
	Header* free_list;
}

<*
 @require allocator != null  : "An underlying memory provider must be given"
 @require !self.free_list : "The allocator may not be already initialized"
*>
fn void SimpleHeapAllocator.init(&self, MemoryAllocFn allocator)
{
	self.alloc_fn = allocator;
	self.free_list = null;
}

fn void*? SimpleHeapAllocator.acquire(&self, usz size, AllocInitType init_type, usz alignment) @dynamic
{
	if (init_type == ZERO)
	{
		return alignment > 0 ? @aligned_alloc_fn(self, simple_alloc_calloc, size, alignment) : simple_alloc_calloc(self, size);
	}
	return alignment > 0 ? @aligned_alloc_fn(self, simple_alloc_alloc, size, alignment) : simple_alloc_alloc(self, size);
}

fn void*? SimpleHeapAllocator.resize(&self, void* old_pointer, usz size, usz alignment) @dynamic
{
	return alignment > 0
		? @aligned_realloc_fn(self, simple_alloc_calloc, simple_alloc_free, old_pointer, size, alignment)
		:  simple_alloc_realloc(self, old_pointer, size);
}

fn void SimpleHeapAllocator.release(&self, void* old_pointer, bool aligned) @dynamic
{
	if (aligned)
	{
		@aligned_free_fn(self, simple_alloc_free, old_pointer)!!;
	}
	else
	{
		simple_alloc_free(self, old_pointer);
	}
}

<*
 @require old_pointer && bytes > 0
*>
fn void*? simple_alloc_realloc(SimpleHeapAllocator* self, void* old_pointer, usz bytes) @local
{
	// Find the block header.
	Header* block = (Header*)old_pointer - 1;
	if (block.size >= bytes) return old_pointer;
	void* new = simple_alloc_alloc(self, bytes)!;
	usz max_to_copy = math::min(block.size, bytes);
	mem::copy(new, old_pointer, max_to_copy);
	simple_alloc_free(self, old_pointer);
	return new;
}

fn void*? simple_alloc_calloc(SimpleHeapAllocator* self, usz bytes) @local
{
	void* data = simple_alloc_alloc(self, bytes)!;
	mem::clear(data, bytes, mem::DEFAULT_MEM_ALIGNMENT);
	return data;
}

fn void*? simple_alloc_alloc(SimpleHeapAllocator* self, usz bytes) @local
{
	usz aligned_bytes = mem::aligned_offset(bytes, mem::DEFAULT_MEM_ALIGNMENT);
	if (!self.free_list)
	{
		simple_alloc_add_block(self, aligned_bytes)!;
	}

	Header* current = self.free_list;
	Header* previous = current;
	while (current)
	{
		switch
		{
			case current.size >= aligned_bytes && current.size <= aligned_bytes + Header.sizeof + 64:
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
				Header* unallocated = (Header*)((char*)current + aligned_bytes + Header.sizeof);
				unallocated.size = current.size - aligned_bytes - Header.sizeof;
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
	simple_alloc_add_block(self, aligned_bytes)!;
	return simple_alloc_alloc(self, aligned_bytes);
}

fn void? simple_alloc_add_block(SimpleHeapAllocator* self, usz aligned_bytes) @local
{
	assert(mem::aligned_offset(aligned_bytes, mem::DEFAULT_MEM_ALIGNMENT) == aligned_bytes);
	char[] result = self.alloc_fn(aligned_bytes + Header.sizeof)!;
	Header* new_block = (Header*)result.ptr;
	new_block.size = result.len - Header.sizeof;
	new_block.next = null;
	simple_alloc_free(self, new_block + 1);
}


fn void simple_alloc_free(SimpleHeapAllocator* self, void* ptr) @local
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
			block.size += current.size + Header.sizeof;
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
			prev.size += block.size + Header.sizeof;
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
		usz size;
	}
	usz align;
}
