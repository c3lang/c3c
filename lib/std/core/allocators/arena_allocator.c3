// Copyright (c) 2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::mem::allocator;

struct ArenaAllocator (Allocator)
{
	char[] data;
	usz used;
}

/**
 * Initialize a memory arena for use using the provided bytes.
 **/
fn void ArenaAllocator.init(&self, char[] data)
{
	self.data = data;
	self.used = 0;
}

fn void ArenaAllocator.clear(&self)
{
	self.used = 0;
}

struct ArenaAllocatorHeader @local
{
	usz size;
	char[*] data;
}

fn void ArenaAllocator.release(&self, void* ptr, bool) @dynamic
{
	if (!ptr) return;
	assert((uptr)ptr >= (uptr)self.data.ptr, "Pointer originates from a different allocator.");
	ArenaAllocatorHeader* header = ptr - ArenaAllocatorHeader.sizeof;
	// Reclaim memory if it's the last element.
	if (ptr + header.size == &self.data[self.used])
	{
		self.used -= header.size + ArenaAllocatorHeader.sizeof;
	}
}
fn usz ArenaAllocator.mark(&self) @dynamic => self.used;
fn void ArenaAllocator.reset(&self, usz mark) @dynamic => self.used = mark;

/**
 * @require !alignment || math::is_power_of_2(alignment)
 * @require alignment <= mem::MAX_MEMORY_ALIGNMENT `alignment too big`
 * @require offset <= mem::MAX_MEMORY_ALIGNMENT `offset too big`
 * @require offset <= size && offset >= 0
 * @require mem::aligned_offset(offset, ArenaAllocatorHeader.alignof) == offset
 **/
fn void*! ArenaAllocator.acquire(&self, usz size, bool clear, usz alignment, usz offset) @dynamic
{
	if (!size) return null;
	alignment = alignment_for_allocation(alignment);
	usz total_len = self.data.len;
	if (size > total_len) return AllocationFailure.CHUNK_TOO_LARGE?;
	void* start_mem = self.data.ptr;
	void* unaligned_pointer_to_offset = start_mem + self.used + ArenaAllocatorHeader.sizeof + offset;
	void* aligned_pointer_to_offset = mem::aligned_pointer(unaligned_pointer_to_offset, alignment);
	usz end = (usz)(aligned_pointer_to_offset - self.data.ptr) + size - offset;
	if (end > total_len) return AllocationFailure.OUT_OF_MEMORY?;
	self.used = end;
	void* mem = aligned_pointer_to_offset - offset;
	ArenaAllocatorHeader* header = mem - ArenaAllocatorHeader.sizeof;
	header.size = size;
	if (clear) mem::clear(mem, size, mem::DEFAULT_MEM_ALIGNMENT);
	return mem;
}

/**
 * @require !alignment || math::is_power_of_2(alignment)
 * @require alignment <= mem::MAX_MEMORY_ALIGNMENT `alignment too big`
 * @require offset <= mem::MAX_MEMORY_ALIGNMENT `offset too big`
 * @require offset <= size && offset >= 0
 * @require mem::aligned_offset(offset, ArenaAllocatorHeader.alignof) == offset
 **/
fn void*! ArenaAllocator.resize(&self, void *old_pointer, usz size, usz alignment, usz offset) @dynamic
{
	if (!size)
	{
		self.release(old_pointer, alignment > 0);
		return null;
	}
	if (!old_pointer)
	{
		return self.acquire(size, true, alignment, offset);
	}
	alignment = alignment_for_allocation(alignment);
	assert(old_pointer >= self.data.ptr, "Pointer originates from a different allocator.");
	usz total_len = self.data.len;
	if (size > total_len) return AllocationFailure.CHUNK_TOO_LARGE?;
	ArenaAllocatorHeader* header = old_pointer - ArenaAllocatorHeader.sizeof;
	usz old_size = header.size;
	// Do last allocation and alignment match?
	if (&self.data[self.used] == old_pointer + old_size && mem::ptr_is_aligned(old_pointer + offset, alignment))
	{
		if (old_size >= size)
		{
			self.used -= old_size - size;
		}
		else
		{
			usz new_used = self.used + size - old_size;
			if (new_used > total_len) return AllocationFailure.OUT_OF_MEMORY?;
			self.used = new_used;
		}
		header.size = size;
		return old_pointer;
	}
	// Otherwise just allocate new memory.
	void* mem = self.acquire(size, false, alignment, offset)!;
	mem::copy(mem, old_pointer, old_size, mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);
	return mem;
}