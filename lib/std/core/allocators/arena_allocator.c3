// Copyright (c) 2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::mem::allocator;

struct ArenaAllocator
{
	inline Allocator allocator;
	char[] data;
	usz used;
}

/**
 * Initialize a memory arena for use using the provided bytes.
 *
 * @require this != null
 **/
fn void ArenaAllocator.init(ArenaAllocator* this, char[] data)
{
	this.function = &arena_allocator_function;
	this.data = data;
	this.used = 0;
}

/**
 * @require this != null
 **/
fn void ArenaAllocator.reset(ArenaAllocator* this)
{
	this.used = 0;
}

struct ArenaAllocatorHeader
{
	usz size;
	char[*] data;
}
/**
 * @require !alignment || math::is_power_of_2(alignment)
 * @require data `unexpectedly missing the allocator`
 */
fn void*! arena_allocator_function(Allocator* data, usz size, usz alignment, usz offset, void* old_pointer, AllocationKind kind) @private
{
	ArenaAllocator* arena = (ArenaAllocator*)data;
	bool clear = false;
	switch (kind)
	{
		case CALLOC:
		case ALIGNED_CALLOC:
			clear = true;
			nextcase;
		case ALLOC:
		case ALIGNED_ALLOC:
			assert(!old_pointer, "Unexpected old pointer for alloc.");
			if (!size) return null;
			alignment = alignment_for_allocation(alignment);
			void* mem = arena._alloc(size, alignment, offset)!;
			if (clear) mem::clear(mem, size, mem::DEFAULT_MEM_ALIGNMENT);
        	return mem;
		case ALIGNED_REALLOC:
		case REALLOC:
			if (!size) nextcase FREE;
			if (!old_pointer) nextcase ALLOC;
			alignment = alignment_for_allocation(alignment);
			return arena._realloc(old_pointer, size, alignment, offset)!;
		case ALIGNED_FREE:
		case FREE:
			if (!old_pointer) return null;
			assert((uptr)old_pointer >= (uptr)arena.data.ptr, "Pointer originates from a different allocator.");
			ArenaAllocatorHeader* header = old_pointer - ArenaAllocatorHeader.sizeof;
			// Reclaim memory if it's the last element.
			if (old_pointer + header.size == &arena.data[arena.used])
        	{
        		arena.used -= header.size + ArenaAllocatorHeader.sizeof;
        	}
        	return null;
        case MARK:
            return (void*)(uptr)arena.used;
		case RESET:
			arena.used = size;
			return null;
	}
	unreachable();
}

/**
 * @require alignment > 0 `alignment must be non zero`
 * @require math::is_power_of_2(alignment)
 * @require size > 0
 * @require alignment <= mem::MAX_MEMORY_ALIGNMENT `alignment too big`
 * @require offset <= mem::MAX_MEMORY_ALIGNMENT `offset too big`
 * @require offset <= size && offset >= 0
 * @require mem::aligned_offset(offset, ArenaAllocatorHeader.alignof) == offset
 * @require this != null
 **/
fn void*! ArenaAllocator._alloc(ArenaAllocator* this, usz size, usz alignment, usz offset) @private
{
	usz total_len = this.data.len;
	if (size > total_len) return AllocationFailure.CHUNK_TOO_LARGE?;
    void* start_mem = this.data.ptr;
	void* unaligned_pointer_to_offset = start_mem + this.used + ArenaAllocatorHeader.sizeof + offset;
	void* aligned_pointer_to_offset = mem::aligned_pointer(unaligned_pointer_to_offset, alignment);
	usz end = (usz)(aligned_pointer_to_offset - this.data.ptr) + size - offset;
	if (end > total_len) return AllocationFailure.OUT_OF_MEMORY?;
	this.used = end;
	void* mem = aligned_pointer_to_offset - offset;
	ArenaAllocatorHeader* header = mem - ArenaAllocatorHeader.sizeof;
	header.size = size;
	return mem;
}

/**
 * @require alignment > 0 `alignment must be non zero`
 * @require math::is_power_of_2(alignment)
 * @require size > 0
 * @require alignment <= mem::MAX_MEMORY_ALIGNMENT `alignment too big`
 * @require offset <= mem::MAX_MEMORY_ALIGNMENT `offset too big`
 * @require offset <= size && offset >= 0
 * @require mem::aligned_offset(offset, ArenaAllocatorHeader.alignof) == offset
 * @require this != null
 **/
fn void*! ArenaAllocator._realloc(ArenaAllocator* this, void *old_pointer, usz size, usz alignment, usz offset) @private
{
	assert(old_pointer >= this.data.ptr, "Pointer originates from a different allocator.");
	usz total_len = this.data.len;
	if (size > total_len) return AllocationFailure.CHUNK_TOO_LARGE?;
	ArenaAllocatorHeader* header = old_pointer - ArenaAllocatorHeader.sizeof;
	usz old_size = header.size;
	// Do last allocation and alignment match?
	if (&this.data[this.used] == old_pointer + old_size && mem::ptr_is_aligned(old_pointer + offset, alignment))
	{
		if (old_size >= size)
        {
        	this.used -= old_size - size;
        }
        else
        {
	        usz new_used = this.used + size - old_size;
            if (new_used > total_len) return AllocationFailure.OUT_OF_MEMORY?;
            this.used = new_used;
        }
        header.size = size;
        return old_pointer;
	}
	// Otherwise just allocate new memory.
	void* mem = this._alloc(size, alignment, offset)!;
	mem::copy(mem, old_pointer, old_size, mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);
	return mem;
}