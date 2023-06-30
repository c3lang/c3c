module std::core::mem::allocator;

struct OnStackAllocator
{
	inline Allocator allocator;
	Allocator* backing_allocator;
	char[] data;
	usz used;
	OnStackAllocatorExtraChunk* chunk;
}


struct OnStackAllocatorExtraChunk @local
{
	bool is_aligned;
	OnStackAllocatorExtraChunk* prev;
	void* data;
}

/**
 * Initialize a memory arena for use using the provided bytes.
 *
 * @require this != null
 **/
fn void OnStackAllocator.init(OnStackAllocator* this, char[] data, Allocator* using = mem::heap())
{
	this.function = &on_stack_allocator_function;
	this.data = data;
	this.backing_allocator = using;
	this.used = 0;
}

/**
 * @require this != null
 **/
fn void OnStackAllocator.free(OnStackAllocator* this)
{
	OnStackAllocatorExtraChunk* chunk = this.chunk;
	while (chunk)
	{
		if (chunk.is_aligned)
		{
			this.backing_allocator.free_aligned(chunk.data)!!;
		}
		else
		{
			this.backing_allocator.free(chunk.data)!!;
		}
		void* old = chunk;
		chunk = chunk.prev;
		this.backing_allocator.free(old)!!;
	}
	this.chunk = null;
	this.used = 0;
}

struct OnStackAllocatorHeader
{
	usz size;
	char[*] data;
}

/**
 * @require !alignment || math::is_power_of_2(alignment)
 * @require data `unexpectedly missing the allocator`
 */
fn void*! on_stack_allocator_function(Allocator* data, usz size, usz alignment, usz offset, void* old_pointer, AllocationKind kind) @private
{
	OnStackAllocator* allocator = (OnStackAllocator*)data;
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
			return on_stack_allocator_alloc(allocator, size, alignment, offset, clear, kind == AllocationKind.ALIGNED_ALLOC || kind == AllocationKind.ALIGNED_CALLOC);
		case ALIGNED_REALLOC:
		case REALLOC:
			if (!size) nextcase FREE;
			if (!old_pointer) nextcase ALLOC;
			return on_stack_allocator_realloc(allocator, old_pointer, size, alignment, offset, kind == AllocationKind.ALIGNED_REALLOC);
		case ALIGNED_FREE:
		case FREE:
			if (!old_pointer) return null;
			if (allocation_in_stack_mem(allocator, old_pointer)) return null;
			on_stack_allocator_remove_chunk(allocator, old_pointer);
			if (kind == AllocationKind.ALIGNED_FREE)
			{
				allocator.backing_allocator.free_aligned(old_pointer)!;
			}
			else
			{
				allocator.backing_allocator.free(old_pointer)!;
			}
        	return null;
        case MARK:
		case RESET:
			unreachable("Reset unsupported");
	}
	unreachable();
}

fn bool allocation_in_stack_mem(OnStackAllocator* a, void* ptr) @local
{
	return ptr >= a.data.ptr && ptr <= &a.data[^1];
}

fn void on_stack_allocator_remove_chunk(OnStackAllocator* a, void* ptr) @local
{
	OnStackAllocatorExtraChunk* chunk = a.chunk;
	OnStackAllocatorExtraChunk** addr = &a.chunk;
	while (chunk)
	{
		if (chunk.data == ptr)
		{
			*addr = chunk.prev;
			a.backing_allocator.free(chunk)!!;
			return;
		}
		addr = &chunk.prev;
		chunk = *addr;
	}
	unreachable("Missing chunk");
}

fn OnStackAllocatorExtraChunk* on_stack_allocator_find_chunk(OnStackAllocator* a, void* ptr) @local
{
	OnStackAllocatorExtraChunk* chunk = a.chunk;
	while (chunk)
	{
		if (chunk.data == ptr) return chunk;
		chunk = chunk.prev;
	}
	return null;
}

/**
 * @require size > 0
 * @require alignment <= mem::MAX_MEMORY_ALIGNMENT `alignment too big`
 * @require offset <= mem::MAX_MEMORY_ALIGNMENT `offset too big`
 * @require offset <= size && offset >= 0
 * @require mem::aligned_offset(offset, ArenaAllocatorHeader.alignof) == offset
 * @require a != null
 **/
fn void*! on_stack_allocator_realloc(OnStackAllocator* a, void* old_pointer, usz size, usz alignment, usz offset, bool aligned) @local @inline
{
	if (!allocation_in_stack_mem(a, old_pointer))
	{
		OnStackAllocatorExtraChunk* chunk = on_stack_allocator_find_chunk(a, old_pointer);
		assert(chunk, "Tried to realloc pointer not belonging to the allocator");
		if (aligned)
		{
			return chunk.data = a.backing_allocator.realloc_aligned(old_pointer, size, alignment, offset)!;
		}
		return chunk.data = a.backing_allocator.realloc(old_pointer, size)!;
	}

	OnStackAllocatorHeader* header = old_pointer - OnStackAllocatorHeader.sizeof;
	usz old_size = header.size;
	void* mem = on_stack_allocator_alloc(a, size, alignment, offset, true, aligned)!;
	mem::copy(mem, old_pointer, old_size, mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);
	return mem;
}

import std::io;
/**
 * @require size > 0
 * @require alignment <= mem::MAX_MEMORY_ALIGNMENT `alignment too big`
 * @require offset <= mem::MAX_MEMORY_ALIGNMENT `offset too big`
 * @require offset <= size && offset >= 0
 * @require mem::aligned_offset(offset, ArenaAllocatorHeader.alignof) == offset
 * @require a != null
 **/
fn void*! on_stack_allocator_alloc(OnStackAllocator* a, usz size, usz alignment, usz offset, bool clear, bool aligned) @local @inline
{
	alignment = alignment_for_allocation(alignment);
	usz total_len = a.data.len;
    void* start_mem = a.data.ptr;
	void* unaligned_pointer_to_offset = start_mem + a.used + OnStackAllocatorHeader.sizeof + offset;
	void* aligned_pointer_to_offset = mem::aligned_pointer(unaligned_pointer_to_offset, alignment);
	usz end = (usz)(aligned_pointer_to_offset - a.data.ptr) + size - offset;

	Allocator* backing_allocator = a.backing_allocator;

	if (end > total_len)
	{
		OnStackAllocatorExtraChunk* chunk = backing_allocator.alloc(OnStackAllocatorExtraChunk.sizeof)!;
		defer catch backing_allocator.free(chunk)!!;
		defer try a.chunk = chunk;
		*chunk = { .prev = a.chunk, .is_aligned = aligned };
		void* data @noinit;
		switch
		{
			case !aligned && !clear:
				data = backing_allocator.alloc(size)!;
			case aligned && !clear:
				data = backing_allocator.alloc_aligned(size, alignment, offset)!;
			case !aligned && clear:
				data = backing_allocator.calloc(size)!;
			case aligned && clear:
				data = backing_allocator.calloc_aligned(size, alignment, offset)!;
		}
		return chunk.data = data;
	}
	a.used = end;
	void *mem = aligned_pointer_to_offset - offset;
	OnStackAllocatorHeader* header = mem - OnStackAllocatorHeader.sizeof;
	header.size = size;
	return mem;
}
