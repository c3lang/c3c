module std::core::mem::allocator;

/**
 * @require !alignment || math::is_power_of_2(alignment)
 * @require data `unexpectedly missing the allocator`
 */
private fn void*! arena_allocator_function(Allocator* data, usize size, usize alignment, void* old_pointer, AllocationKind kind)
{
	ArenaAllocator* arena = (ArenaAllocator*)data;
	switch (kind)
	{
		case CALLOC:
		case ALLOC:
			assert(!old_pointer, "Unexpected old pointer for alloc.");
			if (!size) return null;
			alignment = alignment_for_allocation(alignment);
			void* mem = arena.alloc(size, alignment, DEFAULT_SIZE_PREFIX)?;
			*(usize*)(mem - DEFAULT_SIZE_PREFIX) = size;
			if (kind == AllocationKind.CALLOC) mem::memset(mem, 0, size, false, DEFAULT_MEM_ALIGNMENT);
        	return mem;
		case REALLOC:
			if (!size) nextcase FREE;
			if (!old_pointer) nextcase ALLOC;
			assert((uptr)old_pointer >= (uptr)arena.data.ptr, "Pointer originates from a different allocator.");
			if (size > arena.data.len) return AllocationFailure.CHUNK_TOO_LARGE!;
			alignment = alignment_for_allocation(alignment);
			usize* old_size_ptr = (usize*)(old_pointer - DEFAULT_SIZE_PREFIX);
			usize old_size = *old_size_ptr;
			// Do last allocation and alignment match?
			if (&arena.data[arena.used]  == old_pointer + old_size && mem::ptr_is_aligned(old_pointer, alignment))
			{
				if (old_size >= size)
				{
					*old_size_ptr = size;
					arena.used -= old_size - size;
					return old_pointer;
				}
				usize new_used = arena.used + size - old_size;
				if (new_used > arena.data.len) return AllocationFailure.OUT_OF_MEMORY!;
				arena.used = new_used;
				*old_size_ptr = size;
				return old_pointer;
			}
			// Otherwise just allocate new memory.
			void* mem = arena.alloc(size, alignment, DEFAULT_SIZE_PREFIX)?;
			*(usize*)(mem - DEFAULT_SIZE_PREFIX) = size;
			mem::memcpy(mem, old_pointer, old_size, false, DEFAULT_MEM_ALIGNMENT, DEFAULT_MEM_ALIGNMENT);
			return mem;
		case FREE:
			if (!old_pointer) return null;
			assert((uptr)old_pointer >= (uptr)arena.data.ptr, "Pointer originates from a different allocator.");
			usize old_size = *(usize*)(old_pointer - DEFAULT_SIZE_PREFIX);
			if (old_pointer + old_size == &arena.data[arena.used])
        	{
        		arena.used -= old_size;
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
 * @require alignment <= MAX_MEMORY_ALIGNMENT `alignment too big`
 * @require this != null
 **/
private fn void*! ArenaAllocator.alloc(ArenaAllocator* this, usize size, usize alignment, usize prefixed_bytes = 0)
{
	void* start_mem = this.data.ptr;
	void* unaligned_pointer = start_mem + this.used + prefixed_bytes;
	if ((uptr)unaligned_pointer < (uptr)start_mem) return AllocationFailure.OUT_OF_MEMORY!;
	usize offset_start = mem::aligned_offset((usize)(uptr)unaligned_pointer, alignment) - (usize)(uptr)start_mem;
	usize end = offset_start + size;
	if (end > this.data.len || end < offset_start) return AllocationFailure.OUT_OF_MEMORY!;
	this.used = end;
	return start_mem + offset_start;
}