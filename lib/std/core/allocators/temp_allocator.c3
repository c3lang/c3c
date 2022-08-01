module std::core::mem::allocator;
import std::io;

private struct TempAllocatorChunk
{
	usize size;
	char[*] data;
}

struct TempAllocator
{
	inline Allocator allocator;
	Allocator* backing_allocator;
	TempAllocatorPage* last_page;
	usize used;
	usize capacity;
	char[*] data;
}

struct TempAllocatorPage
{
	TempAllocatorPage* prev_page;
	usize mark;
	void* start;
	usize size;
	usize ident;
	char[*] data;
}

/**
 * @require size >= 16
 **/
fn TempAllocator*! new_temp(usize size, Allocator* backing_allocator)
{
	TempAllocator* allocator = backing_allocator.alloc(size + TempAllocator.sizeof)?;
	allocator.last_page = null;
	allocator.function = &temp_allocator_function;
	allocator.backing_allocator = backing_allocator;
	allocator.used = 0;
	allocator.capacity = size;
	return allocator;
}

/**
 * @require !alignment || math::is_power_of_2(alignment)
 * @require data `unexpectedly missing the allocator`
 */
private fn void*! temp_allocator_function(Allocator* data, usize size, usize alignment, void* old_pointer, AllocationKind kind)
{
	TempAllocator* arena = (TempAllocator*)data;
	switch (kind)
	{
		case CALLOC:
		case ALLOC:
			assert(!old_pointer, "Unexpected old pointer for alloc.");
			if (!size) return null;
			return arena._alloc(size, alignment_for_allocation(alignment), kind == AllocationKind.CALLOC);
		case REALLOC:
			if (!size) nextcase FREE;
			if (!old_pointer) nextcase ALLOC;
			return arena._realloc(old_pointer, size, alignment_for_allocation(alignment));
		case FREE:
			if (!old_pointer) return null;
			assert((uptr)old_pointer >= (uptr)&arena.data, "Pointer originates from a different allocator.");
			usize old_size = *(usize*)(old_pointer - DEFAULT_SIZE_PREFIX);
			if (old_pointer + old_size == &arena.data[arena.used])
        	{
        		arena.used -= old_size;
        	}
        	return null;
        case MARK:
            return (void*)(uptr)arena.used;
		case RESET:
			arena._reset(size)?;
			return null;
	}
	unreachable();
}


private fn void! TempAllocator._reset(TempAllocator* this, usize mark)
{
	TempAllocatorPage *last_page = this.last_page;
	while (last_page)
	{
		if (last_page.mark < mark) break;
		void* mem = last_page.start;
		last_page = last_page.prev_page;
		this.backing_allocator.free(mem)?;
	}
	this.last_page = last_page;
	this.used = mark;
}

private fn void*! TempAllocator._realloc(TempAllocator* this, void* pointer, usize size, usize alignment)
{
	TempAllocatorChunk *chunk = pointer - TempAllocatorChunk.sizeof;
	if (chunk.size == ~(usize)0)
	{
		assert(this.last_page, "Realloc of non temp pointer");
		// First grab the page
		TempAllocatorPage *page = pointer - TempAllocatorPage.sizeof;

		// Then the actual start pointer:
		void* real_pointer = page.start;

		// Walk backwards to find the pointer to this page.
		TempAllocatorPage **pointer_to_prev = &this.last_page;

		// Remove the page from the list
		while (*pointer_to_prev != page)
		{
			pointer_to_prev = &((*pointer_to_prev).prev_page);
		}
		*pointer_to_prev = page.prev_page;

		// Find the new header size
		usize actual_header_size = mem::aligned_offset(TempAllocatorPage.sizeof, alignment);
		// And the old one.
		uptrdiff prev_header_size = pointer - real_pointer;

		// If the new header size needs to move, we're out of luck.
		// We need to make a copy
		if (actual_header_size > prev_header_size)
		{
			// Clear only if the new size is bigger than the old one.
			void* data = this._alloc(size, alignment, size > page.size)?;
			mem::memcpy(data, pointer, page.size);
			this.backing_allocator.free(real_pointer)?;
			return data;
		}
		void* new_start = this.backing_allocator.realloc(real_pointer, prev_header_size + size, alignment)?;
		page = new_start + prev_header_size - TempAllocatorPage.sizeof;
		page.mark = this.used;
		page.prev_page = this.last_page;
		this.last_page = page;
		io::printf("Size: %d\n", size);
		page.size = size;
		page.start = new_start;
		return &page.data;
	}
	assert(pointer < &this.data + this.capacity && pointer >= &this.data, "This is not a temp allocated pointer.");
	assert(pointer < &this.data + this.used, "This is a stale temp pointer.");

	io::printf("realloc normal %s\n", size);
	// TODO optimize last allocation
	TempAllocatorChunk* data = this._alloc(size, alignment, size > chunk.size)?;
	mem::memcpy(data, pointer, chunk.size, false, DEFAULT_MEM_ALIGNMENT, DEFAULT_MEM_ALIGNMENT);

	return data;
}

/**
 * @require alignment > 0 `alignment must be non zero`
 * @require math::is_power_of_2(alignment)
 * @require size > 0
 * @require alignment <= MAX_MEMORY_ALIGNMENT `alignment too big`
 * @require this != null
 **/
private fn void*! TempAllocator._alloc(TempAllocator* this, usize size, usize alignment, bool clear)
{
	void* start_mem = &this.data;
	uptr starting_ptr = (uptr)start_mem + this.used;
	uptr aligned_header_start = mem::aligned_offset(starting_ptr, $alignof(TempAllocatorChunk));
	uptr unaligned_data_start = aligned_header_start + TempAllocatorChunk.sizeof;
	usize data_start = mem::aligned_offset(unaligned_data_start, alignment);

	usize new_usage = data_start + size - (uptr)start_mem;

	// Fallback to backing allocator
	if (new_usage > this.capacity)
	{
		// Enlarge the header if needed
		usize actual_header_size = mem::aligned_offset(TempAllocator.sizeof, alignment);
		usize total_alloc_size = actual_header_size + size;
		void* start = clear ? this.backing_allocator.calloc(total_alloc_size, alignment) : this.backing_allocator.alloc(total_alloc_size, alignment)?;

		// Move forward to the memory
		void* mem = start + actual_header_size;
		TempAllocatorPage* page = mem - TempAllocator.sizeof;
		page.start = start;
		page.ident = ~(usize)0;
		page.mark = this.used;
		page.size = size;
		page.prev_page = this.last_page;
		this.last_page = page;
		assert(&page.data == mem, "Expected match");
		return mem;
	}

	TempAllocatorChunk* chunk_start = (TempAllocatorChunk*)(data_start - TempAllocatorChunk.sizeof);
	chunk_start.size = size;
	this.used = new_usage;
	void* mem = &chunk_start.data;
	if (clear) mem::memset(mem, 0, size, false, DEFAULT_MEM_ALIGNMENT);
	return mem;
}