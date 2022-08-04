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


private const usize PAGE_IS_ALIGNED = (usize)isize.max + 1;


struct TempAllocatorPage
{
	TempAllocatorPage* prev_page;
	void* start;
	usize mark;
	usize size;
	usize ident;
	char[*] data;
}

macro usize TempAllocatorPage.pagesize(TempAllocatorPage* page) { return page.size & ~PAGE_IS_ALIGNED; }
macro bool TempAllocatorPage.is_aligned(TempAllocatorPage* page) { return page.size & PAGE_IS_ALIGNED == PAGE_IS_ALIGNED; }

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
private fn void*! temp_allocator_function(Allocator* data, usize size, usize alignment, usize offset, void* old_pointer, AllocationKind kind)
{
	TempAllocator* arena = (TempAllocator*)data;
	switch (kind)
	{
		case CALLOC:
		case ALIGNED_CALLOC:
			assert(!old_pointer, "Unexpected old pointer for alloc.");
			if (!size) return null;
			return arena._alloc(size, alignment, offset, true);
		case ALLOC:
		case ALIGNED_ALLOC:
			assert(!old_pointer, "Unexpected old pointer for alloc.");
			if (!size) return null;
			return arena._alloc(size, alignment_for_allocation(alignment), offset, false);
		case ALIGNED_REALLOC:
		case REALLOC:
			if (!size) nextcase FREE;
			if (!old_pointer) nextcase ALLOC;
			return arena._realloc(old_pointer, size, alignment_for_allocation(alignment), offset);
		case FREE:
		case ALIGNED_FREE:
			if (!old_pointer) return null;
			io::println("Freeing stuff\n");
			arena._free(old_pointer)?;
        	return null;
        case MARK:
            return (void*)(uptr)arena.used;
		case RESET:
			arena._reset(size)?;
			return null;
	}
	unreachable();
}

private fn void! TempAllocator._free(TempAllocator* this, void* old_pointer)
{

	// TODO fix free
	assert((uptr)old_pointer >= (uptr)&this.data, "Pointer originates from a different allocator.");
	usize old_size = *(usize*)(old_pointer - DEFAULT_SIZE_PREFIX);
	if (old_pointer + old_size == &this.data[this.used])
    {
        this.used -= old_size;
    }
}
private fn void! TempAllocator._reset(TempAllocator* this, usize mark)
{
	TempAllocatorPage *last_page = this.last_page;
	while (last_page && last_page.mark >= mark)
	{
		TempAllocatorPage *to_free = last_page;
		last_page = last_page.prev_page;
		this._free_page(to_free)?;
	}
	this.last_page = last_page;
	this.used = mark;
}

private fn void! TempAllocator._free_page(TempAllocator* this, TempAllocatorPage* page) @inline
{
	void* mem = page.start;
	if (page.is_aligned()) return this.backing_allocator.free_aligned(mem);
	return this.backing_allocator.free(mem);
}

private fn void*! TempAllocator._realloc_page(TempAllocator* this, TempAllocatorPage* page, usize size, usize alignment, usize offset) @inline
{
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
	usize page_size = page.pagesize();
	// Clear on size > original size.
	void* data = this._alloc(size, alignment, offset, false)?;
	mem::copy(data, &page.data[0], page_size, DEFAULT_MEM_ALIGNMENT, DEFAULT_MEM_ALIGNMENT);
	if (page.is_aligned())
	{
		this.backing_allocator.free_aligned(real_pointer)?;
	}
	else
	{
		this.backing_allocator.free(real_pointer)?;
	}
	return data;
}

private fn void*! TempAllocator._realloc(TempAllocator* this, void* pointer, usize size, usize alignment, usize offset) @inline
{
	TempAllocatorChunk *chunk = pointer - TempAllocatorChunk.sizeof;
	if (chunk.size == (usize)-1)
	{
		assert(this.last_page, "Realloc of non temp pointer");
		// First grab the page
		TempAllocatorPage *page = pointer - TempAllocatorPage.sizeof;
		return this._realloc_page(page, size, alignment, offset);
	}
	assert(pointer < &this.data + this.capacity && pointer >= &this.data, "This is not a temp allocated pointer.");
	assert(pointer < &this.data + this.used, "This is a stale temp pointer.");

	// TODO optimize last allocation
	TempAllocatorChunk* data = this._alloc(size, alignment, offset, size > chunk.size)?;
	mem::copy(data, pointer, chunk.size, DEFAULT_MEM_ALIGNMENT, DEFAULT_MEM_ALIGNMENT);

	return data;
}

/**
 * @require math::is_power_of_2(alignment)
 * @require size > 0
 * @require alignment <= MAX_MEMORY_ALIGNMENT `alignment too big`
 * @require this != null
 **/
private fn void*! TempAllocator._alloc(TempAllocator* this, usize size, usize alignment, usize offset, bool clear)
{
	void* start_mem = &this.data;
	void* starting_ptr = start_mem + this.used;
	void* aligned_header_start = mem::aligned_pointer(starting_ptr, $alignof(TempAllocatorChunk));
	void* mem = aligned_header_start + TempAllocatorChunk.sizeof;
	if (alignment > $alignof(TempAllocatorChunk))
	{
		mem = mem::aligned_pointer(mem + offset, alignment) - offset;
	}
	usize new_usage = (usize)(mem - start_mem) + size;

	// Arena alignment, simple!
	if (new_usage <= this.capacity)
	{
		TempAllocatorChunk* chunk_start = mem - TempAllocatorChunk.sizeof;
    	chunk_start.size = size;
    	this.used = new_usage;
    	if (clear) mem::clear(mem, size, DEFAULT_MEM_ALIGNMENT);
    	return mem;
	}

	// Fallback to backing allocator
	TempAllocatorPage* page;

	// We have something we need to align.
	if (alignment > DEFAULT_MEM_ALIGNMENT || offset)
	{
		// This is actually simpler, since it will create the offset for us.
		usize total_alloc_size = TempAllocatorPage.sizeof + size;
    	if (clear)
		{
			page = this.backing_allocator.calloc_aligned(total_alloc_size, alignment, TempAllocatorPage.sizeof + offset)?;
		}
		else
		{
			page = this.backing_allocator.alloc_aligned(total_alloc_size, alignment, TempAllocatorPage.sizeof + offset)?;
		}
		page.start = page;
		page.size = size | PAGE_IS_ALIGNED;
	}
	else
	{
		// Here we might need to pad
		usize padded_header_size = mem::aligned_offset(TempAllocatorPage.sizeof, DEFAULT_MEM_ALIGNMENT);
		usize total_alloc_size = padded_header_size + size;
		void* alloc = clear ? this.backing_allocator.calloc(total_alloc_size) : this.backing_allocator.alloc(total_alloc_size)?;

		// Find the page.
		page = alloc + padded_header_size - TempAllocatorPage.sizeof;
		assert(mem::ptr_is_aligned(page, $alignof(TempAllocator)));
		assert(mem::ptr_is_aligned(&page.data[0], DEFAULT_MEM_ALIGNMENT));
		page.start = alloc;
		page.size = size;
	}

	// Mark it as a page
	page.ident = ~(usize)0;
	// Store when it was created
	page.mark = this.used;
	// Hook up the page.
	page.prev_page = this.last_page;
	this.last_page = page;
	return &page.data[0];
}

fn void TempAllocator.print_pages(TempAllocator* this, File f)
{
	TempAllocatorPage *last_page = this.last_page;
	if (!last_page)
	{
		f.printf("No pages.\n");
		return;
	}
	f.printf("---Pages----\n");
	uint index = 0;
	while (last_page)
	{
		bool is_not_aligned = !(last_page.size & (1u64 << 63));
		f.printf("%d. Alloc: %d %d at %p%s\n", ++index,
				last_page.size & ~(1u64 << 63), last_page.mark, &last_page.data[0], is_not_aligned ? "" : " [aligned]");
		last_page = last_page.prev_page;
	}
}
