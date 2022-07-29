module std::core::mem::allocator;

private struct DynamicArenaPage
{
	void* memory;
	void* prev_arena;
	usize total;
	usize used;
	void* last_ptr;
}


/**
 * @require ptr && this
 * @require this.page `tried to free pointer on invalid allocator`
 */
private fn void DynamicArenaAllocator.free(DynamicArenaAllocator* this, void* ptr)
{
	DynamicArenaPage* current_page = this.page;
	if (ptr == current_page.last_ptr)
	{
		current_page.used = (usize)((ptr - DEFAULT_SIZE_PREFIX) - current_page.memory);
	}
	current_page.last_ptr = null;
}

/**
 * @require old_pointer && size > 0
 * @require this.page `tried to realloc pointer on invalid allocator`
 */
private fn void*! DynamicArenaAllocator.realloc(DynamicArenaAllocator* this, void* old_pointer, usize size, usize alignment)
{
	DynamicArenaPage* current_page = this.page;
	alignment = alignment_for_allocation(alignment);
	usize* old_size_ptr = old_pointer - DEFAULT_SIZE_PREFIX;
	usize old_size = *old_size_ptr;
	// We have the old pointer and it's correctly aligned.
	if (old_size >= size && mem::ptr_is_aligned(old_pointer, alignment))
	{
		*old_size_ptr = size;
		if (current_page.last_ptr == old_pointer)
		{
			current_page.used = (usize)((old_pointer - DEFAULT_SIZE_PREFIX) - current_page.memory);
		}
		return old_pointer;
	}
	if REUSE: (current_page.last_ptr == old_pointer && mem::ptr_is_aligned(old_pointer, alignment))
	{
		assert(size > old_size);
		usize add_size = size - old_size;
		if (add_size + current_page.used > current_page.total) break REUSE;
		*old_size_ptr = size;
		current_page.used += add_size;
		return old_pointer;
	}
	void* new_mem = this.alloc(size, alignment)?;
	mem::copy(new_mem, old_pointer, old_size);
	return new_mem;
}

private fn void DynamicArenaAllocator.reset(DynamicArenaAllocator* this)
{
	DynamicArenaPage* page = this.page;
	DynamicArenaPage** unused_page_ptr = &this.unused_page;
    while (page)
    {
        DynamicArenaPage* next_page = page.prev_arena;
        page.used = 0;
        DynamicArenaPage* prev_unused = *unused_page_ptr;
        *unused_page_ptr = page;
        page.prev_arena = prev_unused;
        page = next_page;
    }
    this.page = page;
}

/**
 * @require math::is_power_of_2(alignment)
 * @require size > 0
 */
private fn void*! DynamicArenaAllocator.alloc_new(DynamicArenaAllocator* this, usize size, usize alignment)
{
	usize page_size = max(this.page_size, size + DEFAULT_SIZE_PREFIX + alignment);
	void* mem = this.backing_allocator.alloc(page_size)?;
	DynamicArenaPage*! page = this.backing_allocator.alloc(DynamicArenaPage.sizeof);
	if (catch err = page)
	{
		this.backing_allocator.free(mem)?;
		return err!;
	}
    page.memory = mem;
    usize offset = mem::aligned_offset((usize)mem + DEFAULT_SIZE_PREFIX, alignment) - (usize)mem;
    usize* size_ptr = mem + offset - DEFAULT_SIZE_PREFIX;
    *size_ptr = size;
	page.prev_arena = this.page;
	page.total = page_size;
	page.used = size + offset;
	this.page = page;

	return page.last_ptr = page.memory + offset;
}

/**
 * @require !alignment || math::is_power_of_2(alignment)
 * @require size > 0
 * @require this
 */
private fn void*! DynamicArenaAllocator.alloc(DynamicArenaAllocator* this, usize size, usize alignment)
{
	alignment = alignment_for_allocation(alignment);
	DynamicArenaPage* page = this.page;
	if (!page && this.unused_page)
	{
		this.page = page = this.unused_page;
		this.unused_page = page.prev_arena;
		page.prev_arena = null;
	}
    if (!page) return this.alloc_new(size, alignment);
    usize start = mem::aligned_offset((uptr)page.memory + page.used + DEFAULT_SIZE_PREFIX, alignment) - (usize)page.memory;
    usize new_used = start + size;
    if ALLOCATE_NEW: (new_used > page.total)
    {
        if ((page = this.unused_page))
        {
            start = mem::aligned_offset((uptr)page.memory + DEFAULT_SIZE_PREFIX, alignment) - (usize)page.memory;
            new_used = start + size;
            if (page.total >= new_used)
            {
	    		this.unused_page = page.prev_arena;
    	    	page.prev_arena = this.page;
    		    this.page = page;
    		    break ALLOCATE_NEW;
            }
        }
 	    return this.alloc_new(size, alignment);
    }
    page.used = new_used;
    void* mem = page.memory + start;
    usize* size_offset = mem - DEFAULT_SIZE_PREFIX;
    *size_offset = size;
    return mem;
}

/**
 * @require !alignment || math::is_power_of_2(alignment)
 * @require data `unexpectedly missing the allocator`
 */
private fn void*! dynamic_arena_allocator_function(Allocator* data, usize size, usize alignment, void* old_pointer, AllocationKind kind)
{
	DynamicArenaAllocator* allocator = (DynamicArenaAllocator*)data;
	switch (kind)
	{
		case CALLOC:
			assert(!old_pointer, "Unexpected no old pointer for calloc.");
			if (!size) return null;
			void* mem = allocator.alloc(size, alignment)?;
			mem::set(mem, 0, size);
			return mem;
		case ALLOC:
			assert(!old_pointer, "Unexpected no old pointer for alloc.");
			if (!size) return null;
			return allocator.alloc(size, alignment);
		case REALLOC:
			if (!size)
			{
				if (!old_pointer) return null;
				allocator.free(old_pointer);
				return null;
			}
			if (!old_pointer) return allocator.alloc(size, alignment);
			void* mem = allocator.realloc(old_pointer, size, alignment)?;
			return mem;
		case FREE:
			if (!old_pointer) return null;
			allocator.free(old_pointer);
			return null;
		case RESET:
			allocator.reset();
			return null;
	}
	unreachable();
}
