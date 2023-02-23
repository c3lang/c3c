module std::core::mem::allocator;

struct DynamicArenaAllocator
{
	inline Allocator allocator;
	Allocator* backing_allocator;
	DynamicArenaPage* page;
	DynamicArenaPage* unused_page;
	usz page_size;
}

/**
 * @require page_size >= 128
 * @require this != null
 **/
fn void DynamicArenaAllocator.init(DynamicArenaAllocator* this, usz page_size, Allocator* backing_allocator = mem::current_allocator())
{
	this.function = &dynamic_arena_allocator_function;
	this.page = null;
	this.unused_page = null;
	this.page_size = page_size;
	this.backing_allocator = backing_allocator;
}

/**
 * @require this != null
 **/
fn void DynamicArenaAllocator.destroy(DynamicArenaAllocator* this)
{
	DynamicArenaPage* page = this.page;
	while (page)
	{
		DynamicArenaPage* next_page = page.prev_arena;
		this.backing_allocator.free(page)!!;
		page = next_page;
	}
	page = this.unused_page;
	while (page)
	{
		DynamicArenaPage* next_page = page.prev_arena;
		this.backing_allocator.free(page)!!;
		page = next_page;
	}
	this.page = null;
	this.unused_page = null;
}

struct DynamicArenaPage
{
	void* memory;
	void* prev_arena;
	usz total;
	usz used;
	void* last_ptr;
}

struct DynamicArenaChunk @local
{
	usz size;
}

/**
 * @require ptr && this
 * @require this.page `tried to free pointer on invalid allocator`
 */
fn void DynamicArenaAllocator.free(DynamicArenaAllocator* this, void* ptr) @private
{
	DynamicArenaPage* current_page = this.page;
	if (ptr == current_page.last_ptr)
	{
		current_page.used = (usz)((ptr - DEFAULT_SIZE_PREFIX) - current_page.memory);
	}
	current_page.last_ptr = null;
}

/**
 * @require old_pointer && size > 0
 * @require this.page `tried to realloc pointer on invalid allocator`
 */
fn void*! DynamicArenaAllocator._realloc(DynamicArenaAllocator* this, void* old_pointer, usz size, usz alignment, usz offset) @local
{
	DynamicArenaPage* current_page = this.page;
	alignment = alignment_for_allocation(alignment);
	usz* old_size_ptr = old_pointer - DEFAULT_SIZE_PREFIX;
	usz old_size = *old_size_ptr;
	// We have the old pointer and it's correctly aligned.
	if (old_size >= size && mem::ptr_is_aligned(old_pointer, alignment))
	{
		*old_size_ptr = size;
		if (current_page.last_ptr == old_pointer)
		{
			current_page.used = (usz)((old_pointer - DEFAULT_SIZE_PREFIX) - current_page.memory);
		}
		return old_pointer;
	}
	if REUSE: (current_page.last_ptr == old_pointer && mem::ptr_is_aligned(old_pointer, alignment))
	{
		assert(size > old_size);
		usz add_size = size - old_size;
		if (add_size + current_page.used > current_page.total) break REUSE;
		*old_size_ptr = size;
		current_page.used += add_size;
		return old_pointer;
	}
	void* new_mem = this._alloc(size, alignment, offset)?;
	mem::copy(new_mem, old_pointer, old_size, DEFAULT_MEM_ALIGNMENT);
	return new_mem;
}

fn void DynamicArenaAllocator.reset(DynamicArenaAllocator* this) @private
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
fn void*! DynamicArenaAllocator._alloc_new(DynamicArenaAllocator* this, usz size, usz alignment, usz offset) @local
{
	// First, make sure that we can align it, extending the page size if needed.
	usz page_size = max(this.page_size, mem::aligned_offset(size + DynamicArenaChunk.sizeof + offset, alignment) - offset);

	// Grab the page without alignment (we do it ourselves)
	void* mem = this.backing_allocator.alloc(page_size)?;
	DynamicArenaPage*! page = this.backing_allocator.alloc(DynamicArenaPage.sizeof);
	if (catch err = page)
	{
		this.backing_allocator.free(mem)?;
		return err!;
	}
    page.memory = mem;
    void* mem_start = mem::aligned_pointer(mem + offset + DynamicArenaChunk.sizeof, alignment) - offset;
    assert(mem_start + size < mem + page_size);
    DynamicArenaChunk* chunk = (DynamicArenaChunk*)mem_start - 1;
    chunk.size = size;
	page.prev_arena = this.page;
	page.total = page_size;
	page.used = mem_start + size - page.memory;
	this.page = page;
	page.last_ptr = mem_start;
	return mem_start;
}

/**
 * @require !alignment || math::is_power_of_2(alignment)
 * @require size > 0
 * @require this
 */
fn void*! DynamicArenaAllocator._alloc(DynamicArenaAllocator* this, usz size, usz alignment, usz offset) @local
{
	alignment = alignment_for_allocation(alignment);
	DynamicArenaPage* page = this.page;
	if (!page && this.unused_page)
	{
		this.page = page = this.unused_page;
		this.unused_page = page.prev_arena;
		page.prev_arena = null;
	}
    if (!page) return this._alloc_new(size, alignment, offset);
    void* start = mem::aligned_pointer(page.memory + page.used + DynamicArenaChunk.sizeof + offset, alignment) - offset;
    usz new_used = start - page.memory + size;
    if ALLOCATE_NEW: (new_used > page.total)
    {
        if ((page = this.unused_page))
        {
            start = mem::aligned_pointer(page.memory + page.used + DynamicArenaChunk.sizeof + offset, alignment) - offset;
            new_used = start + size - page.memory;
            if (page.total >= new_used)
            {
	    		this.unused_page = page.prev_arena;
    	    	page.prev_arena = this.page;
    		    this.page = page;
    		    break ALLOCATE_NEW;
            }
        }
 	    return this._alloc_new(size, alignment, offset);
    }

    page.used = new_used;
	assert(start + size == page.memory + page.used);
    void* mem = start;
    DynamicArenaChunk* chunk = (DynamicArenaChunk*)mem - 1;
    chunk.size = size;
    return mem;
}

/**
 * @require !alignment || math::is_power_of_2(alignment)
 * @require data `unexpectedly missing the allocator`
 */
fn void*! dynamic_arena_allocator_function(Allocator* data, usz size, usz alignment, usz offset, void* old_pointer, AllocationKind kind) @private
{
	DynamicArenaAllocator* allocator = (DynamicArenaAllocator*)data;
	switch (kind)
	{
		case CALLOC:
		case ALIGNED_CALLOC:
			assert(!old_pointer, "Unexpected no old pointer for calloc.");
			if (!size) return null;
			void* mem = allocator._alloc(size, alignment, offset)?;
			mem::clear(mem, size, DEFAULT_MEM_ALIGNMENT);
			return mem;
		case ALLOC:
		case ALIGNED_ALLOC:
			assert(!old_pointer, "Unexpected no old pointer for alloc.");
			if (!size) return null;
			return allocator._alloc(size, alignment, offset);
		case REALLOC:
		case ALIGNED_REALLOC:
			if (!size)
			{
				if (!old_pointer) return null;
				allocator.free(old_pointer);
				return null;
			}
			if (!old_pointer) return allocator._alloc(size, alignment, offset);
			void* mem = allocator._realloc(old_pointer, size, alignment, offset)?;
			return mem;
		case ALIGNED_FREE:
		case FREE:
			if (!old_pointer) return null;
			allocator.free(old_pointer);
			return null;
		case MARK:
			unreachable("Tried to mark a dynamic arena");
		case RESET:
			allocator.reset();
			return null;
	}
	unreachable();
}
