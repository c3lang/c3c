module std::mem;

define AllocatorFunction = fn void*!(void* alloc_data, usize new_size, usize alignment, void* old_pointer, AllocationKind kind);

const MAX_MEMORY_ALIGNMENT = 0x1000_0000;
const DEFAULT_MEM_ALIGNMENT = $alignof(void*) * 2;
const DEFAULT_SIZE_PREFIX = usize.sizeof;
const DEFAULT_SIZE_PREFIX_ALIGNMENT = $alignof(usize);

Allocator main_allocator = { null, SYSTEM_ALLOCATOR };

const AllocatorFunction NULL_ALLOCATOR = &null_allocator_fn;
const AllocatorFunction SYSTEM_ALLOCATOR = &libc_allocator_fn;

/**
 * @require !alignment || math::is_power_of_2(alignment)
 */
fn void*! Allocator.alloc(Allocator *allocator, usize size, usize alignment = 0) @inline
{
	return allocator.function(allocator.data, size, alignment, null, ALLOC);
}

/**
 * @require !alignment || math::is_power_of_2(alignment)
 */
fn void*! Allocator.realloc(Allocator *allocator, void* old_pointer, usize size, usize alignment = 0) @inline
{
	return allocator.function(allocator.data, size, alignment, old_pointer, REALLOC);
}

/**
 * @require !alignment || math::is_power_of_2(alignment)
 */
fn void*! Allocator.calloc(Allocator *allocator, usize size, usize alignment = 0) @inline
{
	return allocator.function(allocator.data, size, alignment, null, CALLOC);
}

fn void! Allocator.free(Allocator *allocator, void* old_pointer) @inline
{
	allocator.function(allocator.data, 0, 0, old_pointer, FREE)?;
}

fn void Allocator.reset(Allocator *allocator)
{
	allocator.function(allocator.data, 0, 0, null, RESET)!!;
}


fn Allocator arena_allocator(MemoryArena *arena)
{
	return { arena, &arena_allocator_function };
}

fn Allocator dynamic_arena_allocator(DynamicArenaAllocator* allocator)
{
	return { allocator, &dynamic_arena_allocator_function };
}


private fn usize alignment_for_allocation(usize alignment) @inline
{
	if (alignment < DEFAULT_MEM_ALIGNMENT)
	{
		alignment = DEFAULT_SIZE_PREFIX_ALIGNMENT;
	}
	return alignment;
}

/**
 * @require !alignment || math::is_power_of_2(alignment)
 * @require data `unexpectedly missing the allocator`
 */
fn void*! arena_allocator_function(void* data, usize size, usize alignment, void* old_pointer, AllocationKind kind)
{
	MemoryArena* arena = data;
	switch (kind)
	{
		case CALLOC:
		case ALLOC:
			assert(!old_pointer, "Unexpected old pointer for alloc.");
			if (!size) return null;
			alignment = alignment_for_allocation(alignment);
			void* mem = arena.alloc(size, alignment, DEFAULT_SIZE_PREFIX)?;
			*(usize*)(mem - DEFAULT_SIZE_PREFIX) = size;
			if (kind == AllocationKind.CALLOC) mem::set(mem, 0, size);
        	return mem;
		case REALLOC:
			if (!size) nextcase FREE;
			if (!old_pointer) nextcase ALLOC;
			assert((uptr)old_pointer >= (uptr)arena.memory, "Pointer originates from a different allocator.");
			if (size > arena.total) return AllocationFailure.OUT_OF_MEMORY!;
			alignment = alignment_for_allocation(alignment);
			usize* old_size_ptr = (usize*)(old_pointer - DEFAULT_SIZE_PREFIX);
			usize old_size = *old_size_ptr;
			// Do last allocation and alignment match?
			if (arena.memory + arena.used  == old_pointer + old_size && ptr_is_aligned(old_pointer, alignment))
			{
				if (old_size >= size)
				{
					*old_size_ptr = size;
					arena.used -= old_size - size;
					return old_pointer;
				}
				usize new_used = arena.used + size - old_size;
				if (new_used > arena.total) return AllocationFailure.OUT_OF_MEMORY!;
				arena.used = new_used;
				*old_size_ptr = size;
				return old_pointer;
			}
			// Otherwise just allocate new memory.
			void* mem = arena.alloc(size, alignment, DEFAULT_SIZE_PREFIX)?;
			*(usize*)(mem - DEFAULT_SIZE_PREFIX) = size;
			copy(mem, old_pointer, old_size);
			return mem;
		case FREE:
			if (!old_pointer) return null;
			assert((uptr)old_pointer >= (uptr)arena.memory, "Pointer originates from a different allocator.");
			usize old_size = *(usize*)(old_pointer - DEFAULT_SIZE_PREFIX);
			if (old_pointer + old_size == arena.memory + arena.used)
        	{
        		arena.used -= old_size;
        	}
        	return null;
		case RESET:
			arena.used = 0;
			return null;
	}
	unreachable();
}

struct DynamicArenaAllocator
{
	Allocator backing_allocator;
	DynamicArenaPage* page;
	DynamicArenaPage* unused_page;
	usize page_size;
}

private struct DynamicArenaPage
{
	void* memory;
	void* prev_arena;
	usize total;
	usize used;
	void* last_ptr;
}


/**
 * @require !alignment || math::is_power_of_2(alignment)
 * @require data `unexpectedly missing the allocator`
 */
fn void*! dynamic_arena_allocator_function(void* data, usize size, usize alignment, void* old_pointer, AllocationKind kind)
{
	DynamicArenaAllocator* allocator = data;
	switch (kind)
	{
		case CALLOC:
			assert(!old_pointer, "Unexpected no old pointer for calloc.");
			if (!size) return null;
			void* mem = allocator.alloc(size, alignment)?;
			set(mem, 0, size);
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

/**
 * @require page_size >= 128
 * @require this != null
 **/
fn void DynamicArenaAllocator.init(DynamicArenaAllocator* this, usize page_size, Allocator* backing_allocator = null)
{
	this.page = null;
	this.unused_page = null;
	this.page_size = page_size;
	this.backing_allocator = backing_allocator ? *backing_allocator : main_allocator;
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
		this.backing_allocator.free(page);
		page = next_page;
	}
	page = this.unused_page;
	while (page)
	{
		DynamicArenaPage* next_page = page.prev_arena;
		this.backing_allocator.free(page);
		page = next_page;
	}
	this.page = null;
	this.unused_page = null;
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
	if (old_size >= size && ptr_is_aligned(old_pointer, alignment))
	{
		*old_size_ptr = size;
		if (current_page.last_ptr == old_pointer)
		{
			current_page.used = (usize)((old_pointer - DEFAULT_SIZE_PREFIX) - current_page.memory);
		}
		return old_pointer;
	}
	if REUSE: (current_page.last_ptr == old_pointer && ptr_is_aligned(old_pointer, alignment))
	{
		assert(size > old_size);
		usize add_size = size - old_size;
		if (add_size + current_page.used > current_page.total) break REUSE;
		*old_size_ptr = size;
		current_page.used += add_size;
		return old_pointer;
	}
	void* new_mem = this.alloc(size, alignment)?;
	copy(new_mem, old_pointer, old_size);
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
		this.backing_allocator.free(mem);
		return err!;
	}
    page.memory = mem;
    usize offset = aligned_offset((usize)mem + DEFAULT_SIZE_PREFIX, alignment) - (usize)mem;
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
    usize start = aligned_offset((uptr)page.memory + page.used + DEFAULT_SIZE_PREFIX, alignment) - (usize)page.memory;
    usize new_used = start + size;
    if ALLOCATE_NEW: (new_used > page.total)
    {
        if ((page = this.unused_page))
        {
            start = aligned_offset((uptr)page.memory + DEFAULT_SIZE_PREFIX, alignment) - (usize)page.memory;
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

