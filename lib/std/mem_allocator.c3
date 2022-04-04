module std::mem;

define AllocatorFunction = fn void*!(void *data, usize new_size, usize alignment, void* old_pointer, AllocationKind kind);

const DEFAULT_MEM_ALIGNMENT = $alignof(void*) * 2;

Allocator main_allocator = { SYSTEM_ALLOCATOR, null };

const AllocatorFunction NULL_ALLOCATOR = &null_allocator_fn;
const AllocatorFunction SYSTEM_ALLOCATOR = &libc_allocator_fn;

/**
 * @require !alignment || @math::is_power_of_2(alignment)
 */
fn void*! Allocator.alloc(Allocator *allocator, usize size, usize alignment = 0) @inline
{
	return allocator.function(allocator.data, size, alignment, null, ALLOC);
}

/**
 * @require !alignment || @math::is_power_of_2(alignment)
 */
fn void*! Allocator.realloc(Allocator *allocator, void* old_pointer, usize size, usize alignment = 0) @inline
{
	return allocator.function(allocator.data, size, alignment, old_pointer, REALLOC);
}

/**
 * @require !alignment || @math::is_power_of_2(alignment)
 */
fn void*! Allocator.calloc(Allocator *allocator, usize size, usize alignment = 0) @inline
{
	return allocator.function(allocator.data, size, alignment, null, CALLOC);
}

fn void! Allocator.free(Allocator *allocator, void* old_pointer) @inline
{
	allocator.function(allocator.data, 0, 0, old_pointer, FREE)?;
}


struct ArenaAllocator
{
	void* memory;
	void* last_ptr;
	usize total;
	usize used;
}

macro void*! allocator_to_function($Type, void* data, usize new_size, usize alignment, void* old_pointer, AllocationKind kind)
{
	$Type* allocator = data;
	switch (kind)
	{
		case ALLOC:
			return allocator.alloc(new_size, alignment) @inline;
		case CALLOC:
			return allocator.calloc(new_size, alignment) @inline;
		case REALLOC:
			return allocator.realloc(old_pointer, new_size, alignment) @inline;
		case FREE:
			allocator.free(old_pointer) @inline?;
			return null;
	}
	@unreachable();
}

fn void*! arena_allocator_function(void* allocator, usize new_size, usize alignment, void* old_pointer, AllocationKind kind)
{
	return @allocator_to_function(ArenaAllocator, allocator, new_size, alignment, old_pointer, kind);
}

fn Allocator ArenaAllocator.to_allocator(ArenaAllocator* allocator) @inline
{
	return { &arena_allocator_function, allocator };
}


/**
 * @require !alignment || @math::is_power_of_2(alignment)
 */
fn void*! ArenaAllocator.alloc(ArenaAllocator* allocator, usize bytes, usize alignment = 0)
{
	if (!bytes) return null;
	if (!alignment) alignment = DEFAULT_MEM_ALIGNMENT;
	iptr next = aligned_offset((iptr)allocator.memory + allocator.used, alignment);
	usize next_after = next - (iptr)allocator.memory + bytes;
	if (next_after > allocator.total) return AllocationFailure.OUT_OF_MEMORY!;
	allocator.used = next_after;
	return allocator.last_ptr = (void*)next;
}

fn void*! ArenaAllocator.calloc(ArenaAllocator* allocator, usize bytes, usize alignment = 0)
{
	char* bits = allocator.alloc(bytes) @inline?;
	mem::set(bits, 0, bytes);
	return bits;
}

/**
 * @require ptr != null
 * @require allocator != null
 **/
fn void*! ArenaAllocator.realloc(ArenaAllocator* allocator, void *ptr, usize bytes, usize alignment = 0)
{
	if (!ptr) return allocator.alloc(bytes, alignment);
	if (!alignment) alignment = DEFAULT_MEM_ALIGNMENT;

	// Is last allocation and alignment matches?
	if (allocator.last_ptr == ptr && ptr_is_aligned(ptr, alignment))
	{
		usize new_used = (usize)(ptr - allocator.memory) + bytes;
		if (new_used > allocator.total) return AllocationFailure.OUT_OF_MEMORY!;
		allocator.used = new_used;
		return ptr;
	}

	// Otherwise just allocate new memory.
	void* new_mem = allocator.alloc(bytes, alignment)?;
	// And copy too much probably!
	copy(new_mem, ptr, (new_mem - ptr) > bytes ? bytes : (usize)(new_mem - ptr));
	return new_mem;
}

fn void! ArenaAllocator.free(ArenaAllocator* allocator, void* ptr)
{
	if (!ptr) return;
	if (ptr == allocator.last_ptr)
	{
		allocator.used = (usize)(ptr - allocator.memory);
		allocator.last_ptr = null;
		return;
	}
}

fn void! ArenaAllocator.init(ArenaAllocator* allocator, usize arena_size)
{
	allocator.memory = alloc_checked(arena_size)?;
	allocator.total = arena_size;
	allocator.used = 0;
	allocator.last_ptr = null;
}

fn void ArenaAllocator.reset(ArenaAllocator* allocator)
{
	allocator.used = 0;
	allocator.last_ptr = null;
}

fn void ArenaAllocator.destroy(ArenaAllocator* allocator)
{
	assert(allocator.memory);
	free(allocator.memory);
	allocator.total = allocator.used = 0;
}


private struct DynamicArenaPage
{
	void* memory;
	void* prev_arena;
	usize total;
	usize used;
}

struct DynamicArenaAllocator
{
	DynamicArenaPage* page;
	usize total;
	usize used;
	usize page_size;
	Allocator allocator;
}

fn void DynamicArenaAllocator.init(DynamicArenaAllocator* this, usize page_size, Allocator allocator = { null, null })
{
	this.page = null;
	this.used = this.total = 0;
	this.page_size = page_size;
	this.allocator = allocator.function ? allocator : thread_allocator;
}

fn void! DynamicArenaAllocator.reset(DynamicArenaAllocator* this)
{
	DynamicArenaPage* page = this.page;
	Allocator allocator = this.allocator;
	while (page && page.prev_arena)
	{
		DynamicArenaPage* next_page = page.prev_arena;
		void* mem = page.memory;
		allocator.free(page)?;
		allocator.free(mem)?;
		page = next_page;
	}
	this.page = page;
}

fn void*! dynamic_arena_allocator_function(void* allocator, usize new_size, usize alignment, void* old_pointer, AllocationKind kind)
{
	return @allocator_to_function(DynamicArenaAllocator, allocator, new_size, alignment, old_pointer, kind);
}

fn Allocator DynamicArenaAllocator.to_allocator(DynamicArenaAllocator* this)
{
	return { &dynamic_arena_allocator_function, this };
}

fn void! DynamicArenaAllocator.destroy(DynamicArenaAllocator* this)
{
	this.reset();
	DynamicArenaPage* first_page = this.page;
	if (!first_page) return;
	void* mem = first_page.memory;
	this.allocator.free(this.page)?;
	this.page = null;
	this.allocator.free(mem)?;
}

fn void! DynamicArenaAllocator.free(DynamicArenaAllocator* allocator, void* ptr)
{
	// This can be made smarter.
	return;
}

/**
 * @require @math::is_power_of_2(alignment)
 */
private fn void*! DynamicArenaAllocator.alloc_new(DynamicArenaAllocator* this, usize size, usize alignment)
{
	usize page_size = @max(this.page_size, size);
	void* mem = this.allocator.alloc(page_size, alignment)?;
	DynamicArenaPage*! page = this.allocator.alloc(DynamicArenaPage.sizeof);
	if (catch err = page)
	{
		this.allocator.free(mem);
		return err!;
	}
    page.memory = mem;
	page.prev_arena = this.page;
	page.total = page_size;
	page.used = size;
	this.page = page;

	return page.memory;
}

/**
 * @require !alignment || @math::is_power_of_2(alignment)
 */
fn void*! DynamicArenaAllocator.calloc(DynamicArenaAllocator* allocator, usize size, usize alignment = 0)
{
	void* mem = allocator.alloc(size, alignment)?;
	set(mem, 0, size);
	return mem;
}

/**
 * @require !alignment || @math::is_power_of_2(alignment)
 */
fn void*! DynamicArenaAllocator.realloc(DynamicArenaAllocator* allocator, void* ptr, usize size, usize alignment = 0)
{
	void* mem = allocator.alloc(size, alignment)?;
	copy(mem, ptr, size);
	return mem;
}

/**
 * @require !alignment || @math::is_power_of_2(alignment)
 */
fn void*! DynamicArenaAllocator.alloc(DynamicArenaAllocator* this, usize size, usize alignment)
{
	DynamicArenaPage *page = this.page;
	if (!alignment) alignment = DEFAULT_MEM_ALIGNMENT;
	if (!page) return this.alloc_new(size, alignment);
	usize start = aligned_offset((uptr)page.memory + page.used, alignment) - (usize)page.memory;
	usize new_used = start + size;
	if (new_used > page.total) return this.alloc_new(size, alignment);
	page.used = new_used;
	return page.memory + start;
}
