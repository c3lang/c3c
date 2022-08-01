module std::core::mem::allocator;

const MAX_MEMORY_ALIGNMENT = 0x1000_0000;
const DEFAULT_MEM_ALIGNMENT = $alignof(void*) * 2;
const DEFAULT_SIZE_PREFIX = usize.sizeof;
const DEFAULT_SIZE_PREFIX_ALIGNMENT = $alignof(usize);

const Allocator* NULL_ALLOCATOR = &_NULL_ALLOCATOR;
const Allocator* LIBC_ALLOCATOR = &_SYSTEM_ALLOCATOR;

define AllocatorFunction = fn void*!(Allocator* allocator, usize new_size, usize alignment, void* old_pointer, AllocationKind kind);

struct Allocator
{
    AllocatorFunction function;
}

enum AllocationKind
{
    ALLOC,
    CALLOC,
    REALLOC,
    FREE,
    RESET,
    MARK,
}

fault AllocationFailure
{
    OUT_OF_MEMORY,
    UNSUPPORTED_OPERATION,
    CHUNK_TOO_LARGE,
}



/**
 * @require !alignment || math::is_power_of_2(alignment)
 */
fn void*! Allocator.alloc(Allocator* allocator, usize size, usize alignment = 0) @inline
{
	return allocator.function(allocator, size, alignment, null, ALLOC);
}

/**
 * @require !alignment || math::is_power_of_2(alignment)
 */
fn void*! Allocator.realloc(Allocator* allocator, void* old_pointer, usize size, usize alignment = 0) @inline
{
	return allocator.function(allocator, size, alignment, old_pointer, REALLOC);
}

fn usize! Allocator.mark(Allocator* allocator) @inline
{
	return (usize)(uptr)allocator.function(allocator, 0, 0, null, MARK);
}

/**
 * @require !alignment || math::is_power_of_2(alignment)
 */
fn void*! Allocator.calloc(Allocator* allocator, usize size, usize alignment = 0) @inline
{
	return allocator.function(allocator, size, alignment, null, CALLOC);
}

fn void! Allocator.free(Allocator* allocator, void* old_pointer) @inline
{
	allocator.function(allocator, 0, 0, old_pointer, FREE)?;
}

fn void Allocator.reset(Allocator* allocator, usize mark = 0)
{
	allocator.function(allocator, mark, 0, null, RESET)!!;
}

private fn usize alignment_for_allocation(usize alignment) @inline
{
	if (alignment < DEFAULT_MEM_ALIGNMENT)
	{
		alignment = DEFAULT_SIZE_PREFIX_ALIGNMENT;
	}
	return alignment;
}

struct DynamicArenaAllocator
{
	inline Allocator allocator;
	Allocator* backing_allocator;
	DynamicArenaPage* page;
	DynamicArenaPage* unused_page;
	usize page_size;
}

/**
 * @require page_size >= 128
 * @require this != null
 **/
fn void DynamicArenaAllocator.init(DynamicArenaAllocator* this, usize page_size, Allocator* backing_allocator = mem::allocator())
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


struct ArenaAllocator
{
	inline Allocator allocator;
	char[] data;
	usize used;
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