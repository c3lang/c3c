module std::core::mem::allocator;

const MAX_MEMORY_ALIGNMENT = 0x1000_0000;
const DEFAULT_MEM_ALIGNMENT = (void*.alignof) * 2;
const DEFAULT_SIZE_PREFIX = usz.sizeof;
const DEFAULT_SIZE_PREFIX_ALIGNMENT = usz.alignof;

const Allocator* NULL_ALLOCATOR = &_NULL_ALLOCATOR;
const Allocator* LIBC_ALLOCATOR = &_SYSTEM_ALLOCATOR;

typedef AllocatorFunction = fn void*!(Allocator* allocator, usz new_size, usz alignment, usz offset, void* old_pointer, AllocationKind kind);

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
    ALIGNED_ALLOC,
    ALIGNED_CALLOC,
    ALIGNED_REALLOC,
    ALIGNED_FREE,
    RESET,
    MARK,
}

fault AllocationFailure
{
    OUT_OF_MEMORY,
    UNSUPPORTED_OPERATION,
    CHUNK_TOO_LARGE,
}



fn void*! Allocator.alloc(Allocator* allocator, usz size) @inline
{
	return allocator.function(allocator, size, 0, 0, null, ALLOC);
}

/**
 * @require alignment && math::is_power_of_2(alignment)
 */
fn void*! Allocator.alloc_aligned(Allocator* allocator, usz size, usz alignment, usz offset = 0) @inline
{
	return allocator.function(allocator, size, alignment, offset, null, ALIGNED_ALLOC);
}

fn void*! Allocator.realloc(Allocator* allocator, void* old_pointer, usz size) @inline
{
	return allocator.function(allocator, size, 0, 0, old_pointer, REALLOC);
}

/**
 * @require alignment && math::is_power_of_2(alignment)
 */
fn void*! Allocator.realloc_aligned(Allocator* allocator, void* old_pointer, usz size, usz alignment, usz offset = 0) @inline
{
	return allocator.function(allocator, size, alignment, offset, old_pointer, ALIGNED_REALLOC);
}

fn usz! Allocator.mark(Allocator* allocator) @inline
{
	return (usz)(uptr)allocator.function(allocator, 0, 0, 0, null, MARK);
}


fn void*! Allocator.calloc(Allocator* allocator, usz size) @inline
{
	return allocator.function(allocator, size, 0, 0, null, CALLOC);
}

/**
 * @require alignment && math::is_power_of_2(alignment)
 */
fn void*! Allocator.calloc_aligned(Allocator* allocator, usz size, usz alignment, usz offset = 0) @inline
{
	return allocator.function(allocator, size, alignment, offset, null, ALIGNED_CALLOC);
}

fn void! Allocator.free(Allocator* allocator, void* old_pointer) @inline
{
	allocator.function(allocator, 0, 0, 0, old_pointer, FREE)?;
}

fn void! Allocator.free_aligned(Allocator* allocator, void* old_pointer) @inline
{
	allocator.function(allocator, 0, 0, 0, old_pointer, ALIGNED_FREE)?;
}

fn void Allocator.reset(Allocator* allocator, usz mark = 0)
{
	allocator.function(allocator, mark, 0, 0, null, RESET)!!;
}

fn usz alignment_for_allocation(usz alignment) @inline @private
{
	if (alignment < DEFAULT_MEM_ALIGNMENT)
	{
		alignment = DEFAULT_MEM_ALIGNMENT;
	}
	return alignment;
}



