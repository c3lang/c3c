module std::core::mem::allocator;

const DEFAULT_SIZE_PREFIX = usz.sizeof;
const DEFAULT_SIZE_PREFIX_ALIGNMENT = usz.alignof;

const Allocator* NULL_ALLOCATOR = &_NULL_ALLOCATOR;
const Allocator* LIBC_ALLOCATOR = &_SYSTEM_ALLOCATOR;

def AllocatorFunction = fn void*!(Allocator* allocator, usz new_size, usz alignment, usz offset, void* old_pointer, AllocationKind kind);
def MemoryAllocFn = fn char[]!(usz);

macro bool is_allocator($Type)
{
	return $checks(
		$Type mem,
		usz sz = 1,
		void*! x = mem.alloc(sz),
		void*! y = mem.calloc(sz),
		void*! z = mem.realloc(x, sz),
		(void)mem.free(x)
	);
}

macro bool is_valid_aligned_allocator($Type)
{
	return !$checks($Type.alloc_aligned) ||
		$checks(
			$Type mem,
			usz sz = 1,
			void*! x = mem.alloc_aligned(sz, sz, az),
			void*! y = mem.calloc_aligned(sz, sz, sz),
			void*! z = mem.realloc_aligned(x, sz, sz, sz),
			(void)mem.free_aligned(x, sz, sz)
		);
}


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
    CHUNK_TOO_LARGE,
}



macro void*! Allocator.alloc(Allocator* allocator, usz size)
{
	return allocator.function(allocator, size, 0, 0, null, ALLOC);
}

/**
 * @require alignment && math::is_power_of_2(alignment)
 */
macro void*! Allocator.alloc_aligned(Allocator* allocator, usz size, usz alignment, usz offset = 0)
{
	return allocator.function(allocator, size, alignment, offset, null, ALIGNED_ALLOC);
}

macro void*! Allocator.realloc(Allocator* allocator, void* old_pointer, usz size)
{
	return allocator.function(allocator, size, 0, 0, old_pointer, REALLOC);
}

/**
 * @require alignment && math::is_power_of_2(alignment)
 */
macro void*! Allocator.realloc_aligned(Allocator* allocator, void* old_pointer, usz size, usz alignment, usz offset = 0)
{
	return allocator.function(allocator, size, alignment, offset, old_pointer, ALIGNED_REALLOC);
}

macro usz Allocator.mark(Allocator* allocator)
{
	return (usz)(uptr)allocator.function(allocator, 0, 0, 0, null, MARK) ?? 0;
}


macro void*! Allocator.calloc(Allocator* allocator, usz size)
{
	return allocator.function(allocator, size, 0, 0, null, CALLOC);
}

/**
 * @require alignment && math::is_power_of_2(alignment)
 */
macro void*! Allocator.calloc_aligned(Allocator* allocator, usz size, usz alignment, usz offset = 0)
{
	return allocator.function(allocator, size, alignment, offset, null, ALIGNED_CALLOC);
}

macro void! Allocator.free(Allocator* allocator, void* old_pointer)
{
	allocator.function(allocator, 0, 0, 0, old_pointer, FREE)!;
}

macro void! Allocator.free_aligned(Allocator* allocator, void* old_pointer)
{
	allocator.function(allocator, 0, 0, 0, old_pointer, ALIGNED_FREE)!;
}

macro void Allocator.reset(Allocator* allocator, usz mark = 0)
{
	(void)allocator.function(allocator, mark, 0, 0, null, RESET);
}

fn usz alignment_for_allocation(usz alignment) @inline @private
{
	if (alignment < mem::DEFAULT_MEM_ALIGNMENT)
	{
		alignment = mem::DEFAULT_MEM_ALIGNMENT;
	}
	return alignment;
}



