module std::core::mem::allocator;

const DEFAULT_SIZE_PREFIX = usz.sizeof;
const DEFAULT_SIZE_PREFIX_ALIGNMENT = usz.alignof;

const Allocator* NULL_ALLOCATOR = &_NULL_ALLOCATOR;
const Allocator* LIBC_ALLOCATOR = &_SYSTEM_ALLOCATOR;

def AllocatorFunction = fn void*!(Allocator* allocator, usz new_size, usz alignment, usz offset, void* old_pointer, AllocationKind kind);

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

/**
 * @require is_allocator($AllocatorType) : "Type does not implement all allocator methods"
 * @require is_valid_aligned_allocator($AllocatorType) : "Type does not implement all aligned methods"
 **/
macro AllocatorFunction allocator_fn($AllocatorType)
{
	return fn void*!(Allocator* data, usz size, usz alignment, usz offset, void* old_pointer, AllocationKind kind)
	{
		$AllocatorType* allocator = ($AllocatorType*)data;
		bool $supports_aligned = $defined(allocator.alloc_aligned);
		switch (kind)
		{
			case CALLOC:
				return allocator.calloc(size) @inline;
			case ALIGNED_CALLOC:
				$if $supports_aligned:
					return allocator.calloc_aligned(size, alignment, offset) @inline;
				$else
				    return @aligned_calloc(allocator.calloc, size, alignment, offset);
				$endif
			case ALLOC:
				return allocator.alloc(size);
			case ALIGNED_ALLOC:
				$if $supports_aligned:
					return allocator.alloc_aligned(size, alignment, offset) @inline;
				$else
				    return @aligned_alloc(allocator.alloc, size, alignment, offset);
				$endif
			case REALLOC:
				return allocator.realloc(old_pointer, size) @inline;
			case ALIGNED_REALLOC:
				$if $supports_aligned:
					return allocator.realloc_aligned(old_pointer, size, alignment, offset) @inline;
				$else
			        return @aligned_realloc(allocator.alloc, allocator.free, old_pointer, size, alignment, offset);
				$endif
			case ALIGNED_FREE:
				$if $supports_aligned:
					allocator.free_aligned(old_pointer, alignment, offset) @inline!;
					return null;
				$else
					@aligned_free(allocator.free, old_pointer) @inline!;
					return null;
				$endif
			case FREE:
				allocator.free(old_pointer)!;
				return null;
			case MARK:
				$if $defined(allocator.mark):
					allocator.mark() @inline;
					return null;
				$else
					return AllocationFailure.UNSUPPORTED_OPERATION?;
				$endif
			case RESET:
				$if $defined(allocator.reset):
					allocator.reset() @inline;
					return null;
				$else
					return AllocationFailure.UNSUPPORTED_OPERATION?;
				$endif
		}
		unreachable();
	};
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
	allocator.function(allocator, 0, 0, 0, old_pointer, FREE)!;
}

fn void! Allocator.free_aligned(Allocator* allocator, void* old_pointer) @inline
{
	allocator.function(allocator, 0, 0, 0, old_pointer, ALIGNED_FREE)!;
}

fn void Allocator.reset(Allocator* allocator, usz mark = 0)
{
	allocator.function(allocator, mark, 0, 0, null, RESET)!!;
}

fn usz alignment_for_allocation(usz alignment) @inline @private
{
	if (alignment < mem::DEFAULT_MEM_ALIGNMENT)
	{
		alignment = mem::DEFAULT_MEM_ALIGNMENT;
	}
	return alignment;
}



