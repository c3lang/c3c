module std::core::mem::allocator;
import libc;

private const Allocator _NULL_ALLOCATOR = { &null_allocator_fn };
private const Allocator _SYSTEM_ALLOCATOR = { &libc_allocator_fn };

private fn void*! null_allocator_fn(Allocator* this, usize bytes, usize alignment, void* old_pointer, AllocationKind kind)
{
	switch (kind)
	{
		case ALLOC:
        case CALLOC:
        case REALLOC:
			return AllocationFailure.OUT_OF_MEMORY!;
		default:
			return null;
	}
}

fn void*! libc_allocator_fn(Allocator* unused, usize bytes, usize alignment, void* old_pointer, AllocationKind kind) @inline
{
	if (!alignment) alignment = DEFAULT_MEM_ALIGNMENT;
	assert(math::is_power_of_2(alignment), "Alignment was not a power of 2");

    void* data;
    switch (kind)
    {
        case ALLOC:
            if (alignment > DEFAULT_MEM_ALIGNMENT)
            {
                if (alignment > bytes) bytes = alignment;
	            data = libc::aligned_alloc(alignment, bytes);
            }
            else
            {
                data = libc::malloc(bytes);
            }
            if (!data) return AllocationFailure.OUT_OF_MEMORY!;
            return data;
        case CALLOC:
            if (alignment > DEFAULT_MEM_ALIGNMENT)
            {
                if (alignment > bytes) bytes = alignment;
                data = libc::aligned_alloc(bytes, alignment);
                mem::memset(data, 0, bytes, false, DEFAULT_MEM_ALIGNMENT);
            }
            else
            {
                data = libc::calloc(bytes, 1);
            }
            if (!data) return AllocationFailure.OUT_OF_MEMORY!;
            return data;
        case REALLOC:
            if (!bytes) nextcase FREE;
            if (!old_pointer) nextcase CALLOC;
            $if (libc::HAS_MALLOC_SIZE):
	            if (alignment > DEFAULT_MEM_ALIGNMENT)
                {
                    usize size = libc::malloc_size(old_pointer);
                if (alignment > bytes) bytes = alignment;
	                data = libc::aligned_alloc(bytes, alignment);
		            if (!data) return AllocationFailure.OUT_OF_MEMORY!;
	                if (bytes > size)
	                {
	                    mem::memset(data + size, 0, bytes - size, false, DEFAULT_MEM_ALIGNMENT);
	                }
                    mem::memcpy(data, old_pointer, size, false, DEFAULT_MEM_ALIGNMENT, DEFAULT_MEM_ALIGNMENT);
                    libc::free(old_pointer);
                    return data;
                }
            $endif;
            data = libc::realloc(old_pointer, bytes);
            if (!data) return AllocationFailure.OUT_OF_MEMORY!;
            return data;
        case RESET:
            return AllocationFailure.UNSUPPORTED_OPERATION!;
        case FREE:
            libc::free(old_pointer);
            return null;
    }
    unreachable();
}
