module std::mem;
import libc;

private fn void*! null_allocator_fn(void *data, usize bytes, usize alignment, void* old_pointer, AllocationKind kind)
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

fn void*! libc_allocator_fn(void *unused, usize bytes, usize alignment, void* old_pointer, AllocationKind kind) @inline
{
	if (!alignment) alignment = DEFAULT_MEM_ALIGNMENT;
	assert(math::is_power_of_2(alignment), "Alignment was not a power of 2");

    void* data;
    switch (kind)
    {
        case ALLOC:
            if (alignment > DEFAULT_MEM_ALIGNMENT)
            {
	            data = (void*)aligned_offset((iptr)libc::malloc(bytes + alignment), alignment);
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
	            data = (void*)aligned_offset((iptr)libc::calloc(bytes + alignment, 1), alignment);
            }
            else
            {
                data = libc::malloc(bytes);
            }
            if (!data) return AllocationFailure.OUT_OF_MEMORY!;
            return data;
        case REALLOC:
            if (alignment > DEFAULT_MEM_ALIGNMENT)
            {
	            data = (void*)aligned_offset((iptr)libc::realloc(old_pointer, bytes + alignment), alignment);
            }
            else
            {
                data = libc::realloc(old_pointer, bytes);
            }
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
