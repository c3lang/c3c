module std::core::mem::allocator;
import libc;

private const Allocator _NULL_ALLOCATOR = { &null_allocator_fn };
private const Allocator _SYSTEM_ALLOCATOR = { &libc_allocator_fn };

private fn void*! null_allocator_fn(Allocator* this, usz bytes, usz alignment, usz offset, void* old_pointer, AllocationKind kind)
{
	switch (kind)
	{
		case ALLOC:
        case CALLOC:
        case REALLOC:
        case ALIGNED_ALLOC:
        case ALIGNED_REALLOC:
        case ALIGNED_CALLOC:
			return AllocationFailure.OUT_OF_MEMORY!;
		default:
			return null;
	}
}

private struct AlignedBlock
{
	usz len;
	void* start;
}


/**
 * @require bytes > 0
 * @require alignment > 0
 **/
private fn void* _libc_aligned_alloc(usz bytes, usz alignment, usz offset) @inline
{
	usz header = mem::aligned_offset(AlignedBlock.sizeof + offset, alignment) - offset;
	void* data = libc::malloc(header + bytes);
    void* mem = mem::aligned_pointer(data + offset, alignment) - offset;
	assert(mem > data);
	AlignedBlock* desc = (AlignedBlock*)mem - 1;
	*desc = { bytes, data };
	return mem;
}

/**
 * @require bytes > 0
 * @require alignment > 0
 **/
private fn void* _libc_aligned_calloc(usz bytes, usz alignment, usz offset) @inline
{
	usz header = mem::aligned_offset(AlignedBlock.sizeof + offset, alignment) - offset;
	void* data = libc::calloc(header + bytes, 1);
	void* mem = mem::aligned_pointer(data + offset, alignment) - offset;
	AlignedBlock* desc = (AlignedBlock*)mem - 1;
	assert(mem > data);
	*desc = { bytes, data };
	return mem;
}

/**
 * @require bytes > 0
 * @require alignment > 0
 **/
private fn void* _libc_aligned_realloc(void* old_pointer, usz bytes, usz alignment, usz offset) @inline
{
	AlignedBlock* desc = (AlignedBlock*)old_pointer - 1;
    void* data_start = desc.start;
    void* new_data = _libc_aligned_calloc(bytes, alignment, offset);
	mem::copy(new_data, old_pointer, desc.len > bytes ? desc.len : bytes, DEFAULT_MEM_ALIGNMENT, DEFAULT_MEM_ALIGNMENT);
	libc::free(data_start);
	return new_data;
}

private fn void _libc_aligned_free(void* old_pointer) @inline
{
	AlignedBlock* desc = (AlignedBlock*)old_pointer - 1;
	libc::free(desc.start);
}

fn void*! libc_allocator_fn(Allocator* unused, usz bytes, usz alignment, usz offset, void* old_pointer, AllocationKind kind) @inline
{
	if (!alignment) alignment = DEFAULT_MEM_ALIGNMENT;
	assert(math::is_power_of_2(alignment), "Alignment was not a power of 2");

    void* data;
    switch (kind)
    {
        case ALIGNED_ALLOC:
            if (alignment <= DEFAULT_MEM_ALIGNMENT) nextcase ALLOC;
            data = _libc_aligned_alloc(bytes, alignment, offset);
        case ALLOC:
            data = libc::malloc(bytes);
        case ALIGNED_CALLOC:
            if (alignment <= DEFAULT_MEM_ALIGNMENT) nextcase CALLOC;
            data = _libc_aligned_calloc(bytes, alignment, offset);
		case CALLOC:
            data = libc::calloc(bytes, 1);
        case ALIGNED_REALLOC:
            if (!bytes) nextcase ALIGNED_FREE;
            if (!old_pointer) nextcase ALIGNED_CALLOC;
			data = _libc_aligned_realloc(old_pointer, bytes, alignment, offset);
        case REALLOC:
            if (!bytes) nextcase FREE;
            if (!old_pointer) nextcase CALLOC;
            data = libc::realloc(old_pointer, bytes);
        case RESET:
            return AllocationFailure.UNSUPPORTED_OPERATION!;
        case ALIGNED_FREE:
            _libc_aligned_free(old_pointer);
            return null;
        case FREE:
            libc::free(old_pointer);
            return null;
        default:
            unreachable();
    }
    if (!data) return AllocationFailure.OUT_OF_MEMORY!;
    return data;
}
