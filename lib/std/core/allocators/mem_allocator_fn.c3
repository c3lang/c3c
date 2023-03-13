// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

module std::core::mem::allocator;
import libc;

const Allocator _NULL_ALLOCATOR @private = { &null_allocator_fn };
const Allocator _SYSTEM_ALLOCATOR @private = { &libc_allocator_fn };

fn void*! null_allocator_fn(Allocator* this, usz bytes, usz alignment, usz offset, void* old_pointer, AllocationKind kind) @private
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

struct AlignedBlock
{
	usz len;
	void* start;
}

/**
 * @require bytes > 0
 * @require alignment > 0
 **/
macro void*! @aligned_alloc(#alloc_fn, usz bytes, usz alignment, usz offset)
{
	usz header = mem::aligned_offset(AlignedBlock.sizeof + offset, alignment) - offset;
	$if ($checks(#alloc_fn(bytes)?)):
		void* data = #alloc_fn(header + bytes)?;
	$else:
		void* data = #alloc_fn(header + bytes);
	$endif;
    void* mem = mem::aligned_pointer(data + header + offset, alignment) - offset;
	assert(mem > data);
	AlignedBlock* desc = (AlignedBlock*)mem - 1;
	*desc = { bytes, data };
	return mem;
}

/**
 * @require bytes > 0
 * @require alignment > 0
 **/
macro void*! @aligned_calloc(#calloc_fn, usz bytes, usz alignment, usz offset)
{
	usz header = mem::aligned_offset(AlignedBlock.sizeof + offset, alignment) - offset;
	$if ($checks(#calloc_fn(bytes)?)):
		void* data = #calloc_fn(header + bytes)?;
	$else:
		void* data = #calloc_fn(header + bytes);
	$endif;
	void* mem = mem::aligned_pointer(data + header + offset, alignment) - offset;
	AlignedBlock* desc = (AlignedBlock*)mem - 1;
	assert(mem > data);
	*desc = { bytes, data };
	return mem;
}

/**
 * @require bytes > 0
 * @require alignment > 0
 **/
macro void*! @aligned_realloc(#calloc_fn, #free_fn, void* old_pointer, usz bytes, usz alignment, usz offset)
{
	AlignedBlock* desc = (AlignedBlock*)old_pointer - 1;
	void* data_start = desc.start;
    void* new_data = @aligned_calloc(#calloc_fn, bytes, alignment, offset)?;
	mem::copy(new_data, old_pointer, desc.len > bytes ? desc.len : bytes, mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);
	$if ($checks(#free_fn(data_start)?)):
	#free_fn(data_start)?;
	$else:
	#free_fn(data_start);
	$endif;
	return new_data;
}

macro void! @aligned_free(#free_fn, void* old_pointer)
{
	AlignedBlock* desc = (AlignedBlock*)old_pointer - 1;
	$if ($checks(#free_fn(desc.start)?)):
	#free_fn(desc.start)?;
	$else:
	#free_fn(desc.start);
	$endif;
}

fn void*! libc_allocator_fn(Allocator* unused, usz bytes, usz alignment, usz offset, void* old_pointer, AllocationKind kind) @inline
{
	if (!alignment) alignment = mem::DEFAULT_MEM_ALIGNMENT;
	assert(math::is_power_of_2(alignment), "Alignment was not a power of 2");

    void* data;
    switch (kind)
    {
        case ALIGNED_ALLOC:
            data = @aligned_alloc(libc::malloc, bytes, alignment, offset)!!;
        case ALLOC:
            data = libc::malloc(bytes);
        case ALIGNED_CALLOC:
            data = @aligned_calloc(fn void*(usz bytes) => libc::calloc(bytes, 1), bytes, alignment, offset)!!;
		case CALLOC:
            data = libc::calloc(bytes, 1);
        case ALIGNED_REALLOC:
            if (!bytes) nextcase ALIGNED_FREE;
            if (!old_pointer) nextcase ALIGNED_CALLOC;
            data = @aligned_realloc(fn void*(usz bytes) => libc::calloc(bytes, 1), libc::free, old_pointer, bytes, alignment, offset)!!;
        case REALLOC:
            if (!bytes) nextcase FREE;
            if (!old_pointer) nextcase CALLOC;
            data = libc::realloc(old_pointer, bytes);
        case RESET:
            return AllocationFailure.UNSUPPORTED_OPERATION!;
        case ALIGNED_FREE:
            @aligned_free(libc::free, old_pointer)!!;
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
