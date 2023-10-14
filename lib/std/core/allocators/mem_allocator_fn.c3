// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

module std::core::mem::allocator;
import libc;


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
	$if $checks(#alloc_fn(bytes)!):
		void* data = #alloc_fn(header + bytes)!;
	$else
		void* data = #alloc_fn(header + bytes);
	$endif
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
	$if $checks(#calloc_fn(bytes)!):
		void* data = #calloc_fn(header + bytes)!;
	$else
		void* data = #calloc_fn(header + bytes);
	$endif
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
	void* new_data = @aligned_calloc(#calloc_fn, bytes, alignment, offset)!;
	mem::copy(new_data, old_pointer, desc.len < bytes ? desc.len : bytes, mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);
	$if $checks(#free_fn(data_start)!):
	#free_fn(data_start)!;
	$else
	#free_fn(data_start);
	$endif
	return new_data;
}

macro void! @aligned_free(#free_fn, void* old_pointer)
{
	AlignedBlock* desc = (AlignedBlock*)old_pointer - 1;
	$if $checks(#free_fn(desc.start)!):
	#free_fn(desc.start)!;
	$else
	#free_fn(desc.start);
	$endif
}

distinct LibcAllocator (Allocator) = uptr;

fn void*! LibcAllocator.acquire(&self, usz bytes, bool clear, usz alignment, usz offset) @dynamic
{
	assert(alignment != 0 || offset == 0);
	if (clear)
	{
		void* data = alignment ? @aligned_calloc(fn void*(usz bytes) => libc::calloc(bytes, 1), bytes, alignment, offset)!! : libc::calloc(bytes, 1);
        return data ?: AllocationFailure.OUT_OF_MEMORY?;
	}
	else
	{
		void* data = alignment ? @aligned_alloc(libc::malloc, bytes, alignment, offset)!! : libc::malloc(bytes);
        if (!data) return AllocationFailure.OUT_OF_MEMORY?;
        $if env::TESTING:
		    for (usz i = 0; i < bytes; i++) ((char*)data)[i] = 0xAA;
        $endif
        return data;
	}
}

fn void*! LibcAllocator.resize(&self, void* old_ptr, usz new_bytes, usz alignment, usz offset) @dynamic
{
	assert(alignment != 0 || offset == 0);
	if (!new_bytes)
	{
		self.release(old_ptr, alignment > 0);
		return null;
	}
	if (!old_ptr)
	{
		return self.acquire(new_bytes, true, alignment, offset);
	}
	if (alignment)
	{
		void* data = @aligned_realloc(fn void*(usz bytes) => libc::calloc(bytes, 1), libc::free, old_ptr, new_bytes, alignment, offset)!!;
		return data ?: AllocationFailure.OUT_OF_MEMORY?;
	}
	return libc::realloc(old_ptr, new_bytes) ?: AllocationFailure.OUT_OF_MEMORY?;
}


fn void LibcAllocator.release(&self, void* old_ptr, bool aligned) @dynamic
{
	if (aligned)
	{
		@aligned_free(libc::free, old_ptr)!!;
	}
	else
	{
		libc::free(old_ptr);
	}
}
