// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

module std::core::mem::allocator;
import libc;

const LibcAllocator LIBC_ALLOCATOR = {};


distinct LibcAllocator (Allocator) = uptr;

fn void*! LibcAllocator.acquire(&self, usz bytes, bool clear, usz alignment, usz offset, TrackingEnv* env) @dynamic
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

fn void*! LibcAllocator.resize(&self, void* old_ptr, usz new_bytes, usz alignment, usz offset, TrackingEnv* env) @dynamic
{
	assert(alignment != 0 || offset == 0);
	if (!new_bytes)
	{
		self.release(old_ptr, alignment > 0, env);
		return null;
	}
	if (!old_ptr)
	{
		return self.acquire(new_bytes, true, alignment, offset, env);
	}
	if (alignment)
	{
		void* data = @aligned_realloc(fn void*(usz bytes) => libc::calloc(bytes, 1), libc::free, old_ptr, new_bytes, alignment, offset)!!;
		return data ?: AllocationFailure.OUT_OF_MEMORY?;
	}
	return libc::realloc(old_ptr, new_bytes) ?: AllocationFailure.OUT_OF_MEMORY?;
}


fn void LibcAllocator.release(&self, void* old_ptr, bool aligned, TrackingEnv* env) @dynamic
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
