// Copyright (c) 2021-2025 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::mem::allocators @if(env::LIBC);
import std::io, libc;

<*
 The LibcAllocator is a wrapper around malloc to conform to the Allocator interface.
*>
typedef LibcAllocator (Allocator) = uptr;
const LibcAllocator LIBC_ALLOCATOR = {};

module std::core::mem::allocators @if(env::POSIX);
import std::os;
import libc;

fn void*? LibcAllocator.acquire(&self, sz bytes, AllocInitType init_type, sz alignment) @dynamic
{
	if (init_type == ZERO)
	{
		void* data @noinit;
		if (alignment > mem::DEFAULT_MEM_ALIGNMENT)
		{
			if (posix::posix_memalign(&data, (usz)alignment, (usz)bytes)) return mem::OUT_OF_MEMORY~;
			mem::clear(data, bytes, mem::DEFAULT_MEM_ALIGNMENT);
			return data;
		}
		return libc::calloc(1, (usz)bytes) ?: mem::OUT_OF_MEMORY~;
	}
	else
	{
		void* data @noinit;
		if (alignment > mem::DEFAULT_MEM_ALIGNMENT)
		{
			if (posix::posix_memalign(&data, (usz)alignment, (usz)bytes)) return mem::OUT_OF_MEMORY~;
		}
		else
		{
			if (!(data = libc::malloc((usz)bytes))) return mem::OUT_OF_MEMORY~;
		}
		$if env::TESTING:
			for (sz i = 0; i < bytes; i++) ((char*)data)[i] = 0xAA;
		$endif
		return data;
	}
}

fn void*? LibcAllocator.resize(&self, void* old_ptr, sz new_bytes, sz alignment) @dynamic
{
 	if (alignment <= mem::DEFAULT_MEM_ALIGNMENT) return libc::realloc(old_ptr, (usz)new_bytes) ?: mem::OUT_OF_MEMORY~;

	// Try realloc, even though it might be unaligned.
	void* new_ptr = libc::realloc(old_ptr, (usz)new_bytes) ?: mem::OUT_OF_MEMORY~!;

	// If it's aligned then we're done!
	uptr ptr_val = (uptr)new_ptr;
	if (ptr_val & ((uptr)alignment - 1u) == 0) return new_ptr;

	// We failed, so we need to use memalign
	// We will free new_ptr before we exit.
	defer libc::free(new_ptr);

	// Create a pointer which is sure to be aligned.
	void* aligned_ptr;
	if (posix::posix_memalign(&aligned_ptr, (usz)alignment, (usz)new_bytes))
	{
		return mem::OUT_OF_MEMORY~;
	}
	// Now it is safe to copy the full range of data.
	mem::copy(aligned_ptr, new_ptr, new_bytes, mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);
	return aligned_ptr;

}


fn void LibcAllocator.release(&self, void* old_ptr, bool aligned) @dynamic
{
	libc::free(old_ptr);
}

module std::core::mem::allocator @if(env::WIN32);
import std::os::win32;
import libc;

fn void*? LibcAllocator.acquire(&self, sz bytes, AllocInitType init_type, sz alignment) @dynamic
{
	if (init_type == ZERO)
	{
		if (alignment > 0)
		{
			return win32::_aligned_recalloc(null, 1, (usz)bytes, (usz)alignment) ?: mem::OUT_OF_MEMORY~;
		}
		return libc::calloc(1, (usz)bytes) ?: mem::OUT_OF_MEMORY~;
	}
	void* data = alignment > 0 ? win32::_aligned_malloc((usz)bytes, (usz)alignment) : libc::malloc((usz)bytes);
	if (!data) return mem::OUT_OF_MEMORY~;
	$if env::TESTING:
		for (sz i = 0; i < bytes; i++) ((char*)data)[i] = 0xAA;
	$endif
	return data;
}

fn void*? LibcAllocator.resize(&self, void* old_ptr, sz new_bytes, sz alignment) @dynamic
{
	if (alignment)
	{
		return win32::_aligned_realloc(old_ptr, (usz)new_bytes, (usz)alignment) ?: mem::OUT_OF_MEMORY~;
	}
	return libc::realloc(old_ptr, (usz)new_bytes) ?: mem::OUT_OF_MEMORY~;
}

fn void LibcAllocator.release(&self, void* old_ptr, bool aligned) @dynamic
{
	if (aligned)
	{
		win32::_aligned_free(old_ptr);
		return;
	}
	libc::free(old_ptr);
}

module std::core::mem::allocator @if(!env::WIN32 && !env::POSIX && env::LIBC);
import libc;

fn void*? LibcAllocator.acquire(&self, sz bytes, AllocInitType init_type, sz alignment) @dynamic
{
	if (init_type == ZERO)
	{
		void* data = alignment ? allocators::@aligned_alloc(fn void*(sz bytes) => libc::calloc(bytes, 1), bytes, alignment)!! : libc::calloc(bytes, 1);
		return data ?: mem::OUT_OF_MEMORY~;
	}
	else
	{
		void* data = alignment ? allocators::@aligned_alloc(libc::malloc, bytes, alignment)!! : libc::malloc(bytes);
		if (!data) return mem::OUT_OF_MEMORY~;
		$if env::TESTING:
			for (sz i = 0; i < bytes; i++) ((char*)data)[i] = 0xAA;
		$endif
		return data;
	}
}


fn void*? LibcAllocator.resize(&self, void* old_ptr, sz new_bytes, sz alignment) @dynamic
{
	if (alignment)
	{
		void* data = allocators::@aligned_realloc(fn void*(sz bytes) => libc::malloc((usz)bytes), libc::free, old_ptr, new_bytes, alignment)!!;
		return data ?: mem::OUT_OF_MEMORY~;
	}
	return libc::realloc(old_ptr, (usz)new_bytes) ?: mem::OUT_OF_MEMORY~;
}


fn void LibcAllocator.release(&self, void* old_ptr, bool aligned) @dynamic
{
	if (aligned)
	{
		allocators::@aligned_free(libc::free, old_ptr)!!;
	}
	else
	{
		libc::free(old_ptr);
	}
}
