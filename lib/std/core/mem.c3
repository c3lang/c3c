// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::mem;

macro @volatile_load(&x)
{
    return $$volatile_load(&x);
}

macro @volatile_store(&x, y)
{
    return $$volatile_store(&x, y);
}

/**
 * @require math::is_power_of_2(alignment)
 **/
fn usize aligned_offset(usize offset, usize alignment)
{
	return alignment * ((offset + alignment - 1) / alignment);
}


/**
 * @require math::is_power_of_2(alignment)
 **/
fn bool ptr_is_aligned(void* ptr, usize alignment) @inline
{
	return (uptr)ptr & ((uptr)alignment - 1) == 0;
}

fn void copy(char* dst, char* src, usize size) @inline
{
	memcpy(dst, src, size);
}

macro void memcpy(void* dst, void* src, usize size, bool $is_volatile = false, usize $dst_align = 0, usize $src_align = 0)
{
	$$memcpy(dst, src, size, $is_volatile, $dst_align, $src_align);
}

fn void set(void* dst, char val, usize bytes) @inline
{
	memset(dst, val, bytes);
}

macro void memset(void* dst, char val, usize bytes, bool $is_volatile = false, usize $dst_align = 0)
{
	$$memset(dst, val, bytes, $is_volatile, $dst_align);
}



macro @clone(&value) @builtin
{
	$typeof(value)* x = malloc($typeof(value));
	*x = value;
	return x;
}

macro malloc($Type) @builtin
{
    return ($Type*)(mem::alloc($Type.sizeof));
}

fn char[] alloc_bytes(usize bytes) @inline
{
	return ((char*)thread_allocator.alloc(bytes, 1))[0..bytes - 1]!!;
}

/**
 * @require !alignment || math::is_power_of_2(alignment)
 */
fn void* alloc(usize size, usize alignment = 0)
{
	return thread_allocator.alloc(size, alignment)!!;
}

/**
 * @require !alignment || math::is_power_of_2(alignment)
 */
fn void*! alloc_checked(usize size, usize alignment = 0)
{
	return thread_allocator.alloc(size, alignment);
}


/**
 * @require !alignment || math::is_power_of_2(alignment)
 */
fn void* calloc(usize size, usize alignment = 0)
{
	return thread_allocator.calloc(size, alignment)!!;
}

/**
 * @require !alignment || math::is_power_of_2(alignment)
 */
fn void*! calloc_checked(usize size, usize alignment = 0)
{
	return thread_allocator.calloc(size, alignment);
}

/**
 * @require !alignment || math::is_power_of_2(alignment)
 */
fn void* realloc(void *ptr, usize new_size, usize alignment = 0)
{
	return thread_allocator.realloc(ptr, new_size, alignment)!!;
}

/**
 * @require !alignment || math::is_power_of_2(alignment)
 */
fn void*! realloc_checked(void *ptr, usize new_size, usize alignment = 0)
{
	return thread_allocator.realloc(ptr, new_size, alignment);
}

fn void free(void* ptr) @builtin
{
	return thread_allocator.free(ptr)!!;
}

/**
 * Run with a specific allocator inside of the macro body.
 **/
macro void @scoped(Allocator* allocator; @body())
{
	Allocator* old_allocator = thread_allocator;
	thread_allocator = allocator;
	defer thread_allocator = old_allocator;
	@body();
}

macro void @tscoped(;@body())
{
	Allocator* old_allocator = thread_allocator;
	TempAllocator* temp = temp_allocator();
	usize mark = temp.mark()!!;
	thread_allocator = temp;
	defer temp.reset(mark);
	defer thread_allocator = old_allocator;
	@body();
}

fn void* talloc(usize size, usize alignment = 0)
{
    return temp_allocator().alloc(size, alignment)!!;
}

fn void* tcalloc(usize size, usize alignment = 0)
{
	return temp_allocator().calloc(size, alignment)!!;
}

fn void* trealloc(void* ptr, usize size, usize alignment = 0)
{
	return temp_allocator().realloc(ptr, size, alignment)!!;
}

macro void @pool(;@body) @builtin
{
	TempAllocator* temp = temp_allocator();
	usize mark = temp.used;
	defer temp.reset(mark);
	@body();
}

private tlocal Allocator* thread_allocator = allocator::LIBC_ALLOCATOR;
private tlocal TempAllocator* thread_temp_allocator = null;

macro TempAllocator* temp_allocator()
{
	if (!thread_temp_allocator)
	{
		thread_temp_allocator = allocator::new_temp(env::TEMP_ALLOCATOR_SIZE, allocator::LIBC_ALLOCATOR)!!;
	}
	return thread_temp_allocator;
}

macro Allocator* current_allocator()
{
	return thread_allocator;
}




