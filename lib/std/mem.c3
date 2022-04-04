// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::mem;

macro volatile_load(&x)
{
    return $$volatile_load(&x);
}

macro volatile_store(&x, y)
{
    return $$volatile_store(&x, y);
}

/**
 * @require @math::is_power_of_2(alignment)
 **/
fn usize aligned_offset(usize offset, usize alignment)
{
	return alignment * ((offset + alignment - 1) / alignment);
}


/**
 * @require @math::is_power_of_2(alignment)
 **/
fn bool ptr_is_aligned(void* ptr, usize alignment) @inline
{
	return (uptr)ptr & ((uptr)alignment - 1) == 0;
}

fn void copy(char* dst, char* src, usize size) @inline
{
	@memcpy(dst, src, size);
}

macro void memcpy(void* dst, void* src, usize size, bool $is_volatile = false, usize $dst_align = 0, usize $src_align = 0)
{
	$$memcpy(dst, src, size, $is_volatile, $dst_align, $src_align);
}

fn void set(void* dst, char val, usize bytes) @inline
{
	@memset(dst, val, bytes);
}

macro void memset(void* dst, char val, usize bytes, bool $is_volatile = false, usize $dst_align = 0)
{
	$$memset(dst, val, bytes, $is_volatile, $dst_align);
}

macro bitcast(expr, $Type)
{
    var $size = (usize)($sizeof(expr));
    $assert($size == $Type.sizeof, "Cannot bitcast between types of different size.");
    $Type x = void;
    @memcpy(&x, &expr, $size, false, $alignof($Type), $alignof(expr));
    return x;
}


enum AllocationKind
{
    ALLOC,
    CALLOC,
    REALLOC,
    FREE,
}

fault AllocationFailure
{
    OUT_OF_MEMORY
}

private tlocal Allocator thread_allocator = { SYSTEM_ALLOCATOR, null };

struct Allocator
{
    AllocatorFunction function;
    void *data;
}

macro malloc($Type)
{
    return ($Type*)(mem::alloc($Type.sizeof));
}

/**
 * @require !alignment || @math::is_power_of_2(alignment)
 */
fn void* alloc(usize size, usize alignment = 0)
{
	return thread_allocator.alloc(size, alignment)!!;
}

/**
 * @require !alignment || @math::is_power_of_2(alignment)
 */
fn void*! alloc_checked(usize size, usize alignment = 0)
{
	return thread_allocator.alloc(size, alignment);
}


/**
 * @require !alignment || @math::is_power_of_2(alignment)
 */
fn void* calloc(usize size, usize alignment = 0)
{
	return thread_allocator.calloc(size, alignment)!!;
}

/**
 * @require !alignment || @math::is_power_of_2(alignment)
 */
fn void*! calloc_checked(usize size, usize alignment = 0)
{
	return thread_allocator.calloc(size, alignment);
}

/**
 * @require !alignment || @math::is_power_of_2(alignment)
 */
fn void* realloc(void *ptr, usize new_size, usize alignment = 0)
{
	return thread_allocator.realloc(ptr, new_size, alignment)!!;
}

/**
 * @require !alignment || @math::is_power_of_2(alignment)
 */
fn void*! realloc_checked(void *ptr, usize new_size, usize alignment = 0)
{
	return thread_allocator.realloc(ptr, new_size, alignment);
}

fn void free(void* ptr)
{
	return thread_allocator.free(ptr)!!;
}

macro void with_allocator(Allocator allocator; @body())
{
	Allocator old_allocator = thread_allocator;
	thread_allocator = allocator;
	defer thread_allocator = old_allocator;
	@body();
}

fn void*! talloc(usize size)
{
    return default_allocator.alloc(size);
}

