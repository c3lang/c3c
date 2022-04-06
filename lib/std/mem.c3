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
    RESET,
}

fault AllocationFailure
{
    OUT_OF_MEMORY,
    UNSUPPORTED_OPERATION,
}

private tlocal Allocator thread_allocator = { null, SYSTEM_ALLOCATOR };

struct Allocator
{
    void* data;
    AllocatorFunction function;
}

macro malloc($Type)
{
    return ($Type*)(mem::alloc($Type.sizeof));
}

fn char[] alloc_bytes(usize bytes) @inline
{
	return ((char*)thread_allocator.alloc(bytes, 1))[0..bytes - 1]!!;
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

/**
 * Run with a specific allocator inside of the macro body.
 **/
macro void with_allocator(Allocator allocator; @body())
{
	Allocator old_allocator = thread_allocator;
	thread_allocator = allocator;
	defer thread_allocator = old_allocator;
	@body();
}

fn void*! talloc(usize size)
{
    return temp_allocator.alloc(size);
}

struct MemoryArena
{
	void* memory;
	usize total;
	usize used;
}

/**
 * @require alignment > 0 `alignment must be non zero`
 * @require @math::is_power_of_2(alignment)
 * @require size > 0
 * @require alignment <= MAX_MEMORY_ALIGNMENT `alignment too big`
 * @require this != null
 **/
fn void*! MemoryArena.alloc(MemoryArena* this, usize size, usize alignment, usize prefixed_bytes = 0)
{
	void* start_mem = this.memory;
	void* unaligned_pointer = start_mem + this.used + prefixed_bytes;
	if ((uptr)unaligned_pointer < (uptr)start_mem) return AllocationFailure.OUT_OF_MEMORY!;
	usize offset_start = aligned_offset((usize)(uptr)unaligned_pointer, alignment) - (usize)(uptr)start_mem;
	usize end = offset_start + size;
	if (end > this.total || end < offset_start) return AllocationFailure.OUT_OF_MEMORY!;
	this.used = end;
	return start_mem + offset_start;
}

/**
 * Initialize a memory arena for use using the provided bytes.
 *
 * @require this != null
 **/
fn void MemoryArena.init(MemoryArena* this, char[] data)
{
	this.memory = data.ptr;
	this.total = data.len;
	this.used = 0;
}
/**
 * @require this != null
 **/
fn void MemoryArena.reset(MemoryArena* this)
{
	this.used = 0;
}