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

macro void* aligned_pointer(void* ptr, usize alignment)
{
	return (void*)(uptr)aligned_offset((uptr)ptr, alignment);
}


/**
 * @require math::is_power_of_2(alignment)
 **/
fn bool ptr_is_aligned(void* ptr, usize alignment) @inline
{
	return (uptr)ptr & ((uptr)alignment - 1) == 0;
}

macro void copy(void* dst, void* src, usize len, usize $dst_align = 0, usize $src_align = 0, bool $is_volatile = false)
{
	$$memcpy(dst, src, len, $is_volatile, $dst_align, $src_align);
}

macro void set(void* dst, char val, usize len, usize $dst_align = 0, bool $is_volatile = false)
{
	$$memset(dst, val, len, $is_volatile, $dst_align);
}

macro void clear(void* dst, usize len, usize $dst_align = 0, bool $is_volatile = false)
{
	$$memset(dst, (char)0, len, $is_volatile, $dst_align);
}

/**
 * @require $typeof(a).kindof == TypeKind.SUBARRAY || $typeof(a).kindof == TypeKind.POINTER
 * @require $typeof(b).kindof == TypeKind.SUBARRAY || $typeof(b).kindof == TypeKind.POINTER
 * @require $typeof(a).kindof != TypeKind.SUBARRAY || len == -1
 * @require $typeof(a).kindof != TypeKind.POINTER || len > -1
 * @checked (a = b), (b = a)
 **/
macro bool equals(a, b, isize len = -1, usize $align = 0)
{
	$if (!$align):
		$align = $alignof($typeof(a[0]));
	$endif;
	void* x = void;
	void* y = void;
	$if ($typeof(a).kindof == TypeKind.SUBARRAY):
		len = a.len;
		if (len != b.len) return false;
		x = a.ptr;
		y = b.ptr;
	$else:
		x = a;
		y = b;
		assert(len >= 0, "A zero or positive length must be given when comparing pointers.");
	$endif;

	if (!len) return true;
    $switch ($align):
		$case 1:
			var $Type = char;
		$case 2:
			var $Type = ushort;
		$case 4:
			var $Type = uint;
		$case 8:
		$default:
			var $Type = ulong;
	$endswitch;
	var $step = $Type.sizeof;
	usize end = len / $step;
	for (usize i = 0; i < end; i++)
	{
		if ((($Type*)x)[i] != (($Type*)y)[i]) return false;
	}
	usize last = len % $align;
	for (usize i = len - last; i < len; i++)
	{
		if (((char*)x)[i] != ((char*)y)[i]) return false;
	}
	return true;
}

macro @clone(&value) @builtin
{
	$typeof(value)* x = malloc($typeof(value));
	*x = value;
	return x;
}

macro @tclone(&value) @builtin
{
	$typeof(value)* x = talloc($typeof(value));
	*x = value;
	return x;
}

fn void* malloc(usize size) @builtin @inline
{
	return thread_allocator.alloc(size)!!;
}

fn void*! malloc_checked(usize size) @builtin @inline
{
	return thread_allocator.alloc(size);
}

/**
 * @require alignment && math::is_power_of_2(alignment)
 */
fn void*! malloc_aligned(usize size, usize alignment) @builtin @inline
{
	return thread_allocator.alloc_aligned(size, alignment);
}

fn char[] alloc_bytes(usize bytes) @inline
{
	return ((char*)thread_allocator.alloc(bytes))[:bytes]!!;
}

macro alloc($Type)
{
	return ($Type*)thread_allocator.alloc($Type.sizeof)!!;
}


fn void* calloc(usize size) @builtin @inline
{
	return thread_allocator.calloc(size)!!;
}

fn void*! calloc_checked(usize size) @builtin @inline
{
	return thread_allocator.calloc(size);
}

/**
 * @require alignment && math::is_power_of_2(alignment)
 */
fn void*! calloc_aligned(usize size, usize alignment) @builtin @inline
{
	return thread_allocator.calloc_aligned(size, alignment);
}

fn void* realloc(void *ptr, usize new_size) @builtin @inline
{
	return thread_allocator.realloc(ptr, new_size)!!;
}

fn void*! realloc_checked(void *ptr, usize new_size) @builtin @inline
{
	return thread_allocator.realloc(ptr, new_size);
}

/**
 * @require alignment && math::is_power_of_2(alignment)
 */
fn void*! realloc_aligned(void *ptr, usize new_size, usize alignment) @builtin @inline
{
	return thread_allocator.realloc_aligned(ptr, new_size, alignment);
}

fn void free(void* ptr) @builtin @inline
{
	return thread_allocator.free(ptr)!!;
}

fn void free_aligned(void* ptr) @builtin @inline
{
	return thread_allocator.free_aligned(ptr)!!;
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

macro talloc($Type) @builtin
{
	return temp_allocator().alloc_aligned($Type.sizeof, $alignof($Type))!!;
}

fn void* tmalloc(usize size, usize alignment = allocator::DEFAULT_MEM_ALIGNMENT) @builtin @inline
{
    return temp_allocator().alloc_aligned(size, alignment)!!;
}

fn void* tcalloc(usize size, usize alignment = allocator::DEFAULT_MEM_ALIGNMENT) @builtin @inline
{
	return temp_allocator().calloc_aligned(size, alignment)!!;
}

fn void* trealloc(void* ptr, usize size, usize alignment = allocator::DEFAULT_MEM_ALIGNMENT) @builtin @inline
{
	return temp_allocator().realloc_aligned(ptr, size, alignment)!!;
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




