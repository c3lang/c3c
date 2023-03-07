// Copyright (c) 2021-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::mem;

const MAX_MEMORY_ALIGNMENT = 0x1000_0000;
const DEFAULT_MEM_ALIGNMENT = (void*.alignof) * 2;


macro @volatile_load(&x) @builtin
{
    return $$volatile_load(&x);
}

macro @volatile_store(&x, y) @builtin
{
    return $$volatile_store(&x, ($typeof(x))y);
}

enum AtomicOrdering : int
{
	NOT_ATOMIC,         // Not atomic
	UNORDERED,          // No lock
	MONOTONIC,          // Consistent ordering
	ACQUIRE,            // Barrier locking load/store
	RELEASE,            // Barrier releasing load/store
	ACQUIRE_RELEASE,    // Barrier fence to load/store
	SEQ_CONSISTENT,     // Acquire semantics, ordered with other seq_consistent
}

/**
 * @param [in] x "the variable or dereferenced pointer to load."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @param $volatile "whether the load should be volatile, defaults to 'false'"
 * @return "returns the value of x"
 *
 * @require $ordering != AtomicOrdering.RELEASE "Release ordering is not valid for load."
 * @require $ordering != AtomicOrdering.ACQUIRE_RELEASE "Acquire release is not valid for load."
 * @require types::may_load_atomic($typeof(x)) "Only integer, float and pointers may be used."
 **/
macro @atomic_load(&x, AtomicOrdering $ordering = SEQ_CONSISTENT, $volatile = false) @builtin
{
	return $$atomic_load(&x, $volatile, (int)$ordering);
}

/**
 * @param [out] x "the variable or dereferenced pointer to store to."
 * @param value "the value to store."
 * @param $ordering "the atomic ordering of the store, defaults to SEQ_CONSISTENT"
 * @param $volatile "whether the store should be volatile, defaults to 'false'"
 *
 * @require $ordering != AtomicOrdering.ACQUIRE "Acquire ordering is not valid for store."
 * @require $ordering != AtomicOrdering.ACQUIRE_RELEASE "Acquire release is not valid for store."
 * @require types::may_load_atomic($typeof(x)) "Only integer, float and pointers may be used."
 **/
macro void @atomic_store(&x, value, AtomicOrdering $ordering = SEQ_CONSISTENT, $volatile = false) @builtin
{
	$$atomic_store(&x, value, $volatile, (int)$ordering);
}

macro compare_exchange(ptr, compare, value, AtomicOrdering $success = SEQ_CONSISTENT, AtomicOrdering $failure = SEQ_CONSISTENT, bool $volatile = true, bool $weak = false, usz $alignment = 0)
{
	return $$compare_exchange(ptr, compare, value, $volatile, $weak, $success.ordinal, $failure.ordinal, $alignment);
}

macro compare_exchange_volatile(ptr, compare, value, AtomicOrdering $success = SEQ_CONSISTENT, AtomicOrdering $failure = SEQ_CONSISTENT)
{
	return compare_exchange(ptr, compare, value, $success, $failure, true);
}

/**
 * @require math::is_power_of_2(alignment)
 **/
fn usz aligned_offset(usz offset, usz alignment)
{
	return alignment * ((offset + alignment - 1) / alignment);
}

macro void* aligned_pointer(void* ptr, usz alignment)
{
	return (void*)(uptr)aligned_offset((uptr)ptr, alignment);
}

/**
 * @require math::is_power_of_2(alignment)
 **/
fn bool ptr_is_aligned(void* ptr, usz alignment) @inline
{
	return (uptr)ptr & ((uptr)alignment - 1) == 0;
}

macro void clear(void* dst, usz len, usz $dst_align = 0, bool $is_volatile = false, bool $inlined = false)
{
	$if ($inlined):
		$$memset_inline(dst, (char)0, len, $is_volatile, $dst_align);
	$else:
		$$memset(dst, (char)0, len, $is_volatile, $dst_align);
	$endif;
}

macro void copy(void* dst, void* src, usz len, usz $dst_align = 0, usz $src_align = 0, bool $is_volatile = false, bool $inlined = false)
{
	$if ($inlined):
		$$memcpy_inline(dst, src, len, $is_volatile, $dst_align, $src_align);
	$else:
		$$memcpy(dst, src, len, $is_volatile, $dst_align, $src_align);
	$endif;
}

macro void move(void* dst, void* src, usz len, usz $dst_align = 0, usz $src_align = 0, bool $is_volatile = false)
{
	$$memmove(dst, src, len, $is_volatile, $dst_align, $src_align);
}

macro void set(void* dst, char val, usz len, usz $dst_align = 0, bool $is_volatile = false, bool $inlined = false)
{
	$if ($inlined):
		$$memset_inline(dst, val, len, $is_volatile, $dst_align);
	$else:
		$$memset(dst, val, len, $is_volatile, $dst_align);
	$endif;
}

/**
 * @require $typeof(a).kindof == TypeKind.SUBARRAY || $typeof(a).kindof == TypeKind.POINTER
 * @require $typeof(b).kindof == TypeKind.SUBARRAY || $typeof(b).kindof == TypeKind.POINTER
 * @require $typeof(a).kindof != TypeKind.SUBARRAY || len == -1
 * @require $typeof(a).kindof != TypeKind.POINTER || len > -1
 * @checked (a = b), (b = a)
 **/
macro bool equals(a, b, isz len = -1, usz $align = 0)
{
	$if (!$align):
		$align = $typeof(a[0]).alignof;
	$endif;
	void* x @noinit;
	void* y @noinit;
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
	var $Type;
    $switch ($align):
		$case 1:
			$Type = char;
		$case 2:
			$Type = ushort;
		$case 4:
			$Type = uint;
		$case 8:
		$default:
			$Type = ulong;
	$endswitch;
	var $step = $Type.sizeof;
	usz end = len / $step;
	for (usz i = 0; i < end; i++)
	{
		if ((($Type*)x)[i] != (($Type*)y)[i]) return false;
	}
	usz last = len % $align;
	for (usz i = len - last; i < len; i++)
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

macro type_alloc_must_be_aligned($Type)
{
	return $Type.alignof > DEFAULT_MEM_ALIGNMENT;
}
/**
 * @require $vacount > 0 && $vacount < 3 "Expected size, type, or type + len"
 * @require $vacount != 2 || $checks($vatype(0).sizeof) "Expected 'malloc(Foo, 12)'"
 **/
macro malloc(..., Allocator* using = mem::heap(), usz end_padding = 0) @builtin
{
	return malloc_checked($vasplat(), .using = using, .end_padding = end_padding)!!;
}

/**
 * @require $vacount > 0 && $vacount < 3 "Expected size, type, or type + len"
 * @require $vacount != 2 || $checks($vatype(0).sizeof) "Expected 'malloc(Foo, 12)'"
 **/
macro malloc_checked(..., Allocator* using = mem::heap(), usz end_padding = 0) @builtin
{
	$if ($checks($vatype(0).sizeof)):
		var $Type = $vatype(0);
		$assert(!type_alloc_must_be_aligned($vatype(0)),  "Type must be allocated with malloc_aligned");
		$if ($vacount == 2):
			usz size = $vaarg(1);
			return (($Type*)using.alloc($Type.sizeof * size + end_padding))[:size];
		$else:
			return ($Type*)using.alloc($Type.sizeof + end_padding);
		$endif;
	$else:
		return using.alloc($vaarg(0) + end_padding);
	$endif;
}


/**
 * @require $vacount > 0 && $vacount < 3 "Expected size, type, or type + len"
 * @require $vacount != 2 || $checks($vatype(0).sizeof) "Expected 'malloc(Foo, 12)'"
 * @require alignment && math::is_power_of_2(alignment)
 **/
macro malloc_aligned(..., usz alignment = 0, usz end_padding = 0, Allocator* using = mem::heap()) @builtin
{
	$if ($checks($vatype(0).sizeof)):
		var $Type = $vatype(0);
		$if ($vacount == 2):
			usz size = $vaarg(1);
			return (($Type*)using.alloc_aligned($Type.sizeof * size + end_padding, alignment))[:size];
		$else:
			return ($Type*)using.alloc_aligned($Type.sizeof + end_padding, alignment);
		$endif;
	$else:
		return using.alloc_aligned($vaarg(0) + end_padding, alignment);
	$endif;
}

macro alloc($Type) @deprecated => malloc($Type);

macro char[] alloc_bytes(usz bytes) @deprecated => malloc(char, bytes);

/**
 * @require $vacount > 0 && $vacount < 3 "Expected size, type, or type + len"
 * @require $vacount != 2 || $checks($vatype(0).sizeof) "Expected 'malloc(Foo, 12)'"
 **/
macro calloc(..., Allocator* using = mem::heap(), usz end_padding = 0) @builtin
{
	return calloc_checked($vasplat(), .using = using, .end_padding = end_padding)!!;
}

/**
 * @require $vacount > 0 && $vacount < 3 "Expected size, type, or type + len"
 * @require $vacount != 2 || $checks($vatype(0).sizeof) "Expected 'malloc(Foo, 12)'"
 **/
macro calloc_checked(..., Allocator* using = mem::heap(), usz end_padding = 0) @builtin
{
	$if ($checks($vatype(0).sizeof)):
		var $Type = $vatype(0);
		$assert(!type_alloc_must_be_aligned($vatype(0)),  "Type must be allocated with calloc_aligned");
		$if ($vacount == 2):
			usz size = $vaarg(1);
			return (($Type*)using.calloc($Type.sizeof * size + end_padding))[:size];
		$else:
			return ($Type*)using.calloc($Type.sizeof + end_padding);
		$endif;
	$else:
		return using.calloc($vaarg(0) + end_padding);
	$endif;
}


/**
 * @require $vacount > 0 && $vacount < 3 "Expected size, type, or type + len"
 * @require $vacount != 2 || $checks($vatype(0).sizeof) "Expected 'malloc(Foo, 12)'"
 * @require alignment && math::is_power_of_2(alignment)
 **/
macro calloc_aligned(..., usz alignment = 0, Allocator* using = mem::heap(), usz end_padding = 0) @builtin
{
	$if ($checks($vatype(0).sizeof)):
		var $Type = $vatype(0);
		$if ($vacount == 2):
			usz size = $vaarg(1);
			return (($Type*)using.calloc_aligned($Type.sizeof * size + end_padding, alignment))[:size];
		$else:
			return ($Type*)using.calloc_aligned($Type.sizeof + end_padding, alignment);
		$endif;
	$else:
		return using.calloc_aligned($vaarg(0) + end_padding, alignment);
	$endif;
}

fn void* realloc(void *ptr, usz new_size, Allocator* using = mem::heap()) @builtin @inline
{
	return using.realloc(ptr, new_size)!!;
}

fn void*! realloc_checked(void *ptr, usz new_size, Allocator* using = mem::heap()) @builtin @inline
{
	return using.realloc(ptr, new_size);
}

/**
 * @require alignment && math::is_power_of_2(alignment)
 */
fn void*! realloc_aligned(void *ptr, usz new_size, usz alignment, Allocator* using = mem::heap()) @builtin @inline
{
	return using.realloc_aligned(ptr, new_size, alignment);
}

macro void free(void* ptr, Allocator* using = mem::heap()) @builtin => using.free(ptr)!!;
macro void! free_checked(void* ptr, Allocator* using = mem::heap()) @builtin => using.free(ptr);
macro void free_aligned(void* ptr, Allocator* using = mem::heap()) @builtin => using.free_aligned(ptr)!!;
macro void! free_aligned_checked(void* ptr, Allocator* using = mem::heap()) @builtin => using.free_aligned(ptr);

/**
 * Run with a specific allocator inside of the macro body.
 **/
macro void @scoped(Allocator* using; @body())
{
	Allocator* old_allocator = thread_allocator;
	thread_allocator = using;
	defer thread_allocator = old_allocator;
	@body();
}

macro talloc($Type) @builtin @deprecated => tmalloc($Type);

/**
 * @require $vacount > 0 && $vacount < 3 "Expected size, type, or type + len"
 * @require $vacount != 2 || $checks($vatype(0).sizeof) "Expected 'malloc(Foo, 12)'"
 **/
macro tmalloc(..., usz end_padding = 0, usz alignment = DEFAULT_MEM_ALIGNMENT) @builtin
{
	$if ($checks($vatype(0).sizeof)):
		var $Type = $vatype(0);
		$if ($vacount == 2):
			usz size = $vaarg(1);
			return (($Type*)temp_allocator().alloc_aligned($Type.sizeof * size + end_padding, alignment))[:size]!!;
		$else:
			return ($Type*)temp_allocator().alloc_aligned($Type.sizeof + end_padding, alignment)!!;
		$endif;
	$else:
		return temp_allocator().alloc_aligned($vaarg(0) + end_padding, alignment)!!;
	$endif;
}

/**
 * @require $vacount > 0 && $vacount < 3 "Expected size, type, or type + len"
 * @require $vacount != 2 || $checks($vatype(0).sizeof) "Expected 'malloc(Foo, 12)'"
 **/
macro tcalloc(..., usz end_padding = 0, usz alignment = mem::DEFAULT_MEM_ALIGNMENT) @builtin
{
	$if ($checks($vatype(0).sizeof)):
		var $Type = $vatype(0);
		$if ($vacount == 2):
			usz size = $vaarg(1);
			return (($Type*)temp_allocator().calloc_aligned($Type.sizeof * size + end_padding, alignment))[:size]!!;
		$else:
			return ($Type*)temp_allocator().calloc_aligned($Type.sizeof + end_padding, alignment)!!;
		$endif;
	$else:
		return temp_allocator().calloc_aligned($vaarg(0) + end_padding, alignment)!!;
	$endif;
}

fn void* trealloc(void* ptr, usz size, usz alignment = mem::DEFAULT_MEM_ALIGNMENT) @builtin @inline
{
	return temp_allocator().realloc_aligned(ptr, size, alignment)!!;
}

macro void @pool(;@body) @builtin
{
	TempAllocator* temp = temp_allocator();
	usz mark = temp.used;
	defer temp.reset(mark);
	@body();
}

tlocal Allocator* thread_allocator @private = allocator::LIBC_ALLOCATOR;
tlocal TempAllocator* thread_temp_allocator @private = null;

macro TempAllocator* temp_allocator() => temp();

macro TempAllocator* temp()
{
	if (!thread_temp_allocator)
	{
		$switch (env::MEMORY_ENV):
		$case NORMAL:
			thread_temp_allocator = allocator::new_temp(1024 * 256, thread_allocator)!!;
		$case SMALL:
			thread_temp_allocator = allocator::new_temp(1024 * 16, thread_allocator)!!;
		$case TINY:
			thread_temp_allocator = allocator::new_temp(1024 * 2, thread_allocator)!!;
		$case NONE:
			unreachable("Temp allocator must explicitly created when memory-env is set to 'none'.");
		$endswitch;
	}
	return thread_temp_allocator;
}

macro Allocator* current_allocator() => thread_allocator;
macro Allocator* heap() => thread_allocator;

$if (!env::COMPILER_LIBC_AVAILABLE && env::ARCH_TYPE == ArchType.WASM32 || env::ARCH_TYPE == ArchType.WASM64):

SimpleHeapAllocator wasm_allocator @private;

extern int __heap_base;

static initialize @priority(1)
{
	allocator::wasm_memory.allocate_block(mem::DEFAULT_MEM_ALIGNMENT)!!; // Give us a valid null.
	// Check if we need to move the heap.
	uptr start = (uptr)&__heap_base;
	if (start > mem::DEFAULT_MEM_ALIGNMENT) allocator::wasm_memory.use = start;
	wasm_allocator.init(fn (x) => allocator::wasm_memory.allocate_block(x));
	thread_allocator = &wasm_allocator;
}
$endif;


