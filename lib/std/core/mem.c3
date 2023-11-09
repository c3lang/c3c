// Copyright (c) 2021-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::mem;

const MAX_MEMORY_ALIGNMENT = 0x1000_0000;
const DEFAULT_MEM_ALIGNMENT = (void*.alignof) * 2;


/**
 * Load a vector from memory according to a mask assuming default alignment.
 *
 * @param ptr "The pointer address to load from."
 * @param mask "The mask for the load"
 * @param passthru "The value to use for non masked values"
 * @require $assignable(&&passthru, $typeof(ptr)) : "Pointer and passthru must match"
 * @require @typekind(passthru) == VECTOR : "Expected passthru to be a vector"
 * @require passthru.len == mask.len : "Mask and passthru must have the same length"
 *
 * @return "A vector with the loaded values where the mask is true, passthru where the mask is false"
 **/
macro masked_load(ptr, bool[<*>] mask, passthru)
{
	return $$masked_load(ptr, mask, passthru, 0);
}

/**
 * Load a vector from memory according to a mask.
 *
 * @param ptr "The pointer address to load from."
 * @param mask "The mask for the load"
 * @param passthru "The value to use for non masked values"
 * @param $alignment "The alignment to assume for the pointer"
 *
 * @require $assignable(&&passthru, $typeof(ptr)) : "Pointer and passthru must match"
 * @require @typekind(passthru) == VECTOR : "Expected passthru to be a vector"
 * @require passthru.len == mask.len : "Mask and passthru must have the same length"
 * @require math::is_power_of_2($alignment) : "The alignment must be a power of two"
 *
 * @return "A vector with the loaded values where the mask is true, passthru where the mask is false"
 **/
macro @masked_load_aligned(ptr, bool[<*>] mask, passthru, usz $alignment)
{
	return $$masked_load(ptr, mask, passthru, $alignment);
}

/**
 * Load values from a pointer vector, assuming default alignment.
 *
 * @param ptrvec "The vector of pointers to load from."
 * @param mask "The mask for the load"
 * @param passthru "The value to use for non masked values"
 *
 * @require @typekind(ptrvec) == VECTOR : "Expected ptrvec to be a vector"
 * @require @typekind(passthru) == VECTOR : "Expected passthru to be a vector"
 * @require $assignable(&&passthru[0], $typeof(ptrvec[0])) : "Pointer and passthru must match"
 * @require passthru.len == mask.len : "Mask and passthru must have the same length"
 * @require mask.len == ptrvec.len : "Mask and ptrvec must have the same length"
 *
 * @return "A vector with the loaded values where the mask is true, passthru where the mask is false"
 **/
macro gather(ptrvec, bool[<*>] mask, passthru)
{
	return $$gather(ptrvec, mask, passthru, 0);
}


/**
 * Load values from a pointer vector.
 *
 * @param ptrvec "The vector of pointers to load from."
 * @param mask "The mask for the load"
 * @param passthru "The value to use for non masked values"
 * @param $alignment "The alignment to assume for the pointers"
 *
 * @require @typekind(ptrvec) == VECTOR : "Expected ptrvec to be a vector"
 * @require @typekind(passthru) == VECTOR : "Expected passthru to be a vector"
 * @require $assignable(&&passthru[0], $typeof(ptrvec[0])) : "Pointer and passthru must match"
 * @require passthru.len == mask.len : "Mask and passthru must have the same length"
 * @require mask.len == ptrvec.len : "Mask and ptrvec must have the same length"
 * @require math::is_power_of_2($alignment) : "The alignment must be a power of two"
 *
 * @return "A vector with the loaded values where the mask is true, passthru where the mask is false"
 **/
macro @gather_aligned(ptrvec, bool[<*>] mask, passthru, usz $alignment)
{
	return $$gather(ptrvec, mask, passthru, $alignment);
}


/**
 * Store parts of a vector according to the mask, assuming default alignment.
 *
 * @param ptr "The pointer address to store to."
 * @param value "The value to store masked"
 * @param mask "The mask for the store"
 *
 * @require $assignable(&&value, $typeof(ptr)) : "Pointer and value must match"
 * @require @typekind(value) == VECTOR : "Expected value to be a vector"
 * @require value.len == mask.len : "Mask and value must have the same length"
 **/
macro masked_store(ptr, value, bool[<*>] mask)
{
	return $$masked_store(ptr, value, mask, 0);
}

/**
 * @param ptr "The pointer address to store to."
 * @param value "The value to store masked"
 * @param mask "The mask for the store"
 * @param $alignment "The alignment of the pointer"
 *
 * @require $assignable(&&value, $typeof(ptr)) : "Pointer and value must match"
 * @require @typekind(value) == VECTOR : "Expected value to be a vector"
 * @require value.len == mask.len : "Mask and value must have the same length"
 * @require math::is_power_of_2($alignment) : "The alignment must be a power of two"
 *
 **/
macro @masked_store_aligned(ptr, value, bool[<*>] mask, usz $alignment)
{
	return $$masked_store(ptr, value, mask, $alignment);
}

/**
 * @param ptrvec "The vector pointer containing the addresses to store to."
 * @param value "The value to store masked"
 * @param mask "The mask for the store"
 * @require @typekind(ptrvec) == VECTOR : "Expected ptrvec to be a vector"
 * @require @typekind(value) == VECTOR : "Expected value to be a vector"
 * @require $assignable(&&value[0], $typeof(ptrvec[0])) : "Pointer and value must match"
 * @require value.len == mask.len : "Mask and value must have the same length"
 * @require mask.len == ptrvec.len : "Mask and ptrvec must have the same length"
 *
 **/
macro scatter(ptrvec, value, bool[<*>] mask)
{
	return $$scatter(ptrvec, value, mask, 0);
}

/**
 * @param ptrvec "The vector pointer containing the addresses to store to."
 * @param value "The value to store masked"
 * @param mask "The mask for the store"
 * @param $alignment "The alignment of the load"
 *
 * @require @typekind(ptrvec) == VECTOR : "Expected ptrvec to be a vector"
 * @require @typekind(value) == VECTOR : "Expected value to be a vector"
 * @require $assignable(&&value[0], $typeof(ptrvec[0])) : "Pointer and value must match"
 * @require value.len == mask.len : "Mask and value must have the same length"
 * @require mask.len == ptrvec.len : "Mask and ptrvec must have the same length"
 * @require math::is_power_of_2($alignment) : "The alignment must be a power of two"
 **/
macro @scatter_aligned(ptrvec, value, bool[<*>] mask, usz $alignment)
{
	return $$scatter(ptrvec, value, mask, $alignment);
}

macro @volatile_load(&x) @builtin
{
	return $$volatile_load(x);
}

/**
 * @require $assignable(y, $typeof(*x)) : "The value doesn't match the variable"
 **/
macro @volatile_store(&x, y) @builtin
{
	return $$volatile_store(x, ($typeof(*x))y);
}

enum AtomicOrdering : int
{
	NOT_ATOMIC,         // Not atomic
	UNORDERED,          // No lock
	RELAXED,            // Consistent ordering
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
 * @require @typekind(x) == POINTER "You can only load from a pointer"
 **/
macro @atomic_load(&x, AtomicOrdering $ordering = SEQ_CONSISTENT, $volatile = false) @builtin
{
	return $$atomic_load(x, $volatile, (int)$ordering);
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
	$$atomic_store(x, value, $volatile, (int)$ordering);
}

/**
 * @require $success != AtomicOrdering.NOT_ATOMIC && $success != AtomicOrdering.UNORDERED "Acquire ordering is not valid."
 * @require $failure != AtomicOrdering.RELEASE && $failure != AtomicOrdering.ACQUIRE_RELEASE "Acquire release is not valid."
 **/
macro compare_exchange(ptr, compare, value, AtomicOrdering $success = SEQ_CONSISTENT, AtomicOrdering $failure = SEQ_CONSISTENT, bool $volatile = true, bool $weak = false, usz $alignment = 0)
{
	return $$compare_exchange(ptr, compare, value, $volatile, $weak, $success.ordinal, $failure.ordinal, $alignment);
}

/**
 * @require $success != AtomicOrdering.NOT_ATOMIC && $success != AtomicOrdering.UNORDERED "Acquire ordering is not valid."
 * @require $failure != AtomicOrdering.RELEASE && $failure != AtomicOrdering.ACQUIRE_RELEASE "Acquire release is not valid."
 **/
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
	$if $inlined:
		$$memset_inline(dst, (char)0, len, $is_volatile, $dst_align);
	$else
		$$memset(dst, (char)0, len, $is_volatile, $dst_align);
	$endif
}

/**
 * Copy memory from src to dst efficiently, assuming the memory ranges do not overlap.
 *
 * @param [&out] dst "The destination to copy to"
 * @param [&in] src "The source to copy from"
 * @param len "The number of bytes to copy"
 * @param $dst_align "the alignment of the destination if different from the default, 0 assumes the default"
 * @param $src_align "the alignment of the destination if different from the default, 0 assumes the default"
 * @param $is_volatile "True if this copy should be treated as volatile, i.e. it can't be optimized away."
 * @param $inlined "True if this copy should never call the OS memcpy."
 *
 * @require len == 0 || dst + len <= src || src + len <= dst : "Ranges may not overlap"
 **/
macro void copy(void* dst, void* src, usz len, usz $dst_align = 0, usz $src_align = 0, bool $is_volatile = false, bool $inlined = false)
{
	$if $inlined:
		$$memcpy_inline(dst, src, len, $is_volatile, $dst_align, $src_align);
	$else
		$$memcpy(dst, src, len, $is_volatile, $dst_align, $src_align);
	$endif
}

/**
 * Copy memory from src to dst but correctly handle the possibility of overlapping ranges.
 *
 * @param [&out] dst "The destination to copy to"
 * @param [&in] src "The source to copy from"
 * @param len "The number of bytes to copy"
 * @param $dst_align "the alignment of the destination if different from the default, 0 assumes the default"
 * @param $src_align "the alignment of the destination if different from the default, 0 assumes the default"
 * @param $is_volatile "True if this copy should be treated as volatile, i.e. it can't be optimized away."
 **/
macro void move(void* dst, void* src, usz len, usz $dst_align = 0, usz $src_align = 0, bool $is_volatile = false)
{
	$$memmove(dst, src, len, $is_volatile, $dst_align, $src_align);
}

/**
 * Sets all memory in a region to that of the provided byte.
 *
 * @param [&out] dst "The destination to copy to"
 * @param val "The value to copy into memory"
 * @param len "The number of bytes to copy"
 * @param $dst_align "the alignment of the destination if different from the default, 0 assumes the default"
 * @param $is_volatile "True if this copy should be treated as volatile, i.e. it can't be optimized away."
 * @param $inlined "True if this copy should never call the OS memset."
 *
 * @ensure !len || (dst[0] == val && dst[len - 1] == val)
 **/
macro void set(void* dst, char val, usz len, usz $dst_align = 0, bool $is_volatile = false, bool $inlined = false)
{
	$if $inlined:
		$$memset_inline(dst, val, len, $is_volatile, $dst_align);
	$else
		$$memset(dst, val, len, $is_volatile, $dst_align);
	$endif
}

/**
 * @require values::@inner_kind(a) == TypeKind.SUBARRAY || values::@inner_kind(a) == TypeKind.POINTER
 * @require values::@inner_kind(b) == TypeKind.SUBARRAY || values::@inner_kind(b) == TypeKind.POINTER
 * @require values::@inner_kind(a) != TypeKind.SUBARRAY || len == -1
 * @require values::@inner_kind(a) != TypeKind.POINTER || len > -1
 * @require values::@assign_to(a, b) && values::@assign_to(b, a)
 **/
macro bool equals(a, b, isz len = -1, usz $align = 0)
{
	$if !$align:
		$align = $typeof(a[0]).alignof;
	$endif
	void* x @noinit;
	void* y @noinit;
	$if values::@inner_kind(a) == TypeKind.SUBARRAY:
		len = a.len;
		if (len != b.len) return false;
		x = a.ptr;
		y = b.ptr;
	$else
		x = a;
		y = b;
		assert(len >= 0, "A zero or positive length must be given when comparing pointers.");
	$endif

	if (!len) return true;
	var $Type;
	$switch ($align)
		$case 1:
			$Type = char;
		$case 2:
			$Type = ushort;
		$case 4:
			$Type = uint;
		$case 8:
		$default:
			$Type = ulong;
	$endswitch
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


macro type_alloc_must_be_aligned($Type)
{
	return $Type.alignof > DEFAULT_MEM_ALIGNMENT;
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

macro void @report_heap_allocs_in_scope(;@body())
{
	TrackingAllocator tracker;
	tracker.init(thread_allocator);
	Allocator* old_allocator = thread_allocator;
	thread_allocator = &tracker;
	defer
	{
		thread_allocator = old_allocator;
		tracker.print_report();
		tracker.free();
	}
	@body();
}

macro void @stack_mem(usz $size; @body(Allocator* mem)) @builtin
{
	char[$size] buffer;
	OnStackAllocator allocator;
	allocator.init(&buffer, mem::heap());
	defer allocator.free();
	@body(&allocator);
}

macro void @stack_pool(usz $size; @body) @builtin
{
	char[$size] buffer;
	OnStackAllocator allocator;
	allocator.init(&buffer, mem::heap());
	defer allocator.free();
	mem::@scoped(&allocator)
	{
		@body();
	};
}

macro void @pool(TempAllocator* #other_temp = null; @body) @builtin
{
	TempAllocator* current = temp();
	var $has_arg = !$is_const(#other_temp);
	$if $has_arg:
		TempAllocator* original = current;
		if (current == (void*)#other_temp) current = temp_allocator_next();
	$endif
	usz mark = current.used;
	defer
	{
		current.reset(mark);
		$if $has_arg:
			thread_temp_allocator = original;
		$endif;
	}
	@body();
}

tlocal Allocator* thread_allocator @private = &allocator::LIBC_ALLOCATOR;
tlocal TempAllocator* thread_temp_allocator @private = null;
tlocal TempAllocator*[2] temp_allocator_pair @private;
Allocator* temp_base_allocator @private = &allocator::LIBC_ALLOCATOR;

macro TempAllocator* create_default_sized_temp_allocator(Allocator* allocator) @local
{
	$switch (env::MEMORY_ENV)
	$case NORMAL:
		return allocator::new_temp(1024 * 256, allocator)!!;
	$case SMALL:
		return allocator::new_temp(1024 * 16, allocator)!!;
	$case TINY:
		return allocator::new_temp(1024 * 2, allocator)!!;
	$case NONE:
		unreachable("Temp allocator must explicitly created when memory-env is set to 'none'.");
	$endswitch
}

fn TempAllocator *temp_allocator_next() @private
{
	if (!thread_temp_allocator)
	{
		init_default_temp_allocators();
		return thread_temp_allocator;
	}
	usz index = thread_temp_allocator == temp_allocator_pair[0] ? 1 : 0;
	return thread_temp_allocator = temp_allocator_pair[index];
}

import libc;

fn void init_default_temp_allocators() @private
{
	temp_allocator_pair[0] = create_default_sized_temp_allocator(temp_base_allocator);
	temp_allocator_pair[1] = create_default_sized_temp_allocator(temp_base_allocator);
	thread_temp_allocator = temp_allocator_pair[0];
}

macro TempAllocator* temp()
{
	if (!thread_temp_allocator)
	{
		init_default_temp_allocators();
	}
	return thread_temp_allocator;
}

macro Allocator* current_allocator() => thread_allocator;
macro Allocator* heap() => thread_allocator;


module std::core::mem @if(WASM_NOLIBC);

SimpleHeapAllocator wasm_allocator @private;
extern int __heap_base;

fn void initialize_wasm_mem() @init(1) @private
{
	allocator::wasm_memory.allocate_block(mem::DEFAULT_MEM_ALIGNMENT)!!; // Give us a valid null.
	// Check if we need to move the heap.
	uptr start = (uptr)&__heap_base;
	if (start > mem::DEFAULT_MEM_ALIGNMENT) allocator::wasm_memory.use = start;
	wasm_allocator.init(fn (x) => allocator::wasm_memory.allocate_block(x));
	temp_base_allocator = &wasm_allocator;
	thread_allocator = &wasm_allocator;
}

module std::core::mem;


macro TrackingEnv* get_tracking_env()
{
	$if env::TRACK_MEMORY:
		return &&TrackingEnv { $$FILE, $$FUNC, $$LINE };
	$else
		return null;
	$endif
}

macro @clone(value, TrackingEnv* env = mem::get_tracking_env()) @builtin
{
	return mem::heap().clone(value, env);
}

macro @tclone(value) @builtin
{
	return mem::temp().clone(value);
}

fn void* malloc(usz size, TrackingEnv* env = mem::get_tracking_env()) @builtin @inline
{
	return mem::heap().alloc(size, env);
}

fn void* tmalloc(usz size, usz alignment = 0, usz offset = 0, TrackingEnv* env = mem::get_tracking_env()) @builtin @inline
{
	return temp().acquire(size, false, alignment, offset, env)!!;
}

macro new($Type, TrackingEnv* env = mem::get_tracking_env())
{
	return heap().new($Type, .env = env);
}

macro new_clear($Type, TrackingEnv* env = mem::get_tracking_env())
{
	return heap().new_clear($Type, env);
}

macro new_temp($Type)
{
	return tmalloc($Type.sizeof);
}

macro new_temp_clear($Type)
{
	return tcalloc($Type.sizeof);
}

macro new_array($Type, usz elements, TrackingEnv* env = mem::get_tracking_env())
{
	return heap().new_array($Type, elements, .env = env);
}

macro temp_array($Type, usz elements)
{
	return (($Type*)tmalloc($Type.sizeof * elements, $Type.alignof))[:elements];
}

macro new_zero_array($Type, usz elements, TrackingEnv* env = mem::get_tracking_env())
{
	return heap().new_zero_array($Type, elements, env);
}

macro temp_zero_array($Type, usz elements)
{
	return (($Type*)tcalloc($Type.sizeof * elements, $Type.alignof))[:elements];
}

fn void* calloc(usz size, TrackingEnv* env = mem::get_tracking_env()) @builtin @inline
{
	return heap().calloc(size, env);
}

fn void* tcalloc(usz size, usz alignment = 0, usz offset = 0) @builtin @inline
{
	return temp().acquire(size, false, alignment, offset, null)!!;
}

fn void* realloc(void *ptr, usz new_size, TrackingEnv* env = mem::get_tracking_env()) @builtin @inline
{
	return heap().realloc(ptr, new_size, env);
}

fn void free(void* ptr, TrackingEnv* env = mem::get_tracking_env()) @builtin @inline
{
	heap().free(ptr, env);
}

fn void* trealloc(void* ptr, usz size, usz alignment = mem::DEFAULT_MEM_ALIGNMENT) @builtin @inline
{
	return temp().resize(ptr, size, alignment, 0, null)!!;
}

