// Copyright (c) 2021-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::mem;
import std::core::mem::allocator @public;
import std::os::posix, std::os::win32;
import std::math;

const MAX_MEMORY_ALIGNMENT = 0x1000_0000;
const DEFAULT_MEM_ALIGNMENT = env::WASM ? 16 : (void*.alignof) * 2;
const ulong KB = 1024;
const ulong MB = KB * 1024;
const ulong GB = MB * 1024;
const ulong TB = GB * 1024;

faultdef OUT_OF_MEMORY, INVALID_ALLOC_SIZE;

macro bool @constant_is_power_of_2($x) @const @private
{
	return $x != 0 && ($x & ($x - 1)) == 0;
}

<*
 @return "The os page size."
*>
fn usz os_pagesize()
{
	$switch:
		$case env::POSIX:
			static usz pagesize;
			if (pagesize) return pagesize;
			return pagesize = posix::getpagesize();
		$case env::WIN32:
			static usz pagesize;
			if (pagesize) return pagesize;
		    Win32_SYSTEM_INFO info;
            win32::getSystemInfo(&info);
			return pagesize = info.dwPageSize;
		$default:
			return 4096;
	$endswitch
}

<*
 Load a vector from memory according to a mask assuming default alignment.

 @param ptr : "The pointer address to load from."
 @param mask : "The mask for the load"
 @param passthru : "The value to use for non masked values"
 @require $defined(*ptr = passthru) : "Pointer and passthru must match"
 @require $kindof(passthru) == VECTOR : "Expected passthru to be a vector"
 @require passthru.len == mask.len : "Mask and passthru must have the same length"

 @return "A vector with the loaded values where the mask is true, passthru where the mask is false"
*>
macro masked_load(ptr, bool[<*>] mask, passthru)
{
	return $$masked_load(ptr, mask, passthru, 0);
}

<*
 Load a vector from memory according to a mask.

 @param ptr : "The pointer address to load from."
 @param mask : "The mask for the load"
 @param passthru : "The value to use for non masked values"
 @param $alignment : "The alignment to assume for the pointer"

 @require $defined(*ptr = passthru) : "Pointer and passthru must match"
 @require $kindof(passthru) == VECTOR : "Expected passthru to be a vector"
 @require passthru.len == mask.len : "Mask and passthru must have the same length"
 @require @constant_is_power_of_2($alignment) : "The alignment must be a power of two"

 @return "A vector with the loaded values where the mask is true, passthru where the mask is false"
 @ensure $typeof(return) == $typeof(*ptr)
*>
macro @masked_load_aligned(ptr, bool[<*>] mask, passthru, usz $alignment)
{
	return $$masked_load(ptr, mask, passthru, $alignment);
}

<*
 Load values from a pointer vector, assuming default alignment.

 @param ptrvec : "The vector of pointers to load from."
 @param mask : "The mask for the load"
 @param passthru : "The value to use for non masked values"

 @require $kindof(ptrvec) == VECTOR : "Expected ptrvec to be a vector"
 @require $kindof(passthru) == VECTOR : "Expected passthru to be a vector"
 @require $defined(*ptrvec[0] = passthru[0]) : "Pointer and passthru must match"
 @require passthru.len == mask.len : "Mask and passthru must have the same length"
 @require mask.len == ptrvec.len : "Mask and ptrvec must have the same length"

 @return "A vector with the loaded values where the mask is true, passthru where the mask is false"
*>
macro gather(ptrvec, bool[<*>] mask, passthru)
{
	return $$gather(ptrvec, mask, passthru, 0);
}


<*
 Load values from a pointer vector.

 @param ptrvec : "The vector of pointers to load from."
 @param mask : "The mask for the load"
 @param passthru : "The value to use for non masked values"
 @param $alignment : "The alignment to assume for the pointers"

 @require $kindof(ptrvec) == VECTOR : "Expected ptrvec to be a vector"
 @require $kindof(passthru) == VECTOR : "Expected passthru to be a vector"
 @require $defined(*ptrvec[0] = passthru[0]) : "Pointer and passthru must match"
 @require passthru.len == mask.len : "Mask and passthru must have the same length"
 @require mask.len == ptrvec.len : "Mask and ptrvec must have the same length"
 @require @constant_is_power_of_2($alignment) : "The alignment must be a power of two"

 @return "A vector with the loaded values where the mask is true, passthru where the mask is false"
*>
macro @gather_aligned(ptrvec, bool[<*>] mask, passthru, usz $alignment)
{
	return $$gather(ptrvec, mask, passthru, $alignment);
}


<*
 Store parts of a vector according to the mask, assuming default alignment.

 @param ptr : "The pointer address to store to."
 @param value : "The value to store masked"
 @param mask : "The mask for the store"

 @require $defined(*ptr = value) : "Pointer and value must match"
 @require $kindof(value) == VECTOR : "Expected value to be a vector"
 @require value.len == mask.len : "Mask and value must have the same length"
*>
macro masked_store(ptr, value, bool[<*>] mask)
{
	return $$masked_store(ptr, value, mask, 0);
}

<*
 @param ptr : "The pointer address to store to."
 @param value : "The value to store masked"
 @param mask : "The mask for the store"
 @param $alignment : "The alignment of the pointer"

 @require $defined(*ptr = value) : "Pointer and value must match"
 @require $kindof(value) == VECTOR : "Expected value to be a vector"
 @require value.len == mask.len : "Mask and value must have the same length"
 @require @constant_is_power_of_2($alignment) : "The alignment must be a power of two"

*>
macro @masked_store_aligned(ptr, value, bool[<*>] mask, usz $alignment)
{
	return $$masked_store(ptr, value, mask, $alignment);
}

<*
 @param ptrvec : "The vector pointer containing the addresses to store to."
 @param value : "The value to store masked"
 @param mask : "The mask for the store"
 @require $kindof(ptrvec) == VECTOR : "Expected ptrvec to be a vector"
 @require $kindof(value) == VECTOR : "Expected value to be a vector"
 @require $defined(*ptrvec[0] = value[0]) : "Pointer and value must match"
 @require value.len == mask.len : "Mask and value must have the same length"
 @require mask.len == ptrvec.len : "Mask and ptrvec must have the same length"

*>
macro scatter(ptrvec, value, bool[<*>] mask)
{
	return $$scatter(ptrvec, value, mask, 0);
}

<*
 @param ptrvec : "The vector pointer containing the addresses to store to."
 @param value : "The value to store masked"
 @param mask : "The mask for the store"
 @param $alignment : "The alignment of the load"

 @require $kindof(ptrvec) == VECTOR : "Expected ptrvec to be a vector"
 @require $kindof(value) == VECTOR : "Expected value to be a vector"
 @require $defined(*ptrvec[0] = value[0]) : "Pointer and value must match"
 @require value.len == mask.len : "Mask and value must have the same length"
 @require mask.len == ptrvec.len : "Mask and ptrvec must have the same length"
 @require @constant_is_power_of_2($alignment) : "The alignment must be a power of two"
*>
macro @scatter_aligned(ptrvec, value, bool[<*>] mask, usz $alignment)
{
	return $$scatter(ptrvec, value, mask, $alignment);
}

<*
 @param #x : "The variable or dereferenced pointer to load."
 @param $alignment : "The alignment to assume for the load"
 @return "The value of the variable"

 @require @constant_is_power_of_2($alignment) : "The alignment must be a power of two"
 @require $defined(&#x) : "This must be a variable or dereferenced pointer"
*>
macro @unaligned_load(#x, usz $alignment) @builtin
{
	return $$unaligned_load(&#x, $alignment, false);
}

<*
 @param #x : "The variable or dereferenced pointer to store to."
 @param value : "The value to store."
 @param $alignment : "The alignment to assume for the store"
 @return "The value stored"

 @require $defined(&#x) : "This must be a variable or dereferenced pointer"
 @require $defined(#x = value) : "The value doesn't match the variable"
 @require @constant_is_power_of_2($alignment) : "The alignment must be a power of two"
*>
macro @unaligned_store(#x, value, usz $alignment) @builtin
{
	return $$unaligned_store(&#x, ($typeof(#x))value, $alignment, false);
}


<*
 @param #x : "The variable or dereferenced pointer to load."
 @return "The value of the variable"

 @require $defined(&#x) : "This must be a variable or dereferenced pointer"
*>
macro @volatile_load(#x) @builtin
{
	return $$volatile_load(&#x);
}

<*
 @param #x : "The variable or dereferenced pointer to store to."
 @param value : "The value to store."
 @return "The value stored"

 @require $defined(&#x) : "This must be a variable or dereferenced pointer"
 @require $defined(#x = value) : "The value doesn't match the variable"
*>
macro @volatile_store(#x, value) @builtin
{
	return $$volatile_store(&#x, ($typeof(#x))value);
}

<*
 @param ptr : "The pointer to load from"
 @param $align : "The alignment to assume for the load"
 @param $volatile : "Whether the load is volatile or not, defaults to false"
 @return "The value of the variable"

 @require $defined(*ptr) : "This must be a typed pointer"
 @require @constant_is_power_of_2($align) : "The alignment must be a power of two"
*>
macro load(ptr, usz $align, bool $volatile = false)
{
	return $$unaligned_load(ptr, $align, $volatile);
}

<*
 @param ptr : "The pointer to store to."
 @param value : "The value to store."
 @param $align : "The alignment to assume for the store"
 @param $volatile : "Whether the store is volatile, defaults to false"
 @return "The value stored"

 @require $defined(*ptr) : "This must be a typed pointer"
 @require $defined(*ptr = value) : "The value doesn't match the variable"
 @require @constant_is_power_of_2($align) : "The alignment must be a power of two"
*>
macro store(ptr, value, usz $align, bool $volatile = false)
{
	return $$unaligned_store(ptr, ($typeof(*ptr))value, $align, $volatile);
}

<*
 All possible atomic orderings
*>
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

<*
 @param #x : "the variable or dereferenced pointer to load."
 @param $ordering : "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 @param $volatile : "whether the load should be volatile, defaults to 'false'"
 @return "returns the value of x"

 @require $defined(&#x) : "This must be a variable or dereferenced pointer"
 @require $ordering != AtomicOrdering.RELEASE : "Release ordering is not valid for load."
 @require $ordering != AtomicOrdering.ACQUIRE_RELEASE : "Acquire release is not valid for load."
 @require types::may_load_atomic($typeof(#x)) : "Only integer, float and pointers may be used."
*>
macro @atomic_load(#x, AtomicOrdering $ordering = SEQ_CONSISTENT, $volatile = false) @builtin
{
	return $$atomic_load(&#x, $volatile, $ordering.ordinal);
}

<*
 @param #x : "the variable or dereferenced pointer to store to."
 @param value : "the value to store."
 @param $ordering : "the atomic ordering of the store, defaults to SEQ_CONSISTENT"
 @param $volatile : "whether the store should be volatile, defaults to 'false'"

 @require $ordering != AtomicOrdering.ACQUIRE : "Acquire ordering is not valid for store."
 @require $ordering != AtomicOrdering.ACQUIRE_RELEASE : "Acquire release is not valid for store."
 @require types::may_load_atomic($typeof(#x)) : "Only integer, float and pointers may be used."
 @require $defined(&#x) : "This must be a variable or dereferenced pointer"
 @require $defined(#x = value) : "The value doesn't match the variable"
*>
macro void @atomic_store(#x, value, AtomicOrdering $ordering = SEQ_CONSISTENT, $volatile = false) @builtin
{
	$$atomic_store(&#x, value, $volatile, $ordering.ordinal);
}

<*
 @require $success != AtomicOrdering.NOT_ATOMIC && $success != AtomicOrdering.UNORDERED : "Acquire ordering is not valid."
 @require $failure != AtomicOrdering.RELEASE && $failure != AtomicOrdering.ACQUIRE_RELEASE : "Acquire release is not valid."
*>
macro compare_exchange(ptr, compare, value, AtomicOrdering $success = SEQ_CONSISTENT, AtomicOrdering $failure = SEQ_CONSISTENT, bool $volatile = true, bool $weak = false, usz $alignment = 0)
{
	return $$compare_exchange(ptr, compare, value, $volatile, $weak, $success.ordinal, $failure.ordinal, $alignment);
}

<*
 @require $success != AtomicOrdering.NOT_ATOMIC && $success != AtomicOrdering.UNORDERED : "Acquire ordering is not valid."
 @require $failure != AtomicOrdering.RELEASE && $failure != AtomicOrdering.ACQUIRE_RELEASE : "Acquire release is not valid."
*>
macro compare_exchange_volatile(ptr, compare, value, AtomicOrdering $success = SEQ_CONSISTENT, AtomicOrdering $failure = SEQ_CONSISTENT)
{
	return compare_exchange(ptr, compare, value, $success, $failure, true);
}

<*
 @require math::is_power_of_2(alignment)
*>
fn usz aligned_offset(usz offset, usz alignment)
{
	return (offset + alignment - 1) & ~(alignment - 1);
}

macro void* aligned_pointer(void* ptr, usz alignment)
{
	return (void*)(uptr)aligned_offset((uptr)ptr, alignment);
}

<*
 @require math::is_power_of_2(alignment)
*>
fn bool ptr_is_aligned(void* ptr, usz alignment) @inline
{
	return (uptr)ptr & ((uptr)alignment - 1) == 0;
}

fn bool ptr_is_page_aligned(void* ptr) @inline
{
	return (uptr)ptr & ((uptr)os_pagesize() - 1) == 0;
}

macro void zero_volatile(char[] data)
{
	$$memset(data.ptr, (char)0, data.len, true, (usz)1);
}

macro void clear(void* dst, usz len, usz $dst_align = 0, bool $is_volatile = false)
{
	$$memset(dst, (char)0, len, $is_volatile, $dst_align);
}

macro void clear_inline(void* dst, usz $len, usz $dst_align = 0, bool $is_volatile = false)
{
	$$memset_inline(dst, (char)0, $len, $is_volatile, $dst_align);
}

<*
 Copy memory from src to dst efficiently, assuming the memory ranges do not overlap.

 @param [&out] dst : "The destination to copy to"
 @param [in] src : "The source to copy from"
 @param len : "The number of bytes to copy"
 @param $dst_align : "the alignment of the destination if different from the default, 0 assumes the default"
 @param $src_align : "the alignment of the destination if different from the default, 0 assumes the default"
 @param $is_volatile : "True if this copy should be treated as volatile, i.e. it can't be optimized away."

 @require src != null || len == 0 : "Copying a null with non-zero length is invalid"
 @require len == 0 || dst + len <= src || src + len <= dst : "Ranges may not overlap"
*>
macro void copy(void* dst, void* src, usz len, usz $dst_align = 0, usz $src_align = 0, bool $is_volatile = false)
{
	$$memcpy(dst, src, len, $is_volatile, $dst_align, $src_align);
}

<*
 Copy memory from src to dst efficiently, assuming the memory ranges do not overlap, it
 will always be inlined and never call memcopy

 @param [&out] dst : "The destination to copy to"
 @param [in] src : "The source to copy from"
 @param $len : "The number of bytes to copy"
 @param $dst_align : "the alignment of the destination if different from the default, 0 assumes the default"
 @param $src_align : "the alignment of the destination if different from the default, 0 assumes the default"
 @param $is_volatile : "True if this copy should be treated as volatile, i.e. it can't be optimized away."

 @require src != null || $len == 0 : "Copying a null with non-zero length is invalid"
 @require $len == 0 || dst + $len <= src || src + $len <= dst : "Ranges may not overlap"
*>
macro void copy_inline(void* dst, void* src, usz $len, usz $dst_align = 0, usz $src_align = 0, bool $is_volatile = false)
{
	$$memcpy_inline(dst, src, $len, $is_volatile, $dst_align, $src_align);
}

<*
 Copy memory from src to dst but correctly handle the possibility of overlapping ranges.

 @param [&out] dst : "The destination to copy to"
 @param [in] src : "The source to copy from"
 @param len : "The number of bytes to copy"
 @param $dst_align : "the alignment of the destination if different from the default, 0 assumes the default"
 @param $src_align : "the alignment of the destination if different from the default, 0 assumes the default"
 @param $is_volatile : "True if this copy should be treated as volatile, i.e. it can't be optimized away."

 @require src != null || len == 0 : "Moving a null with non-zero length is invalid"
*>
macro void move(void* dst, void* src, usz len, usz $dst_align = 0, usz $src_align = 0, bool $is_volatile = false)
{
	$$memmove(dst, src, len, $is_volatile, $dst_align, $src_align);
}

<*
 Sets all memory in a region to that of the provided byte.

 @param [&out] dst : "The destination to copy to"
 @param val : "The value to copy into memory"
 @param len : "The number of bytes to copy"
 @param $dst_align : "the alignment of the destination if different from the default, 0 assumes the default"
 @param $is_volatile : "True if this copy should be treated as volatile, i.e. it can't be optimized away."

 @ensure !len || (dst[0] == val && dst[len - 1] == val)
*>
macro void set(void* dst, char val, usz len, usz $dst_align = 0, bool $is_volatile = false)
{
	$$memset(dst, val, len, $is_volatile, $dst_align);
}

<*
 Sets all memory in a region to that of the provided byte. Never calls OS memset.

 @param [&out] dst : "The destination to copy to"
 @param val : "The value to copy into memory"
 @param $len : "The number of bytes to copy"
 @param $dst_align : "the alignment of the destination if different from the default, 0 assumes the default"
 @param $is_volatile : "True if this copy should be treated as volatile, i.e. it can't be optimized away."

 @ensure !$len || (dst[0] == val && dst[$len - 1] == val)
*>
macro void set_inline(void* dst, char val, usz $len, usz $dst_align = 0, bool $is_volatile = false)
{
	$$memset_inline(dst, val, $len, $is_volatile, $dst_align);
}
<*
 Test if n elements are equal in a slice, pointed to by a pointer etc.

 @require values::@inner_kind(a) == TypeKind.SLICE || values::@inner_kind(a) == TypeKind.POINTER
 @require values::@inner_kind(b) == TypeKind.SLICE || values::@inner_kind(b) == TypeKind.POINTER
 @require values::@inner_kind(a) != TypeKind.SLICE || len == -1
 @require values::@inner_kind(a) != TypeKind.POINTER || len > -1
 @require $defined(a = b, b = a)
*>
macro bool equals(a, b, isz len = -1, usz $align = 0)
{
	$if !$align:
		$align = $typeof(a[0]).alignof;
	$endif
	void* x @noinit;
	void* y @noinit;
	$if values::@inner_kind(a) == TypeKind.SLICE:
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
	$switch $align:
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

<*
 Check if an allocation must be aligned given the type.

 @return `true if the alignment of the type exceeds DEFAULT_MEM_ALIGNMENT.`
*>
macro bool type_alloc_must_be_aligned($Type)
{
	return $Type.alignof > DEFAULT_MEM_ALIGNMENT;
}

<*
 Run with a specific allocator inside of the macro body.
*>
macro void @scoped(Allocator allocator; @body())
{
	Allocator old_allocator = allocator::thread_allocator;
	allocator::thread_allocator = allocator;
	defer allocator::thread_allocator = old_allocator;
	@body();
}

<*
 Run the tracking allocator in the scope, then
 print out stats.

 @param $enabled : "Set to false to disable tracking"
*>
macro void @report_heap_allocs_in_scope($enabled = true; @body())
{
	$if $enabled:
		TrackingAllocator tracker;
		tracker.init(allocator::thread_allocator);
		Allocator old_allocator = allocator::thread_allocator;
		allocator::thread_allocator = &tracker;
		defer
		{
			allocator::thread_allocator = old_allocator;
			tracker.print_report();
			tracker.free();
		}
	$endif
	@body();
}

<*
 Assert on memory leak in the scope of the macro body.

 @param $report : "Set to false to disable memory report"
*>
macro void @assert_leak($report = true; @body()) @builtin
{
	$if env::DEBUG_SYMBOLS || $feature(MEMORY_ASSERTS):
		TrackingAllocator tracker;
		tracker.init(mem);
		Allocator old_allocator = mem;
		allocator::thread_allocator = &tracker;
		defer
		{
			allocator::thread_allocator = old_allocator;
			defer tracker.free();
			usz allocated = tracker.allocated();
			if (allocated)
			{
				DString report;
				report.init(old_allocator);
				defer report.free();
				$if $report:
					report.append_char('\n');
					(void)tracker.fprint_report(&report);
				$endif
				assert(allocated == 0, "Memory leak detected"
					" (%d bytes allocated).%s",
					allocated, report.str_view());
			}
		}
	$endif
	@body();
}

<*
 Allocate [size] bytes on the stack to use for allocation,
 with the heap allocator as the backing allocator.

 Release everything on scope exit.

 @param $size : `the size of the buffer`
*>
macro void @stack_mem(usz $size; @body(Allocator mem)) @builtin
{
	char[$size] buffer;
	OnStackAllocator allocator;
	allocator.init(&buffer, mem);
	defer allocator.free();
	@body(&allocator);
}

macro void @stack_pool(usz $size; @body) @builtin
{
	char[$size] buffer;
	OnStackAllocator allocator;
	allocator.init(&buffer, mem);
	defer allocator.free();
	mem::@scoped(&allocator)
	{
		@body();
	};
}

<*
 Push the current temp allocator. A push must always be balanced with a pop using the current state.
*>
fn PoolState temp_push()
{
	return allocator::push_pool() @inline;
}

<*
 Pop the current temp allocator. A pop must always be balanced with a push.
*>
fn void temp_pop(PoolState old_state)
{
	allocator::pop_pool(old_state) @inline;
}

<*
 @require pool_size >= 64
 @require realloc_size >= 64
 @require allocator.type != TempAllocator.typeid : "You may not create a temp allocator with a TempAllocator as the backing allocator."
 @require min_size > TempAllocator.sizeof + 64 : "Min size must meaningfully hold the data + some bytes"
*>
macro void @pool_init(Allocator allocator, usz pool_size,
	usz reserve_size = allocator::temp_allocator_reserve_size,
	usz min_size = allocator::temp_allocator_min_size,
	usz realloc_size = allocator::temp_allocator_realloc_size; @body) @builtin
{
	Allocator current = allocator::current_temp;
	TempAllocator* top = allocator::top_temp;
	allocator::create_temp_allocator(allocator, pool_size, reserve_size, min_size, realloc_size);
	defer
	{
		allocator::destroy_temp_allocators();
		allocator::top_temp = top;
		allocator::current_temp = current;
	}
	@body();
}

<*
 Create a new temporary allocator.

 The `reserve` parameter allows you to determine how many bytes should be reserved for
 allocations on the current temporary allocator, if allocations are made inside of the pool scope.
 It is made available for optimization, and can usually be ignored.

 @param reserve : "The amount of bytes to reserve for out-of-order allocations, 0 gives the default."
*>
macro void @pool(usz reserve = 0; @body) @builtin
{
	PoolState state = allocator::push_pool(reserve) @inline;
	defer
	{
		allocator::pop_pool(state) @inline;
	}
	@body();
}

module std::core::mem @if(env::FREESTANDING_WASM);
import std::core::mem::allocator @public;
SimpleHeapAllocator wasm_allocator @private;
extern int __heap_base;

fn void initialize_wasm_mem() @init(1024) @private
{
	allocator::wasm_memory.allocate_block(mem::DEFAULT_MEM_ALIGNMENT)!!; // Give us a valid null.
	// Check if we need to move the heap.
	uptr start = (uptr)&__heap_base;
	if (start > mem::DEFAULT_MEM_ALIGNMENT) allocator::wasm_memory.use = start;
	wasm_allocator.init(fn (x) => allocator::wasm_memory.allocate_block(x));
	allocator::thread_allocator = &wasm_allocator;
	allocator::temp_base_allocator = &wasm_allocator;
}

module std::core::mem;


macro TrackingEnv* get_tracking_env()
{
	$if env::TRACK_MEMORY:
		return &&(TrackingEnv){ $$FILE, $$FUNC, $$LINE };
	$else
		return null;
	$endif
}

<*
 @param value : "The value to clone"
 @return "A pointer to the cloned value"
 @require $alignof(value) <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'clone_aligned' instead"
*>
macro @clone(value) @builtin @nodiscard
{
	return allocator::clone(mem, value);
}

<*
 @param value : "The value to clone"
 @return "A pointer to the cloned value"
*>
macro @clone_slice(value) @builtin @nodiscard => allocator::clone_slice(mem, value);

<*
 @param value : "The value to clone"
 @return "A pointer to the cloned value, which must be released using free_aligned"
*>
macro @clone_aligned(value) @builtin @nodiscard
{
	return allocator::clone_aligned(mem, value);
}

<*
 @param value : "The value to clone"
 @return "A pointer to the cloned value"
*>
macro @tclone(value) @builtin @nodiscard
{
	$if $alignof(value) <= mem::DEFAULT_MEM_ALIGNMENT:
		return tnew($typeof(value), value);
	$else
		return allocator::clone_aligned(tmem, value);
	$endif
}

<*
 @param value : "The value to clone"
 @return "A pointer to the cloned value"
*>
macro @tclone_slice(value) @builtin @nodiscard => allocator::clone_slice(tmem, value);

fn void* malloc(usz size) @builtin @inline @nodiscard
{
	return allocator::malloc(mem, size);
}

<*
 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.
*>
fn void* malloc_aligned(usz size, usz alignment) @builtin @inline @nodiscard
{
	return allocator::malloc_aligned(mem, size, alignment)!!;
}

fn void* tmalloc(usz size, usz alignment = 0) @builtin @inline @nodiscard
{
	if (!size) return null;
	return tmem.acquire(size, NO_ZERO, alignment)!!;
}

<*
 @param $Type : "The type to allocate"
 @param #init : "The optional initializer"

 @require !$defined(#init) ||| $defined($Type a = #init) : "#init must be an initializer for the type"
 @require $Type.alignof <= DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_aligned' instead"
 @return "A pointer to data of type $Type."
*>
macro new($Type, #init = ...) @nodiscard @safemacro
{
	$if $defined(#init):
		$Type* val = malloc($Type.sizeof);
		*val = #init;
		return val;
	$else
		return ($Type*)calloc($Type.sizeof);
	$endif
}

<*
 @param $Type : "The type to allocate"
 @param padding : "The padding to add after the allocation"
 @param #init : "The optional initializer"

 @require !$defined(#init) ||| $defined($Type a = #init) : "#init must be an initializer for the type"
 @return "A pointer to data of type $Type."
*>
macro new_with_padding($Type, usz padding, #init = ...) @nodiscard @safemacro
{
	$if $defined(#init):
		$Type* val = malloc($Type.sizeof + padding);
		*val = #init;
		return val;
	$else
		return ($Type*)calloc($Type.sizeof + padding);
	$endif
}

<*
 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.

 @param $Type : "The type to allocate"
 @param #init : "The optional initializer"

 @require !$defined(#init) ||| $defined($Type a = #init) : "#init must be an initializer for the type"
 @return "A pointer to data of type $Type with the proper alignment"
*>
macro new_aligned($Type, #init = ...) @nodiscard @safemacro
{
	$if $defined(#init):
		$Type* val = malloc_aligned($Type.sizeof, $Type.alignof);
		*val = #init;
		return val;
	$else
		return ($Type*)calloc_aligned($Type.sizeof, $Type.alignof);
	$endif
}

<*
 @param $Type : "The type to allocate"

 @require $Type.kindof != OPTIONAL : "Expected a non-optional type"
 @require $Type.alignof <= DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_aligned' instead"
 @return "A pointer to uninitialized data for the type $Type"
*>
macro alloc($Type) @nodiscard
{
	return ($Type*)malloc($Type.sizeof);
}

<*
 @param $Type : "The type to allocate"
 @param padding : "The padding to add after the allocation"

 @require $Type.kindof != OPTIONAL : "Expected a non-optional type"
  @require $Type.alignof <= DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_aligned' instead"
 @return "A pointer to uninitialized data for the type $Type"
*>
macro alloc_with_padding($Type, usz padding) @nodiscard
{
	return ($Type*)malloc($Type.sizeof + padding);
}

<*

 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.

 @param $Type : "The type to allocate"
 @require $Type.kindof != OPTIONAL : "Expected a non-optional type"
 @return "A pointer to uninitialized data for the type $Type with the proper alignment"
*>
macro alloc_aligned($Type) @nodiscard
{
	return ($Type*)malloc_aligned($Type.sizeof, $Type.alignof);
}

<*
 @param $Type : "The type to allocate"
 @param #init : "The optional initializer"

 @require $Type.kindof != OPTIONAL : "Expected a non-optional type"
 @require !$defined(#init) ||| $defined($Type a = #init) : "#init must be an initializer for the type"
 @return "A pointer to temporary data of type $Type."
*>
macro tnew($Type, #init = ...) @nodiscard @safemacro
{
	$if $defined(#init):
		$Type* val = tmalloc($Type.sizeof, $Type.alignof) @inline;
		*val = #init;
		return val;
	$else
		return ($Type*)tcalloc($Type.sizeof, $Type.alignof) @inline;
	$endif
}

<*
 @param $Type : "The type to allocate"
 @param padding : "The padding to add after the allocation"
 @param #init : "The optional initializer"

 @require $Type.kindof != OPTIONAL : "Expected a non-optional type"
 @require !$defined(#init) ||| $defined($Type a = #init) : "#init must be an initializer for the type"
 @return "A pointer to temporary data of type $Type with added padding at the end."
*>
macro temp_with_padding($Type, usz padding, #init = ...) @nodiscard @safemacro
{
	$if $defined(#init):
		$Type* val = tmalloc($Type.sizeof + padding, $Type.alignof) @inline;
		*val = #init;
		return val;
	$else
		return ($Type*)tcalloc($Type.sizeof + padding, $Type.alignof) @inline;
	$endif
}

<*
 @require $Type.kindof != OPTIONAL : "Expected a non-optional type"
*>
macro talloc($Type) @nodiscard
{
	return tmalloc($Type.sizeof, $Type.alignof);
}

<*
 @require $Type.kindof != OPTIONAL : "Expected a non-optional type"
*>
macro talloc_with_padding($Type, usz padding) @nodiscard
{
	return tmalloc($Type.sizeof + padding, $Type.alignof);
}

<*
 @require $Type.kindof != OPTIONAL : "Expected a non-optional type"
 @require $Type.alignof <= DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'new_array_aligned' instead"
*>
macro new_array($Type, usz elements) @nodiscard
{
	return allocator::new_array(mem, $Type, elements);
}

<*
 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.

 @require $Type.kindof != OPTIONAL : "Expected a non-optional type"
*>
macro new_array_aligned($Type, usz elements) @nodiscard
{
	return allocator::new_array_aligned(mem, $Type, elements);
}

<*
 @require $Type.kindof != OPTIONAL : "Expected a non-optional type"
 @require $Type.alignof <= DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_array_aligned' instead"
*>
macro alloc_array($Type, usz elements) @nodiscard
{
	return allocator::alloc_array(mem, $Type, elements);
}

<*
 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.

 @require $Type.kindof != OPTIONAL : "Expected a non-optional type"
*>
macro alloc_array_aligned($Type, usz elements) @nodiscard
{
	return allocator::alloc_array_aligned(mem, $Type, elements);
}

macro talloc_array($Type, usz elements) @nodiscard
{
	return (($Type*)tmalloc($Type.sizeof * elements, $Type.alignof))[:elements];
}

macro temp_array($Type, usz elements) @nodiscard
{
	return (($Type*)tcalloc($Type.sizeof * elements, $Type.alignof))[:elements];
}

fn void* calloc(usz size) @builtin @inline @nodiscard
{
	return allocator::calloc(mem, size);
}

<*
 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.
*>
fn void* calloc_aligned(usz size, usz alignment) @builtin @inline @nodiscard
{
	return allocator::calloc_aligned(mem, size, alignment)!!;
}

fn void* tcalloc(usz size, usz alignment = 0) @builtin @inline @nodiscard
{
	if (!size) return null;
	return tmem.acquire(size, ZERO, alignment)!!;
}

fn void* realloc(void *ptr, usz new_size) @builtin @inline @nodiscard
{
	return allocator::realloc(mem, ptr, new_size);
}

fn void* realloc_aligned(void *ptr, usz new_size, usz alignment) @builtin @inline @nodiscard
{
	return allocator::realloc_aligned(mem, ptr, new_size, alignment)!!;
}

fn void free(void* ptr) @builtin @inline
{
	return allocator::free(mem, ptr);
}

fn void free_aligned(void* ptr) @builtin @inline
{
	return allocator::free_aligned(mem, ptr);
}

fn void* trealloc(void* ptr, usz size, usz alignment = mem::DEFAULT_MEM_ALIGNMENT) @builtin @inline @nodiscard
{
	if (!size) return null;
	if (!ptr) return tmalloc(size, alignment);
	return tmem.resize(ptr, size, alignment)!!;
}

<*
 Takes the address of a possibly unaligned variable or member,
 and offers safe access to that member, by constructing an UnalignedRef.

 @require $defined(&#arg) : "It must be possible to take the address of the argument."
 @return "An 'UnalignedRef' with the proper type and alignment, with a pointer to argument"
*>
macro @unaligned_addr(#arg) @builtin
{
	return (UnalignedRef{$typeof(#arg), $alignof(#arg)})&#arg;
}

module std::core::mem @if(env::NO_LIBC);

fn CInt __memcmp(void* s1, void* s2, usz n) @weak @export("memcmp")
{
	char* p1 = s1;
	char* p2 = s2;
	for (usz i = 0; i < n; i++, p1++, p2++)
	{
		char c1 = *p1;
		char c2 = *p2;
		if (c1 < c2) return -1;
		if (c1 > c2) return 1;
	}
	return 0;
}

fn void* __memset(void* str, CInt c, usz n) @weak @export("memset")
{
	char* p = str;
	char cc = (char)c;
	for (usz i = 0; i < n; i++, p++)
	{
		*p = cc;
	}
	return str;
}

fn void* __memcpy(void* dst, void* src, usz n) @weak @export("memcpy")
{
	char* d = dst;
	char* s = src;
	for (usz i = 0; i < n; i++, d++, s++)
	{
		*d = *s;
	}
	return dst;
}


module std::core::mem::volatile <Type>;

typedef Volatile @structlike = Type;

macro Type Volatile.get(&self)
{
	return @volatile_load(*(Type*)self);
}

macro Type Volatile.set(&self, Type val)
{
	return @volatile_store(*(Type*)self, val);
}

<*
 @require mem::@constant_is_power_of_2(ALIGNMENT) : "The alignment must be a power of 2"
*>
module std::core::mem::alignment <Type, ALIGNMENT>;
import std::core::mem @public;

<*
 An UnalignedRef offers correctly aligned access to addresses that may be unaligned or overaligned.
*>
typedef UnalignedRef = Type*;

macro Type UnalignedRef.get(self)
{
	return @unaligned_load(*(Type*)self, ALIGNMENT, false);
}

macro Type UnalignedRef.set(&self, Type val)
{
	return @unaligned_store(*(Type*)self, val, ALIGNMENT, false);
}
