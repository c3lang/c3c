// Copyright (c) 2021-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::mem;
import std::core::mem::allocators @public;
import std::os::posix, std::os::win32, std::math, std::io;


<* The largest alignment any allocation in this module may request. *>
const MAX_MEMORY_ALIGNMENT = 0x1000_0000;

<* The alignment used by allocations that don't request a specific one. *>
const DEFAULT_MEM_ALIGNMENT = env::EMSCRIPTEN ? 8 : (env::WASM ? 16 : (void*::alignment) * 2);

const long KB = 1024;
const long MB = KB * 1024;
const long GB = MB * 1024;
const long TB = GB * 1024;

<* Faults an allocation macro/function may return: out of memory, or a request for an invalid (negative) size. *>
excuse { OUT_OF_MEMORY, INVALID_ALLOC_SIZE }

macro bool @constant_is_power_of_2($x) @const @private
{
	return $x != 0 && ($x & ($x - 1)) == 0;
}

macro bool @is_valid_alignment($alignment) @const
{
	return @constant_is_power_of_2($alignment) && $alignment <= MAX_MEMORY_ALIGNMENT;
}

macro bool is_valid_alignment(alignment)
{
	return math::is_power_of_2(alignment) && alignment < MAX_MEMORY_ALIGNMENT;
}

<*
 @return "The os page size."
*>
fn sz os_pagesize()
{
	$switch:
		$case env::POSIX:
			static sz pagesize;
			if (pagesize) return pagesize;
			return pagesize = posix::getpagesize();
		$case env::WIN32:
			static sz pagesize;
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

 @param [&in] ptr : "The pointer address to load from."
 @param mask : "The mask for the load"
 @param passthru : "The value to use for non masked values"
 @require [passthru] $defined($Typeof(*ptr) x = passthru) : "Pointer and passthru must match"
 @require [passthru] @kindof(passthru) == VECTOR : "Expected passthru to be a vector"
 @require [passthru] passthru.len == mask.len : "Mask and passthru must have the same length"

 @return "A vector with the loaded values where the mask is true, passthru where the mask is false"
*>
macro masked_load(ptr, bool[<*>] mask, passthru)
{
	return $$masked_load(ptr, mask, passthru, 0);
}

<*
 Load a vector from memory according to a mask.

 @param [&in] ptr : "The pointer address to load from."
 @param mask : "The mask for the load"
 @param passthru : "The value to use for non masked values"
 @param $alignment : "The alignment to assume for the pointer"

 @return "A vector with the loaded values where the mask is true, passthru where the mask is false"

 @require [passthru] $defined($Typeof(*ptr) x = passthru) : "Pointer and passthru must match"
 @require [passthru] @kindof(passthru) == VECTOR : "Expected passthru to be a vector"
 @require [passthru] passthru.len == mask.len : "Mask and passthru must have the same length"
 @require [$alignment] @is_valid_alignment($alignment) : "The alignment must be a valid power of two"
 @ensure $Typeof(return) == $Typeof(*ptr)
*>
macro @masked_load_aligned(ptr, bool[<*>] mask, passthru, sz $alignment)
{
	return $$masked_load(ptr, mask, passthru, $alignment);
}

<*
 Load values from a pointer vector, assuming default alignment.

 @param ptrvec : "The vector of pointers to load from."
 @param mask : "The mask for the load"
 @param passthru : "The value to use for non masked values"

 @return "A vector with the loaded values where the mask is true, passthru where the mask is false"

 @require [ptrvec] @kindof(ptrvec) == VECTOR : "Expected ptrvec to be a vector"
 @require [passthru] @kindof(passthru) == VECTOR : "Expected passthru to be a vector"
 @require [passthru] $defined(*ptrvec[0] = passthru[0]) : "Pointer and passthru must match"
 @require [passthru] passthru.len == mask.len : "Mask and passthru must have the same length"
 @require [mask] mask.len == ptrvec.len : "Mask and ptrvec must have the same length"
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

 @return "A vector with the loaded values where the mask is true, passthru where the mask is false"

 @require [ptrvec] @kindof(ptrvec) == VECTOR : "Expected ptrvec to be a vector"
 @require [passthru] @kindof(passthru) == VECTOR : "Expected passthru to be a vector"
 @require [passthru] $defined(*ptrvec[0] = passthru[0]) : "Pointer and passthru must match"
 @require [passthru] passthru.len == mask.len : "Mask and passthru must have the same length"
 @require [mask] mask.len == ptrvec.len : "Mask and ptrvec must have the same length"
 @require [$alignment] @is_valid_alignment($alignment) : "The alignment must be a valid power of two"
*>
macro @gather_aligned(ptrvec, bool[<*>] mask, passthru, sz $alignment)
{
	return $$gather(ptrvec, mask, passthru, $alignment);
}


<*
 Store parts of a vector according to the mask, assuming default alignment.

 @param [&out] ptr : "The pointer address to store to."
 @param value : "The value to store masked"
 @param mask : "The mask for the store"

 @require [value] $defined(*ptr = value) : "Pointer and value must match"
 @require [value] @kindof(value) == VECTOR : "Expected value to be a vector"
 @require [value] value.len == mask.len : "Mask and value must have the same length"
*>
macro masked_store(ptr, value, bool[<*>] mask)
{
	return $$masked_store(ptr, value, mask, 0);
}

<*
 @param [&out] ptr : "The pointer address to store to."
 @param value : "The value to store masked"
 @param mask : "The mask for the store"
 @param $alignment : "The alignment of the pointer"

 @require [value] $defined(*ptr = value) : "Pointer and value must match"
 @require [value] @kindof(value) == VECTOR : "Expected value to be a vector"
 @require [value] value.len == mask.len : "Mask and value must have the same length"
 @require [$alignment] @is_valid_alignment($alignment) : "The alignment must be a valid power of two"
*>
macro @masked_store_aligned(ptr, value, bool[<*>] mask, sz $alignment)
{
	return $$masked_store(ptr, value, mask, $alignment);
}

<*
 @param ptrvec : "The vector pointer containing the addresses to store to."
 @param value : "The value to store masked"
 @param mask : "The mask for the store"

 @require [ptrvec] @kindof(ptrvec) == VECTOR : "Expected ptrvec to be a vector"
 @require [value] @kindof(value) == VECTOR : "Expected value to be a vector"
 @require [value] $defined(*ptrvec[0] = value[0]) : "Pointer and value must match"
 @require [value] value.len == mask.len : "Mask and value must have the same length"
 @require [mask] mask.len == ptrvec.len : "Mask and ptrvec must have the same length"

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

 @require [ptrvec] @kindof(ptrvec) == VECTOR : "Expected ptrvec to be a vector"
 @require [value] @kindof(value) == VECTOR : "Expected value to be a vector"
 @require [value] $defined(*ptrvec[0] = value[0]) : "Pointer and value must match"
 @require [value] value.len == mask.len : "Mask and value must have the same length"
 @require [mask] mask.len == ptrvec.len : "Mask and ptrvec must have the same length"
 @require [$alignment] @is_valid_alignment($alignment) : "The alignment must be a valid power of two"
*>
macro @scatter_aligned(ptrvec, value, bool[<*>] mask, sz $alignment)
{
	return $$scatter(ptrvec, value, mask, $alignment);
}



<*
 @param #x : "The variable or dereferenced pointer to load."
 @return "The value of the variable"

 @require [#x] $defined(&#x) : "This must be a variable or dereferenced pointer"
*>
macro @volatile_load(#x) @builtin
{
	return $$volatile_load(&#x);
}

<*
 @param #x : "The variable or dereferenced pointer to store to."
 @param value : "The value to store."
 @return "The value stored"

 @require [#x] $defined(&#x) : "This must be a variable or dereferenced pointer"
 @require $defined(#x = value) : "The value doesn't match the variable"
*>
macro @volatile_store(#x, value) @builtin
{
	return $$volatile_store(&#x, ($Typeof(#x))value);
}

<*
 @param [&in] ptr : "The pointer to load from"
 @param $align : "The alignment to assume for the load"
 @param $volatile : "Whether the load is volatile or not, defaults to false"
 @return "The value of the variable"

 @require [ptr] $defined(*ptr) : "This must be a typed pointer"
 @require [$align] @is_valid_alignment($align) : "The alignment must be a valid power of two"
*>
macro load(ptr, sz $align, bool $volatile = false)
{
	return $$unaligned_load(ptr, $align, $volatile);
}

<*
 @param [&out] ptr : "The pointer to store to."
 @param value : "The value to store."
 @param $align : "The alignment to assume for the store"
 @param $volatile : "Whether the store is volatile, defaults to false"
 @return "The value stored"

 @require [ptr] $defined(*ptr) : "This must be a typed pointer"
 @require [value] $defined(*ptr = value) : "The value doesn't match the variable"
 @require [$align] @is_valid_alignment($align) : "The alignment must be a valid power of two"
*>
macro store(ptr, value, sz $align, bool $volatile = false)
{
	return $$unaligned_store(ptr, ($Typeof(*ptr))value, $align, $volatile);
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

 @require [#x] $defined(&#x) : "This must be a variable or dereferenced pointer"
 @require [$ordering] $ordering != AtomicOrdering.RELEASE : "Release ordering is not valid for load."
 @require [$ordering] $ordering != AtomicOrdering.ACQUIRE_RELEASE : "Acquire release is not valid for load."
 @require [#x] types::may_load_atomic($Typeof(#x)) : "Only integer, float and pointers may be used."
*>
macro @atomic_load(#x, AtomicOrdering $ordering = SEQ_CONSISTENT, $volatile = false) @builtin
{
	return $$atomic_load(&#x, $volatile, $ordering.ordinal, $reflect(#x).alignment);
}

<*
 @param #x : "the variable or dereferenced pointer to store to."
 @param value : "the value to store."
 @param $ordering : "the atomic ordering of the store, defaults to SEQ_CONSISTENT"
 @param $volatile : "whether the store should be volatile, defaults to 'false'"
 @param $alignment : "the alignment to assume, 0 infers it from `#x`"

 @require [$alignment] !$alignment || @is_valid_alignment($alignment) : "The alignment must be 0 or a power of two"
 @require [$ordering] $ordering != AtomicOrdering.ACQUIRE : "Acquire ordering is not valid for store."
 @require [$ordering] $ordering != AtomicOrdering.ACQUIRE_RELEASE : "Acquire release is not valid for store."
 @require [#x] types::may_load_atomic($Typeof(#x)) : "Only integer, float and pointers may be used."
 @require [#x] $defined(&#x) : "This must be a variable or dereferenced pointer"
 @require [value] $defined(#x = value) : "The value doesn't match the variable"
*>
macro void @atomic_store(#x, value, AtomicOrdering $ordering = SEQ_CONSISTENT, $volatile = false, $alignment = 0) @builtin
{
	$$atomic_store(&#x, value, $volatile, $ordering.ordinal, $alignment ?: $reflect(#x).alignment);
}

<*
 Atomically compare `*ptr` to `compare`, and if equal, replace it with `value`.

 @param [&inout] ptr : "The pointer to the value to compare and (maybe) exchange"
 @param compare : "The value to compare `*ptr` against"
 @param value : "The value to write to `*ptr` if the comparison succeeds"
 @param $success : "The ordering to use if the exchange succeeds, defaults to SEQ_CONSISTENT"
 @param $failure : "The ordering to use if the exchange fails, defaults to SEQ_CONSISTENT"
 @param $volatile : "Whether the access is volatile, defaults to true"
 @param $weak : "Whether a weak compare-exchange (may spuriously fail) is allowed, defaults to false"
 @param $alignment : "The alignment to assume for `ptr`, 0 assumes the default"
 @return "The value witnessed at `*ptr` before the exchange: equal to `compare` if it succeeded"

 @require [$success] $success != AtomicOrdering.NOT_ATOMIC && $success != AtomicOrdering.UNORDERED : "Acquire ordering is not valid."
 @require [$failure] $failure != AtomicOrdering.RELEASE && $failure != AtomicOrdering.ACQUIRE_RELEASE : "Acquire release is not valid."
 @require [$alignment] !$alignment || @is_valid_alignment($alignment) : "The alignment must be 0 or a power of two"
*>
macro compare_exchange(ptr, compare, value, AtomicOrdering $success = SEQ_CONSISTENT, AtomicOrdering $failure = SEQ_CONSISTENT, bool $volatile = true, bool $weak = false, sz $alignment = 0)
{
	return $$compare_exchange(ptr, compare, value, $volatile, $weak, $success.ordinal, $failure.ordinal, $alignment);
}

<*
 Volatile variant of `compare_exchange`.

 @param [&inout] ptr : "The pointer to the value to compare and (maybe) exchange"
 @param compare : "The value to compare `*ptr` against"
 @param value : "The value to write to `*ptr` if the comparison succeeds"
 @param $success : "The ordering to use if the exchange succeeds, defaults to SEQ_CONSISTENT"
 @param $failure : "The ordering to use if the exchange fails, defaults to SEQ_CONSISTENT"
 @return "The value witnessed at `*ptr` before the exchange: equal to `compare` if it succeeded"

 @require [$success] $success != AtomicOrdering.NOT_ATOMIC && $success != AtomicOrdering.UNORDERED : "Acquire ordering is not valid."
 @require [$failure] $failure != AtomicOrdering.RELEASE && $failure != AtomicOrdering.ACQUIRE_RELEASE : "Acquire release is not valid."
*>
macro compare_exchange_volatile(ptr, compare, value, AtomicOrdering $success = SEQ_CONSISTENT, AtomicOrdering $failure = SEQ_CONSISTENT)
{
	return compare_exchange(ptr, compare, value, $success, $failure, true);
}

<*
 Round `offset` up to the next multiple of `alignment`.

 @param offset : "The offset to round up"
 @param alignment : "The alignment to round up to, must be a power of 2"
 @return "The smallest multiple of `alignment` that is >= `offset`"

 @require [alignment] math::is_power_of_2(alignment)
*>
fn sz aligned_offset(sz offset, sz alignment)
{
	return (offset + alignment - 1) & ~(alignment - 1);
}

<*
 Round a pointer up to the next address matching `alignment`.

 @param [in] ptr : "The pointer to round up"
 @param alignment : "The alignment to round up to, must be a power of 2"
 @return "The smallest address >= `ptr` that is a multiple of `alignment`"

 @require [alignment] math::is_power_of_2(alignment)
*>
macro void* aligned_pointer(void* ptr, sz alignment)
{
	return (void*)(((uptr)ptr + (uptr)alignment - 1u) & ~((uptr)alignment - 1u));
}

<*
 Check whether a pointer satisfies a given alignment.

 @param [in] ptr : "The pointer to check"
 @param alignment : "The alignment to check against, must be a power of 2"
 @return "true if `ptr` is a multiple of `alignment`"

 @require [alignment] math::is_power_of_2(alignment)
*>
fn bool ptr_is_aligned(void* ptr, sz alignment) @inline
{
	return (uptr)ptr & ((uptr)alignment - 1u) == 0;
}

<*
 Check whether a pointer is aligned to the OS page size.

 @param [in] ptr : "The pointer to check"
 @return "true if `ptr` is a multiple of `os_pagesize()`"
*>
fn bool ptr_is_page_aligned(void* ptr) @inline
{
	return (uptr)ptr & ((uptr)os_pagesize() - 1u) == 0;
}

<*
 Zero out a slice of memory, treating the write as volatile so it can't be optimized away.

 @param [out] data : "The memory to zero"
*>
macro void zero_volatile(char[] data)
{
	$$memset(data.ptr, (char)0, data.len, true, (sz)1);
}

<*
 Zero out a region of memory.

 @param [&out] dst : "The destination to zero"
 @param len : "The number of bytes to zero"
 @param $dst_align : "the alignment of the destination if different from the default, 0 assumes the default"
 @param $is_volatile : "True if this write should be treated as volatile, i.e. it can't be optimized away."

 @require [$dst_align] !$dst_align || @is_valid_alignment($dst_align) : "The alignment must be 0 or a power of two"
*>
macro void clear(void* dst, sz len, sz $dst_align = 0, bool $is_volatile = false)
{
	$$memset(dst, (char)0, len, $is_volatile, $dst_align);
}

<*
 Zero out a region of memory of a compile-time known length. Always inlined and never calls the OS memset.

 @param [&out] dst : "The destination to zero"
 @param $len : "The number of bytes to zero"
 @param $dst_align : "the alignment of the destination if different from the default, 0 assumes the default"
 @param $is_volatile : "True if this write should be treated as volatile, i.e. it can't be optimized away."

 @require [$dst_align] !$dst_align || @is_valid_alignment($dst_align) : "The alignment must be 0 or a power of two"
*>
macro void clear_inline(void* dst, sz $len, sz $dst_align = 0, bool $is_volatile = false)
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

 @require [src] src != null || len == 0 : "Copying a null with non-zero length is invalid"
 @require len == 0 || dst + len <= src || src + len <= dst : "Ranges may not overlap"
 @require [$dst_align] !$dst_align || @is_valid_alignment($dst_align) : "The alignment must be 0 or a power of two"
 @require [$src_align] !$src_align || @is_valid_alignment($src_align) : "The alignment must be 0 or a power of two"
*>
macro void copy(void* dst, void* src, sz len, sz $dst_align = 0, sz $src_align = 0, bool $is_volatile = false)
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

 @require [src] src != null || $len == 0 : "Copying a null with non-zero length is invalid"
 @require $len == 0 || dst + $len <= src || src + $len <= dst : "Ranges may not overlap"
 @require [$dst_align] !$dst_align || @is_valid_alignment($dst_align) : "The alignment must be 0 or a power of two"
 @require [$src_align] !$src_align || @is_valid_alignment($src_align) : "The alignment must be 0 or a power of two"
*>
macro void copy_inline(void* dst, void* src, sz $len, sz $dst_align = 0, sz $src_align = 0, bool $is_volatile = false)
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

 @require [src] src != null || len == 0 : "Moving a null with non-zero length is invalid"
 @require [$dst_align] !$dst_align || @is_valid_alignment($dst_align) : "The alignment must be 0 or a power of two"
 @require [$src_align] !$src_align || @is_valid_alignment($src_align) : "The alignment must be 0 or a power of two"
*>
macro void move(void* dst, void* src, sz len, sz $dst_align = 0, sz $src_align = 0, bool $is_volatile = false)
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
 @require [$dst_align] !$dst_align || @is_valid_alignment($dst_align) : "The alignment must be 0 or a power of two"
*>
macro void set(void* dst, char val, sz len, sz $dst_align = 0, bool $is_volatile = false)
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
 @require [$dst_align] !$dst_align || @is_valid_alignment($dst_align) : "The alignment must be 0 or a power of two"
*>
macro void set_inline(void* dst, char val, sz $len, sz $dst_align = 0, bool $is_volatile = false)
{
	$$memset_inline(dst, val, $len, $is_volatile, $dst_align);
}

<*
 Test if n elements are equal in a slice, pointed to by a pointer etc.

 @require [a] values::@inner_kind(a) == TypeKind.SLICE || values::@inner_kind(a) == TypeKind.POINTER
 @require [b] values::@inner_kind(b) == TypeKind.SLICE || values::@inner_kind(b) == TypeKind.POINTER
 @require [len] values::@inner_kind(a) != TypeKind.SLICE || len == -1
 @require [len] values::@inner_kind(a) != TypeKind.POINTER || len > -1
 @require [b] $defined(a = b, b = a)
 @require [$align] !$align || @is_valid_alignment($align) : "The alignment must be 0 or a power of two"
*>
macro bool equals(a, b, sz len = -1, sz $align = 0)
{
	return fn bool($Typeof(a) x, $Typeof(b) y, sz len) {
		return equals_inline(x, y, len, $align);
	}(a, b, len);
}

<*
 Test if n elements are equal in a slice, pointed to by a pointer etc.

 @require [a] values::@inner_kind(a) == TypeKind.SLICE || values::@inner_kind(a) == TypeKind.POINTER
 @require [b] values::@inner_kind(b) == TypeKind.SLICE || values::@inner_kind(b) == TypeKind.POINTER
 @require [len] values::@inner_kind(a) != TypeKind.SLICE || len == -1
 @require [len] values::@inner_kind(a) != TypeKind.POINTER || len > -1
 @require [b] $defined(a = b, b = a)
 @require [$align] !$align || @is_valid_alignment($align) : "The alignment must be 0 or a power of two"
*>
macro bool equals_inline(a, b, sz len = -1, sz $align = 0)
{
	$if !$align:
		$align = $Typeof(a[0])::alignment;
	$endif
	void* x @noinit;
	void* y @noinit;
	$if values::@inner_kind(a) == TypeKind.SLICE:
		if (a.len != b.len) return false;
		len = a.len * @sizeof(a[0]);
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
	var $step = $Type::size;
	sz end = len / $step;
	for (sz i = 0; i < end; i++)
	{
		if ((($Type*)x)[i] != (($Type*)y)[i]) return false;
	}
	sz last = len % $step;
	for (sz i = len - last; i < len; i++)
	{
		if (((char*)x)[i] != ((char*)y)[i]) return false;
	}
	return true;
}

<*
 Check if an allocation must be aligned given the type.

 @param $Type : "The type to check"
 @return `true if the alignment of the type exceeds DEFAULT_MEM_ALIGNMENT.`
*>
macro bool type_alloc_must_be_aligned($Type)
{
	return $Type::alignment > DEFAULT_MEM_ALIGNMENT;
}

<*
 Run with a specific allocator inside of the macro body.

 @param [&inout] allocator : "The allocator to make current for the duration of the body"
*>
macro void @scoped(Allocator allocator; @body())
{
	Allocator old_allocator = allocators::thread_allocator;
	allocators::thread_allocator = allocator;
	defer allocators::thread_allocator = old_allocator;
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
		tracker.init(allocators::thread_allocator);
		Allocator old_allocator = allocators::thread_allocator;
		allocators::thread_allocator = &tracker;
		defer
		{
			allocators::thread_allocator = old_allocator;
			tracker.print_report();
			tracker.free();
		}
	$endif
	@body();
}

<*
 Assert on memory leak in the scope of the macro body.

 Typically this is disabled on --safe=no, but it can be enabled even when safety
 is off by adding the MEMORY_ASSERTS flag through "-D MEMORY_ASSERTS"

 @param $report : "Set to false to disable memory report"
*>
macro void @assert_leak($report = true; @body()) @builtin
{
	$if env::COMPILER_SAFE_MODE || $feat(MEMORY_ASSERTS):
		TrackingAllocator tracker;
		tracker.init(mem);
		Allocator old_allocator = mem;
		allocators::thread_allocator = &tracker;
		defer
		{
			allocators::thread_allocator = old_allocator;
			defer tracker.free();
			sz allocated = tracker.allocated();
			if (allocated)
			{
				io::eprintfn("Memory leak detected (%d bytes allocated).", allocated);
				$if $report:
					(void)tracker.fprint_report(io::stderr());
				$endif
				abort("Aborting due to memory leak.");
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

 @require [$size] $size > 0 : "The buffer must be at least 1 byte"
*>
macro void @stack_mem(sz $size; @body(Allocator mem)) @builtin
{
	char[$size] buffer;
	OnStackAllocator allocator;
	allocator.init(&buffer, mem);
	defer allocator.free();
	@body(&allocator);
}

<*
 Allocate [size] bytes on the stack and make an allocator backed by them current for the
 duration of the body, falling back to the heap allocator once the stack buffer is exhausted.

 Release everything on scope exit.

 @param $size : `the size of the buffer`

 @require [$size] $size > 0 : "The buffer must be at least 1 byte"
*>
macro void @stack_pool(sz $size; @body) @builtin
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
	return allocators::push_pool() @inline;
}

<*
 Pop the current temp allocator. A pop must always be balanced with a push.
*>
fn void temp_pop(PoolState old_state)
{
	allocators::pop_pool(old_state) @inline;
}

<*
 Create a new temp allocator backed by `allocator`, make it current for the body, and destroy
 it (along with any temp allocator pushed on top of it) on scope exit.

 @param [&inout] allocator : "The allocator backing the new temp allocator"
 @param pool_size : "The initial size of the temp allocator's pool"
 @param reserve_size : "Bytes reserved for out-of-order allocations, 0 gives the default"
 @param min_size : "The minimum size a temp allocator chunk may shrink to"
 @param realloc_size : "The size to grow the pool by when it needs to reallocate"

 @require [pool_size] pool_size >= 64
 @require [realloc_size] realloc_size >= 64
 @require [allocator] allocator.type != TempAllocator : "You may not create a temp allocator with a TempAllocator as the backing allocator."
 @require [min_size] min_size > TempAllocator::size + 64 : "Min size must meaningfully hold the data + some bytes"
*>
macro void @pool_init(Allocator allocator, sz pool_size,
	sz reserve_size = allocators::temp_allocator_reserve_size,
	sz min_size = allocators::temp_allocator_min_size,
	sz realloc_size = allocators::temp_allocator_realloc_size; @body) @builtin
{
	Allocator current = allocators::current_temp;
	TempAllocator* top = allocators::top_temp;
	allocators::create_temp_allocator(allocator, pool_size, reserve_size, min_size, realloc_size);
	defer
	{
		allocators::destroy_temp_allocators();
		allocators::top_temp = top;
		allocators::current_temp = current;
	}
	@body();
}

<*
 Create a new temporary allocator.

 The `reserve` parameter allows you to determine how many bytes should be reserved for
 allocations on the current temporary allocator, if allocations are made inside of the pool scope.
 It is made available for optimization, and can usually be ignored.

 @param reserve : "The amount of bytes to reserve for out-of-order allocations, 0 gives the default."

 @require [reserve] reserve >= 0 : "The reserve may not be negative"
*>
macro void @pool(sz reserve = 0; @body) @builtin
{
	PoolState state = allocators::push_pool(reserve) @inline;
	defer
	{
		allocators::pop_pool(state) @inline;
	}
	@body();
}

module std::core::mem @feat(FREESTANDING_WASM);
import std::core::mem::allocators @public;
SimpleHeapAllocator wasm_allocator @private;
extern int __heap_base;

<*
 Set up the WASM heap allocator as the default thread and temp allocator.
*>
fn void initialize_wasm_mem() @init(1024) @private
{
	allocators::wasm_memory.allocate_block(mem::DEFAULT_MEM_ALIGNMENT)!!; // Give us a valid null.
	// Check if we need to move the heap.
	iptr start = (iptr)&__heap_base;
	if (start > mem::DEFAULT_MEM_ALIGNMENT) allocators::wasm_memory.use = start;
	wasm_allocator.init(fn (x) => allocators::wasm_memory.allocate_block(x));
	allocators::thread_allocator = &wasm_allocator;
	allocators::temp_base_allocator = &wasm_allocator;
}

module std::core::mem;
import std::math;


<*
 @return "The current file/function/line for allocation tracking, or null if TRACK_MEMORY is disabled"
*>
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
 @require [value] @alignof(value) <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'clone_aligned' instead"
*>
macro @clone(value) @builtin @nodiscard
{
	return alloc::clone(mem, value);
}

<*
 @param value : "The value to clone"
 @return "A pointer to the cloned value"
*>
macro @clone_slice(value) @builtin @nodiscard => alloc::clone_slice(mem, value);

<*
 @param value : "The value to clone"
 @return "A pointer to the cloned value, which must be released using free_aligned"
*>
macro @clone_aligned(value) @builtin @nodiscard
{
	return alloc::clone_aligned(mem, value);
}

<*
 @param value : "The value to clone"
 @return "A pointer to the cloned value"
*>
macro @tclone(value) @builtin @nodiscard
{
	$if @alignof(value) <= mem::DEFAULT_MEM_ALIGNMENT:
		return tnew($Typeof(value), value);
	$else
		return alloc::clone_aligned(tmem, value);
	$endif
}

<*
 @param value : "The value to clone"
 @return "A pointer to the cloned value"
*>
macro @tclone_slice(value) @builtin @nodiscard => alloc::clone_slice(tmem, value);

<*
 Allocate `size` bytes of uninitialized memory using the default allocator.

 @param size : "The number of bytes to allocate"
 @return "A pointer to `size` bytes of uninitialized memory"

 @require [size] size >= 0 : "Size may not be negative"
*>
fn void* malloc(sz size) @builtin @inline @nodiscard
{
	return alloc::malloc(mem, size);
}

<*
 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.

 @param size : "The number of bytes to allocate"
 @param alignment : "The required alignment, must be a power of 2"
 @return "A pointer to `size` bytes of uninitialized memory"

 @require [size] size >= 0 : "Size may not be negative"
 @require [alignment] math::is_power_of_2(alignment) : "Alignment must be a power of 2"
*>
fn void* malloc_aligned(sz size, sz alignment) @builtin @inline @nodiscard
{
	return alloc::malloc_aligned(mem, size, alignment)!!;
}

<*
 Allocate `size` bytes of uninitialized memory using the current temp allocator.

 @param size : "The number of bytes to allocate, 0 returns null"
 @param alignment : "The alignment to use, 0 assumes the default, otherwise must be a power of 2"
 @return "A pointer to `size` bytes of uninitialized memory, or null if size is 0"

 @require [size] size >= 0 : "Size may not be negative"
 @require [alignment] !alignment || is_valid_alignment(alignment) : "The alignment must be 0 or a power of two"
*>
fn void* tmalloc(sz size, sz alignment = 0) @builtin @inline @nodiscard
{
	if (!size) return null;
	return tmem.acquire(size, NO_ZERO, alignment)!!;
}

<*
 @param $Type : "The type to allocate"
 @param #init : "The optional initializer"

 @require [#init] !$defined(#init) ||| $defined($Type a = #init) : "#init must be an initializer for the type"
 @require [$Type] $Type::alignment <= DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_aligned' instead"
 @return "A pointer to data of type $Type."
*>
macro new($Type, #init = ...) @nodiscard @safemacro
{
	$if $defined(#init):
		$Type* val = malloc($Type::size);
		*val = #init;
		return val;
	$else
		return ($Type*)calloc($Type::size);
	$endif
}

<*
 @param $Type : "The type to allocate"
 @param padding : "The padding to add after the allocation"
 @param #init : "The optional initializer"
 @return "A pointer to data of type $Type."

 @require [$Type] $Type::kind != OPTIONAL : "Expected a non-optional type"
 @require [#init] !$defined(#init) ||| $defined($Type a = #init) : "#init must be an initializer for the type"
 @require [padding] !@catch($Type::size.overflow_add(padding)) : "$Type::size + padding would overflow sz::max"
*>
macro new_with_padding($Type, sz padding, #init = ...) @nodiscard @safemacro
{
	$if $defined(#init):
		$Type* val = malloc($Type::size + padding);
		*val = #init;
		return val;
	$else
		return ($Type*)calloc($Type::size + padding);
	$endif
}

<*
 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.

 @param $Type : "The type to allocate"
 @param #init : "The optional initializer"
 @return "A pointer to data of type $Type with the proper alignment"

 @require [#init] !$defined(#init) ||| $defined($Type a = #init) : "#init must be an initializer for the type"
*>
macro new_aligned($Type, #init = ...) @nodiscard @safemacro
{
	$if $defined(#init):
		$Type* val = malloc_aligned($Type::size, $Type::alignment);
		*val = #init;
		return val;
	$else
		return ($Type*)calloc_aligned($Type::size, $Type::alignment);
	$endif
}

<*
 @param $Type : "The type to allocate"

 @return "A pointer to uninitialized data for the type $Type"

 @require [$Type] $Type::kind != OPTIONAL : "Expected a non-optional type"
 @require [$Type] $Type::alignment <= DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_aligned' instead"
*>
macro alloc($Type) @nodiscard
{
	return ($Type*)malloc($Type::size);
}

<*
 @param $Type : "The type to allocate"
 @param padding : "The padding to add after the allocation"
 @return "A pointer to uninitialized data for the type $Type"

 @require [$Type] $Type::kind != OPTIONAL : "Expected a non-optional type"
 @require [$Type] $Type::alignment <= DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_aligned' instead"
 @require [padding] !@catch($Type::size.overflow_add(padding)) : "$Type::size + padding would overflow sz::max"
*>
macro alloc_with_padding($Type, sz padding) @nodiscard
{
	return ($Type*)malloc($Type::size + padding);
}

<*

 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.

 @param $Type : "The type to allocate"
 @return "A pointer to uninitialized data for the type $Type with the proper alignment"

 @require [$Type] $Type::kind != OPTIONAL : "Expected a non-optional type"
*>
macro alloc_aligned($Type) @nodiscard
{
	return ($Type*)malloc_aligned($Type::size, $Type::alignment);
}

<*
 @param $Type : "The type to allocate"
 @param #init : "The optional initializer"
 @return "A pointer to temporary data of type $Type."

 @require [$Type] $Type::kind != OPTIONAL : "Expected a non-optional type"
 @require [#init] !$defined(#init) ||| $defined($Type a = #init) : "#init must be an initializer for the type"
*>
macro tnew($Type, #init = ...) @nodiscard @safemacro
{
	$if $defined(#init):
		$Type* val = tmalloc($Type::size, $Type::alignment) @inline;
		*val = #init;
		return val;
	$else
		return ($Type*)tcalloc($Type::size, $Type::alignment) @inline;
	$endif
}

<*
 @param $Type : "The type to allocate"
 @param padding : "The padding to add after the allocation"
 @param #init : "The optional initializer"
 @return "A pointer to temporary data of type $Type with added padding at the end."

 @require [$Type] $Type::kind != OPTIONAL : "Expected a non-optional type"
 @require [#init] !$defined(#init) ||| $defined($Type a = #init) : "#init must be an initializer for the type"
 @require [padding] !@catch($Type::size.overflow_add(padding)) : "$Type::size + padding would overflow sz::max"
*>
macro temp_with_padding($Type, sz padding, #init = ...) @nodiscard @safemacro
{
	$if $defined(#init):
		$Type* val = tmalloc($Type::size + padding, $Type::alignment) @inline;
		*val = #init;
		return val;
	$else
		return ($Type*)tcalloc($Type::size + padding, $Type::alignment) @inline;
	$endif
}

<*
 Allocate uninitialized temp memory for a single value of type $Type.

 @param $Type : "The type to allocate"
 @return "A pointer to temporary, uninitialized data of type $Type"

 @require [$Type] $Type::kind != OPTIONAL : "Expected a non-optional type"
*>
macro talloc($Type) @nodiscard
{
	return tmalloc($Type::size, $Type::alignment);
}

<*
 Allocate uninitialized temp memory for a value of type $Type plus trailing padding.

 @param $Type : "The type to allocate"
 @param padding : "The padding to add after the allocation"
 @return "A pointer to temporary, uninitialized data of type $Type with added padding at the end"

 @require [$Type] $Type::kind != OPTIONAL : "Expected a non-optional type"
 @require [padding] !@catch($Type::size.overflow_add(padding)) : "$Type::size + padding would overflow sz::max"
*>
macro talloc_with_padding($Type, sz padding) @nodiscard
{
	return tmalloc($Type::size + padding, $Type::alignment);
}

<*
 Allocate a zero-initialized array of $Type using the default allocator.

 @param $Type : "The element type to allocate"
 @param elements : "The number of elements to allocate"
 @return "A zero-initialized slice of `elements` values of type $Type"

 @require [$Type] $Type::kind != OPTIONAL : "Expected a non-optional type"
 @require [$Type] $Type::alignment <= DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'new_array_aligned' instead"
 @require [elements] !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro new_array($Type, sz elements) @nodiscard
{
	return alloc::new_array(mem, $Type, elements);
}

<*
 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.

 @param $Type : "The element type to allocate"
 @param elements : "The number of elements to allocate"
 @return "A zero-initialized slice of `elements` values of type $Type, with the proper alignment"

 @require [$Type] $Type::kind != OPTIONAL : "Expected a non-optional type"
 @require [elements] !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro new_array_aligned($Type, sz elements) @nodiscard
{
	return alloc::new_array_aligned(mem, $Type, elements);
}

<*
 Allocate an uninitialized array of $Type using the default allocator.

 @param $Type : "The element type to allocate"
 @param elements : "The number of elements to allocate"
 @return "A slice of `elements` uninitialized values of type $Type"

 @require [$Type] $Type::kind != OPTIONAL : "Expected a non-optional type"
 @require [$Type] $Type::alignment <= DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_array_aligned' instead"
 @require [elements] !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro alloc_array($Type, sz elements) @nodiscard
{
	return alloc::alloc_array(mem, $Type, elements);
}

<*
 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.

 @param $Type : "The element type to allocate"
 @param elements : "The number of elements to allocate"
 @return "A slice of `elements` uninitialized values of type $Type, with the proper alignment"

 @require [$Type] $Type::kind != OPTIONAL : "Expected a non-optional type"
 @require [elements] !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro alloc_array_aligned($Type, sz elements) @nodiscard
{
	return alloc::alloc_array_aligned(mem, $Type, elements);
}

<*
 Allocate an uninitialized array of $Type using the current temp allocator.

 @param $Type : "The element type to allocate"
 @param elements : "The number of elements to allocate"
 @return "A slice of `elements` uninitialized temporary values of type $Type"

 @require [elements] !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro talloc_array($Type, sz elements) @nodiscard
{
	return (($Type*)tmalloc($Type::size * elements, $Type::alignment))[:elements];
}

<*
 Allocate a zero-initialized array of $Type using the current temp allocator.

 @param $Type : "The element type to allocate"
 @param elements : "The number of elements to allocate"
 @return "A zero-initialized slice of `elements` temporary values of type $Type"

 @require [elements] !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro temp_array($Type, sz elements) @nodiscard
{
	return (($Type*)tcalloc($Type::size * elements, $Type::alignment))[:elements];
}

<*
 Allocate `size` bytes of zero-initialized memory using the default allocator.

 @param size : "The number of bytes to allocate"
 @return "A pointer to `size` zero-initialized bytes"

 @require [size] size >= 0 : "Size may not be negative"
*>
fn void* calloc(sz size) @builtin @inline @nodiscard
{
	return alloc::calloc(mem, size);
}

<*
 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.

 @param size : "The number of bytes to allocate"
 @param alignment : "The alignment to use, must be a power of 2"
 @return "A pointer to `size` zero-initialized bytes, with the proper alignment"

 @require [size] size >= 0 : "Size may not be negative"
 @require [alignment] is_valid_alignment(alignment) : "Alignment must be a valid power of 2"
*>
fn void* calloc_aligned(sz size, sz alignment) @builtin @inline @nodiscard
{
	return alloc::calloc_aligned(mem, size, alignment)!!;
}

<*
 Allocate `size` bytes of zero-initialized memory using the current temp allocator.

 @param size : "The number of bytes to allocate, 0 returns null"
 @param alignment : "The alignment to use, 0 assumes the default, otherwise must be a power of 2"
 @return "A pointer to `size` zero-initialized bytes, or null if size is 0"

 @require [size] size >= 0 : "Size may not be negative"
 @require [alignment] !alignment || is_valid_alignment(alignment) : "Alignment must be a valid power of 2"
*>
fn void* tcalloc(sz size, sz alignment = 0) @builtin @inline @nodiscard
{
	if (!size) return null;
	return tmem.acquire(size, ZERO, alignment)!!;
}

<*
 Resize a previously allocated block from the default allocator, panicking on failure.

 @param [drop] ptr : "The pointer to resize, may be null to allocate new memory"
 @param new_size : "The new size in bytes"
 @return "A pointer to the resized memory, which may differ from `ptr`"

 @require [new_size] new_size >= 0 : "Size may not be negative"
*>
fn void* realloc(void *ptr, sz new_size) @builtin @inline @nodiscard
{
	return alloc::realloc(mem, ptr, new_size);
}

<*
 Resize a previously aligned-allocated block from the default allocator, panicking on failure.

 @param [drop] ptr : "The pointer to resize, may be null to allocate new memory"
 @param new_size : "The new size in bytes"
 @param alignment : "The alignment to preserve, must be a power of 2"
 @return "A pointer to the resized memory, which may differ from `ptr`"

 @require [new_size] new_size >= 0 : "Size may not be negative"
 @require [alignment] is_valid_alignment(alignment) : "Alignment must be a valid power of 2"
*>
fn void* realloc_aligned(void *ptr, sz new_size, sz alignment) @builtin @inline @nodiscard
{
	return alloc::realloc_aligned(mem, ptr, new_size, alignment)!!;
}

<*
 Free memory previously allocated (unaligned) from the default allocator.

 @param [drop] ptr : "The pointer to free, may be null"
*>
fn void free(void* ptr) @builtin @inline
{
	return alloc::free(mem, ptr);
}

<*
 Free memory previously allocated with an aligned allocation from the default allocator.

 @param [drop] ptr : "The pointer to free, may be null"
*>
fn void free_aligned(void* ptr) @builtin @inline
{
	return alloc::free_aligned(mem, ptr);
}

<*
 Resize a previously allocated block from the current temp allocator.

 @param [drop] ptr : "The pointer to resize, may be null to allocate new memory"
 @param size : "The new size in bytes, 0 returns null"
 @param alignment : "The alignment to preserve, must be a power of 2, defaults to the default alignment"
 @return "A pointer to the resized memory, which may differ from `ptr`, or null if size is 0"

 @require [size] size >= 0 : "Size may not be negative"
 @require [alignment] is_valid_alignment(alignment) : "Alignment must be a valid power of 2"
*>
fn void* trealloc(void* ptr, sz size, sz alignment = mem::DEFAULT_MEM_ALIGNMENT) @builtin @inline @nodiscard
{
	if (!size) return null;
	if (!ptr) return tmalloc(size, alignment);
	return tmem.resize(ptr, size, alignment)!!;
}

<*
 Takes the address of a possibly unaligned variable or member,
 and offers safe access to that member, by constructing an UnalignedRef.

 @require [#arg] $defined(&#arg) : "It must be possible to take the address of the argument."
 @return "An 'UnalignedRef' with the proper type and alignment, with a pointer to argument"
*>
macro @unaligned_addr(#arg) @builtin
{
	return (UnalignedRef{$Typeof(#arg), @alignof(#arg)})&#arg;
}

module std::core::mem @feat(NO_LIBC);

<*
 Fallback `memcmp` implementation, provided as a weak symbol when no libc is available.

 @param s1 : "The first region to compare"
 @param s2 : "The second region to compare"
 @param n : "The number of bytes to compare"
 @return "0 if equal, negative if `s1` < `s2`, positive if `s1` > `s2`, by unsigned byte value"
*>
fn CInt __memcmp(void* s1, void* s2, sz n) @weak @export("memcmp")
{
	char* p1 = s1;
	char* p2 = s2;
	for (sz i = 0; i < n; i++, p1++, p2++)
	{
		char c1 = *p1;
		char c2 = *p2;
		if (c1 < c2) return -1;
		if (c1 > c2) return 1;
	}
	return 0;
}

<*
 Fallback `memset` implementation, provided as a weak symbol when no libc is available.

 @param str : "The region to fill"
 @param c : "The byte value to fill with"
 @param n : "The number of bytes to fill"
 @return "`str`"
*>
fn void* __memset(void* str, CInt c, sz n) @weak @export("memset")
{
	char* p = str;
	char cc = (char)c;
	for (sz i = 0; i < n; i++, p++)
	{
		*p = cc;
	}
	return str;
}

<*
 Fallback `memcpy` implementation, provided as a weak symbol when no libc is available. Does not
 handle overlapping ranges.

 @param [&out] dst : "The destination to copy to"
 @param [in] src : "The source to copy from"
 @param n : "The number of bytes to copy"
 @return "`dst`"
*>
fn void* __memcpy(void* dst, void* src, sz n) @weak @export("memcpy")
{
	char* d = dst;
	char* s = src;
	for (sz i = 0; i < n; i++, d++, s++)
	{
		*d = *s;
	}
	return dst;
}


<*
 A `Volatile` wraps a value of `Type` so every access to it goes through a volatile
 load/store, preventing the compiler from optimizing accesses away.
*>
module std::core::mem::volatile <Type>;

typedef Volatile = Type;

<*
 @return "The current value, read with a volatile load"
*>
macro Type Volatile.get(&self)
{
	return @volatile_load(*(Type*)self);
}

<*
 @param val : "The value to store"
 @return "The value stored"
*>
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

<*
 @return "The value at this address, loaded assuming ALIGNMENT"
*>
macro Type UnalignedRef.get(self)
{
	return mem::load((Type*)self, ALIGNMENT);
}

<*
 @param val : "The value to store"
 @return "The value stored, written assuming ALIGNMENT"
*>
macro Type UnalignedRef.set(self, Type val)
{
	return mem::store((Type*)self, val, ALIGNMENT);
}
