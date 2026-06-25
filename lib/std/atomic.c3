// Copyright (c) 2023-2025 Eduardo José Gómez Hernández. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::atomic::types;

<*
 A wrapper providing atomic load, store and read-modify-write operations over a
 single value of `Type`. All operations default to SEQ_CONSISTENT ordering.
*>
struct Atomic <Type>
{
	Type data;
}

<*
 Loads data atomically, by default this uses SEQ_CONSISTENT ordering.

 @param $ordering : "The ordering, cannot be release or acquire-release."
 @require $ordering != RELEASE && $ordering != ACQUIRE_RELEASE : "Release and acquire-release are not valid for load"
*>
macro Type Atomic.load(&self, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	return $$atomic_load(&self.data, false, $ordering.ordinal, 0);
}
<*
 Stores data atomically, by default this uses SEQ_CONSISTENT ordering.

 @param $ordering : "The ordering, cannot be acquire or acquire-release."
 @require $ordering != ACQUIRE && $ordering != ACQUIRE_RELEASE : "Acquire and acquire-release are not valid for store"
*>
macro void Atomic.store(&self, Type value, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	$$atomic_store(&self.data, value, false, $ordering.ordinal, 0);
}

<*
 Atomically compare the value with `*expected`: if equal, replace it with `desired`
 and return true. Otherwise write the current value into `*expected` and return false.

 @param [&inout] expected : "On entry the value to compare against; on a failed exchange it receives the current value."
 @param desired : "The value to store if the comparison succeeds."
 @param $success : "Memory ordering applied when the exchange succeeds, defaults to SEQ_CONSISTENT."
 @param $failure : "Memory ordering applied when the exchange fails, defaults to SEQ_CONSISTENT."
 @return "True if the value matched `*expected` and was replaced, false otherwise."

 @require $success != NOT_ATOMIC && $success != UNORDERED : "Invalid success ordering."
 @require $failure != RELEASE && $failure != ACQUIRE_RELEASE : "Invalid failure ordering."
*>
macro bool Atomic.compare_exchange(&self, Type* expected, Type desired, AtomicOrdering $success = SEQ_CONSISTENT, AtomicOrdering $failure = SEQ_CONSISTENT)
{
    Type witnessed = mem::compare_exchange(&self.data, *expected, desired, $success, $failure);
    if (witnessed == *expected) return true;
    *expected = witnessed;   // CAS failed: hand back what was actually there
    return false;
}

<*
 Atomically add `value` to the stored value.

 @param value : "The value to add."
 @param $ordering : "Memory ordering, defaults to SEQ_CONSISTENT."
 @return "The previous value, before the addition."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Invalid ordering for a read-modify-write."
*>
macro Type Atomic.add(&self, Type value, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	return atomic::fetch_add(&self.data, value, $ordering);
}

<*
 Atomically subtract `value` from the stored value.

 @param value : "The value to subtract."
 @param $ordering : "Memory ordering, defaults to SEQ_CONSISTENT."
 @return "The previous value, before the subtraction."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Invalid ordering for a read-modify-write."
*>
macro Type Atomic.sub(&self, Type value, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	return atomic::fetch_sub(&self.data, value, $ordering);
}

<*
 Atomically multiply the stored value by `value`, using a compare-exchange loop.

 @param value : "The value to multiply by."
 @param $ordering : "Memory ordering, defaults to SEQ_CONSISTENT."
 @return "The previous value, before the multiplication."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Invalid ordering for a read-modify-write."
*>
macro Type Atomic.mul(&self, Type value, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	return atomic::fetch_mul(&self.data, value, $ordering);
}

<*
 Atomically divide the stored value by `value`, using a compare-exchange loop.

 @param value : "The value to divide by."
 @param $ordering : "Memory ordering, defaults to SEQ_CONSISTENT."
 @return "The previous value, before the division."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Invalid ordering for a read-modify-write."
*>
macro Type Atomic.div(&self, Type value, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	return atomic::fetch_div(&self.data, value, $ordering);
}

<*
 Atomically replace the stored value with the maximum of it and `value`.

 @param value : "The value to compare against."
 @param $ordering : "Memory ordering, defaults to SEQ_CONSISTENT."
 @return "The previous value, before the operation."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Invalid ordering for a read-modify-write."
*>
macro Type Atomic.max(&self, Type value, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	return atomic::fetch_max(&self.data, value, $ordering);
}

<*
 Atomically replace the stored value with the minimum of it and `value`.

 @param value : "The value to compare against."
 @param $ordering : "Memory ordering, defaults to SEQ_CONSISTENT."
 @return "The previous value, before the operation."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Invalid ordering for a read-modify-write."
*>
macro Type Atomic.min(&self, Type value, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	return atomic::fetch_min(&self.data, value, $ordering);
}

<*
 Atomically bitwise-or the stored value with `value`. Not available for floats.

 @param value : "The value to or with."
 @param $ordering : "Memory ordering, defaults to SEQ_CONSISTENT."
 @return "The previous value, before the operation."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Invalid ordering for a read-modify-write."
*>
macro Type Atomic.or(&self, Type value, AtomicOrdering $ordering = SEQ_CONSISTENT) @if(types::flat_kind(Type) != FLOAT)
{
	return atomic::fetch_or(&self.data, value, $ordering);
}

<*
 Atomically bitwise-xor the stored value with `value`. Not available for floats.

 @param value : "The value to xor with."
 @param $ordering : "Memory ordering, defaults to SEQ_CONSISTENT."
 @return "The previous value, before the operation."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Invalid ordering for a read-modify-write."
*>
macro Type Atomic.xor(&self, Type value, AtomicOrdering $ordering = SEQ_CONSISTENT) @if(types::flat_kind(Type) != FLOAT)
{
	return atomic::fetch_xor(&self.data, value, $ordering);
}

<*
 Atomically bitwise-and the stored value with `value`. Not available for floats.

 @param value : "The value to and with."
 @param $ordering : "Memory ordering, defaults to SEQ_CONSISTENT."
 @return "The previous value, before the operation."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Invalid ordering for a read-modify-write."
*>
macro Type Atomic.and(&self, Type value, AtomicOrdering $ordering = SEQ_CONSISTENT) @if(types::flat_kind(Type) != FLOAT)
{
	return atomic::fetch_and(&self.data, value, $ordering);
}

<*
 Atomically shift the stored value right by `amount`, using a compare-exchange loop.
 Not available for floats.

 @param amount : "The number of bits to shift by."
 @param $ordering : "Memory ordering, defaults to SEQ_CONSISTENT."
 @return "The previous value, before the shift."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Invalid ordering for a read-modify-write."
*>
macro Type Atomic.shr(&self, Type amount, AtomicOrdering $ordering = SEQ_CONSISTENT) @if(types::flat_kind(Type) != FLOAT)
{
	return atomic::fetch_shift_right(&self.data, amount, $ordering);
}

<*
 Atomically shift the stored value left by `amount`, using a compare-exchange loop.
 Not available for floats.

 @param amount : "The number of bits to shift by."
 @param $ordering : "Memory ordering, defaults to SEQ_CONSISTENT."
 @return "The previous value, before the shift."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Invalid ordering for a read-modify-write."
*>
macro Type Atomic.shl(&self, Type amount, AtomicOrdering $ordering = SEQ_CONSISTENT) @if(types::flat_kind(Type) != FLOAT)
{
	return atomic::fetch_shift_left(&self.data, amount, $ordering);
}

<*
 Atomically set the boolean flag to true. Only available for bool.

 @param $ordering : "Memory ordering, defaults to SEQ_CONSISTENT."
 @return "The previous value of the flag."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Invalid ordering for a read-modify-write."
*>
macro Type Atomic.set(&self, AtomicOrdering $ordering = SEQ_CONSISTENT) @if(types::flat_kind(Type) == BOOL)
{
	return atomic::flag_set(&self.data, $ordering);
}

<*
 Atomically clear the boolean flag to false. Only available for bool.

 @param $ordering : "Memory ordering, defaults to SEQ_CONSISTENT."
 @return "The previous value of the flag."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Invalid ordering for a read-modify-write."
*>
macro Type Atomic.clear(&self, AtomicOrdering $ordering = SEQ_CONSISTENT) @if(types::flat_kind(Type) == BOOL)
{
	return atomic::flag_clear(&self.data, $ordering);
}

module std::atomic;
import std::math;

macro bool @is_native_atomic_value(#value) @private
{
	return is_native_atomic_type($Typeof(#value));
}

<*
 Check whether a type can be operated on with native atomic instructions, that is,
 it is an int, pointer, function pointer, float or bool no larger than a pointer.

 @return "True if `$Type` supports native atomic operations."
*>
macro bool is_native_atomic_type($Type)
{
	$if $Type::size > void*::size:
		return false;
	$else
		$switch $Type::kind:
			$case SIGNED_INT:
			$case UNSIGNED_INT:
			$case POINTER:
			$case FUNC:
			$case FLOAT:
			$case BOOL:
				return true;
			$case TYPEDEF:
			$case CONSTDEF:
				return is_native_atomic_type($Type::inner);
			$default:
				return false;
		$endswitch
	$endif
}

<*
 @param [&inout] ptr : "the variable or dereferenced pointer to the data."
 @param [in] y : "the value to be added to ptr."
 @param $ordering : "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 @return "returns the old value of ptr"

 @require !$alignment || math::is_power_of_2($alignment) : "Alignment must be a power of two."
 @require $defined(*ptr) : "Expected a pointer"
 @require @is_native_atomic_value(*ptr) : "Only types that are native atomic may be used."
 @require $defined(*ptr + y) : "+ must be defined between the values."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Acquire ordering is not valid."
*>
macro fetch_add(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT, bool $volatile = false, sz $alignment = 0)
{
	$if $alignment == 0:
		$alignment = $Typeof(*ptr)::size;
	$endif
	return $$atomic_fetch_add(ptr, y, $volatile, $ordering.ordinal, $alignment);
}

<*
 @param [&inout] ptr : "the variable or dereferenced pointer to the data."
 @param [in] y : "the value to be subtracted from ptr."
 @param $ordering : "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 @return "returns the old value of ptr"

 @require !$alignment || math::is_power_of_2($alignment) : "Alignment must be a power of two."
 @require $defined(*ptr) : "Expected a pointer"
 @require @is_native_atomic_value(*ptr) : "Only types that are native atomic may be used."
 @require $defined(*ptr - y) : "- must be defined between the values."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Acquire ordering is not valid."
*>
macro fetch_sub(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT, bool $volatile = false, sz $alignment = 0)
{
	$if $alignment == 0:
		$alignment = $Typeof(*ptr)::size;
	$endif
	return $$atomic_fetch_sub(ptr, y, $volatile, $ordering.ordinal, $alignment);
}

<*
 @param [&inout] ptr : "the variable or dereferenced pointer to the data."
 @param [in] y : "the value to be multiplied with ptr."
 @param $ordering : "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 @return "returns the old value of ptr"

 @require $defined(*ptr) : "Expected a pointer"
 @require @is_native_atomic_value(*ptr) : "Only types that are native atomic may be used."
 @require $defined(*ptr * y) : "* must be defined between the values."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Acquire ordering is not valid."
*>
macro fetch_mul(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $load_ordering = $ordering;
	$if $ordering == RELEASE || $ordering == ACQUIRE_RELEASE:
	    $load_ordering = SEQ_CONSISTENT;
	$endif
	
	var $StorageType = types::lower_to_atomic_compatible_type($Typeof(*ptr));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$Typeof(*ptr) old_value;
	$Typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;

	do
	{
		storage_old_value = $$atomic_load(storage_ptr, false, $load_ordering.ordinal, 0);
		old_value = bitcast(storage_old_value, $Typeof(*ptr));
		new_value = old_value * y;
		storage_new_value = bitcast(new_value, $StorageType);
	}
	while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $load_ordering) != storage_old_value);

	return old_value;
}

<*
 @param [&inout] ptr : "the variable or dereferenced pointer to the data."
 @param [in] y : "the value to divide ptr by."
 @param $ordering : "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 @return "returns the old value of ptr"

 @require $defined(*ptr) : "Expected a pointer"
 @require @is_native_atomic_value(*ptr) : "Only types that are native atomic may be used."
 @require $defined(*ptr / y) : "/ must be defined between the values."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Acquire ordering is not valid."
*>
macro fetch_div(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $load_ordering = $ordering;
	$if $ordering == RELEASE || $ordering == ACQUIRE_RELEASE:
	    $load_ordering = SEQ_CONSISTENT;
	$endif
	
	var $StorageType = types::lower_to_atomic_compatible_type($Typeof(*ptr));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$Typeof(*ptr) old_value;
	$Typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;
	
	do
	{
		storage_old_value = $$atomic_load(storage_ptr, false, $load_ordering.ordinal, 0);
		old_value = bitcast(storage_old_value, $Typeof(*ptr));
		new_value = old_value / y;
		storage_new_value = bitcast(new_value, $StorageType);
	}
	while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $load_ordering) != storage_old_value);

	return old_value;
}

<*
 @param [&inout] ptr : "the variable or dereferenced pointer to the data."
 @param [in] y : "the value to perform a bitwise or with."
 @param $ordering : "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 @return "returns the old value of ptr"

 @require !$alignment || math::is_power_of_2($alignment) : "Alignment must be a power of two."
 @require $defined(*ptr) : "Expected a pointer"
 @require @is_native_atomic_value(*ptr) : "Only types that are native atomic may be used."
 @require $defined(*ptr | y) : "| must be defined between the values."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Acquire ordering is not valid."
*>
macro fetch_or(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT, bool $volatile = false, sz $alignment = 0)
{
	return $$atomic_fetch_or(ptr, y, $volatile, $ordering.ordinal, $alignment);
}

<*
 @param [&inout] ptr : "the variable or dereferenced pointer to the data."
 @param [in] y : "the value to perform a bitwise xor with."
 @param $ordering : "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 @return "returns the old value of ptr"

 @require !$alignment || math::is_power_of_2($alignment) : "Alignment must be a power of two."
 @require $defined(*ptr) : "Expected a pointer"
 @require @is_native_atomic_value(*ptr) : "Only types that are native atomic may be used."
 @require $defined(*ptr ^ y) : "^ must be defined between the values."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Acquire ordering is not valid."
*>
macro fetch_xor(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT, bool $volatile = false, sz $alignment = 0)
{
	return $$atomic_fetch_xor(ptr, y, $volatile, $ordering.ordinal, $alignment);
}

<*
 @param [&inout] ptr : "the variable or dereferenced pointer to the data."
 @param [in] y : "the value to perform a bitwise and with."
 @param $ordering : "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 @return "returns the old value of ptr"

 @require !$alignment || math::is_power_of_2($alignment) : "Alignment must be a power of two."
 @require $defined(*ptr) : "Expected a pointer"
 @require @is_native_atomic_value(*ptr) : "Only types that are native atomic may be used."
 @require $defined(*ptr ^ y) : "& must be defined between the values."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Acquire ordering is not valid."
*>
macro fetch_and(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT, bool $volatile = false, sz $alignment = 0)
{
	return $$atomic_fetch_and(ptr, y, $volatile, $ordering.ordinal, $alignment);
}

<*
 @param [&inout] ptr : "the variable or dereferenced pointer to the data."
 @param [in] y : "the value to shift ptr by."
 @param $ordering : "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 @return "returns the old value of ptr"

 @require $defined(*ptr) : "Expected a pointer"
 @require @is_native_atomic_value(*ptr) : "Only types that are native atomic may be used."
 @require types::is_int($Typeof(*ptr)) : "Only integer pointers may be used."
 @require types::is_int($Typeof(y)) : "The value for shift right must be an integer"
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Acquire ordering is not valid."
*>
macro fetch_shift_right(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $load_ordering = $ordering;
	$if $ordering == RELEASE || $ordering == ACQUIRE_RELEASE:
	    $load_ordering = SEQ_CONSISTENT;
	$endif
	
	var $StorageType = types::lower_to_atomic_compatible_type($Typeof(*ptr));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$Typeof(*ptr) old_value;
	$Typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;
	$StorageType storage_y = ($StorageType)y;
	
	do
	{
		storage_old_value = $$atomic_load(storage_ptr, false, $load_ordering.ordinal, 0);
		old_value = bitcast(storage_old_value, $Typeof(*ptr));
		new_value = storage_old_value >> storage_y;
		storage_new_value = bitcast(new_value, $StorageType);
	}
	while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $load_ordering) != storage_old_value);

	return old_value;
}

<*
 @param [&inout] ptr : "the variable or dereferenced pointer to the data."
 @param [in] y : "the value to shift ptr by."
 @param $ordering : "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 @return "returns the old value of ptr"

 @require $defined(*ptr) : "Expected a pointer"
 @require @is_native_atomic_value(*ptr) : "Only types that are native atomic may be used."
 @require types::is_int($Typeof(*ptr)) : "Only integer pointers may be used."
 @require types::is_int($Typeof(y)) : "The value for shift left must be an integer"
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Acquire ordering is not valid."
*>
macro fetch_shift_left(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $load_ordering = $ordering;
	$if $ordering == RELEASE || $ordering == ACQUIRE_RELEASE:
	    $load_ordering = SEQ_CONSISTENT;
	$endif
	
	var $StorageType = types::lower_to_atomic_compatible_type($Typeof(*ptr));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$Typeof(*ptr) old_value;
	$Typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;
	$StorageType storage_y = ($StorageType)y;
	
	do
	{
		storage_old_value = $$atomic_load(storage_ptr, false, $load_ordering.ordinal, 0);
		old_value = bitcast(storage_old_value, $Typeof(*ptr));
		new_value = storage_old_value << storage_y;
		storage_new_value = bitcast(new_value, $StorageType);
	}
	while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $load_ordering) != storage_old_value);

	return old_value;
}

<*
 @param [&inout] ptr : "the variable or dereferenced pointer to the data."
 @param $ordering : "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 @return "returns the old value of ptr"

 @require $defined(*ptr) : "Expected a pointer"
 @require @is_native_atomic_value(*ptr) : "Only types that are native atomic may be used."
 @require types::flat_kind($Typeof(*ptr)) == BOOL : "Only bool pointers may be used."

 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Acquire ordering is not valid."
*>
macro flag_set(ptr, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	$Typeof(*ptr) old_value;
	$Typeof(*ptr) new_value = true;
	var $load_ordering = $ordering;
	$if $ordering == RELEASE || $ordering == ACQUIRE_RELEASE:
	    $load_ordering = SEQ_CONSISTENT;
	$endif
	do
	{
		old_value = $$atomic_load(ptr, false, $load_ordering.ordinal, 0);
	}
	while (mem::compare_exchange(ptr, old_value, new_value, $ordering, $load_ordering) != old_value);

	return old_value;
}

<*
 @param [&inout] ptr : "the variable or dereferenced pointer to the data."
 @param $ordering : "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 @return "returns the old value of ptr"

 @require $defined(*ptr) : "Expected a pointer"
 @require @is_native_atomic_value(*ptr) : "Only types that are native atomic may be used."
 @require types::flat_kind($Typeof(*ptr)) == BOOL : "Only bool pointers may be used."
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Acquire ordering is not valid."
*>
macro flag_clear(ptr, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	$Typeof(*ptr) old_value;
	$Typeof(*ptr) new_value = false;
	var $load_ordering = $ordering;
	$if $ordering == RELEASE || $ordering == ACQUIRE_RELEASE:
	    $load_ordering = SEQ_CONSISTENT;
	$endif
	do
	{
		old_value = $$atomic_load(ptr, false, $load_ordering.ordinal, 0);
	}
	while (mem::compare_exchange(ptr, old_value, new_value, $ordering, $load_ordering) != old_value);

	return old_value;
}

<*
 @param [&in] ptr : "the variable or dereferenced pointer to the data."
 @param [in] y : "the value to be compared to ptr."
 @param $ordering : "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 @return "returns the old value of ptr"

 @require $defined(*ptr) : "Expected a pointer"
 @require @is_native_atomic_value(*ptr) : "Only types that are native atomic may be used."
 @require $defined(*ptr > y) : "Only values that are comparable with > may be used"
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Acquire ordering is not valid."
*>
macro fetch_max(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT, bool $volatile = false, sz $alignment = 0)
{
	$if $alignment == 0:
		$alignment = $Typeof(*ptr)::size;
	$endif
	return $$atomic_fetch_max(ptr, y, $volatile, $ordering.ordinal, $alignment);
}

<*
 @param [&in] ptr : "the variable or dereferenced pointer to the data."
 @param [in] y : "the value to be compared to ptr."
 @param $ordering : "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 @return "returns the old value of ptr"

 @require $defined(*ptr) : "Expected a pointer"
 @require @is_native_atomic_value(*ptr) : "Only types that are native atomic may be used."
 @require $defined(*ptr > y) : "Only values that are comparable with > may be used"
 @require $ordering != NOT_ATOMIC && $ordering != UNORDERED : "Acquire ordering is not valid."
*>
macro fetch_min(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT, bool $volatile = false, sz $alignment = 0)
{
	$if $alignment == 0:
		$alignment = $Typeof(*ptr)::size;
	$endif
	return $$atomic_fetch_min(ptr, y, $volatile, $ordering.ordinal, $alignment);
}
