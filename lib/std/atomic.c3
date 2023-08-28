// Copyright (c) 2023 Eduardo José Gómez Hernández. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::atomic::types(<Type>);

struct Atomic
{
	Type data;
}

fn Type Atomic.load(&self, AtomicOrdering ordering = SEQ_CONSISTENT)
{
	Type* data = &self.data;
	switch(ordering) 
	{
		case NOT_ATOMIC: return $$atomic_load(data, false, AtomicOrdering.NOT_ATOMIC.ordinal);
		case UNORDERED: return $$atomic_load(data, false, AtomicOrdering.UNORDERED.ordinal);
		case MONOTONIC: return $$atomic_load(data, false, AtomicOrdering.MONOTONIC.ordinal);
		case ACQUIRE: return $$atomic_load(data, false, AtomicOrdering.ACQUIRE.ordinal);
		case RELEASE: return $$atomic_load(data, false, AtomicOrdering.RELEASE.ordinal);
		case ACQUIRE_RELEASE: return $$atomic_load(data, false, AtomicOrdering.ACQUIRE_RELEASE.ordinal);

		default:
		case SEQ_CONSISTENT: return $$atomic_load(data, false, AtomicOrdering.SEQ_CONSISTENT.ordinal);
	}
}

fn void Atomic.store(&self, Type value, AtomicOrdering ordering = SEQ_CONSISTENT)
{
	Type* data = &self.data;
	switch(ordering) 
	{
		case NOT_ATOMIC: $$atomic_store(data, value, false, AtomicOrdering.NOT_ATOMIC.ordinal);
		case UNORDERED: $$atomic_store(data, value, false, AtomicOrdering.UNORDERED.ordinal);
		case MONOTONIC: $$atomic_store(data, value, false, AtomicOrdering.MONOTONIC.ordinal);
		case ACQUIRE: $$atomic_store(data, value, false, AtomicOrdering.ACQUIRE.ordinal);
		case RELEASE: $$atomic_store(data, value, false, AtomicOrdering.RELEASE.ordinal);
		case ACQUIRE_RELEASE: $$atomic_store(data, value, false, AtomicOrdering.ACQUIRE_RELEASE.ordinal);

		default:
		case SEQ_CONSISTENT: $$atomic_store(data, value, false, AtomicOrdering.SEQ_CONSISTENT.ordinal);
	}
}

fn Type Atomic.add(&self, Type value, AtomicOrdering ordering = SEQ_CONSISTENT)
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_add, data, value, ordering);
}

fn Type Atomic.sub(&self, Type value, AtomicOrdering ordering = SEQ_CONSISTENT)
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_sub, data, value, ordering);
}

fn Type Atomic.mul(&self, Type value, AtomicOrdering ordering = SEQ_CONSISTENT)
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_mul, data, value, ordering);
}

fn Type Atomic.div(&self, Type value, AtomicOrdering ordering = SEQ_CONSISTENT)
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_div, data, value, ordering);
}

fn Type Atomic.max(&self, Type value, AtomicOrdering ordering = SEQ_CONSISTENT)
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_div, data, value, ordering);
}

fn Type Atomic.min(&self, Type value, AtomicOrdering ordering = SEQ_CONSISTENT)
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_min, data, value, ordering);
}

fn Type Atomic.or(&self, ulong value, AtomicOrdering ordering = SEQ_CONSISTENT) @if(!types::is_float(Type))
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_or, data, value, ordering);
}

fn Type Atomic.xor(&self, ulong value, AtomicOrdering ordering = SEQ_CONSISTENT) @if(!types::is_float(Type))
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_xor, data, value, ordering);
}

fn Type Atomic.and(&self, ulong value, AtomicOrdering ordering = SEQ_CONSISTENT) @if(!types::is_float(Type))
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_and, data, value, ordering);
}

fn Type Atomic.shift_right(&self, ulong amount, AtomicOrdering ordering = SEQ_CONSISTENT) @if(!types::is_float(Type))
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_shift_right, data, amount, ordering);
}

fn Type Atomic.shift_left(&self, ulong amount, AtomicOrdering ordering = SEQ_CONSISTENT) @if(!types::is_float(Type))
{
	Type* data = &self.data;
	return @atomic_exec(atomic::fetch_shift_left, data, amount, ordering);
}

macro @atomic_exec(#func, data, value, ordering) @local
{
	switch(ordering) 
	{
		case NOT_ATOMIC: return #func(data, value, NOT_ATOMIC);
		case UNORDERED: return #func(data, value, UNORDERED);
		case MONOTONIC: return #func(data, value, MONOTONIC);
		case ACQUIRE: return #func(data, value, ACQUIRE);
		case RELEASE: return #func(data, value, RELEASE);
		case ACQUIRE_RELEASE: return #func(data, value, ACQUIRE_RELEASE);

		default:
		case SEQ_CONSISTENT: return #func(data, value, SEQ_CONSISTENT);
	}
}

module std::atomic;

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 **/
macro fetch_add(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $StorageType = $typefrom(types::lower_to_atomic_compatible_type($typeof(*ptr)));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;
	
	do {
		storage_old_value = $$atomic_load(storage_ptr, false, $ordering.ordinal);
		old_value = bitcast(storage_old_value, $typeof(*ptr));
		new_value = old_value + y;
		storage_new_value = bitcast(new_value, $StorageType);
	} while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $ordering) != storage_old_value);

	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 **/
macro fetch_sub(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $StorageType = $typefrom(types::lower_to_atomic_compatible_type($typeof(*ptr)));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;
	
	do {
		storage_old_value = $$atomic_load(storage_ptr, false, $ordering.ordinal);
		old_value = bitcast(storage_old_value, $typeof(*ptr));
		new_value = old_value - y;
		storage_new_value = bitcast(new_value, $StorageType);
	} while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $ordering) != storage_old_value);

	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 * @require types::is_int($typeof(y)) "The value for or must be an int"
 **/
macro fetch_or(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $StorageType = $typefrom(types::lower_to_atomic_compatible_type($typeof(*ptr)));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;
	$StorageType storage_y = ($StorageType)y;
	
	do {
		storage_old_value = $$atomic_load(storage_ptr, false, $ordering.ordinal);
		old_value = bitcast(storage_old_value, $typeof(*ptr));
		new_value = storage_old_value | storage_y;
		storage_new_value = bitcast(new_value, $StorageType);
	} while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $ordering) != storage_old_value);

	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 * @require types::is_int($typeof(y)) "The value for or must be an int"
 **/
macro fetch_xor(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $StorageType = $typefrom(types::lower_to_atomic_compatible_type($typeof(*ptr)));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;
	$StorageType storage_y = ($StorageType)y;
	
	do {
		storage_old_value = $$atomic_load(storage_ptr, false, $ordering.ordinal);
		old_value = bitcast(storage_old_value, $typeof(*ptr));
		new_value = storage_old_value ^ storage_y;
		storage_new_value = bitcast(new_value, $StorageType);
	} while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $ordering) != storage_old_value);

	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 * @require types::is_int($typeof(y)) "The value for or must be an int"
 **/
macro fetch_and(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $StorageType = $typefrom(types::lower_to_atomic_compatible_type($typeof(*ptr)));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;
	$StorageType storage_y = ($StorageType)y;
	
	do {
		storage_old_value = $$atomic_load(storage_ptr, false, $ordering.ordinal);
		old_value = bitcast(storage_old_value, $typeof(*ptr));
		new_value = storage_old_value & storage_y;
		storage_new_value = bitcast(new_value, $StorageType);
	} while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $ordering) != storage_old_value);

	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 * @require types::is_int($typeof(y)) "The value for or must be an int"
 **/
macro fetch_shift_right(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $StorageType = $typefrom(types::lower_to_atomic_compatible_type($typeof(*ptr)));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;
	$StorageType storage_y = ($StorageType)y;
	
	do {
		storage_old_value = $$atomic_load(storage_ptr, false, $ordering.ordinal);
		old_value = bitcast(storage_old_value, $typeof(*ptr));
		new_value = storage_old_value >> storage_y;
		storage_new_value = bitcast(new_value, $StorageType);
	} while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $ordering) != storage_old_value);

	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 * @require types::is_int($typeof(y)) "The value for or must be an int"
 **/
macro fetch_shift_left(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $StorageType = $typefrom(types::lower_to_atomic_compatible_type($typeof(*ptr)));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;
	$StorageType storage_y = ($StorageType)y;
	
	do {
		storage_old_value = $$atomic_load(storage_ptr, false, $ordering.ordinal);
		old_value = bitcast(storage_old_value, $typeof(*ptr));
		new_value = storage_old_value << storage_y;
		storage_new_value = bitcast(new_value, $StorageType);
	} while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $ordering) != storage_old_value);

	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require types::is_int($typeof(*ptr)) "Only integer pointers may be used."
 **/
macro flag_test_and_set(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value = true;
	
	do {
		old_value = $$atomic_load(ptr, false, $ordering.ordinal);
	} while (mem::compare_exchange(ptr, old_value, new_value, $ordering, $ordering) != old_value);

	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require types::is_int($typeof(*ptr)) "Only integer pointers may be used."
 **/
macro flag_clear(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value = false;
	
	do {
		old_value = $$atomic_load(ptr, false, $ordering.ordinal);
	} while (mem::compare_exchange(ptr, old_value, new_value, $ordering, $ordering) != old_value);
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
* @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 **/
macro fetch_max(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $StorageType = $typefrom(types::lower_to_atomic_compatible_type($typeof(*ptr)));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value = y;

	$StorageType storage_old_value;
	$StorageType storage_new_value = bitcast(new_value, $StorageType);
	
	do {
		storage_old_value = $$atomic_load(storage_ptr, false, $ordering.ordinal);
		old_value = bitcast(storage_old_value, $typeof(*ptr));
		if (old_value >= new_value) break;
	} while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $ordering) != storage_old_value);

	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
* @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 **/
macro fetch_min(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $StorageType = $typefrom(types::lower_to_atomic_compatible_type($typeof(*ptr)));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value = y;

	$StorageType storage_old_value;
	$StorageType storage_new_value = bitcast(new_value, $StorageType);
	
	do {
		storage_old_value = $$atomic_load(storage_ptr, false, $ordering.ordinal);
		old_value = bitcast(storage_old_value, $typeof(*ptr));
		if (old_value <= new_value) break;
	} while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $ordering) != storage_old_value);

	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
* @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 **/
macro fetch_mul(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $StorageType = $typefrom(types::lower_to_atomic_compatible_type($typeof(*ptr)));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;
	
	do {
		storage_old_value = $$atomic_load(storage_ptr, false, $ordering.ordinal);
		old_value = bitcast(storage_old_value, $typeof(*ptr));
		new_value = old_value * y;
		storage_new_value = bitcast(new_value, $StorageType);
	} while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $ordering) != storage_old_value);

	return old_value;
}

/**
 * @param [&in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
* @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 **/
macro fetch_div(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $StorageType = $typefrom(types::lower_to_atomic_compatible_type($typeof(*ptr)));

	$StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
	$typeof(*ptr) new_value;

	$StorageType storage_old_value;
	$StorageType storage_new_value;
	
	do {
		storage_old_value = $$atomic_load(storage_ptr, false, $ordering.ordinal);
		old_value = bitcast(storage_old_value, $typeof(*ptr));
		new_value = old_value / y;
		storage_new_value = bitcast(new_value, $StorageType);
	} while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $ordering) != storage_old_value);

	return old_value;
}