// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::atomic;

/**
 * @param [in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
 * @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 **/
macro fetch_add(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $StorageType = $typefrom(types::get_atomic_compatible_type($typeof(*ptr)));

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
 * @param [in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
* @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 **/
macro fetch_sub(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $StorageType = $typefrom(types::get_atomic_compatible_type($typeof(*ptr)));

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
 * @param [in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
* @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 **/
macro fetch_or(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $StorageType = $typefrom(types::get_atomic_compatible_type($typeof(*ptr)));

  $StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
  $typeof(*ptr) new_value;

  $StorageType storage_old_value;
  $StorageType storage_new_value;
  
  do {
    storage_old_value = $$atomic_load(storage_ptr, false, $ordering.ordinal);
    old_value = bitcast(storage_old_value, $typeof(*ptr));
    new_value = old_value | y;
    storage_new_value = bitcast(new_value, $StorageType);
  } while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $ordering) != storage_old_value);

  return old_value;
}

/**
 * @param [in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
* @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 **/
macro fetch_xor(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $StorageType = $typefrom(types::get_atomic_compatible_type($typeof(*ptr)));

  $StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
  $typeof(*ptr) new_value;

  $StorageType storage_old_value;
  $StorageType storage_new_value;
  
  do {
    storage_old_value = $$atomic_load(storage_ptr, false, $ordering.ordinal);
    old_value = bitcast(storage_old_value, $typeof(*ptr));
    new_value = old_value ^ y;
    storage_new_value = bitcast(new_value, $StorageType);
  } while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $ordering) != storage_old_value);

  return old_value;
}

/**
 * @param [in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
* @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 **/
macro fetch_and(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $StorageType = $typefrom(types::get_atomic_compatible_type($typeof(*ptr)));

  $StorageType* storage_ptr = ($StorageType*)ptr;

	$typeof(*ptr) old_value;
  $typeof(*ptr) new_value;

  $StorageType storage_old_value;
  $StorageType storage_new_value;
  
  do {
    storage_old_value = $$atomic_load(storage_ptr, false, $ordering.ordinal);
    old_value = bitcast(storage_old_value, $typeof(*ptr));
    new_value = old_value & y;
    storage_new_value = bitcast(new_value, $StorageType);
  } while (mem::compare_exchange(storage_ptr, storage_old_value, storage_new_value, $ordering, $ordering) != storage_old_value);

  return old_value;
}

/**
 * @param [in] ptr "the variable or dereferenced pointer to the data."
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
 * @param [in] ptr "the variable or dereferenced pointer to the data."
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
 * @param [in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
* @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 **/
macro fetch_max(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $StorageType = $typefrom(types::get_atomic_compatible_type($typeof(*ptr)));

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
 * @param [in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 * 
* @require types::is_int($typeof(*ptr)) || types::is_float($typeof(*ptr)) "Only integer/float pointers may be used."
 **/
macro fetch_min(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	var $StorageType = $typefrom(types::get_atomic_compatible_type($typeof(*ptr)));

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