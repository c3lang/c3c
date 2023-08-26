// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::atomic;

/**
 * @param [in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 **/
macro fetch_add(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	$typeof(*ptr) old_value;
  $typeof(*ptr) new_value;
  
  do {
    old_value = $$atomic_load(ptr, false, $ordering.ordinal);
    new_value = old_value + y;
  } while (!mem::compare_exchange(ptr, old_value, new_value, $ordering, $ordering));

  return old_value;
}

/**
 * @param [in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 **/
macro fetch_sub(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	$typeof(*ptr) old_value;
  $typeof(*ptr) new_value;
  
  do {
    old_value = $$atomic_load(ptr, false, $ordering.ordinal);
    new_value = old_value - y;
  } while (!mem::compare_exchange(ptr, old_value, new_value, $ordering, $ordering));

  return old_value;
}

/**
 * @param [in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 **/
macro fetch_or(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	$typeof(*ptr) old_value;
  $typeof(*ptr) new_value;
  
  do {
    old_value = $$atomic_load(ptr, false, $ordering.ordinal);
    new_value = old_value | y;
  } while (!mem::compare_exchange(ptr, old_value, new_value, $ordering, $ordering));

  return old_value;
}

/**
 * @param [in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 **/
macro fetch_xor(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	$typeof(*ptr) old_value;
  $typeof(*ptr) new_value;
  
  do {
    old_value = $$atomic_load(ptr, false, $ordering.ordinal);
    new_value = old_value ^ y;
  } while (!mem::compare_exchange(ptr, old_value, new_value, $ordering, $ordering));

  return old_value;
}

/**
 * @param [in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 **/
macro fetch_and(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	$typeof(*ptr) old_value;
  $typeof(*ptr) new_value;
  
  do {
    old_value = $$atomic_load(ptr, false, $ordering.ordinal);
    new_value = old_value & y;
  } while (!mem::compare_exchange(ptr, old_value, new_value, $ordering, $ordering));

  return old_value;
}

/**
 * @param [in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 **/
macro flag_test_and_set(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	$typeof(*ptr) old_value;
  $typeof(*ptr) new_value = true;
  
  do {
    old_value = $$atomic_load(ptr, false, $ordering.ordinal);
  } while (!mem::compare_exchange(ptr, old_value, new_value, $ordering, $ordering));

  return old_value;
}

/**
 * @param [in] ptr "the variable or dereferenced pointer to the data."
 * @param [in] y "the value to be added to ptr."
 * @param $ordering "atomic ordering of the load, defaults to SEQ_CONSISTENT"
 * @return "returns the old value of ptr"
 **/
macro flag_clear(ptr, y, AtomicOrdering $ordering = SEQ_CONSISTENT)
{
	$typeof(*ptr) old_value;
  $typeof(*ptr) new_value = false;
  
  do {
    old_value = $$atomic_load(ptr, false, $ordering.ordinal);
  } while (!mem::compare_exchange(ptr, old_value, new_value, $ordering, $ordering));
}
