// Copyright (c) 2021-2026 Christoffer Lerno. All rights reserved.
// Use of self source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
<*
 @require MAX_SIZE >= 1 : `The size must be at least 1 element big.`
*>
module std::collections::elastic_array <Type, MAX_SIZE>;
import std::io, std::math, std::collections::list_common;

alias ElementPredicate = fn bool(Type *type);
alias ElementTest = fn bool(Type *type, any context);
const ELEMENT_IS_EQUATABLE = types::is_equatable_type(Type);
const ELEMENT_IS_POINTER = Type::kind == POINTER;
macro bool type_is_overaligned() => Type::alignment > mem::DEFAULT_MEM_ALIGNMENT;

struct ElasticArray (Printable)
{
	sz size;
	Type[MAX_SIZE] entries;
}

fn sz? ElasticArray.to_format(&self, Formatter* formatter) @dynamic
{
	return formatter.printf("%s", self.entries[:self.size]);
}

<*
 Try to add an element. Will fail if it is full.

 @return? mem::OUT_OF_MEMORY
*>
fn void? ElasticArray.push_try(&self, Type element) @inline
{
	if (self.size == MAX_SIZE) return mem::OUT_OF_MEMORY~;
	self.entries[self.size++] = element;
}

<*
 @require self.size < MAX_SIZE : `Tried to exceed the max size`
*>
fn void ElasticArray.push(&self, Type element) @inline
{
	self.entries[self.size++] = element;
}

fn Type? ElasticArray.pop(&self)
{
	if (!self.size) return NO_MORE_ELEMENT~;
	return self.entries[--self.size];
}

fn void ElasticArray.clear(&self)
{
	self.size = 0;
}

<*
 @require self.size > 0
*>
fn Type? ElasticArray.pop_first(&self)
{
	if (!self.size) return NO_MORE_ELEMENT~;
	defer self.remove_at(0);
	return self.entries[0];
}

<*
 Remove an element at a particular place.

 @require index < self.size && index >= 0 : "The index is out of bounds"
*>
fn void ElasticArray.remove_at(&self, sz index)
{
	if (!--self.size || index == self.size) return;
	self.entries[index .. self.size - 1] = self.entries[index + 1 .. self.size];
}

<*
 Removes an element at "index" by copying the last element to this slot and reducing
 the length by 1.

 @require index < self.size && index >= 0 : "Index out of bounds"
*>
fn void ElasticArray.remove_unordered_at(&self, sz index)
{
	sz new_size = --self.size;
	if (!new_size || index == new_size) return;
	self.entries[index] = self.entries[new_size];
}
<*
 @require other_list.size + self.size <= MAX_SIZE
*>
fn void ElasticArray.add_all(&self, ElasticArray* other_list)
{
	if (!other_list.size) return;
	foreach (&value : other_list)
	{
		self.entries[self.size++] = *value;
	}
}

<*
 Add as many elements as possible to the new array,
 returning the number of elements that didn't fit.
*>
fn sz ElasticArray.add_all_to_limit(&self, ElasticArray* other_list)
{
	if (!other_list.size) return 0;
	foreach (i, &value : other_list)
	{
		if (self.size == MAX_SIZE) return other_list.size - i;
		self.entries[self.size++] = *value;
	}
	return 0;
}


<*
 Add as many values from this array as possible, returning the
 number of elements that didn't fit.

 @param [in] array
*>
fn sz ElasticArray.push_all_to_limit(&self, Type[] array)
{
	if (!array.len) return 0;
	foreach (i, &value : array)
	{
		if (self.size == MAX_SIZE) return array.len - i;
		self.entries[self.size++] = *value;
	}
	return 0;
}


<*
 Add the values of an array to this list.

 @param [in] array
 @require array.len + self.size <= MAX_SIZE : `Size would exceed max.`
 @ensure self.size >= array.len
*>
fn void ElasticArray.push_all(&self, Type[] array)
{
	if (!array.len) return;
	foreach (&value : array)
	{
		self.entries[self.size++] = *value;
	}
}


<*
 IMPORTANT The returned array must be freed using free_aligned.
*>
fn Type[] ElasticArray.to_aligned_array(&self, Allocator allocator)
{
	return list_common::list_to_aligned_array(Type, self, allocator);
}

<*
 @require !type_is_overaligned() : "This function is not available on overaligned types"
*>
macro Type[] ElasticArray.to_array(&self, Allocator allocator)
{
	return list_common::list_to_array(Type, self, allocator);
}

fn Type[] ElasticArray.to_tarray(&self)
{
	$if type_is_overaligned():
	return self.to_aligned_array(tmem);
	$else
	return self.to_array(tmem);
	$endif;
}

<*
 Reverse the elements in a list.
*>
fn void ElasticArray.reverse(&self)
{
	list_common::list_reverse(self);
}

fn Type[] ElasticArray.array_view(&self)
{
	return self.entries[:self.size];
}

<*
 @require self.size < MAX_SIZE : `List would exceed max size`
*>
fn void ElasticArray.push_front(&self, Type type) @inline
{
	self.insert_at(0, type);
}

<*
 @require self.size < MAX_SIZE : `List would exceed max size`
*>
fn void? ElasticArray.push_front_try(&self, Type type) @inline
{
	return self.insert_at_try(0, type);
}

<*
 @require index <= self.size && index >= 0 : "Index was out of range"
*>
fn void? ElasticArray.insert_at_try(&self, sz index, Type value)
{
	if (self.size == MAX_SIZE) return mem::OUT_OF_MEMORY~;
	self.insert_at(index, value);
}

<*
 @require self.size < MAX_SIZE : `List would exceed max size`
 @require index <= self.size && index >= 0 : "Index was out of range"
*>
fn void ElasticArray.insert_at(&self, sz index, Type type)
{
	for (sz i = self.size; i > index; i--)
	{
		self.entries[i] = self.entries[i - 1];
	}
	self.size++;
	self.entries[index] = type;
}

<*
 @require index <= self.size && index >= 0 : "Index was out of range"
*>
fn void ElasticArray.set_at(&self, sz index, Type type)
{
	self.entries[index] = type;
}

fn void? ElasticArray.remove_last(&self) @maydiscard
{
	if (!self.size) return NO_MORE_ELEMENT~;
	self.size--;
}

fn void? ElasticArray.remove_first(&self) @maydiscard
{
	if (!self.size) return NO_MORE_ELEMENT~;
	self.remove_at(0);
}

fn Type? ElasticArray.first(&self)
{
	if (!self.size) return NO_MORE_ELEMENT~;
	return self.entries[0];
}

fn Type? ElasticArray.last(&self)
{
	if (!self.size) return NO_MORE_ELEMENT~;
	return self.entries[self.size - 1];
}

fn bool ElasticArray.is_empty(&self) @inline
{
	return !self.size;
}

fn sz ElasticArray.byte_size(&self) @inline
{
	return Type::size * self.size;
}

fn sz ElasticArray.len(&self) @operator(len) @inline
{
	return self.size;
}

<*
 @require index <= self.size && index >= 0 : "Index was out of range"
*>
fn Type ElasticArray.get(&self, sz index) @inline
{
	return self.entries[index];
}

fn void ElasticArray.swap(&self, sz i, sz j)
{
	@swap(self.entries[i], self.entries[j]);
}

<*
 @param filter : "The function to determine if it should be removed or not"
 @return "the number of deleted elements"
*>
fn sz ElasticArray.remove_if(&self, ElementPredicate filter)
{
	return list_common::list_remove_if(self, filter, false);
}

<*
 @param selection : "The function to determine if it should be kept or not"
 @return "the number of deleted elements"
*>
fn sz ElasticArray.retain_if(&self, ElementPredicate selection)
{
	return list_common::list_remove_if(self, selection, true);
}

fn sz ElasticArray.remove_using_test(&self, ElementTest filter, any context)
{
	return list_common::list_remove_using_test(self, filter, false, context);
}

fn sz ElasticArray.retain_using_test(&self, ElementTest filter, any context)
{
	return list_common::list_remove_using_test(self, filter, true, context);
}


macro Type ElasticArray.@item_at(&self, sz index) @operator([])
{
	return self.entries[index];
}

fn Type* ElasticArray.get_ref(&self, sz index) @operator(&[]) @inline
{
	return &self.entries[index];
}

<*
 @require index <= self.size && index >= 0 : "Index was out of range"
*>
fn void ElasticArray.set(&self, sz index, Type value) @operator([]=)
{
	self.entries[index] = value;
}


// Functions for equatable types
fn sz? ElasticArray.index_of(&self, Type type) @if(ELEMENT_IS_EQUATABLE)
{
	foreach (i, v : self)
	{
		if (equals(v, type)) return i;
	}
	return NOT_FOUND~;
}

fn sz? ElasticArray.rindex_of(&self, Type type) @if(ELEMENT_IS_EQUATABLE)
{
	foreach_r (i, v : self)
	{
		if (equals(v, type)) return i;
	}
	return NOT_FOUND~;
}

fn bool ElasticArray.equals(&self, ElasticArray other_list) @if(ELEMENT_IS_EQUATABLE)
{
	if (self.size != other_list.size) return false;
	foreach (i, v : self)
	{
		if (!equals(v, other_list.entries[i])) return false;
	}
	return true;
}

<*
 Check for presence of a value in a list.

 @param [&in] self : "the list to find elements in"
 @param value : "The value to search for"
 @return "True if the value is found, false otherwise"
*>
fn bool ElasticArray.contains(&self, Type value) @if(ELEMENT_IS_EQUATABLE)
{
	foreach (i, v : self)
	{
		if (equals(v, value)) return true;
	}
	return false;
}

<*
 @param [&inout] self : "The list to remove elements from"
 @param value : "The value to remove"
 @return "true if the value was found"
*>
fn bool ElasticArray.remove_last_item(&self, Type value) @if(ELEMENT_IS_EQUATABLE)
{
	return @ok(self.remove_at(self.rindex_of(value)));
}

<*
 @param [&inout] self : "The list to remove elements from"
 @param value : "The value to remove"
 @return "true if the value was found"
*>
fn bool ElasticArray.remove_first_item(&self, Type value) @if(ELEMENT_IS_EQUATABLE)
{
	return @ok(self.remove_at(self.index_of(value)));
}

<*
 @param [&inout] self : "The list to remove elements from"
 @param value : "The value to remove"
 @return "the number of deleted elements."
*>
fn sz ElasticArray.remove_item(&self, Type value) @if(ELEMENT_IS_EQUATABLE)
{
	return list_common::list_remove_item(self, value);
}



fn void ElasticArray.remove_all_from(&self, ElasticArray* other_list) @if(ELEMENT_IS_EQUATABLE)
{
	if (!other_list.size) return;
	foreach (v : other_list) self.remove_item(v);
}

<*
 @param [&in] self
 @return "The number non-null values in the list"
*>
fn sz ElasticArray.compact_count(&self) @if(ELEMENT_IS_POINTER)
{
	sz vals = 0;
	foreach (v : self) if (v) vals++;
	return vals;
}

fn sz ElasticArray.compact(&self) @if(ELEMENT_IS_POINTER)
{
	return list_common::list_compact(self);
}
