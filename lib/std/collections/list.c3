// Copyright (c) 2021-2024 Christoffer Lerno. All rights reserved.
// Use of self source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::collections::list{Type};
import std::io, std::math, std::collections::list_common;

alias ElementPredicate = fn bool(Type *type);
alias ElementTest = fn bool(Type *type, any context);
const ELEMENT_IS_EQUATABLE = types::is_equatable_type(Type);
const ELEMENT_IS_POINTER = Type.kindof == POINTER;

const Allocator LIST_HEAP_ALLOCATOR = (Allocator)&dummy;

const List ONHEAP = { .allocator = LIST_HEAP_ALLOCATOR };

macro type_is_overaligned() => Type.alignof > mem::DEFAULT_MEM_ALIGNMENT;

struct List (Printable)
{
	usz size;
	usz capacity;
	Allocator allocator;
	Type *entries;
}

<*
 @param initial_capacity : "The initial capacity to reserve"
 @param [&inout] allocator : "The allocator to use, defaults to the heap allocator"
*>
fn List* List.init(&self, Allocator allocator, usz initial_capacity = 16)
{
	self.allocator = allocator;
	self.size = 0;
	self.capacity = 0;
	self.entries = null;
	self.reserve(initial_capacity);
	return self;
}


<*
 Initialize the list using the temp allocator.

 @param initial_capacity : "The initial capacity to reserve"
*>
fn List* List.tinit(&self, usz initial_capacity = 16)
{
	return self.init(tmem, initial_capacity) @inline;
}

<*
 Initialize a new list with an array.

 @param [in] values : `The values to initialize the list with.`
 @require self.size == 0 : "The List must be empty"
*>
fn List* List.init_with_array(&self, Allocator allocator, Type[] values)
{
	self.init(allocator, values.len) @inline;
	self.add_array(values) @inline;
	return self;
}

<*
 Initialize a temporary list with an array.

 @param [in] values : `The values to initialize the list with.`
 @require self.size == 0 : "The List must be empty"
*>
fn List* List.tinit_with_array(&self, Type[] values)
{
	self.tinit(values.len) @inline;
	self.add_array(values) @inline;
	return self;
}

<*
 @require !self.is_initialized() : "The List must not be allocated"
*>
fn void List.init_wrapping_array(&self, Allocator allocator, Type[] types)
{
	self.allocator = allocator;
	self.capacity = types.len;
	self.entries = types.ptr;
	self.set_size(types.len);
}

fn bool List.is_initialized(&self) @inline => self.allocator != null && self.allocator != (Allocator)&dummy;

fn usz? List.to_format(&self, Formatter* formatter) @dynamic
{
	switch (self.size)
	{
		case 0:
			return formatter.print("[]")!;
		case 1:
			return formatter.printf("[%s]", self.entries[0])!;
		default:
			usz n = formatter.print("[")!;
			foreach (i, element : self.entries[:self.size])
			{
				if (i != 0) formatter.print(", ")!;
				n += formatter.printf("%s", element)!;
			}
			n += formatter.print("]")!;
			return n;
	}
}

fn void List.push(&self, Type element) @inline
{
	self.reserve(1);
	self.entries[self.set_size(self.size + 1)] = element;
}

fn Type? List.pop(&self)
{
	if (!self.size) return NO_MORE_ELEMENT?;
	defer self.set_size(self.size - 1);
	return self.entries[self.size - 1];
}

fn void List.clear(&self)
{
	self.set_size(0);
}

fn Type? List.pop_first(&self)
{
	if (!self.size) return NO_MORE_ELEMENT?;
	defer self.remove_at(0);
	return self.entries[0];
}

<*
 @require index < self.size : `Removed element out of bounds`
*>
fn void List.remove_at(&self, usz index)
{
	usz new_size = self.size - 1;
	defer self.set_size(new_size);
	if (!new_size || index == new_size) return;
	self.entries[index .. new_size - 1] = self.entries[index + 1 .. new_size];
}

fn void List.add_all(&self, List* other_list)
{
	if (!other_list.size) return;
	self.reserve(other_list.size);
	usz index = self.set_size(self.size + other_list.size);
	foreach (&value : other_list)
	{
		self.entries[index++] = *value;
	}
}


<*
 IMPORTANT The returned array must be freed using free_aligned.
*>
fn Type[] List.to_aligned_array(&self, Allocator allocator)
{
	return list_common::list_to_aligned_array(Type, self, allocator);
}

<*
 @require !type_is_overaligned() : "This function is not available on overaligned types"
*>
macro Type[] List.to_array(&self, Allocator allocator)
{
	return list_common::list_to_array(Type, self, allocator);
}

fn Type[] List.to_tarray(&self)
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
fn void List.reverse(&self)
{
	list_common::list_reverse(self);
}

fn Type[] List.array_view(&self)
{
	return self.entries[:self.size];
}

<*
 Add the values of an array to this list.

 @param [in] array
 @ensure self.size >= array.len
*>
fn void List.add_array(&self, Type[] array)
{
	if (!array.len) return;
	self.reserve(array.len);
	usz index = self.set_size(self.size + array.len);
	self.entries[index : array.len] = array[..];
}

fn void List.push_front(&self, Type type) @inline
{
	self.insert_at(0, type);
}

<*
 @require index <= self.size : `Insert was out of bounds`
*>
fn void List.insert_at(&self, usz index, Type type)
{
	self.reserve(1);
	self.set_size(self.size + 1);
	for (isz i = self.size - 1; i > index; i--)
	{
		self.entries[i] = self.entries[i - 1];
	}
	self.entries[index] = type;
}

<*
 @require index < self.size
*>
fn void List.set_at(&self, usz index, Type type)
{
	self.entries[index] = type;
}

fn void? List.remove_last(&self) @maydiscard
{
	if (!self.size) return NO_MORE_ELEMENT?;
	self.set_size(self.size - 1);
}

fn void? List.remove_first(&self) @maydiscard
{
	if (!self.size) return NO_MORE_ELEMENT?;
	self.remove_at(0);
}

fn Type? List.first(&self)
{
	if (!self.size) return NO_MORE_ELEMENT?;
	return self.entries[0];
}

fn Type? List.last(&self)
{
	if (!self.size) return NO_MORE_ELEMENT?;
	return self.entries[self.size - 1];
}

fn bool List.is_empty(&self) @inline
{
	return !self.size;
}

fn usz List.byte_size(&self) @inline
{
	return Type.sizeof * self.size;
}

fn usz List.len(&self) @operator(len) @inline
{
	return self.size;
}

<*
 @require index < self.size : `Access out of bounds`
*>
fn Type List.get(&self, usz index) @inline
{
	return self.entries[index];
}

fn void List.free(&self)
{
	if (!self.allocator || self.allocator.ptr == &dummy || !self.capacity) return;

	self.pre_free(); // Remove sanitizer annotation

	$if type_is_overaligned():
		allocator::free_aligned(self.allocator, self.entries);
	$else
		allocator::free(self.allocator, self.entries);
	$endif;
	self.capacity = 0;
	self.size = 0;
	self.entries = null;
}

<*
 @require i < self.size && j < self.size : `Access out of bounds`
*>
fn void List.swap(&self, usz i, usz j)
{
	@swap(self.entries[i], self.entries[j]);
}

<*
 @param filter : "The function to determine if it should be removed or not"
 @return "the number of deleted elements"
*>
fn usz List.remove_if(&self, ElementPredicate filter)
{
	return list_common::list_remove_if(self, filter, false);
}

<*
 @param selection : "The function to determine if it should be kept or not"
 @return "the number of deleted elements"
*>
fn usz List.retain_if(&self, ElementPredicate selection)
{
	return list_common::list_remove_if(self, selection, true);
}

fn usz List.remove_using_test(&self, ElementTest filter, any context)
{
	usz old_size = self.size;
	defer
	{
		if (old_size != self.size) self._update_size_change(old_size, self.size);
	}
	return list_common::list_remove_using_test(self, filter, false, context);
}



fn usz List.retain_using_test(&self, ElementTest filter, any context)
{
	usz old_size = self.size;
	defer {
		if (old_size != self.size) self._update_size_change(old_size, self.size);
	}
	return list_common::list_remove_using_test(self, filter, true, context);
}

fn void List.ensure_capacity(&self, usz min_capacity) @local
{
	if (!min_capacity) return;
	if (self.capacity >= min_capacity) return;

	// Get a proper allocator
	switch (self.allocator.ptr)
	{
		case &dummy:
			self.allocator = mem;
		case null:
			self.allocator = tmem;
		default:
			break;
	}

	self.pre_free(); // Remove sanitizer annotation

	min_capacity = math::next_power_of_2(min_capacity);
	$if type_is_overaligned():
		self.entries = allocator::realloc_aligned(self.allocator, self.entries, Type.sizeof * min_capacity, alignment: Type[1].alignof)!!;
	$else
		self.entries = allocator::realloc(self.allocator, self.entries, Type.sizeof * min_capacity);
	$endif;
	self.capacity = min_capacity;

	self.post_alloc(); // Add sanitizer annotation
}

<*
 @require index < self.size : `Access out of bounds`
*>
macro Type List.@item_at(&self, usz index) @operator([])
{
	return self.entries[index];
}

<*
 @require index < self.size : `Access out of bounds`
*>
fn Type* List.get_ref(&self, usz index) @operator(&[]) @inline
{
	return &self.entries[index];
}

<*
 @require index < self.size : `Access out of bounds`
*>
fn void List.set(&self, usz index, Type value) @operator([]=)
{
	self.entries[index] = value;
}

fn void List.reserve(&self, usz added)
{
	usz new_size = self.size + added;
	if (self.capacity >= new_size) return;

	assert(new_size < usz.max / 2U);
	usz new_capacity = self.capacity ? 2U * self.capacity : 16U;
	while (new_capacity < new_size) new_capacity *= 2U;
	self.ensure_capacity(new_capacity);
}

fn void List._update_size_change(&self,usz old_size, usz new_size)
{
	if (old_size == new_size) return;
	$if env::ADDRESS_SANITIZER:
		if (self.allocator.ptr != &allocator::LIBC_ALLOCATOR) return;
		sanitizer::annotate_contiguous_container(self.entries,
	                                         &self.entries[self.capacity],
	                                         &self.entries[old_size],
	                                         &self.entries[new_size]);
	$endif
}
<*
 @require new_size == 0 || self.capacity != 0
*>
fn usz List.set_size(&self, usz new_size) @inline @private
{
	usz old_size = self.size;
	self._update_size_change(old_size, new_size);
	self.size = new_size;
	return old_size;
}

macro void List.pre_free(&self) @private
{
	if (!self.capacity) return;
	self._update_size_change(self.size, self.capacity);
}

<*
 @require self.capacity > 0
*>
macro void List.post_alloc(&self) @private
{
	self._update_size_change(self.capacity, self.size);
}

// Functions for equatable types


fn usz? List.index_of(&self, Type type) @if(ELEMENT_IS_EQUATABLE)
{
	foreach (i, v : self)
	{
		if (equals(v, type)) return i;
	}
	return NOT_FOUND?;
}

fn usz? List.rindex_of(&self, Type type) @if(ELEMENT_IS_EQUATABLE)
{
	foreach_r (i, v : self)
	{
		if (equals(v, type)) return i;
	}
	return NOT_FOUND?;
}

fn bool List.equals(&self, List other_list) @if(ELEMENT_IS_EQUATABLE)
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
fn bool List.contains(&self, Type value) @if(ELEMENT_IS_EQUATABLE)
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
fn bool List.remove_last_item(&self, Type value) @if(ELEMENT_IS_EQUATABLE)
{
	return @ok(self.remove_at(self.rindex_of(value)));
}

<*
 @param [&inout] self : "The list to remove elements from"
 @param value : "The value to remove"
 @return "true if the value was found"
*>
fn bool List.remove_first_item(&self, Type value) @if(ELEMENT_IS_EQUATABLE)
{
	return @ok(self.remove_at(self.index_of(value)));
}
<*
 @param [&inout] self : "The list to remove elements from"
 @param value : "The value to remove"
 @return "the number of deleted elements."
*>
fn usz List.remove_item(&self, Type value) @if(ELEMENT_IS_EQUATABLE)
{
	usz old_size = self.size;
	defer {
		if (old_size != self.size) self._update_size_change(old_size, self.size);
	}
	return list_common::list_remove_item(self, value);
}



fn void List.remove_all_from(&self, List* other_list) @if(ELEMENT_IS_EQUATABLE)
{
	if (!other_list.size) return;
	usz old_size = self.size;
	defer {
		if (old_size != self.size) self._update_size_change(old_size, self.size);
	}
	foreach (v : other_list) self.remove_item(v);
}

<*
 @param [&in] self
 @return "The number non-null values in the list"
*>
fn usz List.compact_count(&self) @if(ELEMENT_IS_POINTER)
{
	usz vals = 0;
	foreach (v : self) if (v) vals++;
	return vals;
}

fn usz List.compact(&self) @if(ELEMENT_IS_POINTER)
{
	usz old_size = self.size;
	defer {
		if (old_size != self.size) self._update_size_change(old_size, self.size);
	}
	return list_common::list_compact(self);
}

int dummy @local;
