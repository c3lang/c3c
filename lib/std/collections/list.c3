// Copyright (c) 2021-2024 Christoffer Lerno. All rights reserved.
// Use of self source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::collections::list <Type>;
import std::io, std::math, std::collections::list_common;

alias ElementPredicate = fn bool(Type *type);
alias ElementTest = fn bool(Type *type, any context);
const ELEMENT_IS_EQUATABLE = types::is_equatable_type(Type);
const ELEMENT_IS_POINTER = Type::kind == POINTER;

const Allocator LIST_HEAP_ALLOCATOR = (Allocator)&dummy;

const List ONHEAP = { .allocator = LIST_HEAP_ALLOCATOR };

macro bool type_is_overaligned() => Type::alignment > mem::DEFAULT_MEM_ALIGNMENT;

struct List (Printable)
{
	sz size;
	sz capacity;
	Allocator allocator;
	Type *entries;
}

<*
 @param initial_capacity : "The initial capacity to reserve"
 @param [&inout] allocator : "The allocator to use"
*>
fn List* List.init(&self, Allocator allocator, sz initial_capacity = 16)
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
fn List* List.tinit(&self, sz initial_capacity = 16)
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
	self.push_all(values) @inline;
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
	self.push_all(values) @inline;
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
	list_set_size(self, types.len);
}

fn bool List.is_initialized(&self) @inline => self.allocator != null && self.allocator != (Allocator)&dummy;

fn sz? List.to_format(&self, Formatter* formatter) @dynamic
{
	switch (self.size)
	{
		case 0:
			return formatter.print("[]")!;
		case 1:
			return formatter.printf("[%s]", self.entries[0])!;
		default:
			sz n = formatter.print("[")!;
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
	self.entries[list_set_size(self, self.size + 1)] = element;
}

fn Type? List.pop(&self)
{
	if (!self.size) return NO_MORE_ELEMENT~;
	defer list_set_size(self, self.size - 1);
	return self.entries[self.size - 1];
}

fn void List.clear(&self)
{
	list_set_size(self, 0);
}

fn Type? List.pop_first(&self)
{
	if (!self.size) return NO_MORE_ELEMENT~;
	defer self.remove_at(0);
	return self.entries[0];
}

<*
 @require index < self.size && index >= 0 : "Index out of bounds"
*>
fn void List.remove_at(&self, sz index)
{
	sz new_size = self.size - 1;
	defer list_set_size(self, new_size);
	if (!new_size || index == new_size) return;
	self.entries[index .. new_size - 1] = self.entries[index + 1 .. new_size];
}

<*
 Removes an element at "index" by copying the last element to this slot and reducing
 the length by 1.

 @require index < self.size && index >= 0 : "Index out of bounds"
*>
fn void List.remove_unordered_at(&self, sz index)
{
	sz new_size = self.size - 1;
	defer list_set_size(self, new_size);
	if (!new_size || index == new_size) return;
	self.entries[index] = self.entries[new_size];
}

fn void List.add_all(&self, List* other_list)
{
	if (!other_list.size) return;
	self.reserve(other_list.size);
	sz index = list_set_size(self, self.size + other_list.size);
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
fn void List.push_all(&self, Type[] array)
{
	if (!array.len) return;
	self.reserve(array.len);
	sz index = list_set_size(self, self.size + array.len);
	self.entries[index : array.len] = array[..];
}

fn void List.push_front(&self, Type type) @inline
{
	self.insert_at(0, type);
}

<*
 @require index <= self.size && index >= 0: `Insert was out of bounds`
*>
fn void List.insert_at(&self, sz index, Type type)
{
	self.reserve(1);
	list_set_size(self, self.size + 1);
	for (sz i = self.size - 1; i > index; i--)
	{
		self.entries[i] = self.entries[i - 1];
	}
	self.entries[index] = type;
}

<*
 @require index < self.size && index >= 0 : "Index out of bounds"
*>
fn void List.set_at(&self, sz index, Type type)
{
	self.entries[index] = type;
}

fn void? List.remove_last(&self) @maydiscard
{
	if (!self.size) return NO_MORE_ELEMENT~;
	list_set_size(self, self.size - 1);
}

fn void? List.remove_first(&self) @maydiscard
{
	if (!self.size) return NO_MORE_ELEMENT~;
	self.remove_at(0);
}

fn Type? List.first(&self)
{
	if (!self.size) return NO_MORE_ELEMENT~;
	return self.entries[0];
}

fn Type? List.last(&self)
{
	if (!self.size) return NO_MORE_ELEMENT~;
	return self.entries[self.size - 1];
}

fn bool List.is_empty(&self) @inline
{
	return !self.size;
}

fn sz List.byte_size(&self) @inline
{
	return Type::size * self.size;
}

fn sz List.len(&self) @operator(len) @inline
{
	return self.size;
}

<*
 @require index < self.size && index >= 0 : `Access out of bounds`
*>
fn Type List.get(&self, sz index) @inline
{
	return self.entries[index];
}

fn void List.free(&self)
{
	if (!self.allocator || self.allocator.ptr == &dummy || !self.capacity) return;

	list_pre_free(self); // Remove sanitizer annotation

	$if type_is_overaligned():
		alloc::free_aligned(self.allocator, self.entries);
	$else
		alloc::free(self.allocator, self.entries);
	$endif;
	self.capacity = 0;
	self.size = 0;
	self.entries = null;
}

<*
 @require i < self.size && i >= 0 && j < self.size && j >= 0 : `Access out of bounds`
*>
fn void List.swap(&self, sz i, sz j)
{
	@swap(self.entries[i], self.entries[j]);
}

<*
 @param filter : "The function to determine if it should be removed or not"
 @return "the number of deleted elements"
*>
fn sz List.remove_if(&self, ElementPredicate filter)
{
	return list_common::list_remove_if(self, filter, false);
}

<*
 @param selection : "The function to determine if it should be kept or not"
 @return "the number of deleted elements"
*>
fn sz List.retain_if(&self, ElementPredicate selection)
{
	return list_common::list_remove_if(self, selection, true);
}

fn sz List.remove_using_test(&self, ElementTest filter, any context)
{
	sz old_size = self.size;
	defer
	{
		if (old_size != self.size) list_update_size_change(self, old_size, self.size);
	}
	return list_common::list_remove_using_test(self, filter, false, context);
}



fn sz List.retain_using_test(&self, ElementTest filter, any context)
{
	sz old_size = self.size;
	defer {
		if (old_size != self.size) list_update_size_change(self, old_size, self.size);
	}
	return list_common::list_remove_using_test(self, filter, true, context);
}

fn void list_ensure_capacity(List* self, sz min_capacity) @local
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

	list_pre_free(self); // Remove sanitizer annotation

	min_capacity = math::next_power_of_2(min_capacity);
	$if type_is_overaligned():
		self.entries = alloc::realloc_aligned(self.allocator, self.entries, Type::size * min_capacity, alignment: Type[1]::alignment)!!;
	$else
		self.entries = alloc::realloc(self.allocator, self.entries, Type::size * min_capacity);
	$endif;
	self.capacity = min_capacity;

	list_post_alloc(self); // Add sanitizer annotation
}

<*
 @require index < self.size && index >= 0 : `Access out of bounds`
*>
macro Type List.@item_at(&self, sz index) @operator([])
{
	return self.entries[index];
}

<*
 @require index < self.size && index >= 0 : `Access out of bounds`
*>
fn Type* List.get_ref(&self, sz index) @operator(&[]) @inline
{
	return &self.entries[index];
}

<*
 @require index < self.size && index >= 0 : `Access out of bounds`
*>
fn void List.set(&self, sz index, Type value) @operator([]=)
{
	self.entries[index] = value;
}

fn void List.reserve(&self, sz added)
{
	sz new_size = self.size + added;
	if (self.capacity >= new_size) return;

	assert(new_size < sz::max / 2U);
	sz new_capacity = self.capacity ? 2U * self.capacity : 16U;
	while (new_capacity < new_size) new_capacity *= 2U;
	list_ensure_capacity(self, new_capacity);
}

fn void list_update_size_change(List* self,sz old_size, sz new_size) @private
{
	if (old_size == new_size) return;
	$if env::ADDRESS_SANITIZER:
		if (self.allocator.ptr != &allocators::LIBC_ALLOCATOR) return;
		sanitizer::annotate_contiguous_container(self.entries,
	                                         &self.entries[self.capacity],
	                                         &self.entries[old_size],
	                                         &self.entries[new_size]);
	$endif
}
<*
 @require new_size >= 0 : "The new size cannot be negative"
 @require new_size == 0 || self.capacity > 0
*>
fn sz list_set_size(List* self, sz new_size) @inline @private
{
	sz old_size = self.size;
	list_update_size_change(self, old_size, new_size);
	self.size = new_size;
	return old_size;
}

macro void list_pre_free(List* self) @private
{
	if (!self.capacity) return;
	list_update_size_change(self, self.size, self.capacity);
}

<*
 @require self.capacity > 0
*>
macro void list_post_alloc(List* self) @private
{
	list_update_size_change(self, self.capacity, self.size);
}

// Functions for equatable types


fn sz? List.index_of(&self, Type type) @if (ELEMENT_IS_EQUATABLE)
{
	foreach (i, v : self)
	{
		if (equals(v, type)) return i;
	}
	return NOT_FOUND~;
}

fn sz? List.rindex_of(&self, Type type) @if (ELEMENT_IS_EQUATABLE)
{
	foreach_r (i, v : self)
	{
		if (equals(v, type)) return i;
	}
	return NOT_FOUND~;
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
fn sz List.remove_item(&self, Type value) @if(ELEMENT_IS_EQUATABLE)
{
	sz old_size = self.size;
	defer if (old_size != self.size) list_update_size_change(self, old_size, self.size);
	return list_common::list_remove_item(self, value);
}



fn void List.remove_all_from(&self, List* other_list) @if(ELEMENT_IS_EQUATABLE)
{
	if (!other_list.size) return;
	sz old_size = self.size;
	defer if (old_size != self.size) list_update_size_change(self, old_size, self.size);
	foreach (v : other_list) self.remove_item(v);
}

<*
 @param [&in] self
 @return "The number non-null values in the list"
*>
fn sz List.compact_count(&self) @if(ELEMENT_IS_POINTER)
{
	sz vals = 0;
	foreach (v : self) if (v) vals++;
	return vals;
}

fn sz List.compact(&self) @if(ELEMENT_IS_POINTER)
{
	sz old_size = self.size;
	defer if (old_size != self.size) list_update_size_change(self, old_size, self.size);
	return list_common::list_compact(self);
}

int dummy @local;
