// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of self source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::collections::list(<Type>);
import std::io;
import std::math;

def ElementPredicate = fn bool(Type *type);
def ElementTest = fn bool(Type *type, any* context);
const ELEMENT_IS_EQUATABLE = types::is_equatable_type(Type);
const ELEMENT_IS_POINTER = Type.kindof == POINTER;

struct List (Printable)
{
	usz size;
	usz capacity;
	Allocator *allocator;
	Type *entries;
}


/**
 * @param initial_capacity "The initial capacity to reserve"
 * @param [&inout] allocator "The allocator to use, defaults to the heap allocator"
 **/
fn List* List.new_init(&self, usz initial_capacity = 16, Allocator* allocator = allocator::heap())
{
	self.allocator = allocator;
	self.size = 0;
	if (initial_capacity > 0)
	{
		initial_capacity = math::next_power_of_2(initial_capacity);
		self.entries = allocator::malloc_aligned(allocator, Type.sizeof * initial_capacity, .alignment = Type[1].alignof)!!;
	}
	else
	{
		self.entries = null;
	}
	self.capacity = initial_capacity;
	return self;
}

/**
 * @param initial_capacity "The initial capacity to reserve"
 * @param [&inout] allocator "The allocator to use, defaults to the heap allocator"
 **/
fn List* List.init_new(&self, usz initial_capacity = 16, Allocator* allocator = allocator::heap()) @deprecated("Replaced by new_init")
{
	return self.new_init(initial_capacity, allocator) @inline;
}

/**
 * Initialize the list using the temp allocator.
 *
 * @param initial_capacity "The initial capacity to reserve"
 **/
fn List* List.temp_init(&self, usz initial_capacity = 16)
{
	return self.new_init(initial_capacity, allocator::temp()) @inline;
}

/**
 * Initialize the list using the temp allocator.
 *
 * @param initial_capacity "The initial capacity to reserve"
 **/
fn List* List.init_temp(&self, usz initial_capacity = 16) @deprecated("Replaced by temp_init")
{
	return self.temp_init(initial_capacity) @inline;
}

/**
 * @require self.size == 0 "The List must be empty"
 **/
fn void List.init_wrapping_array(&self, Type[] types, Allocator* allocator = allocator::heap())
{
	self.allocator = allocator;
	self.size = types.len;
	self.capacity = types.len;
	self.entries = types.ptr;
}

fn usz! List.to_format(&self, Formatter* formatter) @dynamic
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

fn String List.to_new_string(&self, Allocator* allocator = allocator::heap()) @dynamic
{
	return string::new_format("%s", *self, .allocator = allocator);
}

fn String List.to_tstring(&self)
{
	return string::tformat("%s", *self);
}

fn void List.push(&self, Type element) @inline
{
	self.append(element);
}

fn void List.append(&self, Type element)
{
	self.ensure_capacity();
	self.entries[self.size++] = element;
}

/**
 * @require self.size > 0
 **/
fn Type List.pop(&self)
{
	return self.entries[--self.size];
}

fn void List.clear(&self)
{
	self.size = 0;
}

/**
 * @require self.size > 0
 **/
fn Type List.pop_first(&self)
{
	Type value = self.entries[0];
	self.remove_at(0);
	return value;
}

/**
 * @require index < self.size
 **/
fn void List.remove_at(&self, usz index)
{
	if (!--self.size || index == self.size) return;
	self.entries[index .. self.size - 1] = self.entries[index + 1 .. self.size];
}

fn void List.add_all(&self, List* other_list)
{
	if (!other_list.size) return;
	self.reserve(other_list.size);
	foreach (&value : other_list)
	{
		self.entries[self.size++] = *value;
	}
}


fn Type[] List.to_new_array(&self, Allocator* allocator = allocator::heap())
{
	if (!self.size) return Type[] {};
	Type[] result = allocator::alloc_array(allocator, Type, self.size);
	result[..] = self.entries[:self.size];
	return result;
}

fn Type[] List.to_tarray(&self)
{
	return self.to_new_array(allocator::temp());
}

/**
 * Reverse the elements in a list.
 **/
fn void List.reverse(&self)
{
	if (self.size < 2) return;
	usz half = self.size / 2U;
	usz end = self.size - 1;
	for (usz i = 0; i < half; i++)
	{
		@swap(self.entries[i], self.entries[end - i]);
	}
}

fn Type[] List.array_view(&self)
{
	return self.entries[:self.size];
}

fn void List.add_array(&self, Type[] array)
{
	if (!array.len) return;
	self.reserve(array.len);
	foreach (&value : array)
	{
		self.entries[self.size++] = *value;
	}
}

fn void List.push_front(&self, Type type) @inline
{
	self.insert_at(0, type);
}

/**
 * @require index < self.size
 **/
fn void List.insert_at(&self, usz index, Type type)
{
	self.ensure_capacity();
	for (usz i = self.size; i > index; i--)
	{
		self.entries[i] = self.entries[i - 1];
	}
	self.size++;
	self.entries[index] = type;
}

/**
 * @require index < self.size
 **/
fn void List.set_at(&self, usz index, Type type)
{
	self.entries[index] = type;
}

/**
 * @require self.size > 0
 **/
fn void List.remove_last(&self)
{
	self.size--;
}

/**
 * @require self.size > 0
 **/
fn void List.remove_first(&self)
{
	self.remove_at(0);
}

fn Type* List.first(&self)
{
	return self.size ? &self.entries[0] : null;
}

fn Type* List.last(&self)
{
	return self.size ? &self.entries[self.size - 1] : null;
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

fn Type List.get(&self, usz index) @inline
{
	return self.entries[index];
}

fn void List.free(&self)
{
	if (!self.allocator) return;
	allocator::free_aligned(self.allocator, self.entries);
	self.capacity = 0;
	self.size = 0;
	self.entries = null;
}

fn void List.swap(&self, usz i, usz j)
{
	@swap(self.entries[i], self.entries[j]);
}

/**
 * @param filter "The function to determine if it should be removed or not"
 * @return "the number of deleted elements"
 **/
fn usz List.remove_if(&self, ElementPredicate filter)
{
	return self._remove_if(filter, false);
}

/**
 * @param selection "The function to determine if it should be kept or not"
 * @return "the number of deleted elements"
 **/
fn usz List.retain_if(&self, ElementPredicate selection)
{
	return self._remove_if(selection, true);
}

macro usz List._remove_if(&self, ElementPredicate filter, bool $invert) @local
{
	usz size = self.size;
	for (usz i = size, usz k = size; k > 0; k = i)
	{
		// Find last index of item to be deleted.
		$if $invert:
			while (i > 0 && !filter(&self.entries[i - 1])) i--;
		$else
			while (i > 0 && filter(&self.entries[i - 1])) i--;
		$endif
		// Remove the items from this index up to the one not to be deleted.
		usz n = self.size - k;
		self.entries[i:n] = self.entries[k:n];
		self.size -= k - i;
		// Find last index of item not to be deleted.
		$if $invert:
			while (i > 0 && filter(&self.entries[i - 1])) i--;
		$else
			while (i > 0 && !filter(&self.entries[i - 1])) i--;
		$endif
	}
	return size - self.size;
}

fn usz List.remove_using_test(&self, ElementTest filter, any* context)
{
	return self._remove_using_test(filter, false, context);
}

fn usz List.retain_using_test(&self, ElementTest filter, any* context)
{
	return self._remove_using_test(filter, true, context);
}

macro usz List._remove_using_test(&self, ElementTest filter, bool $invert, ctx) @local
{
	usz size = self.size;
	for (usz i = size, usz k = size; k > 0; k = i)
	{
		// Find last index of item to be deleted.
		$if $invert:
			while (i > 0 && !filter(&self.entries[i - 1], ctx)) i--;
		$else
			while (i > 0 && filter(&self.entries[i - 1], ctx)) i--;
		$endif
		// Remove the items from this index up to the one not to be deleted.
		usz n = self.size - k;
		self.entries[i:n] = self.entries[k:n];
		self.size -= k - i;
		// Find last index of item not to be deleted.
		$if $invert:
			while (i > 0 && filter(&self.entries[i - 1], ctx)) i--;
		$else
			while (i > 0 && !filter(&self.entries[i - 1], ctx)) i--;
		$endif
	}
	return size - self.size;
}

/**
 * Reserve at least min_capacity
 **/
fn void List.reserve(&self, usz min_capacity)
{
	if (!min_capacity) return;
	if (self.capacity >= min_capacity) return;
	if (!self.allocator) self.allocator = allocator::heap();
	min_capacity = math::next_power_of_2(min_capacity);
	self.entries = allocator::realloc_aligned(self.allocator, self.entries, Type.sizeof * min_capacity, .alignment = Type[1].alignof) ?? null;
	self.capacity = min_capacity;
}

macro Type List.@item_at(&self, usz index) @operator([])
{
	return self.entries[index];
}

fn Type* List.get_ref(&self, usz index) @operator(&[]) @inline
{
	return &self.entries[index];
}

fn void List.set(&self, usz index, Type value) @operator([]=)
{
	self.entries[index] = value;
}

fn void List.ensure_capacity(&self, usz added = 1) @inline @private
{
	usz new_size = self.size + added;
	if (self.capacity >= new_size) return;

	assert(new_size < usz.max / 2U);
	usz new_capacity = self.capacity ? 2U * self.capacity : 16U;
	while (new_capacity < new_size) new_capacity *= 2U;
	self.reserve(new_capacity);
}

// Functions for equatable types


fn usz! List.index_of(&self, Type type) @if(ELEMENT_IS_EQUATABLE)
{
	foreach (i, v : self)
	{
		if (equals(v, type)) return i;
	}
	return SearchResult.MISSING?;
}

fn usz! List.rindex_of(&self, Type type) @if(ELEMENT_IS_EQUATABLE)
{
	foreach_r (i, v : self)
	{
		if (equals(v, type)) return i;
	}
	return SearchResult.MISSING?;
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

/**
 * Check for presence of a value in a list.
 *
 * @param [&in] self "the list to find elements in"
 * @param value "The value to search for"
 * @return "True if the value is found, false otherwise"
 **/
fn bool List.contains(&self, Type value) @if(ELEMENT_IS_EQUATABLE)
{
	foreach (i, v : self)
	{
		if (equals(v, value)) return true;
	}
	return false;
}


/**
 * @param [&inout] self "The list to remove elements from"
 * @param value "The value to remove"
 * @return "the number of deleted elements."
 **/
fn usz List.remove(&self, Type value) @if(ELEMENT_IS_EQUATABLE)
{
	usz size = self.size;
	for (usz i = size; i > 0; i--)
	{
		if (!equals(self.entries[i - 1], value)) continue;
		for (usz j = i; j < size; j++)
		{
			self.entries[j - 1] = self.entries[j];
		}
		self.size--;
	}
	return size - self.size;
}

fn void List.remove_all(&self, List* other_list) @if(ELEMENT_IS_EQUATABLE)
{
	if (!other_list.size) return;
	foreach (v : other_list) self.remove(v);
}

/**
 * @param [&in] self
 * @return "The number non-null values in the list"
 **/
fn usz List.compact_count(&self) @if(ELEMENT_IS_POINTER)
{
	usz vals = 0;
	foreach (v : self) if (v) vals++;
	return vals;
}

fn usz List.compact(&self) @if(ELEMENT_IS_POINTER)
{
	usz size = self.size;
	for (usz i = size; i > 0; i--)
	{
		if (self.entries[i - 1]) continue;
		for (usz j = i; j < size; j++)
		{
			self.entries[j - 1] = self.entries[j];
		}
		self.size--;
	}
	return size - self.size;
}
