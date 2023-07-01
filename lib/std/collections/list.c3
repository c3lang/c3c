// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of self source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::collections::list<Type>;
import std::math;

def ElementPredicate = fn bool(Type *type);
const ELEMENT_IS_EQUATABLE = types::is_equatable_type(Type);
const ELEMENT_IS_POINTER = Type.kindof == POINTER;

struct List
{
    usz size;
    usz capacity;
    Allocator *allocator;
    Type *entries;
}

/**
 * @require using != null "A valid allocator must be provided"
 **/
fn void List.init(&self, usz initial_capacity = 16, Allocator* using = mem::heap())
{
	self.allocator = using;
	self.size = 0;
	if (initial_capacity > 0)
	{
		initial_capacity = math::next_power_of_2(initial_capacity);
		self.entries = malloc_aligned(Type, initial_capacity, .alignment = Type[1].alignof, .using = using)!!;
	}
	else
	{
		self.entries = null;
	}
	self.capacity = initial_capacity;
}

fn void List.tinit(&self, usz initial_capacity = 16)
{
	self.init(initial_capacity, mem::temp()) @inline;
}

fn void! List.to_format(&self, Formatter* formatter) @dynamic
{
	switch (self.size)
	{
		case 0:
			formatter.print("[]")!;
		case 1:
			formatter.printf("[%s]", self.entries[0])!;
		default:
			formatter.print("[")!;
			foreach (i, element : self.entries[:self.size])
			{
				if (i != 0) formatter.print(", ")!;
				formatter.printf("%s", element)!;
			}
			formatter.print("]")!;
	}
}

fn String List.to_string(&self, Allocator* using = mem::heap()) @dynamic
{
	return string::printf("%s", *self);
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
 */
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
 */
fn Type List.pop_first(&self)
{
    Type value = self.entries[0];
    self.remove_at(0);
    return value;
}

fn void List.remove_at(&self, usz index)
{
    for (usz i = index + 1; i < self.size; i++)
    {
        self.entries[i - 1] = self.entries[i];
    }
    self.size--;
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


fn Type[] List.to_array(&self, Allocator* using = mem::heap())
{
	if (!self.size) return Type[] {};
	Type[] result = malloc(Type, self.size, .using = using);
	result[..] = self.entries[:self.size];
	return result;
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

fn void List.remove_last(&self)
{
    self.size--;
}

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

fn bool List.is_empty(&self)
{
    return !self.size;
}

fn usz List.len(&self) @operator(len)
{
    return self.size;
}

fn Type List.get(&self, usz index)
{
    return self.entries[index];
}

fn void List.free(&self)
{
	if (!self.allocator) return;
    free_aligned(self.entries, .using = self.allocator);
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
	usz size = self.size;
	for (usz i = size; i > 0; i--)
	{
		if (filter(&self.entries[i - 1])) continue;
		for (usz j = i; j < size; j++)
		{
			self.entries[j - 1] = self.entries[j];
		}
		self.size--;
	}
	return size - self.size;
}

/**
 * @param selection "The function to determine if it should be kept or not"
 * @return "the number of deleted elements"
 **/
fn usz List.retain_if(&self, ElementPredicate selection)
{
	usz size = self.size;
	for (usz i = size; i > 0; i--)
	{
		if (!selection(&self.entries[i - 1])) continue;
		for (usz j = i; j < size; j++)
		{
			self.entries[j - 1] = self.entries[j];
		}
		self.size--;
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
	if (!self.allocator) self.allocator = mem::heap();
	min_capacity = math::next_power_of_2(min_capacity);
    self.entries = realloc_aligned(self.entries, Type.sizeof * min_capacity, .alignment = Type[1].alignof, .using = self.allocator) ?? null;
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

fn void List.ensure_capacity(&self, usz added = 1) @inline @private
{
	usz new_size = self.size + added;
    if (self.capacity > new_size) return;

	assert(new_size < usz.max / 2U);
	usz new_capacity = self.capacity ? 2U * self.capacity : 16U;
	while (new_size >= new_capacity) new_capacity *= 2U;
	self.reserve(new_capacity);
}

// Functions for equatable types


fn usz! List.index_of(&self, Type type) @if(ELEMENT_IS_EQUATABLE)
{
	foreach (i, v : self)
	{
		if (v == type) return i;
	}
	return SearchResult.MISSING?;
}

fn usz! List.rindex_of(&self, Type type) @if(ELEMENT_IS_EQUATABLE)
{
	foreach_r (i, v : self)
	{
		if (v == type) return i;
	}
	return SearchResult.MISSING?;
}

fn bool List.equals(&self, List other_list) @if(ELEMENT_IS_EQUATABLE)
{
	if (self.size != other_list.size) return false;
	foreach (i, v : self)
	{
		if (v != other_list.entries[i]) return false;
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
		if (v == value) return true;
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
		if (self.entries[i - 1] != value) continue;
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
