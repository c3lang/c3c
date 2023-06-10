// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
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
fn void List.init(List* list, usz initial_capacity = 16, Allocator* using = mem::heap())
{
	list.allocator = using;
	list.size = 0;
	if (initial_capacity > 0)
	{
		initial_capacity = math::next_power_of_2(initial_capacity);
		list.entries = malloc_aligned(Type, initial_capacity, .alignment = Type[1].alignof, .using = using)!!;
	}
	else
	{
		list.entries = null;
	}
	list.capacity = initial_capacity;
}

fn void List.tinit(List* list, usz initial_capacity = 16)
{
	list.init(initial_capacity, mem::temp()) @inline;
}

fn void! List.to_format(List* list, Formatter* formatter) @dynamic
{
	switch (list.size)
	{
		case 0:
			formatter.print("[]")!;
		case 1:
			formatter.printf("[%s]", list.entries[0])!;
		default:
			formatter.print("[")!;
			foreach (i, element : list.entries[:list.size])
			{
				if (i != 0) formatter.print(", ")!;
				formatter.printf("%s", element)!;
			}
			formatter.print("]")!;
	}
}

fn String List.to_string(List* list, Allocator* using = mem::heap()) @dynamic
{
	return string::printf("%s", *list);
}

fn void List.push(List* list, Type element) @inline
{
    list.append(element);
}

fn void List.append(List* list, Type element)
{
    list.ensure_capacity();
    list.entries[list.size++] = element;
}

/**
 * @require list.size > 0
 */
fn Type List.pop(List* list)
{
    return list.entries[--list.size];
}

fn void List.clear(List* list)
{
    list.size = 0;
}

/**
 * @require list.size > 0
 */
fn Type List.pop_first(List* list)
{
    Type value = list.entries[0];
    list.remove_at(0);
    return value;
}

fn void List.remove_at(List* list, usz index)
{
    for (usz i = index + 1; i < list.size; i++)
    {
        list.entries[i - 1] = list.entries[i];
    }
    list.size--;
}

fn void List.add_all(List* list, List* other_list)
{
	if (!other_list.size) return;
	list.reserve(other_list.size);
	foreach (&value : other_list)
	{
	    list.entries[list.size++] = *value;
	}
}


fn Type[] List.to_array(List* list, Allocator* using = mem::heap())
{
	if (!list.size) return Type[] {};
	Type[] result = malloc(Type, list.size, .using = using);
	result[..] = list.entries[:list.size];
	return result;
}

/**
 * Reverse the elements in a list.
 *
 * @param [&inout] list "The list to reverse"
 **/
fn void List.reverse(List* list)
{
	if (list.size < 2) return;
	usz half = list.size / 2U;
	usz end = list.size - 1;
	for (usz i = 0; i < half; i++)
	{
		@swap(list.entries[i], list.entries[end - i]);
	}
}

fn Type[] List.array_view(List* list)
{
	return list.entries[:list.size];
}

fn void List.add_array(List* list, Type[] array)
{
	if (!array.len) return;
	list.reserve(array.len);
	foreach (&value : array)
	{
	    list.entries[list.size++] = *value;
	}
}

fn void List.push_front(List* list, Type type) @inline
{
    list.insert_at(0, type);
}

fn void List.insert_at(List* list, usz index, Type type)
{
    list.ensure_capacity();
    for (usz i = list.size; i > index; i--)
    {
        list.entries[i] = list.entries[i - 1];
    }
    list.size++;
    list.entries[index] = type;
}

/**
 * @require index < list.size
 **/
fn void List.set_at(List* list, usz index, Type type)
{
    list.entries[index] = type;
}

fn void List.remove_last(List* list)
{
    list.size--;
}

fn void List.remove_first(List* list)
{
    list.remove_at(0);
}

fn Type* List.first(List* list)
{
    return list.size ? &list.entries[0] : null;
}

fn Type* List.last(List* list)
{
    return list.size ? &list.entries[list.size - 1] : null;
}

fn bool List.is_empty(List* list)
{
    return !list.size;
}

fn usz List.len(List* list) @operator(len)
{
    return list.size;
}

fn Type List.get(List* list, usz index)
{
    return list.entries[index];
}

fn void List.free(List* list)
{
	if (!list.allocator) return;
    free_aligned(list.entries, .using = list.allocator);
    list.capacity = 0;
    list.size = 0;
    list.entries = null;
}

fn void List.swap(List* list, usz i, usz j)
{
	@swap(list.entries[i], list.entries[j]);
}

/**
 * @param [&inout] list "The list to remove elements from"
 * @param filter "The function to determine if it should be removed or not"
 * @return "the number of deleted elements"
 **/
fn usz List.remove_if(List* list, ElementPredicate filter)
{
	usz size = list.size;
	for (usz i = size; i > 0; i--)
	{
		if (filter(&list.entries[i - 1])) continue;
		for (usz j = i; j < size; j++)
		{
			list.entries[j - 1] = list.entries[j];
		}
		list.size--;
	}
	return size - list.size;
}

/**
 * @param [&inout] list "The list to remove elements from"
 * @param selection "The function to determine if it should be kept or not"
 * @return "the number of deleted elements"
 **/
fn usz List.retain_if(List* list, ElementPredicate selection)
{
	usz size = list.size;
	for (usz i = size; i > 0; i--)
	{
		if (!selection(&list.entries[i - 1])) continue;
		for (usz j = i; j < size; j++)
		{
			list.entries[j - 1] = list.entries[j];
		}
		list.size--;
	}
	return size - list.size;
}


/**
 * Reserve at least min_capacity
 **/
fn void List.reserve(List* list, usz min_capacity)
{
	if (!min_capacity) return;
	if (list.capacity >= min_capacity) return;
	if (!list.allocator) list.allocator = mem::heap();
	min_capacity = math::next_power_of_2(min_capacity);
    list.entries = realloc_aligned(list.entries, Type.sizeof * min_capacity, .alignment = Type[1].alignof, .using = list.allocator) ?? null;
	list.capacity = min_capacity;
}

macro Type List.@item_at(List &list, usz index) @operator([])
{
    return list.entries[index];
}

fn Type* List.get_ref(List* list, usz index) @operator(&[]) @inline
{
    return &list.entries[index];
}

fn void List.ensure_capacity(List* list, usz added = 1) @inline @private
{
	usz new_size = list.size + added;
    if (list.capacity > new_size) return;

	assert(new_size < usz.max / 2U);
	usz new_capacity = list.capacity ? 2U * list.capacity : 16U;
	while (new_size >= new_capacity) new_capacity *= 2U;
	list.reserve(new_capacity);
}

// Functions for equatable types


fn usz! List.index_of(List* list, Type type) @if(ELEMENT_IS_EQUATABLE)
{
	foreach (i, v : list)
	{
		if (v == type) return i;
	}
	return SearchResult.MISSING?;
}

fn usz! List.rindex_of(List* list, Type type) @if(ELEMENT_IS_EQUATABLE)
{
	foreach_r (i, v : list)
	{
		if (v == type) return i;
	}
	return SearchResult.MISSING?;
}

fn bool List.equals(List* list, List other_list) @if(ELEMENT_IS_EQUATABLE)
{
	if (list.size != other_list.size) return false;
	foreach (i, v : list)
	{
		if (v != other_list.entries[i]) return false;
	}
	return true;
}

/**
 * Check for presence of a value in a list.
 *
 * @param [&in] list "the list to find elements in"
 * @param value "The value to search for"
 * @return "True if the value is found, false otherwise"
 **/
fn bool List.contains(List* list, Type value) @if(ELEMENT_IS_EQUATABLE)
{
	foreach (i, v : list)
	{
		if (v == value) return true;
	}
	return false;
}


/**
 * @param [&inout] list "The list to remove elements from"
 * @param value "The value to remove"
 * @return "the number of deleted elements."
 **/
fn usz List.remove(List* list, Type value) @if(ELEMENT_IS_EQUATABLE)
{
	usz size = list.size;
	for (usz i = size; i > 0; i--)
	{
		if (list.entries[i - 1] != value) continue;
		for (usz j = i; j < size; j++)
		{
			list.entries[j - 1] = list.entries[j];
		}
		list.size--;
	}
	return size - list.size;
}

fn void List.remove_all(List* list, List* other_list) @if(ELEMENT_IS_EQUATABLE)
{
	if (!other_list.size) return;
	foreach (v : other_list) list.remove(v);
}

/**
 * @param [&in] list
 * @return "The number non-null values in the list"
 **/
fn usz List.compact_count(List* list) @if(ELEMENT_IS_POINTER)
{
	usz vals = 0;
	foreach (v : list) if (v) vals++;
	return vals;
}

fn usz List.compact(List* list) @if(ELEMENT_IS_POINTER)
{
	usz size = list.size;
	for (usz i = size; i > 0; i--)
	{
		if (list.entries[i - 1]) continue;
		for (usz j = i; j < size; j++)
		{
			list.entries[j - 1] = list.entries[j];
		}
		list.size--;
	}
	return size - list.size;
}
