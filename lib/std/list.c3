// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::array::list<Type>;
import std::math;

struct List
{
    usz size;
    usz capacity;
    Allocator *allocator;
    Type *entries;
}

/**
 * @require allocator != null "A valid allocator must be provided"
 **/
fn void List.init(List* list, usz initial_capacity = 16, Allocator* allocator = mem::current_allocator())
{
	list.allocator = allocator;
	list.size = 0;
	if (initial_capacity > 0)
	{
		initial_capacity = math::next_power_of_2(initial_capacity);
		list.entries = allocator.alloc_aligned(Type.sizeof * initial_capacity, Type[1].alignof)!!;
	}
	else
	{
		list.entries = null;
	}
	list.capacity = initial_capacity;
}

fn void List.tinit(List* list, usz initial_capacity = 16)
{
	list.init(initial_capacity, mem::temp_allocator()) @inline;
}

fn void List.push(List *list, Type element) @inline
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

/**
 * @require list.size > 0
 */
fn Type List.pop_first(List *list)
{
    Type value = list.entries[0];
    list.remove_at(0);
    return value;
}

fn void List.remove_at(List *list, usz index)
{
    for (usz i = index + 1; i < list.size; i++)
    {
        list.entries[i - 1] = list.entries[i];
    }
    list.size--;
}

fn void List.push_front(List *list, Type type) @inline
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

fn void List.remove_last(List* list)
{
    list.size--;
}

fn void List.remove_first(List *list)
{
    list.remove_at(0);
}

fn Type* List.first(List *list)
{
    return list.size ? &list.entries[0] : null;
}

fn Type* List.last(List *list)
{
    return list.size ? &list.entries[list.size - 1] : null;
}

fn bool List.is_empty(List *list)
{
    return !list.size;
}

fn usz List.len(List *list) @operator(len)
{
    return list.size;
}

fn Type List.get(List *list, usz index)
{
    return list.entries[index];
}

fn void List.free(List *list)
{
	if (!list.allocator) return;
    list.allocator.free_aligned(list.entries)!!;
    list.capacity = 0;
    list.size = 0;
    list.entries = null;
}

fn void List.swap(List *list, usz i, usz j)
{
	@swap(list.entries[i], list.entries[j]);
}

/**
 * Reserve at least min_capacity
 **/
fn void List.reserve(List *list, usz min_capacity)
{
	if (!min_capacity) return;
	if (list.capacity >= min_capacity) return;
	if (!list.allocator) list.allocator = mem::temp_allocator();
	min_capacity = math::next_power_of_2(min_capacity);
    list.entries = list.allocator.realloc_aligned(list.entries, Type.sizeof * min_capacity, Type[1].alignof) ?? null;
	list.capacity = min_capacity;
}

macro Type List.@item_at(List &list, usz index) @operator([])
{
    return list.entries[index];
}

macro Type* List.@item_ref(List &list, usz index) @operator(&[])
{
    return &list.entries[index];
}


private fn void List.ensure_capacity(List *list) @inline
{
    if (list.capacity == list.size)
    {
        list.reserve(list.capacity ? 2 * list.capacity : 16);
    }
}
