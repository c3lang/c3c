// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::array::list<Type>;

struct List
{
    usz size;
    usz capacity;
    Type *entries;
}

private fn void List.ensure_capacity(List *list) @inline
{
    if (list.capacity == list.size)
    {
        list.capacity = list.capacity ? 2 * list.capacity : 16;
        list.entries = realloc(list.entries, Type.sizeof * list.capacity);
    }
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
    free(list.entries);
    list.capacity = 0;
    list.size = 0;
    list.entries = null;
}

fn void List.swap(List *list, usz i, usz j)
{
	@swap(list.entries[i], list.entries[j]);
}

macro Type List.@item_at(List &list, usz index) @operator([])
{
    return list.entries[index];
}

macro Type* List.@item_ref(List &list, usz index) @operator(&[])
{
    return &list.entries[index];
}
