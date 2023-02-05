// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::collections::linkedlist<Type>;

private struct Node
{
    Node *next;
    Node *prev;
    Type value;
}

struct LinkedList
{
    Allocator *allocator;
    usz size;
    Node *_first;
    Node *_last;
}

fn void LinkedList.push(LinkedList* list, Type value)
{
    list.link_first(value);
}

fn void LinkedList.push_last(LinkedList* list, Type value)
{
	list.link_last(value);
}

fn void LinkedList.init(LinkedList* list, Allocator* alloc = mem::current_allocator())
{
	*list = { .allocator = alloc };
}

fn void LinkedList.tinit(LinkedList* list) => list.init(mem::temp_allocator()) @inline;

/**
 * @require list.allocator
 **/
private macro void LinkedList.@free_node(LinkedList &list, Node* node)
{
	list.allocator.free(node)!!;
}
private macro Node* LinkedList.@alloc_node(LinkedList &list)
{
	if (!list.allocator) list.allocator = mem::current_allocator();
	return list.allocator.alloc(Node.sizeof)!!;
}

private fn void LinkedList.link_first(LinkedList* list, Type value)
{
    Node *first = list._first;
    Node *new_node = list.@alloc_node();
    *new_node = { .next = first, .value = value };
    list._first = new_node;
    if (!first)
    {
        list._last = new_node;
    }
    else
    {
        first.prev = new_node;
    }
    list.size++;
}

private fn void LinkedList.link_last(LinkedList* list, Type value)
{
    Node *last = list._last;
    Node *new_node = list.@alloc_node();
    *new_node = { .prev = last, .value = value };
    list._last = new_node;
    if (!last)
    {
        list._first = new_node;
    }
    else
    {
        last.next = new_node;
    }
    list.size++;
}

fn Type! peek(LinkedList* list) => list.first() @inline;
fn Type! peek_last(LinkedList* list) => list.last() @inline;

fn Type! LinkedList.first(LinkedList *list)
{
	if (!list._first) return IteratorResult.NO_MORE_ELEMENT!;
	return list._first.value;
}

fn Type! LinkedList.last(LinkedList* list)
{
	if (!list._last) return IteratorResult.NO_MORE_ELEMENT!;
	return list._last.value;
}

fn void LinkedList.free(LinkedList* list) => list.clear() @inline;

fn void LinkedList.clear(LinkedList* list)
{
    for (Node* node = list._first; node != null;)
    {
        Node* next = node.next;
        list.@free_node(node);
        node = next;
    }
    list._first = null;
    list._last = null;
    list.size = 0;
}

fn usz LinkedList.len(LinkedList* list) @inline => list.size;

/**
 * @require index < list.size
 **/
macro Node* LinkedList.node_at_index(LinkedList* list, usz index)
{
	if (index * 2 >= list.size)
	{
	    Node* node = list._last;
	    index = list.size - index - 1;
        while (index--) node = node.prev;
        return node;
	}
    Node* node = list._first;
    while (index--) node = node.next;
    return node;
}
/**
 * @require index < list.size
 **/
fn Type LinkedList.get(LinkedList* list, usz index)
{
	return list.node_at_index(index).value;
}

/**
 * @require index < list.size
 **/
fn void LinkedList.set(LinkedList* list, usz index, Type element)
{
	list.node_at_index(index).value = element;
}

/**
 * @require index < list.size
 **/
fn void LinkedList.remove(LinkedList* list, usz index)
{
	list.unlink(list.node_at_index(index));
}

/**
 * @require index <= list.size
 **/
fn void LinkedList.insert(LinkedList* list, usz index, Type element)
{
	switch (index)
	{
		case 0:
			list.push(element);
		case list.size:
			list.push_last(element);
		default:
			list.link_before(list.node_at_index(index), element);
	}
}
/**
 * @require succ != null
 **/
private fn void LinkedList.link_before(LinkedList *list, Node *succ, Type value)
{
    Node* pred = succ.prev;
    Node* new_node = mem::alloc(Node);
    *new_node = { .prev = pred, .next = succ, .value = value };
    succ.prev = new_node;
    if (!pred)
    {
        list._first = new_node;
    }
    else
    {
        pred.next = new_node;
    }
    list.size++;
}

/**
 * @require list && list._first
 **/
private fn void LinkedList.unlink_first(LinkedList* list)
{
	Node* f = list._first;
    Node* next = f.next;
    list.@free_node(f);
    list._first = next;
    if (!next)
    {
        list._last = null;
    }
    else
    {
        next.prev = null;
    }
    list.size--;
}

fn bool LinkedList.remove_value(LinkedList* list, Type t)
{
    for (Node* node = list._first; node != null; node = node.next)
    {
        if (node.value == t)
        {
            list.unlink(node);
            return true;
        }
    }
    return false;
}

fn bool LinkedList.remove_last_value(LinkedList* list, Type t)
{
    for (Node* node = list._last; node != null; node = node.prev)
    {
        if (node.value == t)
        {
            list.unlink(node);
            return true;
        }
    }
    return false;
}

/**
 * @param [&inout] list
 **/
fn Type! LinkedList.pop(LinkedList* list)
{
	if (!list._first) return IteratorResult.NO_MORE_ELEMENT!;
	defer list.unlink_first();
	return list._first.value;
}

/**
 * @param [&inout] list
 **/
fn void! LinkedList.remove_last(LinkedList* list)
{
	if (!list._first) return IteratorResult.NO_MORE_ELEMENT!;
	list.unlink_last();
}

/**
 * @param [&inout] list
 **/
fn void! LinkedList.remove_first(LinkedList* list)
{
	if (!list._first) return IteratorResult.NO_MORE_ELEMENT!;
	list.unlink_first();
}

/**
 * @param [&inout] list
 * @require list._last
 **/
private fn void LinkedList.unlink_last(LinkedList *list) @inline
{
	Node* l = list._last;
    Node* prev = l.prev;
    list._last = prev;
    list.@free_node(l);
    if (!prev)
    {
        list._first = null;
    }
    else
    {
        prev.next = null;
    }
    list.size--;
}

/**
 * @require list != null, x != null
 **/
private fn void LinkedList.unlink(LinkedList* list, Node* x)
{
    Node* next = x.next;
    Node* prev = x.prev;
    if (!prev)
    {
        list._first = next;
    }
    else
    {
        prev.next = next;
    }
    if (!next)
    {
        list._last = prev;
    }
    else
    {
        next.prev = prev;
    }
    list.@free_node(x);
    list.size--;
}
