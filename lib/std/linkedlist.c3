// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::array::linkedlist<Type>;

private struct Node
{
    Node *next;
    Node *prev;
    Type value;
}

struct LinkedList
{
    usz size;
    Node *first;
    Node *last;
}

fn void LinkedList.push(LinkedList *list, Type value)
{
    list.linkLast(value);
}

private fn void LinkedList.linkFirst(LinkedList *list, Type value)
{
    Node *first = list.first;
    Node *new_node = mem::alloc(Node);
    *new_node = { .next = first, .value = value };
    list.first = new_node;
    if (!first)
    {
        list.last = new_node;
    }
    else
    {
        first.prev = new_node;
    }
    list.size++;
}

private fn void LinkedList.linkLast(LinkedList *list, Type value)
{
    Node *last = list.last;
    Node *new_node = mem::alloc(Node);
    *new_node = { .prev = last, .value = value };
    list.last = new_node;
    if (!last)
    {
        list.first = new_node;
    }
    else
    {
        last.next = new_node;
    }
    list.size++;
}

fn void LinkedList.free(LinkedList *list)
{
    for (Node* node = list.first; node != null;)
    {
        Node* next = node.next;
        free(node);
        node = next;
    }
    list.first = null;
    list.last = null;
    list.size = 0;
}

fn usz LinkedList.len(LinkedList* list) @inline
{
    return list.size;
}

fn Type LinkedList.get(LinkedList* list, usz index)
{
    Node* node = list.first;
    while (index--)
    {
        node = node.next;
    }
    return node.value;
}
/**
 * @require succ != null
 **/
private fn void LinkedList.linkBefore(LinkedList *list, Node *succ, Type value)
{
    Node* pred = succ.prev;
    Node* new_node = mem::alloc(Node);
    *new_node = { .prev = pred, .next = succ, .value = value };
    succ.prev = new_node;
    if (!pred)
    {
        list.first = new_node;
    }
    else
    {
        pred.next = new_node;
    }
    list.size++;
}

/**
 * @require f == list.first && f != null
 **/
private fn void unlinkFirst(LinkedList* list, Node* f)
{
    Node* next = f.next;
    free(f);
    list.first = next;
    if (!next)
    {
        list.last = null;
    }
    else
    {
        next.prev = null;
    }
    list.size--;
}

/**
 * @require l == list.last && l != null
 **/
private fn void LinkedList.unlinkLast(LinkedList *list, Node* l)
{
    Node* prev = l.prev;
    list.last = prev;
    free(l);
    if (!prev)
    {
        list.first = null;
    }
    else
    {
        prev.next = null;
    }
    list.size--;
}

/**
 * @require x != null
 **/
private fn void LinkedList.unlink(LinkedList* list, Node* x)
{
    Node* next = x.next;
    Node* prev = x.prev;
    if (!prev)
    {
        list.first = next;
    }
    else
    {
        prev.next = next;
    }
    if (!next)
    {
        list.last = prev;
    }
    else
    {
        next.prev = prev;
    }
    free(x);
    list.size--;
}
