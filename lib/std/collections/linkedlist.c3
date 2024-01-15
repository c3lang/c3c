// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of self source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::collections::linkedlist(<Type>);

struct Node @private
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

fn void LinkedList.push(&self, Type value)
{
	self.link_first(value);
}

fn void LinkedList.push_last(&self, Type value)
{
	self.link_last(value);
}

/**
 * @param [&inout] allocator "The allocator to use, defaults to the heap allocator"
 * @return "the initialized list"
 **/
fn LinkedList* LinkedList.new_init(&self, Allocator* allocator = allocator::heap())
{
	*self = { .allocator = allocator };
	return self;
}

/**
 * @param [&inout] allocator "The allocator to use, defaults to the heap allocator"
 * @return "the initialized list"
 **/
fn LinkedList* LinkedList.init_new(&self, Allocator* allocator = allocator::heap()) @deprecated("Replaced by new_init")
{
	return self.new_init(allocator);
}

fn LinkedList* LinkedList.temp_init(&self)
{
	return self.new_init(allocator::temp()) @inline;
}

fn LinkedList* LinkedList.init_temp(&self) @deprecated("Replaced by temp_init")
{
	return self.temp_init() @inline;
}

/**
 * @require self.allocator
 **/
macro void LinkedList.free_node(&self, Node* node) @private
{
	allocator::free(self.allocator, node);
}

macro Node* LinkedList.alloc_node(&self) @private
{
	if (!self.allocator) self.allocator = allocator::heap();
	return allocator::alloc(self.allocator, Node);
}

fn void LinkedList.link_first(&self, Type value) @private
{
	Node *first = self._first;
	Node *new_node = self.alloc_node();
	*new_node = { .next = first, .value = value };
	self._first = new_node;
	if (!first)
	{
		self._last = new_node;
	}
	else
	{
		first.prev = new_node;
	}
	self.size++;
}

fn void LinkedList.link_last(&self, Type value) @private
{
	Node *last = self._last;
	Node *new_node = self.alloc_node();
	*new_node = { .prev = last, .value = value };
	self._last = new_node;
	if (!last)
	{
		self._first = new_node;
	}
	else
	{
		last.next = new_node;
	}
	self.size++;
}

fn Type! LinkedList.peek(&self) => self.first() @inline;
fn Type! LinkedList.peek_last(&self) => self.last() @inline;

fn Type! LinkedList.first(&self)
{
	if (!self._first) return IteratorResult.NO_MORE_ELEMENT?;
	return self._first.value;
}

fn Type! LinkedList.last(&self)
{
	if (!self._last) return IteratorResult.NO_MORE_ELEMENT?;
	return self._last.value;
}

fn void LinkedList.free(&self) => self.clear() @inline;

fn void LinkedList.clear(&self)
{
	for (Node* node = self._first; node != null;)
	{
		Node* next = node.next;
		self.free_node(node);
		node = next;
	}
	self._first = null;
	self._last = null;
	self.size = 0;
}

fn usz LinkedList.len(&self) @inline => self.size;

/**
 * @require index < self.size
 **/
macro Node* LinkedList.node_at_index(&self, usz index)
{
	if (index * 2 >= self.size)
	{
		Node* node = self._last;
		index = self.size - index - 1;
		while (index--) node = node.prev;
		return node;
	}
	Node* node = self._first;
	while (index--) node = node.next;
	return node;
}
/**
 * @require index < self.size
 **/
fn Type LinkedList.get(&self, usz index)
{
	return self.node_at_index(index).value;
}

/**
 * @require index < self.size
 **/
fn void LinkedList.set(&self, usz index, Type element)
{
	self.node_at_index(index).value = element;
}

/**
 * @require index < self.size
 **/
fn void LinkedList.remove(&self, usz index)
{
	self.unlink(self.node_at_index(index));
}

/**
 * @require index <= self.size
 **/
fn void LinkedList.insert(&self, usz index, Type element)
{
	switch (index)
	{
		case 0:
			self.push(element);
		case self.size:
			self.push_last(element);
		default:
			self.link_before(self.node_at_index(index), element);
	}
}
/**
 * @require succ != null
 **/
fn void LinkedList.link_before(&self, Node *succ, Type value) @private
{
	Node* pred = succ.prev;
	Node* new_node = self.alloc_node();
	*new_node = { .prev = pred, .next = succ, .value = value };
	succ.prev = new_node;
	if (!pred)
	{
		self._first = new_node;
	}
	else
	{
		pred.next = new_node;
	}
	self.size++;
}

/**
 * @require self._first
 **/
fn void LinkedList.unlink_first(&self) @private
{
	Node* f = self._first;
	Node* next = f.next;
	self.free_node(f);
	self._first = next;
	if (!next)
	{
		self._last = null;
	}
	else
	{
		next.prev = null;
	}
	self.size--;
}

fn bool LinkedList.remove_value(&self, Type t)
{
	for (Node* node = self._first; node != null; node = node.next)
	{
		if (node.value == t)
		{
			self.unlink(node);
			return true;
		}
	}
	return false;
}

fn bool LinkedList.remove_last_value(&self, Type t)
{
	for (Node* node = self._last; node != null; node = node.prev)
	{
		if (node.value == t)
		{
			self.unlink(node);
			return true;
		}
	}
	return false;
}

fn Type! LinkedList.pop(&self)
{
	if (!self._first) return IteratorResult.NO_MORE_ELEMENT?;
	defer self.unlink_first();
	return self._first.value;
}

fn void! LinkedList.remove_last(&self)
{
	if (!self._first) return IteratorResult.NO_MORE_ELEMENT?;
	self.unlink_last();
}

fn void! LinkedList.remove_first(&self)
{
	if (!self._first) return IteratorResult.NO_MORE_ELEMENT?;
	self.unlink_first();
}

/**
 * @require self._last
 **/
fn void LinkedList.unlink_last(&self) @inline @private
{
	Node* l = self._last;
	Node* prev = l.prev;
	self._last = prev;
	self.free_node(l);
	if (!prev)
	{
		self._first = null;
	}
	else
	{
		prev.next = null;
	}
	self.size--;
}

/**
 * @require x != null
 **/
fn void LinkedList.unlink(&self, Node* x) @private
{
	Node* next = x.next;
	Node* prev = x.prev;
	if (!prev)
	{
		self._first = next;
	}
	else
	{
		prev.next = next;
	}
	if (!next)
	{
		self._last = prev;
	}
	else
	{
		next.prev = prev;
	}
	self.free_node(x);
	self.size--;
}
