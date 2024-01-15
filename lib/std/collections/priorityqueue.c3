//  priorityqueue.c3
//  A priority queue using a classic binary heap for C3.
//
//  Copyright (c) 2022 David Kopec
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.
module std::collections::priorityqueue(<Type>);
import std::collections::priorityqueue::private;

distinct PriorityQueue = inline PrivatePriorityQueue(<Type, false>);
distinct PriorityQueueMax = inline PrivatePriorityQueue(<Type, true>);

module std::collections::priorityqueue::private(<Type, MAX>);
import std::collections::list;

def Heap = List(<Type>);

struct PrivatePriorityQueue (Printable)
{
	Heap heap;
}

fn void PrivatePriorityQueue.init_new(&self, usz initial_capacity = 16, Allocator* allocator = allocator::heap()) @inline @deprecated("Replaced by new_init")
{
	return self.new_init(initial_capacity, allocator);
}

fn void PrivatePriorityQueue.new_init(&self, usz initial_capacity = 16, Allocator* allocator = allocator::heap()) @inline
{
	self.heap.new_init(initial_capacity, allocator);
}

fn void PrivatePriorityQueue.temp_init(&self, usz initial_capacity = 16) @inline
{
	self.heap.new_init(initial_capacity, allocator::temp()) @inline;
}

fn void PrivatePriorityQueue.init_temp(&self, usz initial_capacity = 16) @inline @deprecated("Replaced by temp_init")
{
	return self.temp_init(initial_capacity) @inline;
}

fn void PrivatePriorityQueue.push(&self, Type element)
{
	self.heap.push(element);
	usz i = self.heap.len() - 1;
	while (i > 0)
	{
		usz parent = (i - 1) / 2;
		Type item = self.heap[i];
		Type parent_item = self.heap[parent];
		$if MAX:
			bool ok = greater(item, parent_item);
		$else
			bool ok = less(item, parent_item);
		$endif
		if (!ok) break;
		self.heap.swap(i, parent);
		i = parent;
	}
}

/**
* @require self != null
*/
fn Type! PrivatePriorityQueue.pop(&self)
{
	usz i = 0;
	usz len = self.heap.len();
	if (!len) return IteratorResult.NO_MORE_ELEMENT?;
	usz new_count = len - 1;
	self.heap.swap(0, new_count);
	while OUTER: ((2 * i + 1) < new_count)
	{
		usz j = 2 * i + 1;
		Type left = self.heap[j];
		Type item = self.heap[i];
		switch
		{
			case j + 1 < new_count:
				Type right = self.heap[j + 1];
				$if MAX:
				if (!greater(right, left)) nextcase;
				if (!greater(right, item)) break OUTER;
				$else
				if (!greater(left, right)) nextcase;
				if (!greater(item, right)) break OUTER;
				$endif
				j++;
			default:
				$if MAX:
				if (!greater(left, item)) break OUTER;
				$else
				if (!greater(item, left)) break OUTER;
				$endif
		}
		self.heap.swap(i, j);
		i = j;
	}

	return self.heap.pop();
}

fn Type! PrivatePriorityQueue.peek(&self)
{
	if (!self.len()) return IteratorResult.NO_MORE_ELEMENT?;
	return self.heap.get(0);
}

fn void PrivatePriorityQueue.free(&self)
{
	self.heap.free();
}

fn usz PrivatePriorityQueue.len(&self) @operator(len)
{
	return self.heap.len();
}

fn bool PrivatePriorityQueue.is_empty(&self)
{
	return self.heap.is_empty();
}

/**
 * @require index < self.len()
 */
fn Type PrivatePriorityQueue.peek_at(&self, usz index) @operator([])
{
	return self.heap[index];
}

fn usz! PrivatePriorityQueue.to_format(&self, Formatter* formatter) @dynamic
{
	return self.heap.to_format(formatter);
}

fn String PrivatePriorityQueue.to_new_string(&self, Allocator* allocator = allocator::heap()) @dynamic
{
	return self.heap.to_new_string(allocator);
}

