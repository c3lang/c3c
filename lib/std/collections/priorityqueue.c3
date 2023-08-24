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

def PriorityQueue = distinct inline PrivatePriorityQueue(<Type, false>);
def PriorityQueueMax = distinct inline PrivatePriorityQueue(<Type, true>);

module std::collections::priorityqueue::private(<Type, MAX>);
import std::collections::list;

def Heap = List(<Type>);

struct PrivatePriorityQueue
{
	Heap heap;
}

fn void PrivatePriorityQueue.init(&self, usz initial_capacity = 16, Allocator* using = mem::heap()) @inline
{
	self.heap.init(initial_capacity, using);
}

fn void PrivatePriorityQueue.tinit(&self, usz initial_capacity = 16) @inline
{
	self.init(initial_capacity, mem::temp());
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
	usz newCount = len - 1;
	self.heap.swap(0, newCount);
	while ((2 * i + 1) < newCount)
	{
		usz j = 2 * i + 1;
		Type itemj = self.heap[j];
		if ((j + 1) < newCount)
		{
			Type nextj = self.heap[j + 1];
			$if MAX:
				bool ok = greater(nextj, itemj);
			$else
				bool ok = less(nextj, itemj);
			$endif
			if (ok) j++;
		}
		Type item = self.heap[i];
		$if MAX:
			bool ok = less(item, itemj);
		$else
			bool ok = greater(item, itemj);
		$endif
		if (!ok) break;
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

fn String PrivatePriorityQueue.to_string(&self, Allocator* using = mem::heap()) @dynamic
{
	return self.heap.to_string(using);
}