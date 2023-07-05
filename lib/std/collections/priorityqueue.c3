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
import std::collections::list;

def Heap = List(<Type>);

struct PriorityQueue
{
	Heap heap;
	bool max;  // true if max-heap, false if min-heap
}

fn void PriorityQueue.push(&self, Type element)
{
	self.heap.push(element);
	usz i = self.heap.len() - 1;
	while (i > 0)
	{
		usz parent = (i - 1) / 2;
		if ((self.max && greater(self.heap.get(i), self.heap.get(parent))) || (!self.max && less(self.heap.get(i), self.heap.get(parent))))
		{
			self.heap.swap(i, parent);
			i = parent;
			continue;
		}
		break;
	}
}

/**
* @require self != null
*/
fn Type! PriorityQueue.pop(&self)
{
	usz i = 0;
	usz len = self.heap.len() @inline;
	if (!len) return IteratorResult.NO_MORE_ELEMENT?;
	usz newCount = len - 1;
	self.heap.swap(0, newCount);
	while ((2 * i + 1) < newCount)
	{
		usz j = 2 * i + 1;
		if (((j + 1) < newCount) &&
			((self.max && greater(self.heap.get(j + 1), self.heap[j]))
				|| (!self.max && less(self.heap.get(j + 1), self.heap.get(j)))))
		{
			j++;
		}
		if ((self.max && less(self.heap.get(i), self.heap.get(j))) || (!self.max && greater(self.heap.get(i), self.heap.get(j))))
		{
			self.heap.swap(i, j);
			i = j;
			continue;
		}
		break;
	}

	return self.heap.pop();
}

fn Type! PriorityQueue.peek(&self)
{
	if (!self.len()) return IteratorResult.NO_MORE_ELEMENT?;
	return self.heap.get(0);
}

fn void PriorityQueue.free(&self)
{
	self.heap.free();
}

fn usz PriorityQueue.len(&self) @operator(len)
{
	return self.heap.len();
}

/**
 * @require index < self.len()
 */
fn Type PriorityQueue.peek_at(&self, usz index) @operator([])
{
	return self.heap[index];
}

fn void! PriorityQueue.to_format(&self, Formatter* formatter) @dynamic
{
	return self.heap.to_format(formatter);
}

fn String PriorityQueue.to_string(&self, Allocator* using = mem::heap()) @dynamic
{
	return self.heap.to_string(using);
}
