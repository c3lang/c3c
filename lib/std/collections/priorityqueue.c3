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
module std::collections::priorityqueue<Type>;
import std::collections::list;

def Heap = List<Type>;

struct PriorityQueue
{
	Heap heap;
	bool max;  // true if max-heap, false if min-heap
}

fn void PriorityQueue.push(PriorityQueue* pq, Type element)
{
	pq.heap.push(element);
	usz i = pq.heap.len() - 1;
	while (i > 0)
	{
		usz parent = (i - 1) / 2;
		if ((pq.max && greater(pq.heap.get(i), pq.heap.get(parent))) || (!pq.max && less(pq.heap.get(i), pq.heap.get(parent))))
		{
			pq.heap.swap(i, parent);
			i = parent;
			continue;
		}
		break;
	}
}

/**
* @require pq != null
*/
fn Type! PriorityQueue.pop(PriorityQueue* pq)
{
	usz i = 0;
	usz len = pq.heap.len() @inline;
	if (!len) return IteratorResult.NO_MORE_ELEMENT?;
	usz newCount = len - 1;
	pq.heap.swap(0, newCount);
	while ((2 * i + 1) < newCount)
	{
		usz j = 2 * i + 1;
		if (((j + 1) < newCount) &&
			((pq.max && greater(pq.heap.get(j + 1), pq.heap[j]))
				|| (!pq.max && less(pq.heap.get(j + 1), pq.heap.get(j)))))
		{
			j++;
		}
		if ((pq.max && less(pq.heap.get(i), pq.heap.get(j))) || (!pq.max && greater(pq.heap.get(i), pq.heap.get(j))))
		{
			pq.heap.swap(i, j);
			i = j;
			continue;
		}
		break;
	}

	return pq.heap.pop();
}

/**
 * @require pq != null
 */
fn Type! PriorityQueue.peek(PriorityQueue* pq)
{
	if (!pq.len()) return IteratorResult.NO_MORE_ELEMENT?;
	return pq.heap.get(0);
}

/**
 * @require pq != null
 */
fn void PriorityQueue.free(PriorityQueue* pq)
{
	pq.heap.free();
}

/**
 * @require pq != null
 */
fn usz PriorityQueue.len(PriorityQueue* pq) @operator(len)
{
	return pq.heap.len();
}

/**
 * @require pq != null, index < pq.len()
 */
fn Type PriorityQueue.peek_at(PriorityQueue* pq, usz index) @operator([])
{
	return pq.heap[index];
}
