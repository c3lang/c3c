<*
 @require types::is_int($typeof(CAPACITY)) && CAPACITY > 0 : "Capacity must be bigger than 0"
*>
module std::collections::ringlist <Type, CAPACITY>;
import std::io, std::math;

<*
 Ring list is a queue-like, FIFO, fixed size (stack allocated) data structure.
 The useful property of a ring list is that it does not need to have its elements shuffled around (mem::move) when one is consumed by 'pop' operation.
 User can 'push' indefinitely, but subsequent pushes can override the oldest values.
 tail - points to the next write location (push).
 head - points to the current read location (pop).
*>
struct RingList (Printable)
{
	Type[CAPACITY] buf;
	sz head;
	sz tail;
}

fn void RingList.init(&self) @inline
{
	*self = { };
}

fn void RingList.init_from_values(&self, Type[] values)
{
	*self = { };
	self.push_all(values);
}

<*
 Pushes a new element to the end of the ring list.
 Moves the tail pointer, and possibly head pointer.
*>
fn void RingList.push(&self, Type element)
{
	self.buf[self.tail] = element;
	@inc(self.tail);
	if (self.tail == self.head) @inc(self.head);
}

<*
 Pops the element from the start of the ring list.
 Moves the head index.
*>
fn Type? RingList.pop_front(&self)
{
	if (@unlikely(self.is_empty())) return NO_MORE_ELEMENT~;
	return _pop(self);
}

<*
 Returns the element at the start of the ring list.
 Without changing its state.
*>
fn Type? RingList.peek(&self)
{
	if (@unlikely(self.is_empty())) return NO_MORE_ELEMENT~;
	return self.buf[self.head];
}

<*
 Performs subsequent 'push' calls on each value in the array or slice

 @param [in] array : "Array to push elements from"
*>
fn void RingList.push_all(&self, Type[] array)
{
	foreach (e : array) self.push(e);
}

<*
 Performs subsequent 'pop' calls and writes the output into 'target'
 Count of the popped elements calculated by taking the 'min'
 on the input array length and ring lists's internal length

 @param [out] target : "Slice into which data will be read"
 @return "The resulting slice"
*>
fn Type[] RingList.pop_front_into(&self, Type[] target)
{
	if (@unlikely(self.is_empty())) return target[:0];
	sz len = min(target.len, self.len());
	for (sz i = 0; i < len; i++)
	{
		target[i] = _pop(self);
	}
	return target[:len];
}

<*
 Calculates count of elements in ring list.
*>
fn sz RingList.len(&self)
{
	return self.head > self.tail ? self.tail + self.buf.len - self.head : self.tail - self.head;
}

<*
 Tells if the ring list is currently empty.
*>
fn bool RingList.is_empty(&self) @inline
{
	return self.head == self.tail;
}

<*
 Tells if the ring list is currently full, it is full 1 slot behind actually being full or we can't detect it.
*>
fn bool RingList.is_full(&self) @inline
{
	return self.head > 0 ? self.tail == self.head - 1 : self.tail == CAPACITY - 1;
}

<*
 Allocates a slice and copies the contents from the ring list in push order.
 The user is responsible for freeing the memory.
*>
fn Type[]? RingList.copy_to_array(&self, Allocator alloc)
{
	if (@unlikely(self.is_empty())) return {};

	sz len = self.len();
	Type[] slice = alloc::alloc_array(alloc, Type, len);
	if (self.tail > self.head)
	{
		slice[..] = self.buf[self.head:len];
		return slice;
	}
	sz front_tail = self.buf.len - self.head;
	slice[:front_tail] = self.buf[self.head:front_tail];
	slice[front_tail..] = self.buf[0:len - front_tail];
	return slice;
}

fn sz? RingList.to_format(&self, Formatter* formatter) @dynamic
{
	if (self.tail >= self.head)
    {
        return formatter.printf("%s", self.buf[self.head..self.tail - 1]);
    }
	sz front_tail = self.buf.len - self.head;
	sz total = formatter.printf("[%s", self.buf[self.head])!;
	foreach (e : self.buf[self.head + 1:front_tail - 1])
	{
		total += formatter.printf(", %s", e)!;
	}
	foreach (e : self.buf[0..self.tail - 1])
	{
		total += formatter.printf(", %s", e)!;
	}
	return total + formatter.print("]");
}

// ----- Private API -----

<*
 More optimized version of 'pop', when we know that the size is not 0.
 Not for public use.
 @require self.len() > 0 : "Should be bigger than zero"
*>
fn Type _pop(RingList* self) @local
{
	defer @inc(self.head);
	return self.buf[self.head];
}

macro void @inc(sz #value) @local
{
	#value = #value == CAPACITY - 1 ? 0 : #value + 1;
}
