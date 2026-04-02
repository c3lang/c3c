<*
 @require types::is_int($typeof(CAPACITY)) && CAPACITY > 0 : "Capacity must be bigger than 0"
*>
module std::collections::ringbuffer <Type, CAPACITY>;
import std::io;
import std::math;

faultdef RING_BUFFER_EMPTY;

<*
 Ring buffer is a queue-like, FIFO, fixed size (stack allocated) data structure.
 The useful property of a ring buffer is that it does not need to have its elements shuffled around (mem::move) when one is consumed by 'pop' operation.
 User can 'push' indefinitely, but subsequent pushes can override the oldest values.
 It is slightly more optimized when CAPACITY is a power of 2.
 write - points to the next write location (push).
 read - points to the current read location (pop).
*>
struct RingBuffer (Printable)
{
	Type[CAPACITY] buf;
	isz read;
	isz write;
}

fn void RingBuffer.init(&self) @inline
{
	*self = { };
}

fn void RingBuffer.init_from_values(&self, Type[] values)
{
	*self = { };
	self.push_all(values);
}

<*
 Pushes a new element to the end of the ring buffer.
 Moves the write pointer, and possibly read pointer.
*>
fn void RingBuffer.push(&self, Type new_el)
{
	self.buf[self.write] = new_el;
	self.write = math::@wrap_add(self.write, CAPACITY);
	if (self.write == self.read)
	{
		self.read = math::@wrap_add(self.read, CAPACITY);
	}
}

<*
 Pops the element from the start of the ring buffer.
 Moves the read pointer.
*>
fn Type? RingBuffer.pop(&self)
{
	if (@unlikely(self.is_empty()))
	{
		return RING_BUFFER_EMPTY~;
	}
	return pop_private(self);
}

<*
 Returns the element at the start of the ring buffer.
 Without changing its state.
*>
fn Type? RingBuffer.peek(&self)
{
	if (@unlikely(self.is_empty()))
	{
		return RING_BUFFER_EMPTY~;
	}
	Type el = self.buf[self.read];
	return el;
}

<*
 Performs subsequent 'push' calls on each value of 'buff'
 @require in_buf.len > 0 : "Buffer length should be greater than 0"
 @param [in] in_buf : "Buffer data will be written from"
*>
fn void RingBuffer.push_all(&self, Type[] in_buf)
{
	foreach (el : in_buf)
	{
		self.push(el);
	}
}

<*
 Performs subsequent 'pop' calls and writes the output into 'out_buf'
 Count of the popped elements calculated by taking the 'min'
 on the input buffer length and ring buffer's internal buffer length
 @param [out] out_buf : "Preallocated buffer into which data will be read"
 @require out_buf.len > 0 : "Buffer len should be greater than 0"
 @return "Count of values written into out_buf"
*>
fn usz RingBuffer.pop_many(&self, Type[] out_buf)
{
	if (@unlikely(self.is_empty()))
	{
		return 0;
	}
	usz len = min(out_buf.len, self.size());
	isz i = 0;
	for (; i < len; ++i)
	{
		out_buf[i] = pop_private(self);
	}
	return i;
}

<*
 Calculates count of elements in ring buffer.
*>
fn usz RingBuffer.size(&self)
{
	if (!is_wrapped(self))
	{
		return self.write - self.read;
	}
	else
	{
		return self.write + (self.buf.len - self.read);
	}
}

<*
 Tells if the ring buffer is currently empty.
*>
fn bool RingBuffer.is_empty(&self) @inline
{
	return self.write == self.read;
}

<*
 Tells if the ring buffer is currently full.
*>
fn bool RingBuffer.is_full(&self) @inline
{
	return math::@wrap_sub(self.read, CAPACITY) == self.write;
}

<*
 Allocates a slice and copies the contents from the ring buffer in push order.
 The user is responsible for freeing the memory.
*>
fn Type[]? RingBuffer.into_slice(&self, Allocator alloc)
{
	if (@unlikely(self.is_empty()))
	{
		return RING_BUFFER_EMPTY~;
	}

	if (self.write > self.read)
	{
		Type[] slice = alloc::alloc_array(alloc, Type, size_assume_not_wrapped(self));
		slice[..] = view_not_wrapped(self)[..];
		return slice;
	}

	Type[] slice = alloc::alloc_array(alloc, Type, size_assume_wrapped(self));
	isz i = self.read;
	isz j = 0;

	for (; i < CAPACITY; ++i)
	{
		slice[j] = self.buf[i];
		++j;
	}
	for (i = 0; i < self.write; ++i)
	{
		slice[j] = self.buf[i];
		++j;
	}
	return slice;
}

fn isz? RingBuffer.to_format(&self, Formatter* formatter) @dynamic
{
	if (@unlikely(self.is_empty()))
	{
		return formatter.print("[]")!;
	}
	else if (self.write > self.read)
	{
		return formatter.printf("%s", view_not_wrapped(self))!;
	}
	else
	{
		Type[] view = self.into_slice(tmem)!!;
		defer alloc::free(tmem, view);
		return formatter.printf("%s", view)!;
	}
}

// ----- Private API -----

<*
 More optimized version of 'pop', when we know that the size is not 0.
 Not for public use.
 @require self.size() > 0 : "Should be bigger than zero"
*>
fn Type pop_private(RingBuffer* self) @local
{
	Type el = self.buf[self.read];
	self.read = math::@wrap_add(self.read, CAPACITY);
	return el;
}

fn bool is_wrapped(RingBuffer* self) @local @inline
{
	return self.read > self.write;
}

<*
 @require !is_wrapped(self)
*>
fn isz size_assume_not_wrapped(RingBuffer* self) @local @inline
{
	return self.write - self.read;
}

<*
 @require is_wrapped(self)
*>
fn isz size_assume_wrapped(RingBuffer* self) @local @inline
{
	return self.write + (self.buf.len - self.read);
}

<*
 @require self.read < self.write
*>
fn Type[] view_not_wrapped(RingBuffer* self) @local @inline
{
	return self.buf[self.read..self.write - 1];
}
