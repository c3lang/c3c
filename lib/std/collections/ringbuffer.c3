<*
 @require Type.kindof == ARRAY : "Required an array type"
*>
module std::collections::ringbuffer{Type};
import std::io;

alias Element = $typeof((Type){}[0]);

struct RingBuffer (Printable)
{
	Type buf;
	usz written;
	usz head;
}

fn void RingBuffer.init(&self) @inline
{
	*self = {};
}

fn void RingBuffer.push(&self, Element c)
{
	if (self.written < self.buf.len)
	{
		self.buf[self.written] = c;
		self.written++;
	}
	else
	{
		self.buf[self.head] = c;
		self.head = (self.head + 1) % self.buf.len;
	}
}

fn Element RingBuffer.get(&self, usz index) @operator([])
{
	index %= self.buf.len;
	usz avail = self.buf.len - self.head;
	if (index < avail)
	{
		return self.buf[self.head + index];
	}
	return self.buf[index - avail];
}

fn Element? RingBuffer.pop(&self)
{
	switch
	{
		case self.written == 0:
			return NO_MORE_ELEMENT?;
		case self.written < self.buf.len:
			self.written--;
			return self.buf[self.written];
		default:
			self.head = (self.head - 1) % self.buf.len;
			return self.buf[self.head];
	}
}

fn usz? RingBuffer.to_format(&self, Formatter* format) @dynamic
{
	// Improve this?
	return format.printf("%s", self.buf);
}

fn usz RingBuffer.read(&self, usz index, Element[] buffer)
{
	index %= self.buf.len;
	if (self.written < self.buf.len)
	{
		if (index >= self.written) return 0;
		usz end = self.written - index;
		usz n = min(end, buffer.len);
		buffer[:n] = self.buf[index:n];
		return n;
	}
	usz end = self.buf.len - self.head;
	if (index >= end)
	{
		index -= end;
		if (index >= self.head) return 0;
		usz n = min(self.head - index, buffer.len);
		buffer[:n] = self.buf[index:n];
		return n;
	}
	if (buffer.len <= self.buf.len - index)
	{
		usz n = buffer.len;
		buffer[:n] = self.buf[self.head + index:n];
		return n;
	}
	usz n1 = self.buf.len - index;
	buffer[:n1] = self.buf[self.head + index:n1];
	buffer = buffer[n1..];
	index -= n1;
	usz n2 = min(self.head - index, buffer.len);
	buffer[:n2] = self.buf[index:n2];
	return n1 + n2;
}

fn void RingBuffer.write(&self, Element[] buffer)
{
	usz i;
	while (self.written < self.buf.len && i < buffer.len)
	{
		self.buf[self.written] = buffer[i++];
		self.written++;
	}
	foreach (c : buffer[i..])
	{
		self.buf[self.head] = c;
		self.head = (self.head + 1) % self.buf.len;
	}
}