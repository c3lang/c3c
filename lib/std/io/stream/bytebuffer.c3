module std::io;
import std::math;

struct ByteBuffer
{
	Allocator* allocator;
	usz max_read;
	char[] bytes;
	usz read_idx;
	usz write_idx;
	bool has_last;
}

/**
 * ByteBuffer provides a streamable read/write buffer.
 * max_read defines how many bytes might be kept before its internal buffer is shrinked.
 * @require self.bytes.len == 0 "Buffer already initialized."
 **/
fn void! ByteBuffer.init(&self, usz max_read, usz initial_capacity = 16, Allocator* using = mem::heap())
{
	*self = { .allocator = using, .max_read = max_read };
	initial_capacity = max(initial_capacity, 16);
	self.grow(initial_capacity)!;
}

fn void! ByteBuffer.tinit(&self, usz max_read, usz initial_capacity = 16)
{
	self.init(max_read, initial_capacity, mem::temp())!;
}

fn void! ByteBuffer.free(&self)
{
	self.allocator.free(self.bytes)!;
	*self = {};
}

fn Stream ByteBuffer.as_stream(&self)
{
	return { .fns = &bytebuffer_interface, .data = self };
}

StreamInterface bytebuffer_interface = {
	.write_fn = fn(s, char[] bytes) => ((ByteBuffer*)s.data).write(bytes),
	.write_byte_fn = fn(s, c) => ((ByteBuffer*)s.data).write_byte(c),
	.read_fn = fn(s, char[] bytes) => ((ByteBuffer*)s.data).read(bytes),
	.read_byte_fn = fn(s) => ((ByteBuffer*)s.data).read_byte(),
	.pushback_byte_fn = fn(s) => ((ByteBuffer*)s.data).pushback_byte(),
	.available_fn = fn(s) => ((ByteBuffer*)s.data).available(),
};

fn usz! ByteBuffer.write(&self, char[] bytes)
{
	usz cap = self.bytes.len - self.write_idx;
	if (cap < bytes.len) self.grow(bytes.len)!;
	self.bytes[self.write_idx:bytes.len] = bytes[..];
	self.write_idx += bytes.len;
	return bytes.len;
}

fn void! ByteBuffer.write_byte(&self, char c)
{
	usz cap = self.bytes.len - self.write_idx;
	if (cap == 0) self.grow(1)!;
	self.bytes[self.write_idx] = c;
	self.write_idx++;
}

fn usz! ByteBuffer.read(&self, char[] bytes)
{
	usz readable = self.write_idx - self.read_idx;
	if (readable == 0)
	{
		self.has_last = false;
		return IoError.EOF?;
	}
	usz n = min(readable, bytes.len);
	bytes[:n] = self.bytes[self.read_idx:n];
	self.read_idx += n;
	self.has_last = n > 0;
	self.shrink();
	return n;
}

fn char! ByteBuffer.read_byte(&self)
{
	usz readable = self.write_idx - self.read_idx;
	if (readable == 0)
	{
		self.has_last = false;
		return IoError.EOF?;
	}
	char c = self.bytes[self.read_idx];
	self.read_idx++;
	self.has_last = true;
	self.shrink();
	return c;
}

/*
 * Only the last byte of a successful read can be pushed back.
 */
fn void! ByteBuffer.pushback_byte(&self)
{
	if (!self.has_last) return IoError.EOF?;
	assert(self.read_idx > 0);
	self.read_idx--;
	self.has_last = false;
}

fn usz! ByteBuffer.available(&self) @inline
{
	return self.write_idx - self.read_idx;
}

fn void! ByteBuffer.grow(&self, usz n)
{
	n = math::next_power_of_2(n);
	char* p = realloc_aligned(self.bytes, n, .alignment = char.alignof, .using = self.allocator)!;
	self.bytes = p[:n];
}

macro ByteBuffer.shrink(&self)
{
	if (self.read_idx >= self.max_read)
	{
		// Drop the read data besides the last byte (for pushback_byte).
		usz readable = self.write_idx - self.read_idx;
		self.bytes[:1 + readable] = self.bytes[self.read_idx - 1:1 + readable];
		self.write_idx = 1 + readable;
		self.read_idx = 1;
	}
}