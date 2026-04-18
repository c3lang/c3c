module std::io;
import std::math;

struct ByteBuffer (InStream, OutStream)
{
	Allocator allocator;
	sz max_read;
	char[] bytes;
	sz read_idx;
	sz write_idx;
	bool has_last;
}

<*
 ByteBuffer provides a streamable read/write buffer.
 max_read defines how many bytes might be kept before its internal buffer is shrunk.
 @require self.bytes.len == 0 : "Buffer already initialized."
*>
fn ByteBuffer* ByteBuffer.init(&self, Allocator allocator, sz max_read, sz initial_capacity = 16)
{
	*self = { .allocator = allocator, .max_read = max_read };
	initial_capacity = max(initial_capacity, 16);
	self.grow(initial_capacity);
	return self;
}

fn ByteBuffer* ByteBuffer.tinit(&self, sz max_read, sz initial_capacity = 16)
{
	return self.init(tmem, max_read, initial_capacity);
}

<*
 @require buf.len > 0
 @require self.bytes.len == 0 : "Buffer already initialized."
*>
fn ByteBuffer* ByteBuffer.init_with_buffer(&self, char[] buf)
{
	*self = { .max_read = buf.len, .bytes = buf };
	return self;
}

fn void ByteBuffer.free(&self)
{
	if (self.allocator) alloc::free(self.allocator, self.bytes);
	*self = {};
}

fn sz? ByteBuffer.write(&self, char[] bytes) @dynamic
{
	sz cap = self.bytes.len - self.write_idx;
	if (cap < bytes.len) self.grow(bytes.len);
	self.bytes[self.write_idx:bytes.len] = bytes[..];
	self.write_idx += bytes.len;
	return bytes.len;
}

fn void? ByteBuffer.write_byte(&self, char c) @dynamic
{
	sz cap = self.bytes.len - self.write_idx;
	if (cap == 0) self.grow(1);
	self.bytes[self.write_idx] = c;
	self.write_idx++;
}

fn sz? ByteBuffer.read(&self, char[] bytes) @dynamic
{
	sz readable = self.write_idx - self.read_idx;
	if (readable == 0)
	{
		self.has_last = false;
		return io::EOF~;
	}
	sz n = min(readable, bytes.len);
	bytes[:n] = self.bytes[self.read_idx:n];
	self.read_idx += n;
	self.has_last = n > 0;
	self.shrink();
	return n;
}

fn char? ByteBuffer.read_byte(&self) @dynamic
{
	sz readable = self.write_idx - self.read_idx;
	if (readable == 0)
	{
		self.has_last = false;
		return io::EOF~;
	}
	char c = self.bytes[self.read_idx];
	self.read_idx++;
	self.has_last = true;
	self.shrink();
	return c;
}

<*
 Only the last byte of a successful read can be pushed back.
*>
fn void? ByteBuffer.pushback_byte(&self) @dynamic
{
	if (!self.has_last) return io::EOF~;
	assert(self.read_idx > 0);
	self.read_idx--;
	self.has_last = false;
}

fn long? ByteBuffer.cursor(&self) @dynamic
{
	return self.read_idx;
}

fn void? ByteBuffer.seek(&self, long offset, SeekOrigin whence = FROM_START) @dynamic
{
	switch (whence)
	{
		case FROM_START:
			if (offset < 0 || offset > self.write_idx) return INVALID_POSITION~;
			self.read_idx = (sz)offset;
		case FROM_CURSOR:
			if ((offset < 0 && self.read_idx < -offset) ||
				(offset > 0 && self.read_idx + offset > self.write_idx)) return INVALID_POSITION~;
			self.read_idx += (sz)offset;
		case FROM_END:
			if (offset < 0 || offset > self.write_idx) return INVALID_POSITION~;
			self.read_idx = self.write_idx - (sz)offset;
	}
}

fn long? ByteBuffer.available(&self) @inline @dynamic
{
	return (long)self.write_idx - self.read_idx;
}

fn void ByteBuffer.grow(&self, sz n)
{
	n = math::next_power_of_2(self.bytes.len + n);
	char* p = alloc::realloc(self.allocator, self.bytes, n);
	self.bytes = p[:n];
}

macro void ByteBuffer.shrink(&self)
{
	if (self.read_idx >= self.max_read)
	{
		// Drop the read data besides the last byte (for pushback_byte).
		sz readable = self.write_idx - self.read_idx;
		self.bytes[:1 + readable] = self.bytes[self.read_idx - 1:1 + readable];
		self.write_idx = 1 + readable;
		self.read_idx = 1;
	}
}
