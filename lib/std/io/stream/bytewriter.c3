module std::io;
import std::math;

struct ByteWriter (OutStream)
{
	char[] bytes;
	sz index;
	Allocator allocator;
}

<*
 @param [&inout] self
 @param [&inout] allocator
 @param reserve : "The bytes to reserve up front, defaults to 0"

 @require self.bytes.len == 0 : "Init may not run on already initialized data"
 @ensure (bool)allocator, self.index == 0
*>
fn ByteWriter* ByteWriter.init(&self, Allocator allocator, sz reserve = 0)
{
	*self = { .bytes = {}, .allocator = allocator };
	if (reserve > 0) self.ensure_capacity(reserve)!!;
	return self;
}

<*
 @param [&inout] self
 @param reserve : "The bytes to reserve up front, defaults to 0"

 @require self.bytes.len == 0 : "Init may not run on already initialized data"
 @ensure self.index == 0
*>
fn ByteWriter* ByteWriter.tinit(&self, sz reserve = 0)
{
	return self.init(tmem, reserve) @inline;
}

fn ByteWriter* ByteWriter.init_with_buffer(&self, char[] data)
{
	*self = { .bytes = data, .allocator = null };
	return self;
}

fn void? ByteWriter.destroy(&self) @dynamic
{
	if (!self.allocator) return;
	if (void* ptr = self.bytes.ptr) alloc::free(self.allocator, ptr);
	*self = { };
}

fn char[] ByteWriter.array_view(self) @inline
{
	return self.bytes[:self.index];
}

fn String ByteWriter.str_view(&self) @inline
{
	return (String)self.bytes[:self.index];
}

fn void? ByteWriter.ensure_capacity(&self, sz len) @inline
{
	if (self.bytes.len > len) return;
	if (!self.allocator) return OUT_OF_SPACE~;
	if (len < 16) len = 16;
	sz new_capacity = math::next_power_of_2(len);
	char* new_ptr = alloc::realloc_try(self.allocator, self.bytes.ptr, new_capacity)!;
	self.bytes = new_ptr[:new_capacity];
}

fn sz? ByteWriter.write(&self, char[] bytes) @dynamic
{
	self.ensure_capacity(self.index + bytes.len)!;
	mem::copy(&self.bytes[self.index], bytes.ptr, bytes.len);
	self.index += bytes.len;
	return bytes.len;
}

fn void? ByteWriter.write_byte(&self, char c) @dynamic
{
	self.ensure_capacity(self.index + 1)!;
	self.bytes[self.index++] = c;
}

<*
 @param [&inout] self
 @param reader
*>
fn long? ByteWriter.read_from(&self, InStream reader) @dynamic
{
	sz start_index = self.index;
	if (&reader.available)
	{
		while (long available = reader.available()!)
		{
			if (available > sz::max) return OUT_OF_SPACE~;
			self.ensure_capacity(self.index + (sz)available)!;
			sz read = reader.read(self.bytes[self.index..])!;
			self.index += read;
		}
		return (long)self.index - start_index;
	}
	if (self.bytes.len == 0)
	{
		self.ensure_capacity(256)!;
	}
	while (true)
	{
		// See how much we can read.
		sz len_to_read = self.bytes.len - self.index;
		// Less than 16 bytes? Double the capacity
		if (len_to_read < 16)
		{
			self.ensure_capacity(self.bytes.len * 2)!;
			len_to_read = self.bytes.len - self.index;
		}
		// Read into the rest of the buffer
		sz read = reader.read(self.bytes[self.index..])!;
		self.index += read;
		// Ok, we reached the end.
		if (read < len_to_read) return (long)self.index - start_index;
		// Otherwise go another round
	}
}
