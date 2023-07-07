module std::io;

struct ByteWriter
{
	char[] bytes;
	usz index;
	Allocator* allocator;
}

/**
 * @param [&inout] self
 * @param [&in] using
 * @require self.bytes.len == 0 "Init may not run on on already initialized data"
 * @ensure using != null, index == 0
 **/
fn void ByteWriter.init(&self, Allocator* using = mem::heap())
{
	*self = { .bytes = {}, .allocator = using };
}

fn void ByteWriter.init_buffer(&self, char[] data)
{
	*self = { .bytes = data, .allocator = null };
}

/**
 * @param [&inout] self
 * @require self.bytes.len == 0 "Init may not run on on already initialized data"
 **/
fn void ByteWriter.tinit(&self)
{
	*self = { .bytes = {}, .allocator = mem::temp() };
}

fn Stream ByteWriter.as_stream(&self)
{
	return { .fns = &bytewriter_interface, .data = self };
}

fn void ByteWriter.destroy(&self)
{
	if (!self.allocator) return;
	if (void* ptr = self.bytes.ptr) free(ptr, .using = self.allocator);
	*self = { };
}

fn String ByteWriter.as_str(&self)
{
	return (String)self.bytes[:self.index];
}

fn void! ByteWriter.ensure_capacity(&self, usz len) @inline
{
	if (self.bytes.len > len) return;
	if (!self.allocator) return IoError.OUT_OF_SPACE?;
	if (len < 16) len = 16;
	usz new_capacity = math::next_power_of_2(len);
	char* new_ptr = realloc_checked(self.bytes.ptr, new_capacity, .using = self.allocator)!;
	self.bytes = new_ptr[:new_capacity];
}

fn usz! ByteWriter.write(&self, char[] bytes)
{
	self.ensure_capacity(self.index + bytes.len)!;
	mem::copy(&self.bytes[self.index], bytes.ptr, bytes.len);
	self.index += bytes.len;
	return bytes.len;
}

fn void! ByteWriter.write_byte(&self, char c)
{
	self.ensure_capacity(self.index + 1)!;
	self.bytes[self.index++] = c;
}

/**
 * @param [&inout] self
 * @param reader
 **/
fn usz! ByteWriter.read_from(&self, Stream reader)
{
	usz start_index = self.index;
	if (reader.supports_available())
	{
		while (usz available = reader.available()!)
		{
			self.ensure_capacity(self.index + available)!;
			usz read = reader.read(self.bytes[self.index..])!;
			self.index += read;
		}
		return self.index - start_index;
	}
	if (self.bytes.len == 0)
	{
		self.ensure_capacity(16)!;
	}
	while (true)
	{
		// See how much we can read.
		usz len_to_read = self.bytes.len - self.index;
		// Less than 16 bytes? Double the capacity
		if (len_to_read < 16)
		{
			self.ensure_capacity(self.bytes.len * 2)!;
			len_to_read = self.bytes.len - self.index;
		}
		// Read into the rest of the buffer
		usz read = reader.read(self.bytes[self.index..])!;
		self.index += read;
		// Ok, we reached the end.
		if (read < len_to_read) return self.index - start_index;
		// Otherwise go another round
	}
}

StreamInterface bytewriter_interface = {
	.destroy_fn = fn (s) => ((ByteWriter*)s.data).destroy(),
	.len_fn = fn (s) => ((ByteWriter*)s.data).bytes.len,
	.write_fn = fn (s, char[] bytes) => ((ByteWriter*)s.data).write(bytes),
	.write_byte_fn = fn (s, char c) => ((ByteWriter*)s.data).write_byte(c),
	.read_stream_fn = fn (s, reader) => ((ByteWriter*)s.data).read_from(reader),
};
