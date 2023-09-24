module std::io;

struct ByteWriter
{
	inline Stream stream;
	char[] bytes;
	usz index;
	Allocator* allocator;
}

/**
 * @param [&inout] self
 * @param [&in] using
 * @require self.bytes.len == 0 "Init may not run on on already initialized data"
 * @ensure using != null, self.index == 0
 **/
fn ByteWriter* ByteWriter.init(&self, Allocator* using = mem::heap())
{
	*self = { .stream.fns = &BYTEWRITER_INTERFACE, .bytes = {}, .allocator = using };
	return self;
}

fn ByteWriter* ByteWriter.init_buffer(&self, char[] data)
{
	*self = { .stream.fns = &BYTEWRITER_INTERFACE, .bytes = data, .allocator = null };
	return self;
}

/**
 * @param [&inout] self
 * @require self.bytes.len == 0 "Init may not run on on already initialized data"
 **/
fn ByteWriter* ByteWriter.tinit(&self)
{
	return self.init(mem::temp());
}

fn void ByteWriter.destroy(&self)
{
	if (!self.allocator) return;
	if (void* ptr = self.bytes.ptr) free(ptr, .using = self.allocator);
	*self = { };
}

fn String ByteWriter.str_view(&self) @inline
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
fn usz! ByteWriter.read_from(&self, Stream* reader)
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

const StreamInterface BYTEWRITER_INTERFACE = {
	.destroy_fn = fn (s) => ((ByteWriter*)s).destroy(),
	.len_fn = fn (s) => ((ByteWriter*)s).bytes.len,
	.write_fn = (WriteStreamFn)&ByteWriter.write,
	.write_byte_fn = (WriteByteStreamFn)&ByteWriter.write_byte,
	.read_stream_fn = (ReadFromStreamFn)&ByteWriter.read_from,
};
