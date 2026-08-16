module std::io;

struct ReadBuffer (InStream)
{
	CachedInStream wrapped_stream;
	char[] buffer;
	sz read_idx;
	sz write_idx;
}

<*
 Buffer reads from a stream.
 @param [inout] self
 @require buffer.len > 0 : "The buffer must be at least 1 byte"
 @require !self.buffer.len : "Init may not run on already initialized data"
*>
fn ReadBuffer* ReadBuffer.init(&self, InStream wrapped_stream, char[] buffer)
{
	*self = { .wrapped_stream = wrapped_stream.cached_stream(), .buffer = buffer };
	return self;
}

fn String ReadBuffer.str_view(&self) @inline
{
	return (String)self.buffer[self.read_idx:self.write_idx - self.read_idx];
}

fn char[] ReadBuffer.array_view(&self) @inline
{
	return self.buffer[self.read_idx:self.write_idx - self.read_idx];
}

fn void? ReadBuffer.close(&self) @dynamic
{
	if (&self.wrapped_stream.close) self.wrapped_stream.close()!;
}

<*
 @require buffer.len > 0 : "The buffer must be non-zero"
 @return "The number of bytes read, or 0 if the end of the stream is reached"
*>
fn sz? ReadBuffer.read(&self, char[] buffer) @dynamic
{
	if (self.read_idx == self.write_idx)
	{
		if (self.read_idx == 0 && buffer.len >= self.buffer.len)
		{
			// Read directly into the input buffer.
			return self.wrapped_stream.read(buffer)!;
		}
		readbuffer_refill(self)!;
	}
	sz n = min(self.write_idx - self.read_idx, buffer.len);
	buffer[:n] = self.buffer[self.read_idx:n];
	self.read_idx += n;
	return n;
}

fn char? ReadBuffer.read_byte(&self) @dynamic
{
	if (self.read_idx == self.write_idx) readbuffer_refill(self)!;
	if (self.read_idx == self.write_idx) return io::EOF~;
	char c = self.buffer[self.read_idx];
	self.read_idx++;
	return c;
}

fn void? readbuffer_refill(ReadBuffer* self) @local @inline
{
	self.read_idx = 0;
	self.write_idx = self.wrapped_stream.read(self.buffer)!;
}

struct WriteBuffer (OutStream)
{
	CachedOutStream wrapped_stream;
	char[] buffer;
	sz index;
}

<*
 Buffer writes to a stream. Call `flush` when done writing to the buffer.
 @param [inout] self
 @require buffer.len > 0 : "Non-empty buffer required"
 @require self.buffer.len == 0 : "Init may not run on already initialized data"
*>
fn WriteBuffer* WriteBuffer.init(&self, OutStream wrapped_stream, char[] buffer)
{
	*self = { .wrapped_stream = wrapped_stream.cached_stream(), .buffer = buffer };
	return self;
}

fn String WriteBuffer.str_view(&self) @inline
{
	return (String)self.buffer[:self.index];
}

fn void? WriteBuffer.close(&self) @dynamic
{
	if (&self.wrapped_stream.close) return self.wrapped_stream.close();
}

fn void? WriteBuffer.flush(&self) @dynamic
{
	write_buffer_write_pending(self)!;
	if (&self.wrapped_stream.flush) self.wrapped_stream.flush()!;
}

fn sz? WriteBuffer.write(&self, char[] bytes) @dynamic
{
	sz n = self.buffer.len - self.index;
	if (bytes.len < n)
	{
		// Enough room in the buffer.
		self.buffer[self.index:bytes.len] = bytes[..];
		self.index += bytes.len;
		return bytes.len;
	}
	write_buffer_write_pending(self)!;
	if (bytes.len >= self.buffer.len)
	{
		// Write directly to the stream.
		return self.wrapped_stream.write(bytes);
	}
	// Buffer the data.
	self.buffer[:bytes.len] = bytes[..];
	self.index = bytes.len;
	return bytes.len;
}

fn void? WriteBuffer.write_byte(&self, char c) @dynamic
{
	sz n = self.buffer.len - self.index;
	if (n == 0)
	{
		write_buffer_write_pending(self)!;
	}
	self.buffer[self.index] = c;
	self.index += 1;
}

fn void? write_buffer_write_pending(WriteBuffer* self) @local
{
	self.index -= self.wrapped_stream.write(self.buffer[:self.index])!;
	if (self.index != 0) return INCOMPLETE_WRITE~;
}
