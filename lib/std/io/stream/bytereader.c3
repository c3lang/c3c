module std::io;

struct ByteReader
{
	inline Stream stream;
	char[] bytes;
	usz index;
}

fn ByteReader* ByteReader.init(&self, char[] bytes)
{
	*self = { .stream = { &BYTEREADER_INTERFACE }, .bytes = bytes };
	return self;
}

fn usz! ByteReader.read(&self, char[] bytes)
{
	if (self.index >= self.bytes.len) return IoError.EOF?;
	usz len = min(self.bytes.len - self.index, bytes.len);
	if (len == 0) return 0;
	mem::copy(bytes.ptr, &self.bytes[self.index], len);
	self.index += len;
	return len;
}

fn char! ByteReader.read_byte(&self)
{
	if (self.index >= self.bytes.len) return IoError.EOF?;
	return self.bytes[self.index++];
}

fn void! ByteReader.pushback_byte(&self)
{
	if (!self.index) return IoError.INVALID_PUSHBACK?;
	self.index--;
}

fn usz! ByteReader.seek(&self, isz offset, Seek seek)
{
	isz new_index;
	switch (seek)
	{
		case SET: new_index = offset;
		case CURSOR: new_index = self.index + offset;
		case END: new_index = self.bytes.len + offset;
	}
	if (new_index < 0) return IoError.INVALID_POSITION?;
	self.index = new_index;
	return new_index;
}

fn usz! ByteReader.write_stream(&self, Stream* writer)
{
	if (self.index >= self.bytes.len) return 0;
	usz written = writer.write(self.bytes[self.index..])!;
	self.index += written;
	assert(self.index <= self.bytes.len);
	return written;
}

fn usz ByteReader.available(&self) @inline
{
	return max(0, self.bytes.len - self.index);
}

const StreamInterface BYTEREADER_INTERFACE = {
	.len_fn = fn (Stream* self) => ((ByteReader*)self).bytes.len,
	.read_fn = (ReadStreamFn)&ByteReader.read,
	.read_byte_fn = (ReadByteStreamFn)&ByteReader.read_byte,
	.pushback_byte_fn = (PushbackByteStreamFn)&ByteReader.pushback_byte,
	.seek_fn = (SeekStreamFn)&ByteReader.seek,
	.write_stream_fn = (WriteToStreamFn)&ByteReader.write_stream,
	.available_fn = fn (Stream* self) => ((ByteReader*)self).available(),
};


