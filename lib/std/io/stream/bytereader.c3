module std::io;
import std::math;

struct ByteReader
{
	char[] bytes;
	usz index;
}

fn void ByteReader.init(&self, char[] bytes)
{
	*self = { .bytes = bytes };
}

fn Stream ByteReader.as_stream(&self)
{
	return { .fns = &bytereader_interface, .data = self };
}

fn usz! ByteReader.read(&self, char[] bytes)
{
	if (self.index >= self.bytes.len) return IoError.EOF?;
	usz len = math::min(self.bytes.len - self.index, bytes.len);
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

fn usz! ByteReader.write_stream(&self, Stream writer)
{
	if (self.index >= self.bytes.len) return 0;
	usz written = writer.write(self.bytes[self.index..])!;
	self.index += written;
	assert(self.index <= self.bytes.len);
	return written;
}

fn usz ByteReader.available(&self)
{
	return math::max((isz)0, (isz)self.bytes.len - self.index);
}

StreamInterface bytereader_interface = {
	.len_fn = fn (s) => ((ByteReader*)s.data).bytes.len,
	.read_fn = fn (s, char[] bytes) => ((ByteReader*)s.data).read(bytes) @inline,
	.read_byte_fn = fn (s) => ((ByteReader*)s.data).read_byte() @inline,
	.pushback_byte_fn = fn (s) => ((ByteReader*)s.data).pushback_byte() @inline,
	.seek_fn = fn (s, offset, seek) => ((ByteReader*)s.data).seek(offset, seek) @inline,
	.write_stream_fn = fn (s, writer) => ((ByteReader*)s.data).write_stream(writer) @inline,
	.available_fn = fn (s) => ((ByteReader*)s.data).available() @inline,
};


