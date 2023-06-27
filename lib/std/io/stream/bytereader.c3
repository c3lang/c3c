module std::io;
import std::math;

struct ByteReader
{
	char[] bytes;
	usz index;
}

fn void ByteReader.init(ByteReader* reader, char[] bytes)
{
	*reader = { .bytes = bytes };
}

fn Stream ByteReader.as_stream(ByteReader* reader)
{
	return { .fns = &bytereader_interface, .data = reader };
}

fn usz! ByteReader.read(ByteReader* reader, char[] bytes)
{
	if (reader.index >= reader.bytes.len) return IoError.EOF?;
	usz len = math::min(reader.bytes.len - reader.index, bytes.len);
	if (len == 0) return 0;
	mem::copy(bytes.ptr, &reader.bytes[reader.index], len);
	reader.index += len;
	return len;
}

fn char! ByteReader.read_byte(ByteReader* reader)
{
	if (reader.index >= reader.bytes.len) return IoError.EOF?;
	return reader.bytes[reader.index++];
}

fn void! ByteReader.pushback_byte(ByteReader* reader)
{
	if (!reader.index) return IoError.INVALID_PUSHBACK?;
	reader.index--;
}

fn usz! ByteReader.seek(ByteReader* reader, isz offset, Seek seek)
{
	isz new_index;
	switch (seek)
	{
		case SET: new_index = offset;
		case CURSOR: new_index = reader.index + offset;
		case END: new_index = reader.bytes.len + offset;
	}
	if (new_index < 0) return IoError.INVALID_POSITION?;
	reader.index = new_index;
	return new_index;
}

fn usz! ByteReader.write_stream(ByteReader* reader, Stream writer)
{
	if (reader.index >= reader.bytes.len) return 0;
	usz written = writer.write(reader.bytes[reader.index..])!;
	reader.index += written;
	assert(reader.index <= reader.bytes.len);
	return written;
}

fn usz ByteReader.available(ByteReader* reader)
{
	return math::max((isz)0, (isz)reader.bytes.len - reader.index);
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


