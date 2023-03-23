module std::io;

struct ByteWriter
{
	char[] bytes;
	usz index;
	Allocator* allocator;
}

/**
 * @param [&inout] writer
 * @param [&in] using
 * @require writer.bytes.len == 0 "Init may not run on on already initialized data"
 * @ensure using != null, index == 0
 **/
fn void ByteWriter.init(ByteWriter* writer, Allocator* using = mem::heap())
{
	*writer = { .bytes = {}, .allocator = using };
}

fn void ByteWriter.init_buffer(ByteWriter* writer, char[] data)
{
	*writer = { .bytes = data, .allocator = null };
}

/**
 * @param [&inout] writer
 * @require writer.bytes.len == 0 "Init may not run on on already initialized data"
 **/
fn void ByteWriter.tinit(ByteWriter* writer)
{
	*writer = { .bytes = {}, .allocator = mem::temp() };
}

fn Stream ByteWriter.as_stream(ByteWriter* writer)
{
	return { .fns = &bytewriter_interface, .data = writer };
}

fn void ByteWriter.destroy(ByteWriter* writer)
{
	if (!writer.allocator) return;
	if (void* ptr = writer.bytes.ptr) free(ptr, .using = writer.allocator);
	*writer = { };
}

fn String ByteWriter.as_str(ByteWriter* writer)
{
	return (String)writer.bytes[:writer.index];
}

fn void! ByteWriter.ensure_capacity(ByteWriter* writer, usz len) @inline
{
	if (writer.bytes.len > len) return;
	if (!writer.allocator) return IoError.OUT_OF_SPACE?;
	if (len < 16) len = 16;
	usz new_capacity = math::next_power_of_2(len);
	char* new_ptr = realloc_checked(writer.bytes.ptr, new_capacity, .using = writer.allocator)!;
	writer.bytes = new_ptr[:new_capacity];
}

fn usz! ByteWriter.write(ByteWriter* writer, char[] bytes)
{
	writer.ensure_capacity(writer.index + bytes.len)!;
	mem::copy(&writer.bytes[writer.index], bytes.ptr, bytes.len);
	writer.index += bytes.len;
	return bytes.len;
}

fn void! ByteWriter.write_byte(ByteWriter* writer, char c)
{
	writer.ensure_capacity(writer.index + 1)!;
	writer.bytes[writer.index++] = c;
}

/**
 * @param [&inout] writer
 * @param [&inout] reader
 **/
fn usz! ByteWriter.read_from(ByteWriter* writer, Stream* reader)
{
	if (reader.supports_available())
	{
		usz total_read = 0;
		while (usz available = reader.available()!)
		{
			writer.ensure_capacity(writer.index + available)!;
			usz len = reader.read(writer.bytes[writer.index..])!;
			total_read += len;
			writer.index += len;
		}
		return total_read;
	}
	usz total_read = 0;
	while (true)
	{
		// See how much we can read.
		usz len_to_read = writer.bytes.len - writer.index;
		// Less than 16 bytes? Double the capacity
		if (len_to_read < 16)
		{
			writer.ensure_capacity(writer.bytes.len * 2)!;
		}
		// Read into the rest of the buffer
		usz read = reader.read(writer.bytes[writer.index..])!;
		writer.index += read;
		// Ok, we reached the end.
		if (read < len_to_read) return total_read;
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