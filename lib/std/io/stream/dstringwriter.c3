module std::io;

struct DStringStream
{
	inline Stream stream;
	DString* wrapped_string;
}

fn DStringStream* DStringStream.init(&self, DString* wrapped_string)
{
	*self = { .stream.fns = &DSTRINGSTREAM_INTERFACE, .wrapped_string = wrapped_string };
	return self;
}

fn usz DStringStream.len(&self) => self.wrapped_string.len();

fn usz! DStringStream.write(&self, char[] bytes)
{
	self.wrapped_string.append_chars((String)bytes);
	return bytes.len;
}

fn void! DStringStream.write_byte(&self, char c)
{
	self.wrapped_string.append_char(c);
}

fn usz! DStringStream.read_from_stream(&self, Stream* reader)
{
	return self.wrapped_string.read_from_stream(reader);
}

const StreamInterface DSTRINGSTREAM_INTERFACE = {
	.len_fn = (LenStreamFn)&DStringStream.len,
	.write_fn = (WriteStreamFn)&DStringStream.write,
	.write_byte_fn = (WriteByteStreamFn)&DStringStream.write_byte,
	.read_stream_fn = (ReadFromStreamFn)&DStringStream.read_from_stream,
};