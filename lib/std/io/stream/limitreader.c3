module std::io::stream;

struct LimitReader
{
	inline Stream stream;
	Stream* wrapped_stream;
	usz limit;
}

/**
 * @param [&inout] wrapped_stream "The stream to read from"
 * @param limit "The max limit to read"
 **/
fn LimitReader* LimitReader.init(&self, Stream* wrapped_stream, usz limit)
{
	*self = { .stream.fns = &LIMITREADER_INTERFACE, .wrapped_stream = wrapped_stream, .limit = limit };
	return self;
}

fn usz! LimitReader.read(&self, char[] bytes)
{
	if (self.limit == 0) return IoError.EOF?;
	usz m = min(bytes.len, self.limit);
	usz n = self.wrapped_stream.read(bytes[:m])!;
	self.limit -= n;
	return n;
}

fn char! LimitReader.read_byte(&self)
{
	if (self.limit == 0) return IoError.EOF?;
	defer try self.limit--;
	return self.wrapped_stream.read_byte();
}

fn usz LimitReader.available(&self) @inline
{
	return self.limit;
}

const StreamInterface LIMITREADER_INTERFACE = {
	.read_fn = (ReadStreamFn)&LimitReader.read,
	.read_byte_fn = (ReadByteStreamFn)&LimitReader.read_byte,
	.available_fn = fn(s) => ((LimitReader*)s).available(),
};