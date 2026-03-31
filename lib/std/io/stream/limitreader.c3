module std::io;

struct LimitReader (InStream)
{
	InStream wrapped_stream;
	isz limit;
}

<*
 @param [&inout] wrapped_stream : "The stream to read from"
 @param limit : "The max limit to read"
*>
fn LimitReader* LimitReader.init(&self, InStream wrapped_stream, isz limit)
{
	*self = { .wrapped_stream = wrapped_stream, .limit = limit };
	return self;
}

fn void? LimitReader.close(&self) @dynamic
{
	if (&self.wrapped_stream.close) return self.wrapped_stream.close();
}


fn isz? LimitReader.read(&self, char[] bytes) @dynamic
{
	if (self.limit == 0) return io::EOF~;
	isz m = min(bytes.len, self.limit);
	isz n = self.wrapped_stream.read(bytes[:m])!;
	self.limit -= n;
	return n;
}

fn char? LimitReader.read_byte(&self) @dynamic
{
	if (self.limit == 0) return io::EOF~;
	defer try self.limit--;
	return self.wrapped_stream.read_byte();
}

fn ulong? LimitReader.available(&self) @inline @dynamic
{
	return self.limit;
}