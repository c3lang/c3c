module std::io;

/* MultiWriter implements the OutStream interface and duplicates any write
 * operation to all the wrapped writers.
 */
struct MultiWriter (OutStream)
{
	OutStream[] writers;
	Allocator allocator;
}

<*
 @param [&inout] self
 @param [&inout] allocator
 @require writers.len > 0
 @require self.writers.len == 0 : "Init may not run on already initialized data"
*>
fn MultiWriter* MultiWriter.init(&self, Allocator allocator, OutStream... writers)
{
	OutStream[] copy = alloc::new_array(allocator, OutStream, writers.len);
	copy[..] = writers[..];
	*self = { .writers = copy, .allocator = allocator };
	return self;
}

<*
 @param [&inout] self
 @require writers.len > 0
 @require self.writers.len == 0 : "Init may not run on already initialized data"
*>
fn MultiWriter* MultiWriter.tinit(&self, OutStream... writers)
{
	return self.init(tmem, ...writers);
}

fn void MultiWriter.free(&self)
{
	if (!self.allocator) return;
	alloc::free(self.allocator, self.writers);
	*self = {};
}

fn sz? MultiWriter.write(&self, char[] bytes) @dynamic
{
	sz n;
	foreach (w : self.writers) 
	{
		n = w.write(bytes)!;
		if (n != bytes.len) return INCOMPLETE_WRITE~;
	}
	return bytes.len;
}

fn void? MultiWriter.write_byte(&self, char c) @dynamic
{
	char[1] data;
	data[0] = c;
	self.write(data[..])!;
}
