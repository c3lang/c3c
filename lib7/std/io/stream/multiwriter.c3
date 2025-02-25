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
 @require self.writers.len == 0 "Init may not run on already initialized data"
*>
fn MultiWriter* MultiWriter.new_init(&self, OutStream... writers, Allocator allocator = allocator::heap())
{
	OutStream[] copy = allocator::new_array(allocator, OutStream, writers.len);
	copy[..] = writers[..];
	*self = { .writers = copy, .allocator = allocator };
	return self;
}

<*
 @param [&inout] self
 @require writers.len > 0
 @require self.writers.len == 0 "Init may not run on already initialized data"
*>
fn MultiWriter* MultiWriter.temp_init(&self, OutStream... writers)
{
	return self.new_init(...writers, allocator: allocator::temp());
}

fn void MultiWriter.free(&self)
{
	if (!self.allocator) return;
	allocator::free(self.allocator, self.writers);
	*self = {};
}

fn usz! MultiWriter.write(&self, char[] bytes) @dynamic
{
	usz n;
	foreach (w : self.writers) 
	{
		n = w.write(bytes)!;
		if (n != bytes.len) return IoError.INCOMPLETE_WRITE?;
	}
	return bytes.len;
}

fn void! MultiWriter.write_byte(&self, char c) @dynamic
{
	char[1] data;
	data[0] = c;
	self.write(data[..])!;
}
