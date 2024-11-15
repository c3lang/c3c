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
	usz n = writers.len;
	OutStream *ptr =  allocator::new_array(allocator, OutStream, n);
	ptr[:n] = writers[:n];
	*self = { .writers = ptr[:n], .allocator = allocator };
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
	if (void* ptr = self.writers.ptr) allocator::free(self.allocator, ptr);
	*self = {};
}

fn usz! MultiWriter.write(&self, char[] bytes) @dynamic
{
	usz n;
	foreach (w : self.writers) 
	{
		n = w.write(bytes)!;
		if (n != bytes.len) return IoError.SHORT_WRITE?;
	}
	return bytes.len;
}

fn void! MultiWriter.write_byte(&self, char c) @dynamic
{
	char[1] data;
	data[0] = c;
	self.write(data[..])!;
}
