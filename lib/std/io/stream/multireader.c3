module std::io;

/* MultiReader implements the InStream interface and provides a logical
 * concatenation of the provided readers. They are read sequentially. If all the
 * data has been read, io::EOF is returned.
 */
struct MultiReader (InStream)
{
	CachedInStream[] readers;
	sz index;
	Allocator allocator;
}


<*
 @param [&inout] self
 @param [&inout] allocator
 @require self.readers.len == 0 : "Init may not run on already initialized data"
 @ensure self.index == 0
*>
fn MultiReader* MultiReader.init(&self, Allocator allocator, InStream... readers)
{
	CachedInStream[] copy = alloc::new_array(allocator, CachedInStream, readers.len);
	foreach (i, stream : readers) copy[i] = stream.cached_stream();
	*self = { .readers = copy, .allocator = allocator };
	return self;
}

<*
 @param [&inout] self
 @require self.readers.len == 0 : "Init may not run on already initialized data"
 @ensure self.index == 0
*>
fn MultiReader* MultiReader.tinit(&self, InStream... readers)
{
	return self.init(tmem, ...readers);
}

fn void MultiReader.free(&self)
{
	if (!self.allocator) return;
	alloc::free(self.allocator, self.readers);
	*self = {};
}

fn sz? MultiReader.read(&self, char[] buffer) @dynamic
{
	while (true)
	{
		if (self.index >= self.readers.len) return 0;
		InStream r = self.readers[self.index];
		if (sz n = read_buffer(r, buffer)!) return n;
		self.index++;
	}
}

fn char? MultiReader.read_byte(&self) @dynamic
{
	return read_byte_using_read(self);
}
