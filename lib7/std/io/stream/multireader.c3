module std::io;

/* MultiReader implements the InStream interface and provides a logical
 * concatenation of the provided readers. They are read sequentially. If all the
 * data has been read, IoError.EOF is returned.
 */
struct MultiReader (InStream)
{
	InStream[] readers;
	usz index;
	Allocator allocator;
}


<*
 @param [&inout] self
 @param [&inout] allocator
 @require self.readers.len == 0 "Init may not run on already initialized data"
 @ensure self.index == 0
*>
fn MultiReader* MultiReader.new_init(&self, InStream... readers, Allocator allocator = allocator::heap())
{
	InStream []copy = allocator::new_array(allocator, InStream, readers.len);
	copy[..] = readers[..];
	*self = { .readers = copy, .allocator = allocator };
	return self;
}

<*
 @param [&inout] self
 @require self.readers.len == 0 "Init may not run on already initialized data"
 @ensure self.index == 0
*>
fn MultiReader* MultiReader.temp_init(&self, InStream... readers)
{
	return self.new_init(...readers, allocator: allocator::temp());
}

fn void MultiReader.free(&self)
{
	if (!self.allocator) return;
	allocator::free(self.allocator, self.readers);
	*self = {};
}

fn usz! MultiReader.read(&self, char[] bytes) @dynamic
{
	InStream r = self.readers[self.index];
	usz! n = r.read(bytes);
	if (catch err = n)
	{
		case IoError.EOF:
			self.index++;
			if (self.index >= self.readers.len)
			{
				return IoError.EOF?;
			}
			return self.read(bytes);
		default:
			return err?;
	}
	return n;
}

fn char! MultiReader.read_byte(&self) @dynamic
{
	char[1] data;
	self.read(data[..])!;
	return data[0];
}
