module std::io;

struct TeeReader (InStream)
{
	CachedInStream r;
	CachedOutStream w;
}

<* Returns a reader that implements InStream and that will write any data read
 from the wrapped reader r to the writer w. There is no internal buffering.
 
 @param [&inout] r : "Stream r to read from."
 @param [&inout] w : "Stream w to write to what it reads from r."
*>
macro TeeReader tee_reader(InStream r, OutStream w)
{
	return { .r = r.cached_stream(), .w = w.cached_stream() };
}

<*
 @param [&inout] self
 @param [&inout] r : "Stream r to read from."
 @param [&inout] w : "Stream w to write to what it reads from r."
 *>
fn TeeReader* TeeReader.init(&self, InStream r, OutStream w)
{
	*self = tee_reader(r, w);
	return self;
}

fn sz? TeeReader.read(&self, char[] buffer) @dynamic
{
	sz nr = self.r.read(buffer)!;
	if (!nr) return 0;
	io::write_all(self.w, buffer[:nr])!;
	return nr;
}

fn char? TeeReader.read_byte(&self) @dynamic
{
	char[1] data;
	return self.read(data[..])! ? data[0] : io::EOF~;
}
