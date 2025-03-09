module std::io;

struct TeeReader (InStream)
{
	InStream r;
	OutStream w;
}

<* Returns a reader that implements InStream and that will write any data read
 from the wrapped reader r to the writer w. There is no internal buffering.
 
 @param [&inout] r : "Stream r to read from."
 @param [&inout] w : "Stream w to write to what it reads from r."
*>
macro TeeReader tee_reader(InStream r, OutStream w) => { r, w };

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

fn usz? TeeReader.read(&self, char[] bytes) @dynamic
{
	usz nr, nw;
	nr = self.r.read(bytes)!;
	nw = self.w.write(bytes[:nr])!;
	if (nr != nw) return GENERAL_ERROR?;
	return nr;
}

fn char? TeeReader.read_byte(&self) @dynamic
{
	char[1] data;
	self.read(data[..])!;
	return data[0];
}
