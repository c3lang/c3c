module std::io @test;

fn void bytestream()
{
	ByteReader r;
	r.init("abc");
	InStream s = &r;
	assert(s.len() == 3);
	char[5] buffer;
	assert('a' == s.read_byte()!!);
	s.pushback_byte()!!;
	usz len = s.read(&buffer)!!;
	assert((String)buffer[:len] == "abc");
	ByteWriter w;
	w.new_init();
	OutStream ws = &w;
	ws.write("helloworld")!!;
	assert(w.str_view() == "helloworld");
	s.seek(0, SET)!!;
	io::copy_to(s, ws)!!;
	s.seek(1, SET)!!;
	s.write_to(ws)!!;
	assert(w.str_view() == "helloworldabcbc");
}

fn void bytewriter_buffer()
{
	ByteWriter writer;
	char[8] z;
	writer.init_with_buffer(&z);
	OutStream s = &writer;
	s.write("hello")!!;
	s.write_byte(0)!!;
	String o = ((ZString)&z).str_view();
	assert(o == "hello");
	assert(@catch(s.write("xxxx")));
}

fn void bytewriter_read_from()
{
    char[] data = "Lorem ipsum dolor sit amet biam.";
    TestReader r = { .bytes = data };
    InStream s = &r;

    ByteWriter bw;
    bw.temp_init();
    bw.read_from(s)!!;

    assert(bw.str_view() == data);
}

module std::io;
// TestReader only has the read method to trigger the path
// in ByteWriter.read_from that does not rely on the available method.
struct TestReader (InStream)
{
    char[] bytes;
    usz index;
}

fn usz! TestReader.read(&self, char[] bytes) @dynamic
{
    usz left = self.bytes.len - self.index;
    if (left == 0) return 0;
    usz n = min(left, bytes.len);
    mem::copy(bytes.ptr, &self.bytes[self.index], n);
    self.index += n;
    return n;
}

fn char! TestReader.read_byte(&self) @dynamic => io::read_byte_using_read(self);
