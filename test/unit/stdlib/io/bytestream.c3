module std::io @test;

fn void! bytestream()
{
	ByteReader r;
	r.init("abc");
	Stream s = r.as_stream();
	assert(s.len()! == 3);
	char[5] buffer;
	assert('a' == s.read_byte()!);
	s.pushback_byte()!;
	usz len = s.read(&buffer)!;
	assert((String)buffer[:len] == "abc");
	ByteWriter w;
	w.init();
	Stream ws = w.as_stream();
	ws.write("helloworld")!;
	assert(w.as_str() == "helloworld");
	s.seek(0, SET)!;
	ws.read_from(s)!;
	s.seek(1, SET)!;
	s.write_to(ws)!;
	assert(w.as_str() == "helloworldabcbc");
}

fn void! bytewriter_buffer()
{
	ByteWriter writer;
	char[8] z;
	writer.init_buffer(&z);
	Stream s = writer.as_stream();
	s.write("hello")!!;
	s.write_byte(0)!!;
	String o = ((ZString)&z).as_str();
	assert(o == "hello");
	assert(@catchof(s.write("xxxx")));
}

fn void! bytewriter_read_from()
{
    char[] data = "Lorem ipsum dolor sit amet biam.";
    TestReader r = { .bytes = data };
    Stream s = r.as_stream();

    ByteWriter bw;
    bw.tinit();
    bw.read_from(s)!;

    assert(bw.as_str() == data);
}

module std::io;
// TestReader only has the read method to trigger the path
// in ByteWriter.read_from that does not rely on the available method.
struct TestReader
{
    char[] bytes;
    usz index;
}

fn Stream TestReader.as_stream(TestReader *r)
{
    return { .fns = &testReader_interface, .data = r };
}

fn usz! TestReader.read(TestReader *r, char[] bytes)
{
    usz left = r.bytes.len - r.index;
    if (left == 0) return 0;
    usz n = min(left, bytes.len);
    mem::copy(bytes.ptr, &r.bytes[r.index], n);
    r.index += n;
    return n;
}

StreamInterface testReader_interface = {
    .read_fn = fn(s, char[] bytes) => ((TestReader*)s.data).read(bytes),
};