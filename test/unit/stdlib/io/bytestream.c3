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
	ws.read_from(&s)!;
	s.seek(1, SET)!;
	s.write_to(&ws)!;
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
