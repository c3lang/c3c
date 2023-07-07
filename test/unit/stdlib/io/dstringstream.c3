module std::io @test;

fn void! test_writing()
{
	DString foo;
	foo.init();
	Stream s = foo.as_stream();
	s.write("hello")!!;
	s.write_byte('-')!!;
	s.write("what?")!!;
	ByteReader r;
	String test_str = "2134";
	r.init(test_str);
	s.read_from(r.as_stream())!;
	String o = foo.as_str();
	assert(o == "hello-what?2134");
}
