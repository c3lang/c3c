module std::io @test;

fn void! test_writing()
{
	DString foo;
	foo.init();
	Stream* s = DStringStream{}.init(&foo);
	s.write("hello")!!;
	s.write_byte('-')!!;
	s.write("what?-------------------------------------------------------")!!;
	ByteReader r;
	String test_str = "2134";
	s.read_from(r.init(test_str))!;
	String o = foo.str_view();
	assert(o == "hello-what?-------------------------------------------------------2134");
}
