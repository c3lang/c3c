module std::io @test;

fn void test_writing()
{
	DString foo;
	foo.new_init();
	OutStream s = &foo;
	s.write("hello")!!;
	s.write_byte('-')!!;
	s.write("what?-------------------------------------------------------")!!;
	ByteReader r;
	String test_str = "2134";
	io::copy_to(r.init(test_str), s)!!;
	String o = foo.str_view();
	assert(o == "hello-what?-------------------------------------------------------2134");
}
