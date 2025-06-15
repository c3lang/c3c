module string_test;

fn void test_format() @test
{
	test::eq(string::tformat("_%*c_", 5, 'x'),  "_    x_");
	test::eq(string::tformat("_%-*c_", 5, 'x'), "_x    _");
}
fn void test_clear() @test
{
	DString s = dstring::new_with_capacity(mem, 32);
	defer s.free();
	assert(s.len() == 0);
	assert(s.capacity() == 32);
	s.append_repeat('x', 63);
	assert(s.capacity() == 64);
	assert(s.len() == 63);
	char* addr = (char*)s.str_view();
	s.clear();
	assert(s.capacity() == 64);
	assert(s.len() == 0);
	s.append_repeat('x', 63);
	assert(s.capacity() == 64);
	assert(s.len() == 63);
	assert(addr == (char*)s.str_view());
}