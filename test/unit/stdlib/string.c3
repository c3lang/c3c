module string_test;

fn void test_clear() @test
{
	DString s = dstring::new_with_capacity(32);
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