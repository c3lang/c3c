module std::core::dstring::tests @test;

fn void test_insert_at()
{
	DString str = dstring::tnew(" world");
	String s;

	str.insert_at(0, "");
	s = str.str_view();
	assert(s == " world", "got '%s'; want ' world'", s);

	str.insert_at(0, "hello");
	s = str.str_view();
	assert(s == "hello world", "got '%s'; want 'hello world'", s);

	str.insert_at(5, " shiny");
	s = str.str_view();
	assert(s == "hello shiny world", "got '%s'; want 'hello shiny world'", s);
}

fn void test_insert_at_overlaps()
{
	DString str = dstring::tnew("abc");
	String s;
	String v;

	str.insert_at(0, "bc");
	s = str.str_view();
	assert(s == "bcabc", "got '%s'; want 'bcabc'", s);

	// Inserted string is unchanged.
	str.chop(0);
	str.append("abc");
	v = str.str_view();
	str.insert_at(0, v);
	s = str.str_view();
	assert(s == "abc", "got '%s'; want 'abc'", s);

	// Inserted string is part of the tail.
	str.chop(0);
	str.append("abc");
	v = str.str_view()[1..];
	assert(v == "bc");
	str.insert_at(0, v);
	s = str.str_view();
	assert(s == "bcabc", "got '%s'; want 'bcabc'", s);

	// Inserted string is part of the head.
	str.chop(0);
	str.append("abc");
	v = str.str_view()[1..];
	str.insert_at(2, v);
	s = str.str_view();
	assert(s == "abbcc", "got '%s'; want 'abbcc'", s);

	str.chop(0);
	str.append("abcdef");
	v = str.str_view()[3..];
	assert(v == "def");
	str.insert_at(0, v);
	str.chop(3);
	s = str.str_view();
	assert(s == "def", "got '%s'; want 'def'", s);
}