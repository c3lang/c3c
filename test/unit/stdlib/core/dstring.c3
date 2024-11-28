module std::core::dstring2 @test;

const TEST_STRING = "hello world";

fn void test_replace()
{
	DString hello = dstring::new("Hello world where are you? Are you here too?");
	defer hello.free();
	hello.replace("u", "ooo");
	assert(hello.str_view() == "Hello world where are yoooo? Are yoooo here too?");
}

fn void test_reverse()
{
	DString a;
	a.append("abcd");
	a.reverse();
	assert(a.str_view() == "dcba");
	a.reverse();
	assert(a.str_view() == "abcd");
	a.append("e");
	assert(a.str_view() == "abcde");
	a.reverse();
	assert(a.str_view() == "edcba");
}

fn void test_delete()
{
	{
		DString d;
		d.append("Hello cruel world.");
		d.delete_range(5, 10);
		assert(d.str_view() == "Hello world.");
	}
	DString d;
	d.append("Hello cruel world.");
	d.delete(0, 2);
	assert(d.str_view() == "llo cruel world.");
	d.delete(14, 2);
	assert(d.str_view() == "llo cruel worl");
	d.delete(7);
	assert(d.str_view() == "llo crul worl");
	d.delete(2, 0);
	assert(d.str_view() == "llo crul worl");
	d.delete(0, 1);
	assert(d.str_view() == "lo crul worl");
}

fn void test_append()
{
	DString str = dstring::new(TEST_STRING);
	defer str.free();
	String s;

	assert(str.len() == TEST_STRING.len);
	assert(str.capacity() == 16);

	s = str.str_view();
	assert(s == TEST_STRING, "got '%s'; want '%s'", s, TEST_STRING);
	ZString zs;
	zs = str.zstr_view();
	assert(s.ptr[s.len] == 0, "got '%c'; want 0", s.ptr[s.len]);

	str.chop(5);
	s = str.str_view();
	assert(s == TEST_STRING[:5], "got '%s'; want '%s'", s, TEST_STRING[:5]);

	str.append(TEST_STRING[5..]);
	s = str.str_view();
	assert(s == TEST_STRING, "got '%s'; want '%s'", s, TEST_STRING);

	str.append_char('x');
	s = str.str_view();
	assert(s[^1] == 'x', "got '%c'; want 'x'", s[^1]);

	str.append('y');
	s = str.str_view();
	assert(s[^1] == 'y', "got '%c'; want 'y'", s[^1]);

	str.set(0, 'z');
	s = str.str_view();
	assert(s[0] == 'z', "got '%c'; want 'z'", s[0]);

	str.clear();
	assert(str.len() == 0);

	str.append_char32('é');
	s = str.str_view();
	assert(s == "é", "got '%s'; want 'é'", s);

	str.append_utf32({ 'è', 'à' });
	s = str.str_view();
	assert(s == "éèà", "got '%s'; want 'éèà'", s);
	str.clear();

	str.append_chars("foo");
	s = str.str_view();
	assert(s == "foo", "got '%s'; want 'foo'", s);
	str.clear();

	str.append_repeat('x', 3);
	s = str.str_view();
	assert(s == "xxx", "got '%s'; want 'xxx'", s);

	DString str2 = dstring::new("yyy");
	defer str2.free();
	DString str3 = str.new_concat(str2);
	defer str3.free();
	s = str3.str_view();
	assert(s == "xxxyyy", "got '%s'; want 'xxxyyy'", s);

	str3.clear();
	str3.append_string(str2);
	s = str3.str_view();
	assert(s == "yyy", "got '%s'; want 'yyy'", s);
}

fn void test_print()
{
	DString str = dstring::new("");
	defer str.free();
	String s;

	str.appendf("_%s_", "foo");
	s = str.str_view();
	assert(s == "_foo_", "got '%s'; want '_foo_'", s);

	str.clear();
	str.appendfn("_%s_", "foo");
	s = str.str_view();
	assert(s == "_foo_\n", "got '%s'; want '_foo_\n'", s);
}

fn void test_copy()
{
	DString str = dstring::new(TEST_STRING);
	defer str.free();
	String s;

	DString str2 = str.copy();
	defer str2.free();
	s = str2.str_view();
	assert(s == TEST_STRING, "got '%s'; want '%s'", s, TEST_STRING);
}

fn void test_cmp()
{
	DString str = dstring::new(TEST_STRING);
	defer str.free();

	DString str2 = dstring::new(TEST_STRING);
	defer str2.free();

	assert(str.equals(str2));

	str2.clear();
	str2.append("hello you..");
	assert(str.len() == str2.len());
	assert(!str.less(str2));
}

fn void test_join()
{
	DString str = dstring::new_join({"hello", "world"}, " ");
	defer str.free();

	String s = str.str_view();
	assert(s == TEST_STRING, "got '%s'; want '%s'", s, TEST_STRING);
}

fn void test_insert_at()
{
	DString str = dstring::temp_new(" world");
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

	str.insert_at(0, '[');
	s = str.str_view();
	assert(s == "[hello shiny world", "got '%s'; want '[hello shiny world'", s);

	str.insert_at(18, ']');
	s = str.str_view();
	assert(s == "[hello shiny world]", "got '%s'; want 'hello shiny world]'", s);

	str.insert_at(0, 'ꫩ');
	s = str.str_view();
	assert(s == "ꫩ[hello shiny world]", "got '%s'; want 'ꫩ[hello shiny world]'", s);

	// ꫩ is 3 bytes long
	str.insert_utf32_at(4, {'寃', 't', 'e', 'x', 't', '￥'});
	s = str.str_view();
	assert(s == "ꫩ[寃text￥hello shiny world]", "got '%s'; want 'ꫩ[寃text￥hello shiny world]'", s);
}

fn void test_insert_at_overlaps()
{
	DString str = dstring::temp_new("abc");
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

fn void test_char_at()
{
	DString str = dstring::new("hello");
	defer str.free();

	char c = str.char_at(1);
	assert(c == 'e');

	char c_with_operator = str[4];
	assert(c_with_operator == 'o');
}

fn void test_operators()
{
	DString str = dstring::new("hello");
	defer str.free();

	str[0] = 'p';
	assert(str.str_view() == "pello");

	char* c = &str[1];
	assert(*c == 'e');
}

