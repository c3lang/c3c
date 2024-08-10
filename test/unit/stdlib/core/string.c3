module std::core::string::tests @test;

fn void test_starts_with()
{
	String s = "ofke";
	assert(s.starts_with("of"));
	assert(s.starts_with("ofke"));
	assert(!s.starts_with("ofkes"));
	assert(!s.starts_with("ofkf"));
	s = "";
	assert(s.starts_with(""));
	assert(!s.starts_with("o"));
}

fn void test_print_null()
{
	ZString z;
	int* y;
	ZString w = "hello";
	String s = string::new_format("%s %s %s", z, w, y);
	defer free(s);
	assert(s == "(null) hello 0x0");
}

fn void test_strip()
{
	String s = "ofke";
	assert(s.strip("of") == "ke");
	assert(s.strip("ofke") == "");
	assert(s.strip("ofkes") == "ofke");
	assert(s.strip("ofkf") == "ofke");
	assert(s.strip("") == "ofke");
	s = "";
	assert(s.strip("") == "");
	assert(s.strip("o") == "");
}

fn void test_strip_end()
{
	String s = "ofke";
	assert(s.strip_end("ke") == "of");
	assert(s.strip_end("ofke") == "");
	assert(s.strip_end("ofkes") == "ofke");
	assert(s.strip_end("ofkf") == "ofke");
	assert(s.strip_end("") == "ofke");
	s = "";
	assert(s.strip_end("") == "");
	assert(s.strip_end("o") == "");
}

fn void test_ends_with()
{
	String s = "ofke";
	assert(s.ends_with("ke"));
	assert(s.ends_with("ofke"));
	assert(!s.ends_with("ofkes"));
	assert(!s.ends_with("ofkf"));
	s = "";
	assert(s.ends_with(""));
	assert(!s.ends_with("e"));
}
fn void test_trim()
{
	String s = " \t\nabc ";
	assert(s.trim() == "abc");
	assert("\n\t".trim() == "");
	assert("  \n\tok".trim() == "ok");
	assert("!! \n\t ".trim() == "!!");
	assert(s.trim("c \t") == "\nab");
}

fn void test_split()
{
	String test = "abc|b||c|";
	String[] strings = test.split("|");
	assert(strings.len == 5);
	assert(strings[0] == "abc");
	assert(strings[1] == "b");
	assert(strings[2] == "");
	assert(strings[3] == "c");
	assert(strings[4] == "");
	strings = test.split("|", 2);
	assert(strings.len == 2);
	assert(strings[0] == "abc");
	assert(strings[1] == "b||c|");
}

fn void! test_index_of()
{
	String test = "hello world hello";
	assert(test.index_of("o")! == 4);
	assert(test.index_of("ll")! == 2);
	assert(test.index_of(" hello")! == 11);
	assert(@catch(test.index_of("wi")));
}

fn void! test_rindex_of()
{
	String test = "hello world hello";
	assert(test.rindex_of("o")! == 16);
	assert(test.rindex_of("ll")! == 14);
	assert(test.rindex_of("he")! == 12);
	assert(test.rindex_of("world")! == 6);
	assert(test.rindex_of("hello ")! == 0);
	assert(@catch(test.rindex_of("wi")));
}

fn void! test_index_of_char()
{
	String test = "hello world hello";
	assert(test.index_of_char('o')! == 4);
	assert(test.index_of_char('l')! == 2);
	assert(test.index_of_char('h')! == 0);
	assert(@catch(test.index_of_char('x')));
}

fn void! test_rindex_of_char()
{
	String test = "hello world hello";
	assert(test.rindex_of_char('o')! == 16);
	assert(test.rindex_of_char('l')! == 15);
	assert(test.rindex_of_char('h')! == 12);
	assert(@catch(test.index_of_char('x')));
}

fn void! test_hex_conversion()
{
	assert("0x123aCd".to_long()! == 0x123acd);
	assert("123acD".to_long(16)! == 0x123acd);
}