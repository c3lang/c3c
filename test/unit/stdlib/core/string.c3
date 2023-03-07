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

fn void test_trim()
{
	String s = " \t\nabc ";
	assert(s.trim() == "abc");
	assert(((String)"\n\t").trim() == "");
	assert(((String)"  \n\tok").trim() == "ok");
	assert(((String)"!! \n\t ").trim() == "!!");
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
	assert(test.index_of("o")? == 4);
	assert(test.index_of("ll")? == 2);
	assert(catch(test.index_of("wi")));
}

fn void! test_rindex_of()
{
	String test = "hello world hello";
	assert(test.rindex_of("o")? == 16);
	assert(test.rindex_of("ll")? == 14);
	assert(test.rindex_of("he")? == 12);
	assert(test.rindex_of("world")? == 6);
	assert(catch(test.rindex_of("wi")));
}