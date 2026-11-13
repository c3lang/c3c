module string_replace_empty_needle;

fn void string_replace_empty_needle_matches_dstring() @test
{
	assert("abc".treplace("", "x") == "xaxbxcx");
	assert("".treplace("", "x") == "x");
	assert("abc".treplace("", "") == "abc");
	assert("abc".replace(tmem, "", "x") == "xaxbxcx");
	DString d;
	d.tinit();
	d.append("abc");
	d.replace("", "x");
	assert(d.str_view() == "abc".treplace("", "x"));
}

fn void string_replace_normal_needle_unchanged() @test
{
	assert("abcb".treplace("b", "x") == "axcx");
	assert("abc".treplace("d", "x") == "abc");
}
