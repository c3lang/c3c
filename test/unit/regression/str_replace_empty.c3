module str_replace_empty;

fn void str_replace_empty_pattern_inserts_between_chars() @test
{
	assert(@str_replace("abc", "", "x") == "xaxbxcx");
	assert(@str_replace("", "", "x") == "x");
	assert(@str_replace("abc", "", "x", 2) == "xaxbc");
	assert(@str_replace("abcb", "b", "x") == "axcx");
}
