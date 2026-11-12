module tokenize_empty_split;

fn String[] collect(Splitter s)
{
	String[] res = mem::talloc_array(String, 8);
	usz n;
	while (try t = s.next()) res[n++] = t;
	assert(s.at_end());
	return res[:n];
}

fn void tokenize_empty_split_gives_each_character() @test
{
	assert(collect("abc".tokenize("")) == { "a", "b", "c" });
	assert(collect("abc".tokenize_all("")) == { "a", "b", "c" });
	assert(collect("abc".tokenize_all("", skip_last: true)) == { "a", "b", "c" });
	assert(collect("".tokenize("")).len == 0);
	assert(collect("".tokenize_all("")).len == 0);
}

fn void tokenize_empty_split_reset() @test
{
	Splitter s = "ab".tokenize("");
	assert(s.next()!! == "a");
	s.reset();
	assert(collect(s) == { "a", "b" });
}

fn void tokenize_non_empty_split_unchanged() @test
{
	assert(collect("a,b,,c".tokenize(",")) == { "a", "b", "c" });
	assert(collect("a,b,,c".tokenize_all(",")) == { "a", "b", "", "c" });
	assert(collect("a,b,".tokenize_all(",")) == { "a", "b", "" });
}
