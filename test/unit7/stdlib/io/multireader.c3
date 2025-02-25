module std::io @test;

fn void test_multireader()
{
	MultiReader mr;
	mr.temp_init(
		&&wrap_bytes("foo"),
		&&wrap_bytes(" "),
		&&wrap_bytes("bar"),
		&&wrap_bytes("!"),
	);
	defer mr.free();

	ByteWriter w;
	io::copy_to(&mr, w.temp_init())!!;
	
	String want = "foo bar!";
	assert(w.str_view() == want,
		"invalid data read; got: %s, want: %s", w.str_view(), want);
}

