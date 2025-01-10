module std::io @test;

fn void test_multiwriter()
{
	ByteWriter w1, w2;
	MultiWriter mw;
	mw.temp_init(w1.temp_init(), w2.temp_init());
	defer mw.free();

	String want = "foobar";
	io::copy_to(ByteReader{}.init(want), &mw)!!;
	
	assert(w1.str_view() == want,
		"invalid write; got: %s, want: %s", w1.str_view(), want);
	assert(w2.str_view() == want,
		"invalid write; got: %s, want: %s", w2.str_view(), want);
}
