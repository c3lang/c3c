module std::io @test;

fn void test_teereader()
{
	String want = "foobar";

	ByteWriter w;
	TeeReader r = tee_reader(ByteReader{}.init(want), w.temp_init());

	char[16] buf;
	usz n = r.read(buf[..])!!;

	String got = w.str_view();
	assert(n == want.len, "teereader: invalid length");
	assert(got == want, "teereader: got: %s, want: %s", got, want);
	assert(got == (String)buf[:n], "teereader: got: %s, want: %s", got, (String)buf[:n]);
}
