module encoding::hex @test;
import std::encoding::hex;

struct TestCase
{
    char[] dec;
    char[] enc;
}

TestCase[] tests = {
	{"", ""},
	{{'g'}, "67"},
	{{0,1,2,3,4,5,6,7}, "0001020304050607"},
	{{8,9,10,11,12,13,14,15}, "08090a0b0c0d0e0f"},
	{{0xf0, 0xf1, 0xf2, 0xf3}, "f0f1f2f3"},
	{{0xe3, 0xa1}, "e3a1"},
	{{0xe3, 0xa1}, "E3A1"},
};

fn void encode()
{
	usz n;
	char[64] buf;
	foreach (t : tests)
	{
		n = hex::encode_bytes(t.dec, buf[..]);
		String want = ((String)t.enc).temp_ascii_to_lower();
		assert(want == buf[:n], "encode failed: got: %s, want: %s", buf[:n], want);
		@pool()
		{
			assert(want == hex::encode_temp(t.dec));
		};
	}
}

fn void decode()
{
	usz n;
	char[64] buf;
	foreach (t : tests)
	{
		n = hex::decode_bytes(t.enc, buf[..])!!;
		assert(t.dec == buf[:n], "decode failed: got: %s, want: %s", buf[:n], t.dec);
		@pool()
		{
			assert(t.dec == hex::decode_temp(t.enc)!!);
		};
	}
}
