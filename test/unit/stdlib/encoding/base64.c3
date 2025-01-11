module encoding::base64_test @test;
import std::encoding::base64;

// https://www.rfc-editor.org/rfc/rfc4648#section-10

struct TestCase
{
    char[] in;
    char[] out;
}
import std;

fn void encode()
{
    TestCase[] tcases = {
        { "", "" },
        { "f", "Zg==" },
        { "fo", "Zm8=" },
        { "foo", "Zm9v" },
        { "foob", "Zm9vYg==" },
        { "fooba", "Zm9vYmE=" },
        { "foobar", "Zm9vYmFy" },
        { "test", "dGVzdA==" },
    };
    foreach (tc : tcases)
    {
        @pool()
        {

            usz n = base64::encode_len(tc.in.len, base64::DEFAULT_PAD);
            char[64] buf;
            char[] res = base64::encode_buffer(tc.in, buf[:n]);
            assert(res == tc.out);
        };
    }
}

fn void encode_nopadding()
{
    TestCase[] tcases = {
        { "", "" },
        { "f", "Zg" },
        { "fo", "Zm8" },
        { "foo", "Zm9v" },
        { "foob", "Zm9vYg" },
        { "fooba", "Zm9vYmE" },
        { "foobar", "Zm9vYmFy" },
        { "test", "dGVzdA" },
    };
    foreach (tc : tcases)
    {
        usz n = base64::encode_len(tc.in.len, base64::NO_PAD);
        char[64] buf;
        base64::encode_buffer(tc.in, buf[:n], padding: base64::NO_PAD);
        assert(buf[:n] == tc.out);
    }
}

fn void decode()
{
    TestCase[] tcases = {
        { "", "" },
        { "Zg==", "f" },
        { "Zm8=", "fo" },
        { "Zm9v", "foo" },
        { "Zm9vYg==", "foob" },
        { "Zm9vYmE=", "fooba" },
        { "Zm9vYmFy", "foobar" },
        { "Zm9vYmFy", "foobar" },
        { "dGVzdA==", "test" },
    };
    foreach (tc : tcases)
    {
        usz n = base64::decode_len(tc.in.len, base64::DEFAULT_PAD)!!;
        char[64] buf;
        char[] res = base64::decode_buffer(tc.in, buf[:n])!!;
        assert(res == tc.out);
    }
}

fn void decode_nopadding()
{
    TestCase[] tcases = {
        { "", "" },
        { "Zg", "f" },
        { "Zm8", "fo" },
        { "Zm9v", "foo" },
        { "Zm9vYg", "foob" },
        { "Zm9vYmE", "fooba" },
        { "Zm9vYmFy", "foobar" },
        { "dGVzdA", "test" },
    };
    foreach (tc : tcases)
    {
        usz n = base64::decode_len(tc.in.len, base64::NO_PAD)!!;
        char[64] buf;
        char[] res = base64::decode_buffer(tc.in, buf[:n], base64::NO_PAD)!!;
        assert(res == tc.out);
    }
}

fn void urlencode() {
	TestCase[] tcases = {
		{ x"14fb9c03d97e", "FPucA9l-"},
	};

	@pool()
	{
		usz n;
		char[] got;
		char[64] buf;
		foreach (t : tcases)
		{
			char[] res = base64::encode_buffer(t.in, buf[..], alphabet: &base64::URL);
			assert (res == t.out, "got: %s, want: %s", (String)res, (String)t.out);

			res = base64::decode_buffer(t.out, buf[..], alphabet: &base64::URL)!!;
			assert (res == t.in, "got: %s, want: %s", (String)res, (String)t.in);

		}
	};
}
