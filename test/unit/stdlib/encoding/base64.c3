module encoding::base64 @test;
import std::encoding::base64;

// https://www.rfc-editor.org/rfc/rfc4648#section-10

struct TestCase
{
    char[] in;
    char[] out;
}

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
            Base64Encoder b;
            b.init(base64::STD_ALPHABET)!;
            usz n = b.encode_len(tc.in.len);
            char[64] buf;
            b.encode(tc.in, buf[:n])!;
            assert(buf[:n] == tc.out);
            assert(base64::encode_temp(tc.in)! == tc.out);
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
        Base64Encoder b;
        b.init(base64::STD_ALPHABET, -1)!;
        usz n = b.encode_len(tc.in.len);
        char[64] buf;
        b.encode(tc.in, buf[:n])!;
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
        Base64Decoder b;
        b.init(base64::STD_ALPHABET)!;
        usz n = b.decode_len(tc.in.len)!;
        char[64] buf;
        usz nn = b.decode(tc.in, buf[:n])!;
        assert(buf[:nn] == tc.out);
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
        Base64Decoder b;
        b.init(base64::STD_ALPHABET, -1)!;
        usz n = b.decode_len(tc.in.len)!;
        char[64] buf;
        usz nn = b.decode(tc.in, buf[:n])!;
        assert(buf[:nn] == tc.out);
    }
}

fn void! urlencode() {
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
			Base64Encoder enc;
			enc.init(base64::URL_ALPHABET)!;
			n = enc.encode(t.in, buf[..])!;
			assert (buf[:n] == t.out, "got: %s, want: %s", (String)buf[:n], (String)t.out);

			got = base64::urlencode_temp(t.in)!;
			assert (got == t.out, "got: %s, want: %s", got, (String)t.out);

			Base64Decoder dec;
			dec.init(base64::URL_ALPHABET)!;
			n = dec.decode(t.out, buf[..])!;
			assert (buf[:n] == t.in, "got: %s, want: %s", (String)buf[:n], (String)t.in);

			got = base64::urldecode_temp(t.out)!;
			assert (got == t.in, "got: %s, want: %s", got, (String)t.in);
		}
	};
}
