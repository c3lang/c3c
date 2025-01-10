module encoding::base32 @test;
import std::encoding::base32;

// https://www.rfc-editor.org/rfc/rfc4648#section-10

struct TestCase
{
    char[] dec;
    char[] enc;
}

TestCase[*] std_tests = {
    { "", "" },
    { "f", "MY======" },
    { "fo", "MZXQ====" },
    { "foo", "MZXW6===" },
    { "foob", "MZXW6YQ=" },
    { "fooba", "MZXW6YTB" },
    { "foobar", "MZXW6YTBOI======" },
};

TestCase[*] hex_tests = {
   { "", "" },
   { "f", "CO======" },
   { "fo", "CPNG====" },
   { "foo", "CPNMU===" },
   { "foob", "CPNMUOG=" },
   { "fooba", "CPNMUOJ1" },
   { "foobar", "CPNMUOJ1E8======" },
};

macro encode_tests(tests, alphabet, padding)
{
    foreach (t : tests)
    {
		char[64] buf;
		usz n = base32::encode_len(t.dec.len, padding);
		base32::encode_buffer(t.dec, buf[:n], padding, alphabet);

		char[] want = t.enc;
		usz! pad_idx = array::index_of(want, '=');
		if (try pad_idx && !padding)
		{
			want = want[:pad_idx];
		}

		assert(buf[:n] == want, "got: %s, want: %s",
			(String)buf[:n], (String)want);
    }
}

fn void encode()
{
	encode_tests(std_tests, &base32::STANDARD, '=');
	encode_tests(hex_tests, &base32::HEX, '=');
}

fn void encode_nopadding()
{
	encode_tests(std_tests, &base32::STANDARD, base32::NO_PAD);
	encode_tests(hex_tests, &base32::HEX, base32::NO_PAD);
}

macro decode_tests(tests, alphabet, padding)
{
    foreach (t : tests)
    {
		char[] input = t.enc[..];
		usz! pad_idx = array::index_of(input, '=');
		if (try pad_idx && !padding)
		{
			input = input[:pad_idx];
		}

		char[64] buf;
		usz n = base32::decode_len(input.len, padding);
		char[] buf2 = base32::decode_buffer(input, buf[:n], padding, alphabet)!!;

		assert(buf2 == t.dec, "got: %s, want: %s", buf2, (String)t.dec);
    }
}

fn void decode()
{
	decode_tests(std_tests, &base32::STANDARD, '=');
	decode_tests(hex_tests, &base32::HEX, '=');
}

fn void decode_nopadding()
{
	decode_tests(std_tests, &base32::STANDARD, base32::NO_PAD);
	decode_tests(hex_tests, &base32::HEX, base32::NO_PAD);
}

fn void base32_api()
{
	@pool()
	{
		foreach (t : std_tests)
		{
			String got = base32::encode_temp(t.dec)!!;
			assert(got == t.enc, "got: %s, want: %s", got, t.enc);

			char[] got_chars = base32::decode_temp(t.enc)!!;
			assert(got_chars == t.dec, "got: %s, want: %s", got_chars, t.dec);
		}
	};
}
