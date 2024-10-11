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

macro encode_tests(tests, alphabet, padding) {
    foreach (t : tests)
    {
	Base32Encoder b;
	b.init(alphabet, padding)!!;

	char[64] buf;
	usz n = b.encode_len(t.dec.len);
	b.encode(t.dec, buf[:n])!!;

	char[] want = t.enc;
	usz! pad_idx = array::index_of(want, '=');
	if (try pad_idx && padding < 0) {
		want = want[:pad_idx];
	}

	assert(buf[:n] == want, "got: %s, want: %s",
		(String)buf[:n], (String)want);
    }
}

fn void encode()
{
	encode_tests(std_tests, base32::STD_ALPHABET, '=');
	encode_tests(hex_tests, base32::HEX_ALPHABET, '=');
}

fn void encode_nopadding()
{
	encode_tests(std_tests, base32::STD_ALPHABET, -1);
	encode_tests(hex_tests, base32::HEX_ALPHABET, -1);
}

macro decode_tests(tests, alphabet, padding) {
    foreach (t : tests)
    {
	Base32Decoder b;
	b.init(alphabet, padding)!!;

	char[] input = t.enc[..];
	usz! pad_idx = array::index_of(input, '=');
	if (try pad_idx && padding < 0) {
		input = input[:pad_idx];
	}

	char[64] buf;
	usz n = b.decode_len(input.len);
	n = b.decode(input, buf[:n])!!;

	assert(buf[:n] == t.dec, "got: %s, want: %s",
		(String)buf[:n], (String)t.dec);
    }
}

fn void decode()
{
	decode_tests(std_tests, base32::STD_ALPHABET, '=');
	decode_tests(hex_tests, base32::HEX_ALPHABET, '=');
}

fn void decode_nopadding()
{
	decode_tests(std_tests, base32::STD_ALPHABET, -1);
	decode_tests(hex_tests, base32::HEX_ALPHABET, -1);
}
