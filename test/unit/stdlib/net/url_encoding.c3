module url_encode_test @test;

import std::io;
import std::net::url @public;

struct EncodeTest
{
	String in;
	String out;
	anyfault err;
	UrlEncodingMode mode;
}

EncodeTest[*] decode_with_error_tests @local = {
	{
		"",
		"",
		anyfault{},
		UrlEncodingMode.QUERY,
	},
	{
		"abc",
		"abc",
		anyfault{},
		UrlEncodingMode.QUERY,
	},
	{
		"1%41",
		"1A",
		anyfault{},
		UrlEncodingMode.QUERY,
	},
	{
		"1%41%42%43",
		"1ABC",
		anyfault{},
		UrlEncodingMode.QUERY,
	},
	{
		"%4a",
		"J",
		anyfault{},
		UrlEncodingMode.QUERY,
	},
	{
		"%6F",
		"o",
		anyfault{},
		UrlEncodingMode.QUERY,
	},
	{
		"%",
		"",
		UrlDecodingError.INVALID_HEX,
		UrlEncodingMode.QUERY,
	},
	{
		"%a",
		"",
		UrlDecodingError.INVALID_HEX,
		UrlEncodingMode.QUERY,
	},
	{
		"%1",
		"",
		UrlDecodingError.INVALID_HEX,
		UrlEncodingMode.QUERY,
	},
	{
		"123%45%6",
		"",
		UrlDecodingError.INVALID_HEX,
		UrlEncodingMode.QUERY,
	},
	{
		"%zzzzz",
		"",
		UrlDecodingError.INVALID_HEX,
		UrlEncodingMode.QUERY,
	},
	{
		"a+b",
		"a b",
		anyfault{},
		UrlEncodingMode.QUERY,
	},
	{
		"a%20b",
		"a b",
		anyfault{},
		UrlEncodingMode.QUERY,
	},
};

fn void test_decoding_with_error()
{
	String! actual;
	@pool() {
		foreach (test: decode_with_error_tests)
		{
			actual = url::temp_decode(test.in, test.mode);
			if (catch excuse = actual)
			{
				assert(excuse == test.err, "unescape(%s, %s); "
					"got: %s, want: %s", test.in, test.mode, excuse, test.err);
				continue;
			}
			assert(actual == test.out, "unescape(%s, %s); "
				"got: %s, want: %s", test.in, test.mode, actual, test.out);
		}
	};
}

EncodeTest[*] encode_tests @local = {
	{
		"",
		"",
		anyfault{},
		UrlEncodingMode.PATH,
	},
	{
		"abc",
		"abc",
		anyfault{},
		UrlEncodingMode.PATH,
	},
	{
		"abc+def",
		"abc+def",
		anyfault{},
		UrlEncodingMode.PATH,
	},
	{
		"a/b",
		"a/b",
		anyfault{},
		UrlEncodingMode.PATH,
	},
	{
		"one two",
		"one%20two",
		anyfault{},
		UrlEncodingMode.PATH,
	},
	{
		"10%",
		"10%25",
		anyfault{},
		UrlEncodingMode.PATH,
	},
	{
		"",
		"",
		anyfault{},
		UrlEncodingMode.QUERY,
	},
	{
		"abc",
		"abc",
		anyfault{},
		UrlEncodingMode.QUERY,
	},
	{
		"one two",
		"one+two",
		anyfault{},
		UrlEncodingMode.QUERY,
	},
	{
		"10%",
		"10%25",
		anyfault{},
		UrlEncodingMode.QUERY,
	},
	{
		" ?&=#+%!<>#\"{}|\\^[]`☺\t:/@$'()*,;",
		"+%3F%26%3D%23%2B%25%21%3C%3E%23%22%7B%7D%7C%5C%5E%5B%5D%60%E2%98%BA%09%3A%2F%40%24%27%28%29%2A%2C%3B",
		anyfault{},
		UrlEncodingMode.QUERY,
	},

};

fn void test_percent_encode_and_decode()
{
	String actual;
	@pool() {
		foreach (test: encode_tests)
		{
			actual = url::temp_encode(test.in, test.mode);
			assert(actual == test.out, "escape(%s, %s); "
				"got: %s, want: %s", test.in, test.mode, actual, test.out);

			actual = url::temp_decode(test.out, test.mode)!!;
			assert(actual == test.in, "unescape(%s, %s); "
				"got: %s, want: %s", test.out, test.mode, actual, test.in);
		}
	};
}

struct ShouldEncodeTest
{
	char in;
	UrlEncodingMode mode;
	bool escape;
}

ShouldEncodeTest[*] should_encode_tests = {
	{'a', UrlEncodingMode.PATH, false},
	{'a', UrlEncodingMode.USERPASS, false},
	{'a', UrlEncodingMode.QUERY, false},
	{'a', UrlEncodingMode.FRAGMENT, false},
	{'a', UrlEncodingMode.HOST, false},
	{'z', UrlEncodingMode.PATH, false},
	{'A', UrlEncodingMode.PATH, false},
	{'Z', UrlEncodingMode.PATH, false},
	{'0', UrlEncodingMode.PATH, false},
	{'9', UrlEncodingMode.PATH, false},
	{'-', UrlEncodingMode.PATH, false},
	{'-', UrlEncodingMode.USERPASS, false},
	{'-', UrlEncodingMode.QUERY, false},
	{'-', UrlEncodingMode.FRAGMENT, false},
	{'.', UrlEncodingMode.PATH, false},
	{'_', UrlEncodingMode.PATH, false},
	{'~', UrlEncodingMode.PATH, false},

	{'/', UrlEncodingMode.USERPASS, true},
	{'?', UrlEncodingMode.USERPASS, true},
	{'@', UrlEncodingMode.USERPASS, true},
	{'$', UrlEncodingMode.USERPASS, false},
	{'&', UrlEncodingMode.USERPASS, false},
	{'+', UrlEncodingMode.USERPASS, false},
	{',', UrlEncodingMode.USERPASS, false},
	{';', UrlEncodingMode.USERPASS, false},
	{'=', UrlEncodingMode.USERPASS, false},

	{'!', UrlEncodingMode.HOST, false},
	{'$', UrlEncodingMode.HOST, false},
	{'&', UrlEncodingMode.HOST, false},
	{'\'', UrlEncodingMode.HOST, false},
	{'(', UrlEncodingMode.HOST, false},
	{')', UrlEncodingMode.HOST, false},
	{'*', UrlEncodingMode.HOST, false},
	{'+', UrlEncodingMode.HOST, false},
	{',', UrlEncodingMode.HOST, false},
	{';', UrlEncodingMode.HOST, false},
	{'=', UrlEncodingMode.HOST, false},
	{'0', UrlEncodingMode.HOST, false},
	{'9', UrlEncodingMode.HOST, false},
	{'A', UrlEncodingMode.HOST, false},
	{'z', UrlEncodingMode.HOST, false},
	{'_', UrlEncodingMode.HOST, false},
	{'-', UrlEncodingMode.HOST, false},
	{'.', UrlEncodingMode.HOST, false},
};

fn void test_should_encode()
{
	bool actual;
	foreach (test: should_encode_tests)
	{
		actual = url::should_encode(test.in, test.mode);
		assert(actual == test.escape, "should_encode(%c, %s); "
			"got: %s, want: %s", test.in, test.mode, actual, test.escape);
	}
}
