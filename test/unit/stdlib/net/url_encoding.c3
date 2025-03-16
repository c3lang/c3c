module url_encode_test @test;

import std::io;
import std::net::url @public;

struct EncodeTest
{
	String in;
	String out;
	fault err;
	UrlEncodingMode mode;
}

EncodeTest[*] decode_with_error_tests @local = {
	{ "", "", {}, QUERY, },
	{ "abc", "abc", {}, QUERY, },
	{ "1%41", "1A", {}, QUERY, },
	{ "1%41%42%43", "1ABC", {}, QUERY, },
	{ "%4a", "J", {}, QUERY, },
	{ "%6F", "o", {}, QUERY, },
	{ "%", "", url::INVALID_HEX, QUERY, },
	{ "%a", "", url::INVALID_HEX, QUERY, },
	{ "%1", "", url::INVALID_HEX, QUERY, },
	{ "123%45%6", "", url::INVALID_HEX, QUERY, },
	{ "%zzzzz", "", url::INVALID_HEX, QUERY, },
	{ "a+b", "a b", {}, QUERY, },
	{ "a%20b", "a b", {}, QUERY, },
};

fn void test_decoding_with_error() => @pool()
{
	foreach (test: decode_with_error_tests)
	{
		String? actual = url::tdecode(test.in, test.mode);
		if (catch excuse = actual)
		{
			assert(excuse == test.err, "unescape(%s, %s); "
					"got: %s, want: %s", test.in, test.mode, excuse, test.err);
			continue;
		}
		assert(actual == test.out, "unescape(%s, %s); "
			"got: %s, want: %s", test.in, test.mode, actual, test.out);
	}
}

EncodeTest[*] encode_tests @local = {
	{ "", "", {}, PATH, },
	{ "abc", "abc", {}, PATH, },
	{ "abc+def", "abc+def", {}, PATH, },
	{ "a/b", "a/b", {}, PATH, },
	{ "one two", "one%20two", {}, PATH, },
	{ "10%", "10%25", {}, PATH, },
	{ "", "", {}, QUERY, },
	{ "abc", "abc", {}, QUERY, },
	{ "one two", "one+two", {}, QUERY, },
	{ "10%", "10%25", {}, QUERY, },
	{ " ?&=#+%!<>#\"{}|\\^[]`☺\t:/@$'()*,;",
	  "+%3F%26%3D%23%2B%25%21%3C%3E%23%22%7B%7D%7C%5C%5E%5B%5D%60%E2%98%BA%09%3A%2F%40%24%27%28%29%2A%2C%3B",
	  {}, QUERY,
	},
};

fn void test_percent_encode_and_decode() => @pool()
{
	foreach (test: encode_tests)
	{
		String actual = url::tencode(test.in, test.mode);
		assert(actual == test.out, "escape(%s, %s); "
			"got: %s, want: %s", test.in, test.mode, actual, test.out);

		actual = url::tdecode(test.out, test.mode)!!;
		assert(actual == test.in, "unescape(%s, %s); "
				"got: %s, want: %s", test.out, test.mode, actual, test.in);
	}
}

struct ShouldEncodeTest
{
	char in;
	UrlEncodingMode mode;
	bool escape;
}

ShouldEncodeTest[*] should_encode_tests = {
	{'a', PATH, false},
	{'a', USERPASS, false},
	{'a', QUERY, false},
	{'a', FRAGMENT, false},
	{'a', HOST, false},
	{'z', PATH, false},
	{'A', PATH, false},
	{'Z', PATH, false},
	{'0', PATH, false},
	{'9', PATH, false},
	{'-', PATH, false},
	{'-', USERPASS, false},
	{'-', QUERY, false},
	{'-', FRAGMENT, false},
	{'.', PATH, false},
	{'_', PATH, false},
	{'~', PATH, false},

	{'/', USERPASS, true},
	{'?', USERPASS, true},
	{'@', USERPASS, true},
	{'$', USERPASS, false},
	{'&', USERPASS, false},
	{'+', USERPASS, false},
	{',', USERPASS, false},
	{';', USERPASS, false},
	{'=', USERPASS, false},

	{'!', HOST, false},
	{'$', HOST, false},
	{'&', HOST, false},
	{'\'',HOST, false},
	{'(', HOST, false},
	{')', HOST, false},
	{'*', HOST, false},
	{'+', HOST, false},
	{',', HOST, false},
	{';', HOST, false},
	{'=', HOST, false},
	{'0', HOST, false},
	{'9', HOST, false},
	{'A', HOST, false},
	{'z', HOST, false},
	{'_', HOST, false},
	{'-', HOST, false},
	{'.', HOST, false},
};

fn void test_should_encode()
{
	foreach (test : should_encode_tests)
	{
		bool actual = url::should_encode(test.in, test.mode);
		assert(actual == test.escape, "should_encode(%c, %s); "
			"got: %s, want: %s", test.in, test.mode, actual, test.escape);
	}
}
