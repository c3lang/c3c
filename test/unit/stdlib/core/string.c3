module std::core::string::tests @test;
import std::core::test;

struct StringTestPoint
{
	int x;
	int y;
}


fn void test_starts_with()
{
	String s = "ofke";
	assert(s.starts_with("of"));
	assert(s.starts_with("ofke"));
	assert(!s.starts_with("ofkes"));
	assert(!s.starts_with("ofkf"));
	s = "";
	assert(s.starts_with(""));
	assert(!s.starts_with("o"));
}

fn void string_trim()
{
	String s = "     ";
	assert(s.trim_charset() == "");
}

fn void test_count()
{
	String s = "aaabcabd";
	assert(s.count("a") == 4);
	assert(s.count("aa") == 1);
	assert(s.count("ab") == 2);
	assert(s.count("") == 0);
	assert(s.count("e") == 0);
	String s2 = "abbccc";
	assert(s2.count("a") == 1);
	assert(s2.count("b") == 2);
	assert(s2.count("c") == 3);
	assert(s2.count("ab") == 1);
	assert(s2.count(" ") == 0);
}


fn void test_print_null()
{
	ZString z;
	int* y;
	ZString w = "hello";
	String s = string::format(mem, "%s %s %s", z, w, y);
	defer free(s);
	assert(s == "(null) hello 0x0");
}

fn void test_strip()
{
	String s = "ofke";
	assert(s.strip_prefix("of") == "ke");
	assert(s.strip_prefix("ofke") == "");
	assert(s.strip_prefix("ofkes") == "ofke");
	assert(s.strip_prefix("ofkf") == "ofke");
	assert(s.strip_prefix("") == "ofke");
	s = "";
	assert(s.strip_prefix("") == "");
	assert(s.strip_prefix("o") == "");
}

fn void test_strip_end()
{
	String s = "ofke";
	assert(s.strip_suffix("ke") == "of");
	assert(s.strip_suffix("ofke") == "");
	assert(s.strip_suffix("ofkes") == "ofke");
	assert(s.strip_suffix("ofkf") == "ofke");
	assert(s.strip_suffix("") == "ofke");
	s = "";
	assert(s.strip_suffix("") == "");
	assert(s.strip_suffix("o") == "");
}

fn void test_ends_with()
{
	String s = "ofke";
	assert(s.ends_with("ke"));
	assert(s.ends_with("ofke"));
	assert(!s.ends_with("ofkes"));
	assert(!s.ends_with("ofkf"));
	s = "";
	assert(s.ends_with(""));
	assert(!s.ends_with("e"));
}

fn void test_trim()
{
	String s = " \t\nabc ";
	assert(s.trim() == "abc");
	assert("\n\t".trim() == "");
	assert("  \n\tok".trim() == "ok");
	assert("!! \n\t ".trim() == "!!");
	assert(s.trim("c \t") == "\nab");
	assert("".trim() == "");


}

fn void test_trim_left()
{
	String s = " \t\nabc ";
	assert(s.trim_left() == "abc ");
	assert("\n\t".trim_left() == "");
	assert("  \n\tok".trim_left() == "ok");
	assert("!! \n\t ".trim_left() == "!! \n\t ");
	assert("".trim_left() == "");
}

fn void test_trim_right()
{
	String s = " \t\nabc ";
	assert(s.trim_right() == " \t\nabc");
	assert("\n\t".trim_right() == "");
	assert("  \n\tok".trim_right() == "  \n\tok");
	assert("!! \n\t ".trim_right() == "!!");
	assert("".trim_right() == "");
}

// Regression test for String.trim_charset edge cases.
// The buggy implementation used an inclusive `self[start..len - 1]` slice, which
// underflows or inverts the range when start >= len (i.e., empty input or input
// that consists entirely of trimmed characters).
fn void test_trim_charset_edge_cases()
{
	// Empty input: must produce empty, not panic on `self[0..-1]`.
	assert("".trim_charset() == "");

	// All-whitespace inputs: must produce empty, not panic on `self[len..len-1]`.
	assert("   ".trim_charset() == "");
	assert("\t".trim_charset() == "");
	assert("\t\n\r ".trim_charset() == "");

	// Sanity: the non-edge case still works.
	assert(" hello ".trim_charset() == "hello");
	assert("hello".trim_charset() == "hello");
}

fn void test_split()
{
	String test = "abc|b||c|";
	String[] strings = test.split(mem, "|");
	assert(strings.len == 5);
	assert(strings[0] == "abc");
	assert(strings[1] == "b");
	assert(strings[2] == "");
	assert(strings[3] == "c");
	assert(strings[4] == "");
	free(strings);
	strings = test.split(mem, "|", 2);
	assert(strings.len == 2);
	assert(strings[0] == "abc");
	assert(strings[1] == "b||c|");
	free(strings);
}

fn void test_split_skip_empty()
{
	String test = "abc|b||c|";
	String[] strings = test.split(mem, "|", skip_empty: true);
	assert(strings.len == 3);
	assert(strings[0] == "abc");
	assert(strings[1] == "b");
	assert(strings[2] == "c");
	free(strings);
	strings = test.split(mem, "|", 2, skip_empty: true);
	assert(strings.len == 2);
	assert(strings[0] == "abc");
	assert(strings[1] == "b||c|");
	free(strings);
}

fn void test_split_to_buffer_skip_empty()
{
	String[10] buffer;
	String test = "abc|b||c|";
	String[] strings = test.split_to_buffer("|", &buffer, skip_empty: true)!!;
	assert(strings.len == 3);
	assert(strings[0] == "abc");
	assert(strings[1] == "b");
	assert(strings[2] == "c");
	strings = test.split(mem, "|", 2, skip_empty: true);
	assert(strings.len == 2);
	assert(strings[0] == "abc");
	assert(strings[1] == "b||c|");
	free(strings);
}

fn void test_split_to_buffer()
{
	String[5] b;
	String test = "abc|b||c|";
	String[] strings = test.split_to_buffer("|", &b)!!;
	assert(strings.len == 5);
	assert(strings[0] == "abc");
	assert(strings[1] == "b");
	assert(strings[2] == "");
	assert(strings[3] == "c");
	assert(strings[4] == "");
	String[4] c;
	assert(@catch(test.split_to_buffer("|", &c)) == string::BUFFER_EXCEEDED);
	strings = test.split(mem, "|", 2);
	assert(strings.len == 2);
	assert(strings[0] == "abc");
	assert(strings[1] == "b||c|");
	free(strings);
}

fn void test_zstring()
{
	String test = "hello";
	ZString test2 = "hello";
	ZString test3 = "bye";
	assert(test.zstr_tcopy() == test2);
	assert(test.zstr_tcopy() != test3);
	ZString null_string;
	assert(test != null_string);
	assert(null_string == null_string);
	assert(null_string != test);
}

fn void test_replace()
{
	String test = "Befriend some dragons?";
	String val = test.replace(mem, "some", "all");
	test::eq(val, "Befriend all dragons?");
	free(val);
	val = test.replace(mem, "?", "!");
	test::eq(val, "Befriend some dragons!");
	free(val);
	val = test.replace(mem, "Never", "Always");
	test::eq(val, "Befriend some dragons?");
	free(val);
	test = "some dragonsomesome";
	val = test.replace(mem, "some", "x");
	test::eq(val, "x dragonxx");
	free(val);
}

fn void test_treplace() => @pool()
{
	String test = "Befriend some dragons?";
	String val = test.treplace("some", "all");
	test::eq(val, "Befriend all dragons?");
	val = test.treplace("?", "!");
	test::eq(val, "Befriend some dragons!");
	val = test.treplace("Never", "Always");
	test::eq(val, "Befriend some dragons?");
	test = "some dragonsomesome";
	val = test.treplace("some", "x");
	test::eq(val, "x dragonxx");
}

fn void test_index_of()
{
	String test = "hello world hello";
	assert(test.index_of("o")!! == 4);
	assert(test.index_of("ll")!! == 2);
	assert(test.index_of(" hello")!! == 11);
	assert(@catch(test.index_of("wi")));
}

fn void test_rindex_of()
{
	String test = "hello world hello";
	assert(test.rindex_of("o")!! == 16);
	assert(test.rindex_of("ll")!! == 14);
	assert(test.rindex_of("he")!! == 12);
	assert(test.rindex_of("world")!! == 6);
	assert(test.rindex_of("hello ")!! == 0);
	assert(@catch(test.rindex_of("wi")));
}

fn void test_index_of_char()
{
	String test = "hello world hello";
	assert(test.index_of_char('o')!! == 4);
	assert(test.index_of_char('l')!! == 2);
	assert(test.index_of_char('h')!! == 0);
	assert(@catch(test.index_of_char('x')));
}

fn void test_rindex_of_char()
{
	String test = "hello world hello";
	assert(test.rindex_of_char('o')!! == 16);
	assert(test.rindex_of_char('l')!! == 15);
	assert(test.rindex_of_char('h')!! == 12);
	assert(@catch(test.index_of_char('x')));
}

fn void test_contains()
{
	String test = "hello world hello";
	assert(test.contains("o"));
	assert(test.contains("ll"));
	assert(test.contains(" hello"));
	assert(!test.contains("wi"));
}

fn void contains_char()
{
	String test = "hello world hello";
	assert(test.contains_char('o'));
	assert(test.contains_char('l'));
	assert(test.contains_char('h'));
	assert(!test.contains_char('x'));
}

fn void test_base_13_conversion()
{
	assert("13".to_long(13)!! == 13 + 3);
	assert("1a".to_long(13)!! == 13 + 10);
	assert("aa".to_long(13)!! == 13 * 10 + 10);
}

fn void test_hex_conversion()
{
	assert("0x123aCd".to_long()!! == 0x123acd);
	assert("123acD".to_long(16)!! == 0x123acd);
}

fn void tokenize()
{
	String ex = "foo::bar:baz:";
	Splitter sp = ex.tokenize(":");
	DString str;
    while (try s = sp.next())
    {
        str.append(s);
        str.append("-");
	}
	test::eq(str.str_view(), "foo-bar-baz-");
}

fn void tokenize_all()
{
	String ex = "foo::bar:baz:";
	Splitter sp = ex.tokenize_all(":");
	DString str;
    while (try s = sp.next())
    {
        str.append(s);
        str.append("-");
	}
	test::eq(str.str_view(), "foo--bar-baz--");
}

fn void tokenize_all_skip_last()
{
	String ex = "foo::bar:baz:";
	Splitter sp = ex.tokenize_all(":", skip_last: true);
	DString str;
    while (try s = sp.next())
    {
        str.append(s);
        str.append("-");
	}
	test::eq(str.str_view(), "foo--bar-baz-");
}

fn void test_float()
{
	test::eq_approx(- 2.04632e-05," - 2.04632e-05   ".to_float()!!, delta: 0.00001e-5);
	test::eq_approx(2.04632e-05," + 2.04632e-05   ".to_float()!!, delta: 0.00001e-5);
}

fn void test_pascal_to_snake()
{
	String[2][*] test_cases = {
		{ "HTTPRequest2Handler", "http_request2_handler" },
		{ "MyJSON2XMLConverter", "my_json2_xml_converter"},
		{ "Foo3C3CBazHello", "foo3_c3_c_baz_hello" },
		{ "TLAWithABCs", "tla_with_ab_cs" },
		{ "HTMLToPDFConverter", "html_to_pdf_converter"},
		{ "OAuth2Token", "o_auth2_token" },
		{ "startMIDDLELast", "start_middle_last" }
	};
	foreach (s : test_cases)
	{
		test::eq(s[0].pascal_to_snake_copy(tmem), s[1]);
	}
}

fn void test_snake_pascal()
{
	String[2][*] test_cases = {
		{ "http_request2_handler", "HttpRequest2Handler", },
		{ "my_json2_xml_converter", "MyJson2XmlConverter", },
		{ "foo3_c3_c_baz_hello", "Foo3C3CBazHello",  },
		{ "tla_with_ab_cs", "TlaWithAbCs",  },
		{ "html_to_pdf_converter", "HtmlToPdfConverter", },
		{ "o_auth2_token", "OAuth2Token",  },
		{ "start_middle_last", "StartMiddleLast",  }
	};
	foreach (s : test_cases)
	{
		test::eq(s[0].snake_to_pascal_copy(tmem), s[1]);
	}
}

fn void test_snake_pascal_self_modify()
{
	String[2][*] test_cases = {
		{ "http_request2_handler", "HttpRequest2Handler", },
		{ "my_json2_xml_converter", "MyJson2XmlConverter", },
		{ "foo3_c3_c_baz_hello", "Foo3C3CBazHello",  },
		{ "tla_with_ab_cs", "TlaWithAbCs",  },
		{ "html_to_pdf_converter", "HtmlToPdfConverter", },
		{ "o_auth2_token", "OAuth2Token",  },
		{ "start_middle_last", "StartMiddleLast",  }
	};
	foreach (s : test_cases)
	{
		String s2 = s[0].tcopy();
		s2.convert_snake_to_pascal();
		test::eq(s2, s[1]);
	}
}

fn void test_join()
{
	String[] parts = { "a", "bb", "ccc" };
	String joined = string::join(mem, parts, ", ");
	test::eq(joined, "a, bb, ccc");
	joined.free(mem);
	test::eq(string::tjoin(parts, ", "), "a, bb, ccc");

	String[] one = { "solo" };
	String joined_one = string::join(mem, one, ", ");
	test::eq(joined_one, "solo");
	joined_one.free(mem);

	String[] empty;
	String joined_empty = string::join(mem, empty, ", ");
	test::eq(joined_empty, "");
	joined_empty.free(mem);

	String[] with_empty = { "", "b", "" };
	String joined_with_empty = string::join(mem, with_empty, "-");
	test::eq(joined_with_empty, "-b-");
	joined_with_empty.free(mem);

	String[] no_joiner = { "a", "b", "c" };
	String joined_no_joiner = string::join(mem, no_joiner, "");
	test::eq(joined_no_joiner, "abc");
	joined_no_joiner.free(mem);
}

fn void test_compare_to()
{
	test::@check(("abc").compare_to("abd") < 0);
	test::@check(("abd").compare_to("abc") > 0);
	test::eq(("abc").compare_to("abc"), 0);
	test::@check(("ab").compare_to("abc") < 0);
	test::@check(("abc").compare_to("ab") > 0);
}

fn void test_compare_to_ignore_case()
{
	test::eq(("ABC").compare_to_ignore_case("abc"), 0);
	test::@check(("abc").compare_to_ignore_case("ABD") < 0);
	test::@check(("ABD").compare_to_ignore_case("abc") > 0);
}

fn void test_concat()
{
	test::eq(("foo").concat(tmem, "bar"), "foobar");
	test::eq(("foo").tconcat("bar"), "foobar");
	test::eq(("").concat(tmem, "bar"), "bar");
	test::eq(("foo").concat(tmem, ""), "foo");
}

fn void test_copy_family()
{
	String original = "hello";
	String copied = original.copy(mem);
	test::eq(copied, original);
	test::@check(copied.ptr != original.ptr);
	copied.free(mem);

	String tcopied = original.tcopy();
	test::eq(tcopied, original);

	ZString z = original.zstr_copy(mem);
	test::eq(z.str_view(), original);
	mem::free(z);

	ZString tz = original.zstr_tcopy();
	test::eq(tz.str_view(), original);
}

fn void test_zstring_copy_roundtrip()
{
	ZString z = "hello";
	String s = z.copy(mem);
	test::eq(s, "hello");
	s.free(mem);

	String ts = z.tcopy();
	test::eq(ts, "hello");
}

fn void test_case_conversion()
{
	test::eq(("Hello World").to_lower_copy(tmem), "hello world");
	test::eq(("Hello World").to_lower_tcopy(), "hello world");
	test::eq(("Hello World").to_upper_copy(tmem), "HELLO WORLD");
	test::eq(("Hello World").to_upper_tcopy(), "HELLO WORLD");

	String s = ("Hello").tcopy();
	s.convert_to_lower();
	test::eq(s, "hello");

	String s2 = ("Hello").tcopy();
	s2.convert_to_upper();
	test::eq(s2, "HELLO");
}

fn void test_capitalize_copy()
{
	test::eq(("hello").capitalize_copy(tmem), "Hello");
	test::eq(("Hello").capitalize_copy(tmem), "Hello");
	test::eq(("").capitalize_copy(tmem), "");
	test::eq(("123abc").capitalize_copy(tmem), "123abc");
}

fn void test_char_in_set()
{
	test::@check(string::char_in_set('a', "abc"));
	test::@check(!string::char_in_set('z', "abc"));
	test::@check(!string::char_in_set('a', ""));
}

fn void test_utf16_roundtrip()
{
	String original = "hello world";
	Char16[]? u16 = original.to_utf16(mem);
	test::@check(@ok(u16));
	if (try u16v = u16)
	{
		defer mem::free(u16v.ptr);
		String? back = string::from_utf16(mem, u16v);
		test::@check(@ok(back));
		if (try backv = back)
		{
			test::eq(backv, original);
			backv.free(mem);
		}
	}

	Char16[]? tu16 = original.to_temp_utf16();
	test::@check(@ok(tu16));
	if (try tu16v = tu16)
	{
		String? tback = string::tfrom_utf16(tu16v);
		if (try tbackv = tback) test::eq(tbackv, original);
	}
}

fn void test_utf32_roundtrip()
{
	String original = "hello world";
	Char32[]? u32 = original.to_utf32(mem);
	test::@check(@ok(u32));
	if (try u32v = u32)
	{
		defer mem::free(u32v.ptr);
		String? back = string::from_utf32(mem, u32v);
		test::@check(@ok(back));
		if (try backv = back)
		{
			test::eq(backv, original);
			backv.free(mem);
		}
	}
}

fn void test_wstring_roundtrip()
{
	String original = "hello";
	WString? w = original.to_wstring(mem);
	test::@check(@ok(w));
	if (try wv = w)
	{
		defer mem::free(wv);
		String? back = string::from_wstring(mem, wv);
		test::@check(@ok(back));
		if (try backv = back)
		{
			test::eq(backv, original);
			backv.free(mem);
		}
	}
}

fn void test_utf8_codepoints()
{
	test::eq(("hello").utf8_codepoints(), (sz)5);
	test::eq(("").utf8_codepoints(), (sz)0);
}

fn void test_index_of_chars()
{
	String s = "hello";
	char[] chars = { 'l', 'z' };
	sz? index = s.index_of_chars(chars);
	test::@check(@ok(index));
	if (try i = index) test::eq(i, (sz)2);

	char[] none = { 'x', 'y' };
	test::@check(!@ok(s.index_of_chars(none)));
}

fn void test_from_struct()
{
	StringTestPoint p = { 1, 2 };
	String s = string::from_struct(mem, p);
	test::@check(s.len > 0);

	String s2 = string::tfrom_struct(p);
	test::eq(s, s2);
	s.free(mem);
}

fn void test_splitter_reset_and_at_end()
{
	Splitter splitter = ("a:b:c").tokenize(":");
	test::@check(!splitter.at_end());
	while (try tok = splitter.next()) {}
	test::@check(splitter.at_end());
	splitter.reset();
	test::@check(!splitter.at_end());
	if (try first = splitter.next()) test::eq(first, "a");
}

fn void test_string_free()
{
	String s = ("hello").copy(mem);
	s.free(mem);
	test::eq(s, "");

	String empty;
	empty.free(mem);
	test::eq(empty, "");
}
