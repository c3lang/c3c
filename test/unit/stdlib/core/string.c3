module std::core::string::tests @test;
import std::core::test;


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
	assert(s.strip("of") == "ke");
	assert(s.strip("ofke") == "");
	assert(s.strip("ofkes") == "ofke");
	assert(s.strip("ofkf") == "ofke");
	assert(s.strip("") == "ofke");
	s = "";
	assert(s.strip("") == "");
	assert(s.strip("o") == "");
}

fn void test_strip_end()
{
	String s = "ofke";
	assert(s.strip_end("ke") == "of");
	assert(s.strip_end("ofke") == "");
	assert(s.strip_end("ofkes") == "ofke");
	assert(s.strip_end("ofkf") == "ofke");
	assert(s.strip_end("") == "ofke");
	s = "";
	assert(s.strip_end("") == "");
	assert(s.strip_end("o") == "");
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

fn void test_try_to_number_conversions()
{
	int128 i128 = 0;
	long l = 0;
	int i = 0;
	short s = 0;
	ichar ic = 0;
	uint128 u128 = 0;
	ulong ul = 0;
	uint u = 0;
	ushort us = 0;
	char uc = 0;
	double d = 0;
	float f = 0;

	assert("12345".try_to_int128(&i128));
	assert(i128 == 12345LL);
	assert("-1234".try_to_long(&l));
	assert(l == -1234L);
	assert("-27".try_to_int(&i));
	assert(i == (int)-27);
	assert("-32000".try_to_short(&s));
	assert(s == (short)-32000);
	assert("-120".try_to_ichar(&ic));
	assert(ic == (ichar)-120);

	assert("12345".try_to_uint128(&u128));
	assert(u128 == (uint128)12345);
	assert("765".try_to_ulong(&ul));
	assert(ul == (ulong)765);
	assert("0".try_to_uint(&u));
	assert(u == (uint)0);
	assert("65535".try_to_ushort(&us));
	assert(us == (ushort)65535);
	assert("255".try_to_uchar(&uc));
	assert(uc == (char)255);
	i = 0;
	assert("2a".try_to_integer(int, &i, 16));
	assert(i == (int)42);

	assert("3.125".try_to_double(&d));
	test::eq_approx(d, 3.125, delta: 0.000000000001);
	assert("1.5".try_to_float(&f));
	test::eq_approx(f, 1.5f, delta: 0.00001f);

	i = (int)42;
	assert(!"abc".try_to_int(&i));
	assert(i == (int)42);
	assert(!"10".try_to_int(&i, 17));
	assert(i == (int)42);
	assert(!"10".try_to_integer(int, &i, 0));
	assert(i == (int)42);
	uc = 7;
	assert(!"256".try_to_uchar(&uc));
	assert(uc == (char)7);
	f = 1.0f;
	assert(!"+".try_to_float(&f));
	assert(f == 1.0f);
	d = 2.0;
	assert(!"-".try_to_double(&d));
	assert(d == 2.0);
}

fn void test_to_number_or_conversions()
{

	assert("12345".to_integer_or(int, (int)7) == (int)12345);
	assert("-1234".to_integer_or(long, 9) == -1234L);
	assert("".to_integer_or(uint, 11) == 11u);
	assert("NaN".to_integer_or(uint, 420) == 420u);
	assert("-1".to_integer_or(uint, 13) == 13u);
	assert("2a".to_integer_or(int, (int)0, 16) == (int)42);
	assert("10".to_integer_or(int, (int)99, 17) == (int)99);
	assert("ff".to_integer_or(uint, 3, 16) == (uint)255);

	test::eq_approx("3.125".to_real_or(double, 2.0), 3.125, delta: 0.000000000001);
	test::eq_approx("1.5".to_real_or(float, 2.0f), 1.5f, delta: 0.00001f);
	assert("+".to_real_or(double, 8.0) == 8.0);
	assert("-".to_real_or(float, 6.0f) == 6.0f);
	test::eq_approx("2.5".to_real_or(double, 1.0), 2.5, delta: 0.000000000001);
	assert(("44".to_int() ?? (int)9) == (int)44);
	assert(("bad".to_int() ?? (int)9) == (int)9);
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
