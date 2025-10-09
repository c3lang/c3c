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

fn void test_treplace()
{
	String test = "Befriend some dragons?";
	assert(test.treplace("some", "all") == "Befriend all dragons?");
	assert(test.treplace("?", "!") == "Befriend some dragons!");
	assert(test.treplace("Never", "Always") == "Befriend some dragons?");
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
