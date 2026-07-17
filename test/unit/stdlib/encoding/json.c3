module json_test @test;
import std::collections::object;
import std::io;
import std::encoding::json;

fn void test_comments()
{
	Object* x = json::parse(tmem, `{ "a":
		// hello
23,
		/* test */ }`)!!;
	test::eq(string::tformat("%s", x), `{"a":23}`);
}

fn void tparse_respects_flavor()
{
	test::@error(json::tparse(`[1,]`, JSON), json::UNEXPECTED_CHARACTER);
	json::tparse(`[1,]`)!!;
}

fn void parse_unicode_surrogate_pair()
{
	Object* o = json::tparse(`"\uD83D\uDE00"`)!!;
	test::@check(o.s == "\xF0\x9F\x98\x80");
}

fn void parse_lone_surrogate_rejected()
{
	test::@error(json::tparse(`"\uD83D"`), json::INVALID_ESCAPE_SEQUENCE);
	test::@error(json::tparse(`"\uDE00"`), json::INVALID_ESCAPE_SEQUENCE);
	test::@error(json::tparse(`"\uD83Dx"`), json::INVALID_ESCAPE_SEQUENCE);
}

fn void parse_number_requires_fractional_digit()
{
	test::@error(json::tparse(`1.`),  json::INVALID_NUMBER);
	test::@error(json::tparse(`1.e5`), json::INVALID_NUMBER);
	json::tparse(`1.5`)!!;
	json::tparse(`1.5e2`)!!;
}

fn void test_comments_unsupported()
{
	Object*? x = json::parse(tmem, `{ "a":
		// hello
23,
		/* test */ }`, flavor: JSON);
	test::@error(x);
}

fn void simple_test()
{
	ByteReader reader;
	reader.init(`{ "b": 123, "c": [ { "d": 66 }, null, "hello\tworld", false, { "id": "xyz" } ] }`);
	Object* o = json::load(mem, &reader)!!;
	defer o.free();
	test::eq(o.get_int("b")!!, 123);
	test::eq(o.get("c").get_len()!!, 5);
	test::eq(o.get("c").get_at(0).get_int("d")!!, 66);
	test::@check(o.get("c").get_at(1).is_null()!!);
	test::eq(o.get("c").get_string_at(2)!!, "hello\tworld");
	test::eq(o.get("c").get_bool_at(3)!!, false);
	test::eq(o.get("c").get_at(4).get_string("id")!!, "xyz");
}

fn void simple_test2()
{
	ByteReader reader;
	reader.init(`{"jsonrpc":"2.0","id":null,"method":"initialize"}`);
	Object* o = json::load(mem, &reader)!!;
	defer o.free();
}

fn void test_string()
{
	Object* o = json::parse(mem, `{"jsonrpc":"2","id":null,"method":"initialize"}`)!!;
	defer o.free();
	String s = string::tformat("%s", *o);
	Object* o2 = json::parse(mem, s)!!;
	defer o2.free();
	String s2 = string::tformat("%s", *o2);
	test::@check(s2 == s, "Unexpectedly got %s and not %s", s2, s);
}

fn void test_temp_string()
{
	Object* o = json::tparse(`{"jsonrpc":"2","id":null,"method":"initialize"}`)!!;
	defer o.free();
	String s = string::tformat("%s", *o);
	Object* o2 = json::tparse(s)!!;
	defer o2.free();
	String s2 = string::tformat("%s", *o2);
	test::@check(s2 == s, "Unexpectedly got %s and not %s", s2, s);
}

fn void test_valid_numbers()
{
	Object* o1 = json::parse(mem, "123")!!;   defer o1.free();
	test::eq(o1.f, 123.0);

	Object* o2 = json::parse(mem, "-456")!!;  defer o2.free();
	test::eq(o2.f, -456.0);

	Object* o3 = json::parse(mem, "123.456")!!; defer o3.free();
	test::eq(o3.f, 123.456);

	Object* o4 = json::parse(mem, "-123.456")!!; defer o4.free();
	test::eq(o4.f, -123.456);

	Object* o5 = json::parse(mem, "1.23e10")!!; defer o5.free();
	test::eq(o5.f, 1.23e10);

	Object* o6 = json::parse(mem, "1.23e-10")!!; defer o6.free();
	test::eq(o6.f, 1.23e-10);

	Object* o7 = json::parse(mem, "1.23e+10")!!; defer o7.free();
	test::eq(o7.f, 1.23e+10);

	Object* o8 = json::parse(mem, "0")!!;    defer o8.free();
	test::eq(o8.f, 0.0);

	Object* o9 = json::parse(mem, "0.0")!!;  defer o9.free();
	test::eq(o9.f, 0.0);

	Object* o10 = json::parse(mem, "-0")!!;  defer o10.free();
	test::eq(o10.f, 0.0);

	Object* o11 = json::parse(mem, "0e10")!!; defer o11.free();
	test::eq(o11.f, 0.0);

	Object* o12 = json::parse(mem, "0.0e10")!!; defer o12.free();
	test::eq(o12.f, 0.0);
}

fn void test_duplicate_keys()
{
	// Last value wins when a key appears more than once.
	Object* o1 = json::parse(mem, `{"a":"first","a":"second"}`)!!;
	defer o1.free();
	test::@check(o1.has_key("a"));
	test::eq(o1.get_string("a")!!, "second");

	Object* o2 = json::parse(mem, `{"x":1,"x":"string","x":true}`)!!;
	defer o2.free();
	test::eq(o2.get_bool("x")!!, true);

	Object* o3 = json::parse(mem, `{"obj":{"a":1},"obj":{"b":2}}`)!!;
	defer o3.free();
	Object* nested = o3.get("obj")!!;
	test::@check(nested.has_key("b"));
	test::eq(nested.get_int("b")!!, 2);
	test::@check(!nested.has_key("a"));
}

fn void test_invalid_numbers()
{
	test::@error(json::parse(mem, "[.5]"),    json::UNEXPECTED_CHARACTER);
	test::@error(json::parse(mem, "[1.2.3]"), json::UNEXPECTED_CHARACTER);
	test::@error(json::parse(mem, "[1e]"),    json::INVALID_NUMBER);
	test::@error(json::parse(mem, "[1e+]"),   json::INVALID_NUMBER);
	test::@error(json::parse(mem, "[1e-]"),   json::INVALID_NUMBER);
	test::@error(json::parse(mem, "[012]"),   json::INVALID_NUMBER);
	test::@error(json::parse(mem, "[-012]"),  json::INVALID_NUMBER);
	test::@error(json::parse(mem, "[012.5]"), json::INVALID_NUMBER);
}

fn void test_depth_limit()
{
	String deep_json = "[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[1]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]";
	test::@error(json::parse(mem, deep_json), json::MAX_DEPTH_REACHED);
}

fn void test_comments_rejected()
{
	test::@error(json::parse(mem, "/"));
}

fn void test_tload_and_stream_parse()
{
	ByteReader reader;
	reader.init(`{"value": 42}`);
	test::eq(json::tload(&reader)!!.get_int("value")!!, 42);
}
