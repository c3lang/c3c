module json_test @test;
import std::collections::object;
import std::io;
import std::encoding::json;

fn void simple_test()
{
	ByteReader reader;
	reader.init(`{ "b": 123, "c": [ { "d": 66 }, null, "hello\tworld", false, { "id": "xyz" } ] }`);
	Object* o = json::parse(&reader)!!;
	defer o.free();
	assert(o.get_int("b")!! == 123);
	assert(o.get("c").get_len()!! == 5);
	assert(o.get("c").get_at(0).get_int("d")!! == 66);
	assert(o.get("c").get_at(1).is_null()!!);
	assert(o.get("c").get_string_at(2)!! == "hello\tworld");
	assert(o.get("c").get_bool_at(3)!! == false);
	assert(o.get("c").get_at(4).get_string("id")!! == "xyz");
}

fn void simple_test2()
{
	ByteReader reader;
	reader.init(`{"jsonrpc":"2.0","id":null,"method":"initialize"}`);
	Object* o = json::parse(&reader)!!;
	defer o.free();
}


fn void test_string()
{
	Object* o = json::parse_string(`{"jsonrpc":"2","id":null,"method":"initialize"}`)!!;
	defer o.free();
	String s = string::tformat("%s", *o);
	Object* o2 = json::parse_string(s)!!;
	defer o2.free();
	String s2 = string::tformat("%s", *o2);
	assert(s2 == s, "Unexpectedly got %s and not %s", s2, s);
}

fn void test_temp_string()
{
	@pool()
	{
		Object* o = json::temp_parse_string(`{"jsonrpc":"2","id":null,"method":"initialize"}`)!!;
		defer o.free();
		String s = string::tformat("%s", *o);
		Object* o2 = json::temp_parse_string(s)!!;
		defer o2.free();
		String s2 = string::tformat("%s", *o2);
		assert(s2 == s, "Unexpectedly got %s and not %s", s2, s);
	};
}
