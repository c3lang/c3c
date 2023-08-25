module json_test @test;
import std::collections::object;
import std::io;
import std::encoding::json;

fn void! simple_test()
{
	ByteReader reader;
	reader.init(`{ "b": 123, "c": [ { "d": 66 }, null, "hello", false, { "id": "xyz" } ] }`);
	Object* o = json::parse(reader.as_stream())!;
	assert(o.get_int("b")! == 123);
	assert(o.get("c").get_len()! == 5);
	assert(o.get("c").get_at(0).get_int("d")! == 66);
	assert(o.get("c").get_at(1).is_null()!);
	assert(o.get("c").get_string_at(2)! == "hello");
	assert(o.get("c").get_bool_at(3)! == false);
	assert(o.get("c").get_at(4).get_string("id")! == "xyz");
}
