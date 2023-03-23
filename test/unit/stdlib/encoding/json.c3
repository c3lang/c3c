module json_test @test;
import std::collections::object;
import std::io;
import std::encoding::json;

fn void! simple_test()
{
	ByteReader reader;
	reader.init(`{ "b": 123, "c": [ { "d": 66 }, null, "hello", false ] }`);
	JsonParser json;
	json.init(reader.as_stream());
	Object* o = json.parse_any()!;
	assert(o.get_int("b")! == 123);
	assert(o.get("c").get_at(0).get_int("d")! == 66);
	assert(o.get("c").get_at(1).is_null()!);
}
