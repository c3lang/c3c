module object_test @test;
import std::collections::object;

fn void test_general()
{
	Object* root = object::new_obj(allocator::heap());
	root.set("foo", 1);
	root.set("bar", "baz");
	assert(root.get_int("foo")!! == 1);
	assert(root.get_string("bar")!! == "baz");
	Object* goo = root.set("goo", object::new_obj(allocator::heap()));
	goo.push("hello");
	goo.push(132);
	assert(root.get("goo").get_int_at(1)!! == 132);
	assert(root.get("goo").get_string_at(0)!! == "hello");
	Object* abc = root.get_or_create_obj("abc80");
	abc.set("cool", 1.3);
	assert(root.get("abc80").get_int("cool")!! == 1);
	assert(root.get("abc80").get_float("cool")!! == 1.3);
	assert((root.get_int("yyy") ?? -1) == -1);
	root.set("yyy", true);
	assert(root.get_bool("yyy") ?? false);
}

fn void test_to_format_int()
{
	{
		Object* int_object = object::new_int(16, allocator::heap());
		defer int_object.free();
		String s = string::new_format("%s", int_object);
		defer free(s);
		assert(s == "16");
	}
	{
		Object* int_object = object::new_int(-16, allocator::heap());
		defer int_object.free();
		String s = string::new_format("%s", int_object);
		defer free(s);
		assert(s == "-16");
	}
}
