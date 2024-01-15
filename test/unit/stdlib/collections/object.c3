module object_test @test;
import std::collections::object;

fn void test_general()
{
	Object* root = object::new_obj(allocator::heap());
	root.set("foo", 1);
	root.set("bar", "baz");
	assert(root.get_int("foo")! == 1);
	assert(root.get_string("bar")! == "baz");
	Object* goo = root.set("goo", object::new_obj(allocator::heap()));
	goo.append("hello");
	goo.append(132);
	assert(root.get("goo").get_int_at(1)! == 132);
	assert(root.get("goo").get_string_at(0)! == "hello");
	Object* abc = root.get_or_create_obj("abc80");
	abc.set("cool", 1.3);
	assert(root.get("abc80").get_int("cool")! == 1);
	assert(root.get("abc80").get_float("cool")! == 1.3);
	assert((root.get_int("yyy") ?? -1) == -1);
	root.set("yyy", true);
	assert(root.get_bool("yyy") ?? false);
}
