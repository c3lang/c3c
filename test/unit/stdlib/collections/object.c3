module object_test @test;
import std::collections::object;

fn void test_general()
{
	Object* root = object::new_obj(mem);
	defer root.free();

	root.set("foo", 1);
	root.set("bar", "baz");
	assert(root.get_int("foo")!! == 1);
	assert(root.get_string("bar")!! == "baz");
	Object* goo = root.set("goo", object::new_obj(mem));
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
		Object* int_object = object::new_int(mem, 16);
		defer int_object.free();
		String s = string::format(mem, "%s", int_object);
		defer free(s);
		assert(s == "16");
	}
	{
		Object* int_object = object::new_int(mem, -16);
		defer int_object.free();
		String s = string::format(mem, "%s", int_object);
		defer free(s);
		assert(s == "-16");
	}
}

fn void test_to_value()
{
	{
		Object* int_obj = object::new_int(tmem, 99);
		defer int_obj.free();

		char c = int_obj.to_value(char)!!;
		assert(c == 99);
	}
	{
		Object* float_obj = object::new_float(tmem, 99.17);
		defer float_obj.free();

		float f = float_obj.to_value(float)!!;
		assert(f == 99.17f);
	}
	{
		Object* str_obj = object::new_string(tmem, "222");
		defer str_obj.free();

		int i = str_obj.to_value(int, strict: false)!!;
		assert(i == 222);

		if (catch e = str_obj.to_value(int)) 
		{
			assert(e == TYPE_MISMATCH);
		} 
		else 
		{
			assert(false, "Should be an error when strict is true.");
		}
	}
	{
		Object* enum_obj = object::new_enum(tmem, MyEnum.THIRD);
		defer enum_obj.free();
		assert(enum_obj.is_enum());

		MyEnum o = enum_obj.to_value(MyEnum)!!;
		assert(o == THIRD);
	}
}

enum MyEnum : (String value)
{
    FIRST {"first"},
    SECOND {"second"},
    THIRD {"third"}
}
import std::io;

fn void test_enum()
{
	{
		Object* enum_obj = object::new_enum(tmem, MyEnum.FIRST);
		defer enum_obj.free();

		assert(enum_obj.to_value(MyEnum)!! == MyEnum.FIRST);
	}
	{
		Object* obj = object::new_obj(tmem);
		defer obj.free();

		obj.set("Hello", MyEnum.THIRD);

		MyEnum o = obj.get_enum(MyEnum, "Hello")!!;

		assert(o == THIRD);
	}
}
