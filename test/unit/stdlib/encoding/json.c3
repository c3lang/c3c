module json_test @test;
import std::collections::object;
import std::io;
import std::encoding::json;

fn void simple_test()
{
	ByteReader reader;
	reader.init(`{ "b": 123, "c": [ { "d": 66 }, null, "hello\tworld", false, { "id": "xyz" } ] }`);
	Object* o = json::parse(mem, &reader)!!;
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
	Object* o = json::parse(mem, &reader)!!;
	defer o.free();
}


fn void test_string()
{
	Object* o = json::parse_string(mem, `{"jsonrpc":"2","id":null,"method":"initialize"}`)!!;
	defer o.free();
	String s = string::tformat("%s", *o);
	Object* o2 = json::parse_string(mem, s)!!;
	defer o2.free();
	String s2 = string::tformat("%s", *o2);
	assert(s2 == s, "Unexpectedly got %s and not %s", s2, s);
}

fn void test_temp_string()
{
	@pool()
	{
		Object* o = json::tparse_string(`{"jsonrpc":"2","id":null,"method":"initialize"}`)!!;
		defer o.free();
		String s = string::tformat("%s", *o);
		Object* o2 = json::tparse_string(s)!!;
		defer o2.free();
		String s2 = string::tformat("%s", *o2);
		assert(s2 == s, "Unexpectedly got %s and not %s", s2, s);
	};
}

fn void test_valid_numbers()
{
	// Test valid integer
	Object* o1 = json::parse_string(mem, "123")!!;
	defer o1.free();
	assert(o1.f == 123.0);

	// Test valid negative integer
	Object* o2 = json::parse_string(mem, "-456")!!;
	defer o2.free();
	assert(o2.f == -456.0);

	// Test valid decimal
	Object* o3 = json::parse_string(mem, "123.456")!!;
	defer o3.free();
	assert(o3.f == 123.456);

	// Test valid negative decimal
	Object* o4 = json::parse_string(mem, "-123.456")!!;
	defer o4.free();
	assert(o4.f == -123.456);

	// Test valid scientific notation
	Object* o5 = json::parse_string(mem, "1.23e10")!!;
	defer o5.free();
	assert(o5.f == 1.23e10);

	// Test valid scientific notation with negative exponent
	Object* o6 = json::parse_string(mem, "1.23e-10")!!;
	defer o6.free();
	assert(o6.f == 1.23e-10);

	// Test valid scientific notation with positive exponent
	Object* o7 = json::parse_string(mem, "1.23e+10")!!;
	defer o7.free();
	assert(o7.f == 1.23e+10);

	// Test zero
	Object* o8 = json::parse_string(mem, "0")!!;
	defer o8.free();
	assert(o8.f == 0.0);

	// Test zero with decimal
	Object* o9 = json::parse_string(mem, "0.0")!!;
	defer o9.free();
	assert(o9.f == 0.0);

	// Test negative zero
	Object* o10 = json::parse_string(mem, "-0")!!;
	defer o10.free();
	assert(o10.f == 0.0);

	// Test zero with scientific notation
	Object* o11 = json::parse_string(mem, "0e10")!!;
	defer o11.free();
	assert(o11.f == 0.0);

	// Test zero with decimal and scientific notation
	Object* o12 = json::parse_string(mem, "0.0e10")!!;
	defer o12.free();
	assert(o12.f == 0.0);
}

fn void test_duplicate_keys()
{
	// Test that duplicate keys are now allowed (should not fail)
	Object* o1 = json::parse_string(mem, `{"a":"first","a":"second"}`)!!;
	defer o1.free();
	assert(o1.has_key("a"));
	assert(o1.get_string("a")!! == "second"); // Last value wins

	// Test multiple duplicate keys with different types
	Object* o2 = json::parse_string(mem, `{"x":1,"x":"string","x":true}`)!!;
	defer o2.free();
	assert(o2.get_bool("x")!! == true); // Last value wins

	// Test duplicate keys in nested objects
	Object* o3 = json::parse_string(mem, `{"obj":{"a":1},"obj":{"b":2}}`)!!;
	defer o3.free();
	Object* nested = o3.get("obj")!!;
	assert(nested.has_key("b"));
	assert(nested.get_int("b")!! == 2);
	// The first object should be completely replaced
	assert(!nested.has_key("a"));
}

fn void test_invalid_numbers()
{
	// Test invalid: leading decimal point
	if (catch err = json::parse_string(mem, "[.5]"))
	{
		assert(err == json::UNEXPECTED_CHARACTER);
	}
	else
	{
		assert(false, "Expected UNEXPECTED_CHARACTER error for [.5]");
	}

	// Test invalid: multiple decimal points
	if (catch err = json::parse_string(mem, "[1.2.3]"))
	{
		assert(err == json::UNEXPECTED_CHARACTER);
	}
	else
	{
		assert(false, "Expected UNEXPECTED_CHARACTER error for [1.2.3]");
	}

	// Test invalid: scientific notation without digits after e
	if (catch err = json::parse_string(mem, "[1e]"))
	{
		assert(err == json::INVALID_NUMBER);
	}
	else
	{
		assert(false, "Expected INVALID_NUMBER error for [1e]");
	}

	// Test invalid: scientific notation with only sign after e
	if (catch err = json::parse_string(mem, "[1e+]"))
	{
		assert(err == json::INVALID_NUMBER);
	}
	else
	{
		assert(false, "Expected INVALID_NUMBER error for [1e+]");
	}

	// Test invalid: scientific notation with only sign after e
	if (catch err = json::parse_string(mem, "[1e-]"))
	{
		assert(err == json::INVALID_NUMBER);
	}
	else
	{
		assert(false, "Expected INVALID_NUMBER error for [1e-]");
	}

	// Test invalid: leading zeros
	if (catch err = json::parse_string(mem, "[012]"))
	{
		assert(err == json::INVALID_NUMBER);
	}
	else
	{
		assert(false, "Expected INVALID_NUMBER error for [012]");
	}

	// Test invalid: negative leading zeros
	if (catch err = json::parse_string(mem, "[-012]"))
	{
		assert(err == json::INVALID_NUMBER);
	}
	else
	{
		assert(false, "Expected INVALID_NUMBER error for [-012]");
	}

	// Test invalid: leading zeros with decimal
	if (catch err = json::parse_string(mem, "[012.5]"))
	{
		assert(err == json::INVALID_NUMBER);
	}
	else
	{
		assert(false, "Expected INVALID_NUMBER error for [012.5]");
	}
}

fn void test_depth_limit()
{
	// Test that deeply nested arrays with 150 opening brackets exceed the depth limit of 128
	String deep_json = "[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[1]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]";

	if (catch err = json::parse_string(mem, deep_json))
	{
		assert(err == json::MAX_DEPTH_REACHED);
	}
	else
	{
		assert(false, "Expected MAX_DEPTH_REACHED error for deeply nested array with 150 brackets");
	}
}

fn void test_comments_rejected()
{
	// Test that just a slash is rejected
	if (catch err = json::parse_string(mem, "/"))
	{
		// Should fail with some error - slash is not valid JSON
	}
	else
	{
		assert(false, "Expected error for single slash but parsing succeeded");
	}
}
