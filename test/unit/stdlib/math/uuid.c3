module std::math::uuid @test;
import std::io;
import std::math::random;
import std::time;
import std::thread;

fn void test_generate_v4()
{
	for (int i = 0; i < 10; i += 1)
	{
		Uuid id = uuid::generate_v4();
		test::eq(id.variant() & 0b1100, 8);
		test::eq(id.version(), 4);
	}
}

fn void test_generate_v7() @feat(LIBC)
{
	Uuid[20] uuids;
	foreach (&id : uuids)
	{
		Time time_before = time::os::native_timestamp();
		*id = uuid::generate_v7();
		Duration time_diff = id.time_v7() - time_before;
		test::le(math::abs(time_diff), time::ms(1));
		test::eq(id.variant() & 0b1100, 8);
		test::eq(id.version(), 7);
		thread::sleep_ms(1);
	}

	for (int i = 0; i < uuids.len - 1; i += 1)
	{
		test::lt(uuids[i].time_v7(), uuids[i + 1].time_v7());
		test::lt(uuids[i], uuids[i + 1]);
	}
}

fn void to_string() => @pool()
{
	Uuid uuid1      = (Uuid)x"919108f752d143209bacf847db4148a8";
	String expected = "919108f7-52d1-4320-9bac-f847db4148a8";
	test::eq(uuid1.to_string(tmem), expected);
}

fn void from_string_ok()
{
	String input  = "919108f7-52d1-4320-9bac-f847db4148a8";
	Uuid expected = (Uuid)x"919108f752d143209bacf847db4148a8";
	Uuid result   = uuid::from_string(input)!!;
	test::eq(result, expected);
}

fn void from_string_fail()
{
	// Too short
	Uuid? result = uuid::from_string("919108f7-52d1-4320-9bac-f847db4148a");
	test::@error(result, uuid::INVALID_UUID_STRING);

	// Too long
	result = uuid::from_string("919108f7-52d1-4320-9bac-f847db4148a81");
	test::@error(result, uuid::INVALID_UUID_STRING);

	// No dashes
	result = uuid::from_string("919108f7 52d1 4320 9bac f847db4148a");
	test::@error(result, uuid::INVALID_UUID_STRING);
}
