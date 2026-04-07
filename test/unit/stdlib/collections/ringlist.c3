module ringbuffer_test @test;
import std::collections::ringlist;
import std::io;
import std::math;

fn void push_pop()
{
	const sz COUNT = 10;
	RingList{int, COUNT} rlist;
	rlist.init();

	assert (rlist.is_empty());

	int i = 0;

	for (; i < COUNT / 2; ++i)
	{
		rlist.push(i);
	}

	assert (!rlist.is_full());
	assert (!rlist.is_empty());
	assert (rlist.len() == COUNT / 2);

	for (; i < COUNT; ++i)
	{
		rlist.push(i);
	}

	assert (rlist.len() == COUNT - 1);
	assert (rlist.is_full());

	for (int j = 0; j < COUNT; ++j)
	{
		int? el = rlist.pop_first();
		if (catch err = el)
		{
			break;
		}
		assert (el == j + 1);
	}
}

fn void front()
{
	const sz COUNT = 6;
	RingList{int, COUNT} rlist;
	int[*] values = { 0, 1, 2, 3, 4 };
	rlist.init_from_values(values[..]);

	for (int i = 0; i < COUNT - 1; ++i)
	{
		int v = rlist.first()!!;
		test::eq(v, i);
		rlist.pop_first()!!;
	}

	test::eq(rlist.len(), 0);
	test::eq(rlist.is_empty(), true);
}

fn void push_pop_ranges()
{
	const sz COUNT = 20;
	RingList{int, COUNT} rlist;
	rlist.init();

	int[COUNT] push_range;
	int[COUNT] pop_range;

	for (int i = 0; i < COUNT; ++i)
	{
		push_range[i] = i;
	}

	rlist.push_all(push_range[..]);

	test::eq(rlist.len(), COUNT - 1);
	assert (rlist.is_full());
	assert (!rlist.is_empty());

	rlist.pop_first_into(pop_range[..]);

	assert (rlist.is_empty());
	assert (!rlist.is_full());

	for (int j = 0; j < COUNT - 1; ++j)
	{
		test::eq(pop_range[j], j + 1);
	}
	test::eq(pop_range[COUNT - 1], 0);
}

fn void into_slice() => @pool()
{
	 {
		const sz COUNT = 8;
		RingList{int, COUNT} rlist;
		int[*] values = { 1, 2, 3, 4, 5, 6, 7 };
		rlist.init_from_values(values[..]);
		test::eq(string::tformat("%s", rlist), "[1, 2, 3, 4, 5, 6, 7]");
		int[] rlist_slice = rlist.copy_to_array(tmem)!!;
		rlist.push(8);
		test::eq(string::tformat("%s", rlist), "[2, 3, 4, 5, 6, 7, 8]");
		test::eq(rlist.len(), 7);
		rlist.push(9);
		test::eq(string::tformat("%s", rlist), "[3, 4, 5, 6, 7, 8, 9]");
		rlist.push(10);
		test::eq(string::tformat("%s", rlist), "[4, 5, 6, 7, 8, 9, 10]");

		defer alloc::free(tmem, rlist_slice);
		test::eq(rlist_slice, values[..]);
	}

	{
		const sz COUNT = 5;
		RingList{int, COUNT} rlist;
		int[*] values = { 1, 2, 3, 4, 5, 6, 7 };
		rlist.init_from_values(values[..]);
		int[] rlist_slice = rlist.copy_to_array(tmem)!!;
		defer alloc::free(tmem, rlist_slice);
		int[*] expected_result = { 4, 5, 6, 7 };
		test::eq(rlist_slice, expected_result[..]);
	}
}

fn void to_format_edge_cases()
{
    RingList{int, 5} rlist;
    rlist.init();
    test::eq(string::tformat("%s", rlist), "[]");

    rlist.push(1); rlist.push(2); rlist.push(3); rlist.push(4);
    rlist.pop_first()!!; rlist.pop_first()!!; rlist.pop_first()!!;
    rlist.push(5); rlist.push(6);
    rlist.pop_first()!!;
    test::eq(string::tformat("%s", rlist), "[5, 6]");
}

fn void more_elements()
{
	const sz COUNT = math::@next_power_of_2(8000);
	RingList{int, COUNT} rlist;
	for (int i = 0; i < COUNT + COUNT; ++i)
	{
		rlist.push(i);
	}
	test::eq(rlist.is_full(), true);
	test::eq(rlist.len(), COUNT - 1);

	int[] elems = rlist.copy_to_array(tmem)!!;
	defer alloc::free(tmem, elems);

	rlist.push_all(elems);
	for (int i = 0; i < COUNT - 1; ++i)
	{
		rlist.pop_first()!!;
	}
	test::eq(rlist.is_empty(), true);
	test::eq(rlist.len(), 0);
}

fn void for_each_and_clear()
{
	const COUNT = 10;
	RingList{int, COUNT} rlist;
	int[COUNT - 1] push_values = { 0, 1, 2, 3, 4, 5, 6, 7, 8 };
	rlist.init_from_values(push_values[..]);

	test::eq(rlist.is_full(), true);
	test::eq(rlist.is_empty(), false);

	foreach (int i, el : rlist)
	{
		test::eq(i, el);
	}

	rlist.clear();

	test::eq(rlist.is_full(), false);
	test::eq(rlist.is_empty(), true);
}
