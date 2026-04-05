module ringbuffer_test @test;
import std::collections::ringlist;
import std::io;
import std::math;

fn void push_pop()
{
	const sz COUNT = 10;
	RingList{int, COUNT} rbuff;
	rbuff.init();

	assert (rbuff.is_empty());

	int i = 0;

	for (; i < COUNT / 2; ++i)
	{
		rbuff.push(i);
	}

	assert (!rbuff.is_full());
	assert (!rbuff.is_empty());
	assert (rbuff.len() == COUNT / 2);

	for (; i < COUNT; ++i)
	{
		rbuff.push(i);
	}

	assert (rbuff.len() == COUNT - 1);
	assert (rbuff.is_full());

	for (int j = 0; j < COUNT; ++j)
	{
		int? el = rbuff.pop_front();
		if (catch err = el)
		{
			break;
		}
		assert (el == j + 1);
	}
}

fn void peek()
{
	const sz COUNT = 6;
	RingList{int, COUNT} rbuff;
	int[*] values = { 0, 1, 2, 3, 4 };
	rbuff.init_from_values(values[..]);

	for (int i = 0; i < COUNT - 1; ++i)
	{
		int v = rbuff.peek()!!;
		test::eq(v, i);
		rbuff.pop_front()!!;
	}

	test::eq(rbuff.len(), 0);
	test::eq(rbuff.is_empty(), true);
}

fn void push_pop_ranges()
{
	const sz COUNT = 20;
	RingList{int, COUNT} rbuff;
	rbuff.init();

	int[COUNT] push_range;
	int[COUNT] pop_range;

	for (int i = 0; i < COUNT; ++i)
	{
		push_range[i] = i;
	}

	rbuff.push_all(push_range[..]);

	test::eq(rbuff.len(), COUNT - 1);
	assert (rbuff.is_full());
	assert (!rbuff.is_empty());

	rbuff.pop_front_into(pop_range[..]);

	assert (rbuff.is_empty());
	assert (!rbuff.is_full());

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
		RingList{int, COUNT} rbuff;
		int[*] values = { 1, 2, 3, 4, 5, 6, 7 };
		rbuff.init_from_values(values[..]);
		test::eq(string::tformat("%s", rbuff), "[1, 2, 3, 4, 5, 6, 7]");
		int[] rbuff_slice = rbuff.copy_to_array(tmem)!!;
		rbuff.push(8);
		test::eq(string::tformat("%s", rbuff), "[2, 3, 4, 5, 6, 7, 8]");
		test::eq(rbuff.len(), 7);
		rbuff.push(9);
		test::eq(string::tformat("%s", rbuff), "[3, 4, 5, 6, 7, 8, 9]");
		rbuff.push(10);
		test::eq(string::tformat("%s", rbuff), "[4, 5, 6, 7, 8, 9, 10]");

		defer alloc::free(tmem, rbuff_slice);
		test::eq(rbuff_slice, values[..]);
	}

	{
		const sz COUNT = 5;
		RingList{int, COUNT} rbuff;
		int[*] values = { 1, 2, 3, 4, 5, 6, 7 };
		rbuff.init_from_values(values[..]);
		int[] rbuff_slice = rbuff.copy_to_array(tmem)!!;
		defer alloc::free(tmem, rbuff_slice);
		int[*] expected_result = { 4, 5, 6, 7 };
		test::eq(rbuff_slice, expected_result[..]);
	}
}

fn void to_format_edge_cases()
{
    RingList{int, 5} rbuff;
    rbuff.init();
    test::eq(string::tformat("%s", rbuff), "[]");

    rbuff.push(1); rbuff.push(2); rbuff.push(3); rbuff.push(4);
    rbuff.pop_front()!!; rbuff.pop_front()!!; rbuff.pop_front()!!;
    rbuff.push(5); rbuff.push(6);
    rbuff.pop_front()!!;
    test::eq(string::tformat("%s", rbuff), "[5, 6]");
}

fn void more_elements()
{
	const sz COUNT = math::@next_power_of_2(8000);
	RingList{int, COUNT} rbuff;
	for (int i = 0; i < COUNT + COUNT; ++i)
	{
		rbuff.push(i);
	}
	test::eq(rbuff.is_full(), true);
	test::eq(rbuff.len(), COUNT - 1);

	int[] elems = rbuff.copy_to_array(tmem)!!;
	defer alloc::free(tmem, elems);

	rbuff.push_all(elems);
	for (int i = 0; i < COUNT - 1; ++i)
	{
		rbuff.pop_front()!!;
	}
	test::eq(rbuff.is_empty(), true);
	test::eq(rbuff.len(), 0);
}
