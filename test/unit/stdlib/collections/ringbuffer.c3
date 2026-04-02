module ringbuffer_test @test;
import std::collections::ringbuffer;
import std::io;
import std::math;

fn void push_pop()
{
	const isz COUNT = 10;
	RingBuffer{int, COUNT} rbuff;
	rbuff.init();

	assert (rbuff.is_empty());

	int i = 0;

	for (; i < COUNT / 2; ++i)
	{
		rbuff.push(i);
	}

	assert (!rbuff.is_full());
	assert (!rbuff.is_empty());
	assert (rbuff.size() == COUNT / 2);

	for (; i < COUNT; ++i)
	{
		rbuff.push(i);
	}

	assert (rbuff.size() == COUNT - 1);
	assert (rbuff.is_full());

	for (int j = 0; j < COUNT; ++j)
	{
		int? el = rbuff.pop();
		if (catch err = el)
		{
			break;
		}
		assert (el == j + 1);
	}
}

fn void peek()
{
	const isz COUNT = 6;
	RingBuffer{int, COUNT} rbuff;
	int[*] values = { 0, 1, 2, 3, 4 };
	rbuff.init_from_values(values[..]);

	for (int i = 0; i < COUNT - 1; ++i)
	{
		int v = rbuff.peek()!!;
		test::expect(v, i);
		rbuff.pop()!!;
	}

	test::expect(rbuff.size(), 0);
	test::expect(rbuff.is_empty(), true);
}

fn void push_pop_ranges()
{
	const isz COUNT = 20;
	RingBuffer{int, COUNT} rbuff;
	rbuff.init();

	int[COUNT] push_range;
	int[COUNT] pop_range;

	for (int i = 0; i < COUNT; ++i)
	{
		push_range[i] = i;
	}

	rbuff.push_all(push_range[..]);

	test::expect(rbuff.size(), COUNT - 1);
	assert (rbuff.is_full());
	assert (!rbuff.is_empty());

	rbuff.pop_many(pop_range[..]);

	assert (rbuff.is_empty());
	assert (!rbuff.is_full());

	for (int j = 0; j < COUNT - 1; ++j)
	{
		test::expect(pop_range[j], j + 1);
	}
	test::expect(pop_range[COUNT - 1], 0);
}

fn void into_slice()
{
	{
		const isz COUNT = 8;
		RingBuffer{int, COUNT} rbuff;
		int[*] values = { 1, 2, 3, 4, 5, 6, 7 };
		rbuff.init_from_values(values[..]);
		int[] rbuff_slice = rbuff.into_slice(tmem)!!;
		defer alloc::free(tmem, rbuff_slice);
		test::expect(rbuff_slice, values[..]);
	}

	{
		const isz COUNT = 5;
		RingBuffer{int, COUNT} rbuff;
		int[*] values = { 1, 2, 3, 4, 5, 6, 7 };
		rbuff.init_from_values(values[..]);
		int[] rbuff_slice = rbuff.into_slice(tmem)!!;
		defer alloc::free(tmem, rbuff_slice);
		int[*] expected_result = { 4, 5, 6, 7 };
		test::expect(rbuff_slice, expected_result[..]);
	}
}

fn void more_elements()
{
	const isz COUNT = math::@next_power_of_2(8000);
	RingBuffer{int, COUNT} rbuff;
	for (int i = 0; i < COUNT + COUNT; ++i)
	{
		rbuff.push(i);
	}
	test::expect(rbuff.is_full(), true);
	test::expect(rbuff.size(), COUNT - 1);

	int[] elems = rbuff.into_slice(tmem)!!;
	defer alloc::free(tmem, elems);

	rbuff.push_all(elems);

	for (int i = 0; i < COUNT - 1; ++i)
	{
		rbuff.pop()!!;
	}
	test::expect(rbuff.is_empty(), true);
	test::expect(rbuff.size(), 0);
}
