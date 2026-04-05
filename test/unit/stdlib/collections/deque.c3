module deque_test @test;

import std::math;
import std::collections::deque;

fn void push_front_and_back()
{
	const COUNT = 5;
	Deque{int} deque;
	deque.init(mem, COUNT);
	defer deque.free();

	for (int i = 0; i < COUNT; ++i)
	{
		deque.push_front(i);
	}

	for (int i = 0; i < COUNT; ++i)
	{
		deque.push_back(COUNT + i);
	}

	int[COUNT] target_front;
	deque.pop_front_into(target_front[..]);

	test::eq(target_front[..], (int[]){ 4, 3, 2, 1, 0 });

	int[COUNT] target_back;
	deque.pop_back_into(target_back[..]);

	test::eq(target_back[..], (int[]){ 9, 8, 7, 6, 5 });
}

fn void peek_back()
{
	const COUNT = 5;
	Deque{int} deque;
	deque.init(mem, COUNT);
	defer deque.free();

	int[COUNT + COUNT] arr;
	foreach (int i, &e : arr[..])
	{
		*e = i;
	}
	deque.push_back_all(arr[..]);

	for (int i = COUNT + COUNT - 1; i >= 0; --i)
	{
		test::eq(deque.back(), i);
		deque.pop_back()!!;
	}
}

fn void peek_front()
{
	const COUNT = 5;
	Deque{int} deque;
	deque.init(mem, COUNT);
	defer deque.free();

	int[COUNT + COUNT] arr;
	foreach (int i, &e : arr[..])
	{
		*e = i;
	}
	deque.push_front_all(arr[..]);

	for (int i = COUNT + COUNT - 1; i >= 0; --i)
	{
		test::eq(deque.front(), i);
		deque.pop_front()!!;
	}
}

fn void push_back_then_shrink()
{
	const COUNT = 20;
	Deque{int} deque;
	deque.tinit(COUNT);
	defer deque.free();

	int[*] buf_back = { 0, 1, 2, 3, 4, 5, 6, 7 };
	deque.push_back_all(buf_back[..]);

	for (int i = 0; i < buf_back.len - 3; ++i)
	{
		deque.pop_front()!!;
	}

	deque.shrink();
}

fn void push_front_then_shrink()
{
	const COUNT = 10;
	Deque{int} deque;
	deque.tinit(COUNT);
	defer deque.free();

	int[*] buf_front = { 0, 1, 2, 3, 4, 5, 6, 7 };
	deque.push_front_all(buf_front[..]);

	for (int i = 0; i < buf_front.len - 3; ++i)
	{
		deque.pop_back()!!;
	}

	deque.shrink();
}

fn void push_both_sides_then_shrink()
{
	const COUNT = 20;
	Deque{int} deque;
	deque.tinit(COUNT);
	defer deque.free();

	test::eq(deque.is_empty(), true);

	int[COUNT / 4] buf_back = { 0, 1, 2, 3, 4 };
	int[COUNT / 4] buf_front = { 5, 6, 7, 8, 9 };

	deque.push_front_all(buf_front[..]);
	deque.push_back_all(buf_back[..]);

	deque.shrink();

	test::eq(deque.is_empty(), false);
	test::eq(deque.capacity, COUNT / 2);

	int[COUNT / 4] buf_back_check;
	int[COUNT / 4] buf_front_check;
	deque.pop_front_into(buf_front_check[..]);
	deque.pop_back_into(buf_back_check[..]);

	test::eq(buf_back_check[..], (int[]){ 4, 3, 2, 1, 0 });
	test::eq(buf_front_check[..], (int[]){ 9, 8, 7, 6, 5 });
}

fn void for_each()
{
	const COUNT = 10;
	Deque{int} deque;
	deque.tinit(COUNT);
	deque.reserve(100);
	defer deque.free();

	int[*] buf_front = { 4, 3, 2, 1, 0 };
	int[*] buf_back  = { 5, 6, 7, 8, 9 };

	deque.push_front_all(buf_front[..]);
	deque.push_back_all(buf_back[..]);

	deque.normalize();

	foreach (int cmp, el : deque)
	{
		test::eq(el, cmp);
	}
}

fn void clear()
{
	const COUNT = 4;
	const RESERVED = 100;
	Deque{int} deque;
	deque.tinit(COUNT);
	deque.reserve(RESERVED);
	defer deque.free();

	int[*] buf_front = { 4, 3, 2, 1, 0 };
	int[*] buf_back  = { 5, 6, 7, 8, 9 };

	deque.push_front_all(buf_front[..]);
	deque.push_back_all(buf_back[..]);

	deque.clear();
	test::eq(deque.remain_space(), math::next_power_of_2(RESERVED));
}
