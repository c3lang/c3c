module std::core::allocator_test;

import std;

struct Foo
{
	int x;
	int y;
}

struct Bar
{
	int x;
	int y;
	List{Foo} foos;
}

fn void test_new_aligned_compiles() @test
{
	Bar* bar2 = alloc::new_aligned(mem, Bar)!!;
	alloc::free_aligned(mem, bar2);

	Bar* bar = alloc::new_aligned(mem, Bar, {.x = 1, .y = 2, .foos = {}})!!;
	alloc::free_aligned(mem, bar);
}

fn void test_realloc_array_to_zero() @test
{
	sz* x = mem::talloc_array(sz, 24);
	x = alloc::realloc_array(tmem, x, sz, 0);
	test::@check(x == null, "Zero-sized reallocation's pointer is not null: %p", x);
}
