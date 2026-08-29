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

fn void test_new_with_padding() @test
{
	Foo* f = alloc::new_with_padding(mem, Foo, 16)!!;
	defer alloc::free(mem, f);
	f.x = 1;
	f.y = 2;
	test::eq(f.x, 1);
	test::eq(f.y, 2);
	// The padding area should be usable memory.
	char* padding = ((char*)f) + Foo::size;
	padding[0] = 42;
	padding[15] = 43;
	test::eq(padding[0], 42);
	test::eq(padding[15], 43);
}

fn void test_alloc_with_padding() @test
{
	Foo* f = alloc::alloc_with_padding(mem, Foo, 16)!!;
	defer alloc::free(mem, f);
	f.x = 5;
	f.y = 6;
	test::eq(f.x, 5);
	test::eq(f.y, 6);
	char* padding = ((char*)f) + Foo::size;
	padding[0] = 99;
	test::eq(padding[0], 99);
}

fn void test_clone() @test
{
	Foo original = { 10, 20 };
	Foo* cloned = alloc::clone(mem, original);
	defer alloc::free(mem, cloned);
	test::eq(cloned.x, 10);
	test::eq(cloned.y, 20);
	test::@check((void*)cloned != (void*)&original, "Clone should not alias the original");
}

fn void test_clone_slice() @test
{
	int[] arr = { 1, 2, 3, 4 };
	int[] cloned = alloc::clone_slice(mem, arr);
	defer free(cloned);
	test::eq(cloned.len, 4);
	test::eq(cloned[0], 1);
	test::eq(cloned[3], 4);
	test::@check(cloned.ptr != arr.ptr, "Clone should not alias the original slice");

	int[] empty = {};
	int[] cloned_empty = alloc::clone_slice(mem, empty);
	test::eq(cloned_empty.len, 0);
}

fn void test_clone_aligned() @test
{
	Foo original = { 7, 8 };
	Foo* cloned = alloc::clone_aligned(mem, original);
	defer alloc::free_aligned(mem, cloned);
	test::eq(cloned.x, 7);
	test::eq(cloned.y, 8);
}

fn void test_clone_any() @test
{
	Foo original = { 11, 22 };
	any a = any_make(&original, Foo);
	any cloned = alloc::clone_any(mem, a);
	defer alloc::free(mem, cloned.ptr);
	Foo* cloned_foo = (Foo*)cloned.ptr;
	test::eq(cloned_foo.x, 11);
	test::eq(cloned_foo.y, 22);
	test::@check(cloned.ptr != (void*)&original, "Clone should not alias the original");
}

fn void test_new_array_and_alloc_array() @test
{
	int* zeroed = alloc::new_array(mem, int, 5);
	defer alloc::free(mem, zeroed);
	test::eq(zeroed[0], 0);
	test::eq(zeroed[4], 0);

	int* uninit = alloc::alloc_array(mem, int, 5);
	defer alloc::free(mem, uninit);
	uninit[0] = 7;
	uninit[4] = 9;
	test::eq(uninit[0], 7);
	test::eq(uninit[4], 9);
}

fn void test_new_array_try_and_alloc_array_try() @test
{
	int[]? zeroed = alloc::new_array_try(mem, int, 3);
	defer free(zeroed!!);
	test::eq(zeroed!!.len, 3);
	test::eq(zeroed!![0], 0);

	int[]? uninit = alloc::alloc_array_try(mem, int, 3);
	defer free(uninit!!);
	uninit!![1] = 4;
	test::eq(uninit!![1], 4);
}

fn void test_aligned_array_variants() @test
{
	int* zeroed = alloc::new_array_aligned(mem, int, 4);
	defer alloc::free_aligned(mem, zeroed);
	test::eq(zeroed[0], 0);
	test::eq(zeroed[3], 0);

	int* uninit = alloc::alloc_array_aligned(mem, int, 4);
	defer alloc::free_aligned(mem, uninit);
	uninit[1] = 8;
	test::eq(uninit[1], 8);

	int* to_grow = alloc::new_array_aligned(mem, int, 2);
	to_grow[0] = 11;
	to_grow[1] = 22;
	to_grow = alloc::realloc_array_aligned(mem, to_grow, int, 6);
	defer alloc::free_aligned(mem, to_grow);
	test::eq(to_grow[0], 11);
	test::eq(to_grow[1], 22);
	to_grow[5] = 33;
	test::eq(to_grow[5], 33);
}
