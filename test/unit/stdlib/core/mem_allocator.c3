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
	Bar* bar2 = allocator::new_aligned(allocator::heap(), Bar)!!;
	allocator::free_aligned(allocator::heap(), bar2);

	Bar* bar = allocator::new_aligned(allocator::heap(), Bar, {.x = 1, .y = 2, .foos = {}})!!;
	allocator::free_aligned(allocator::heap(), bar);
}
