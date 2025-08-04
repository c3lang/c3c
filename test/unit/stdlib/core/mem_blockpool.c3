module std::core::fixedblockpool_test;

import std;

struct Foo
{
	int a;
	double b;
	bool c;
	char[2] x;
}

fn void test_basic_allocation() @test
{
	FixedBlockPool pool;
	defer pool.free();
	pool.init_for_type(mem, int);

	test::eq(pool.used, 0);
	int *ptr1 = pool.alloc();
	test::eq(pool.used, 1);
	int *ptr2 = pool.alloc();
	assert(((usz) ptr2) - ((usz) ptr1) >= int.sizeof);
	assert(pool.used == 2);
}

struct Bar @align(128)
{
	int a;
	int b;
}
fn void test_basic_overallocation() @test
{
	FixedBlockPool pool;
	defer pool.free();
	pool.init_for_type(mem, Bar);
	assert(pool.alignment == 128);
	test::eq(pool.used, 0);
	int *ptr1 = pool.alloc();
	test::eq(((iptr)ptr1) % 128, 0);
	test::eq(pool.used, 1);
	int *ptr2 = pool.alloc();
	test::eq(((iptr)ptr2) % 128, 0);
	test::eq(pool.used, 2);
}

fn void test_large_allocation() @test
{
	FixedBlockPool pool;
	defer pool.free();
	pool.init_for_type(mem, Foo);

	Foo *ptr1 = pool.alloc();
	Foo *ptr2 = pool.alloc();
	assert(((usz) ptr2) - ((usz) ptr1) >= Foo.sizeof);
	assert(pool.used == 2);
	pool.dealloc(ptr1);
	pool.dealloc(ptr2);
}

fn void test_basic_capacity() @test
{
	FixedBlockPool pool;
	defer pool.free();
	pool.init_for_type(mem, int, 2);

	int *ptr1 = pool.alloc();
	int *ptr2 = pool.alloc();
	assert(((usz) ptr2) - ((usz) ptr1) >= int.sizeof);
	assert(pool.used == 2);
}

fn void test_basic_capacity_different_grow_capacity() @test
{
	FixedBlockPool pool;
	defer pool.free();
	pool.init_for_type(mem, int, 1);

	pool.alloc();
	pool.grow_capacity = 2;

	pool.alloc();
	pool.alloc();
	pool.grow_capacity = 1;

	pool.alloc();

	assert(pool.head.buffer != null);
	assert(pool.head.next.buffer != null);
	assert(pool.head.next.next.buffer != null);
	assert(pool.head.next.next.next == null);
	assert(pool.used == 4);
	assert(pool.allocated == 4);
}

fn void test_basic_object_reuse() @test
{
	FixedBlockPool pool;
	defer pool.free();
	pool.init_for_type(mem, int, 1);

	int*[10] objs;
	for (int i = 0; i < 10; i++) objs[i] = pool.alloc();
	test::eq(pool.used, 10);

	int *obj1 = objs[1];
	pool.dealloc(obj1);
	test::eq(pool.used, 9);
	int *obj3 = objs[3];
	pool.dealloc(obj3);
	test::eq(pool.used, 8);

	int *obj7 = objs[7];
	pool.dealloc(obj7);

	int *obj2 = objs[2];
	pool.dealloc(obj2);

	assert(obj2 == pool.alloc());
	assert(obj7 == pool.alloc());
	assert(obj3 == pool.alloc());
	test::eq(pool.used, 9);
	assert(obj1 == pool.alloc());
	assert(pool.used == 10);
}
