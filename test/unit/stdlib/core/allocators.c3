module std::core::allocators_test;
import std;
import libc;

fn void* test_aligned_malloc(sz bytes) @local => libc::malloc((usz)bytes);
fn void test_aligned_free(void* p) @local => libc::free(p);

fn void test_push_pop_pool() @test
{
	char[] before = mem::talloc_array(char, 4);
	before[0] = 'A';

	PoolState state = allocators::push_pool();
	char[] inner = mem::talloc_array(char, 100);
	inner[0] = 'B';
	test::eq(inner[0], (char)'B');
	allocators::pop_pool(state);

	// The allocation from before push_pool must still be intact after pop_pool.
	test::eq(before[0], (char)'A');
}

fn void test_push_pop_pool_nested() @test
{
	PoolState s1 = allocators::push_pool();
	char[] a = mem::talloc_array(char, 10);
	a[0] = 1;

	PoolState s2 = allocators::push_pool();
	char[] b = mem::talloc_array(char, 10);
	b[0] = 2;
	test::eq(b[0], (char)2);
	allocators::pop_pool(s2);

	// The outer pool's allocation must survive popping the inner pool.
	test::eq(a[0], (char)1);
	allocators::pop_pool(s1);
}

fn void test_aligned_alloc_free() @test
{
	void* p = allocators::@aligned_alloc(test_aligned_malloc, 100, 64)!!;
	test::eq((uptr)p & 63, (uptr)0);

	((char*)p)[0] = 42;
	((char*)p)[99] = 43;
	test::eq(((char*)p)[0], 42);
	test::eq(((char*)p)[99], 43);

	allocators::@aligned_free(test_aligned_free, p)!!;
}

fn void test_aligned_realloc() @test
{
	void* p = allocators::@aligned_alloc(test_aligned_malloc, 100, 64)!!;
	((char*)p)[0] = 42;
	((char*)p)[99] = 43;

	void* p2 = allocators::@aligned_realloc(test_aligned_malloc, test_aligned_free, p, 200, 64)!!;
	test::eq((uptr)p2 & 63, (uptr)0);
	test::eq(((char*)p2)[0], 42);
	test::eq(((char*)p2)[99], 43);

	allocators::@aligned_free(test_aligned_free, p2)!!;
}

fn void test_aligned_alloc_fn_and_realloc_fn() @test
{
	int context = 0;
	void* p = allocators::@aligned_alloc_fn(&context, fn void*(int* ctx, sz bytes) => test_aligned_malloc(bytes), 100, 32)!!;
	test::eq((uptr)p & 31, (uptr)0);
	((char*)p)[0] = 7;

	void* p2 = allocators::@aligned_realloc_fn(&context,
	                                            fn void*(int* ctx, sz bytes) => test_aligned_malloc(bytes),
	                                            fn void(int* ctx, void* ptr) => test_aligned_free(ptr),
	                                            p, 200, 32)!!;
	test::eq((uptr)p2 & 31, (uptr)0);
	test::eq(((char*)p2)[0], 7);

	allocators::@aligned_free_fn(&context, fn void(int* ctx, void* ptr) => test_aligned_free(ptr), p2)!!;
}
