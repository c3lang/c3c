module allocator_test @test;
import std::core::mem;


fn void test_arena_allocator_err()
{
	char[40] data;
	char[40] empty;
	ArenaAllocator* foo = allocator::wrap(&data);
	char* alloc = allocator::malloc(foo, 5);
	alloc[0] = 3;
	assert(alloc >= &data[0] && alloc <= &data[^1]);
	assert(foo.used >= 5);
	assert(data != empty);
	test::@error(allocator::malloc_try(foo, 50), mem::INVALID_ALLOC_SIZE);
	test::@error(allocator::malloc_try(foo, 30), mem::OUT_OF_MEMORY);
	foo.clear();
	(void)allocator::malloc(foo, 20);
}