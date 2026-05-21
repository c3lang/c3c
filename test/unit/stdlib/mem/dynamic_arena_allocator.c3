module dynamic_arena_allocator_test @test;
import std::core::mem;


fn void test_dynamic_arena_resize_shrink_no_alias() @test
{
	DynamicArenaAllocator dyn;
	dyn.init(mem, 1024);
	defer dyn.free();

	char* p1 = alloc::malloc(&dyn, 32);
	mem::set(p1, 0xAA, 32);

	p1 = alloc::realloc(&dyn, p1, 16);

	char* p2 = alloc::malloc(&dyn, 8);

	assert(p1 != p2, "resize shrink corrupted `used`: second alloc aliased the shrunken pointer");

	mem::set(p2, 0xBB, 8);
	assert(p1[0] == 0xAA, "p1 data corrupted: p2 write bled into p1 (pointers aliased)");
}
