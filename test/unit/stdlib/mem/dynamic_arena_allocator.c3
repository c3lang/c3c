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

fn void test_dynamic_arena_fast_path_no_overlap() @test
{
	DynamicArenaAllocator dyn;
	// Large page so all allocations below stay on one page -> fast path.
	dyn.init(mem, 4096);
	defer dyn.free();

	const COUNT = 16;
	const SIZE  = 32;
	char*[COUNT] ptrs;

	// ptrs[0] is the slow path; ptrs[1..] all exercise the fast path.
	for (int i = 0; i < COUNT; i++)
	{
		char* block = alloc::malloc(&dyn, SIZE);
		mem::set(block, (char)(0x10 + i), SIZE);
		ptrs[i] = block;
	}

	// Each block must still hold only its own byte pattern.  A broken fast path
	// (bad `start`, collapsed header reservation, stale bookkeeping) would have
	// one allocation's bytes bleed into another.
	for (int i = 0; i < COUNT; i++)
	{
		for (int j = 0; j < SIZE; j++)
		{
			assert(ptrs[i][j] == (char)(0x10 + i),
				"fast-path block %d corrupted at byte %d (got %d, expected %d)",
				i, j, ptrs[i][j], 0x10 + i);
		}
	}

	// Pointers must be pairwise distinct.
	for (int i = 0; i < COUNT; i++)
	{
		for (int j = i + 1; j < COUNT; j++)
		{
			assert(ptrs[i] != ptrs[j], "fast path aliased blocks %d and %d", i, j);
		}
	}
}

fn void test_dynamic_arena_fast_path_resize_grow() @test
{
	DynamicArenaAllocator dyn;
	dyn.init(mem, 4096);
	defer dyn.free();

	char* a = alloc::malloc(&dyn, 16);   // slow path (creates page)
	mem::set(a, 0xAA, 16);

	char* b = alloc::malloc(&dyn, 16);   // fast path
	mem::set(b, 0xBB, 16);

	// `b` is the most-recent allocation, so growing it is valid, but it must
	// not stomp on `a`.
	b = alloc::realloc(&dyn, b, 64);
	for (int i = 0; i < 16; i++) assert(b[i] == 0xBB, "grow lost b data at %d", i);
	for (int i = 0; i < 16; i++) assert(a[i] == 0xAA, "grow of b corrupted a at %d", i);

	// A fresh fast-path allocation after the grow must not alias a or b.
	char* c = alloc::malloc(&dyn, 16);
	mem::set(c, 0xCC, 16);
	assert(c != a && c != b, "post-grow allocation aliased an existing block");
	for (int i = 0; i < 16; i++) assert(a[i] == 0xAA, "alloc c corrupted a at %d", i);
	for (int i = 0; i < 16; i++) assert(b[i] == 0xBB, "alloc c corrupted b at %d", i);
}

fn void test_dynamic_arena_release_nontop_keeps_live() @test
{
	DynamicArenaAllocator dyn;
	dyn.init(mem, 4096);
	defer dyn.free();

	char* a = alloc::malloc(&dyn, 32);   // slow path; sets current_stack_ptr = a
	mem::set(a, 0xAA, 32);
	char* b = alloc::malloc(&dyn, 32);   // fast path; current_stack_ptr SHOULD now be b
	mem::set(b, 0xBB, 32);

	// `a` is NOT the top of the arena, so freeing it must be a no-op: it must
	// not roll `used` back past the still-live `b`.  If the fast path failed to
	// update current_stack_ptr (leaving it pointing at `a`), release(a) wrongly
	// rolls back and b's storage becomes available for reuse.
	alloc::free(&dyn, a);

	for (int i = 0; i < 4; i++)
	{
		char* x = alloc::malloc(&dyn, 32);
		assert(x != b, "alloc %d aliased the still-live b after freeing non-top a", i);
		mem::set(x, 0xEE, 32);
	}
	for (int i = 0; i < 32; i++)
	{
		assert(b[i] == 0xBB, "b corrupted after freeing non-top a, byte %d (got %d)", i, b[i]);
	}
}

fn void test_dynamic_arena_fast_path_after_reset() @test
{
	DynamicArenaAllocator dyn;
	dyn.init(mem, 4096);
	defer dyn.free();

	char* first = alloc::malloc(&dyn, 32);
	mem::set(first, 0x11, 32);
	char* second = alloc::malloc(&dyn, 32);   // fast path
	mem::set(second, 0x22, 32);

	dyn.reset();

	// After reset the page is reused; the first allocation now goes through the
	// page-reuse branch and the next through the fast path again.
	char* a = alloc::malloc(&dyn, 32);
	mem::set(a, 0x33, 32);
	char* b = alloc::malloc(&dyn, 32);        // fast path on the reused page
	mem::set(b, 0x44, 32);

	assert(a != b, "fast path aliased after reset");
	for (int i = 0; i < 32; i++) assert(a[i] == 0x33, "block a corrupted after reset at %d", i);
	for (int i = 0; i < 32; i++) assert(b[i] == 0x44, "block b corrupted after reset at %d", i);
}
