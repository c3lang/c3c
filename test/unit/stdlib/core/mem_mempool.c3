module std::core::mem::mempool_test;
import std;

struct MempoolTestPair
{
	int x;
	int y;
}

fn void test_init_and_alloc_basic() @test
{
	FixedBlockPool pool;
	pool.init(mem, MempoolTestPair::size, 4);
	defer pool.free();

	test::@check(pool.block_size >= MempoolTestPair::size, "Expected block_size to be at least the requested size");
	test::eq(pool.allocated, 4);
	test::eq(pool.used, 0);

	void* a = pool.alloc();
	void* b = pool.alloc();
	test::@check(a != null && b != null, "Expected non-null blocks");
	test::@check(a != b, "Expected distinct blocks");
	test::eq(pool.used, 2);
}

fn void test_dealloc_reuses_freed_block() @test
{
	FixedBlockPool pool;
	pool.init(mem, MempoolTestPair::size, 4);
	defer pool.free();

	void* a = pool.alloc();
	void* b = pool.alloc();
	pool.dealloc(a);
	test::eq(pool.used, 1);

	void* c = pool.alloc();
	test::eq(c, a);
	test::eq(pool.used, 2);

	// b is still live and untouched by the reuse.
	test::@check(c != b);
}

fn void test_alloc_returns_zeroed_memory() @test
{
	FixedBlockPool pool;
	pool.init(mem, MempoolTestPair::size, 4);
	defer pool.free();

	MempoolTestPair* p = pool.alloc();
	p.x = 123;
	p.y = 456;
	pool.dealloc(p);

	MempoolTestPair* reused = pool.alloc();
	test::eq(reused, (MempoolTestPair*)p);
	test::eq(reused.x, 0);
	test::eq(reused.y, 0);
}

fn void test_grows_when_capacity_exhausted() @test
{
	FixedBlockPool pool;
	pool.init(mem, MempoolTestPair::size, 4);
	defer pool.free();

	void*[10] blocks;
	for (int i = 0; i < 10; i++)
	{
		blocks[i] = pool.alloc();
		test::@check(blocks[i] != null);
	}
	test::@check(pool.allocated >= 10, "Expected the pool to have grown past its initial capacity");
	test::eq(pool.used, 10);

	// All returned blocks are distinct.
	for (int i = 0; i < 10; i++)
	{
		for (int j = i + 1; j < 10; j++)
		{
			test::@check(blocks[i] != blocks[j], "Expected all blocks to be distinct");
		}
	}
}

fn void test_free_resets_pool() @test
{
	FixedBlockPool pool;
	pool.init(mem, MempoolTestPair::size, 4);
	pool.alloc();
	pool.alloc();

	pool.free();
	test::@check(!pool.initialized);
	test::eq(pool.allocated, 0);
	test::eq(pool.used, 0);
}

fn void test_init_for_type_and_tinit_variants() @test
{
	FixedBlockPool pool;
	pool.init_for_type(mem, MempoolTestPair, 4);
	defer pool.free();
	test::@check(pool.block_size >= MempoolTestPair::size);

	FixedBlockPool tpool;
	tpool.tinit_for_type(MempoolTestPair, 4);
	defer tpool.free();
	test::@check(tpool.block_size >= MempoolTestPair::size);

	FixedBlockPool tpool2;
	tpool2.tinit(MempoolTestPair::size, 4);
	defer tpool2.free();
	test::@check(tpool2.block_size >= MempoolTestPair::size);
}

fn void test_block_size_is_bumped_to_hold_a_freelist_entry() @test
{
	// A 1-byte block must still be big enough to hold a FixedBlockPoolEntry (one pointer),
	// since freed blocks are threaded onto the freelist in place.
	FixedBlockPool pool;
	pool.init(mem, 1, 4);
	defer pool.free();

	test::@check(pool.block_size >= FixedBlockPoolEntry::size, "Expected the block size to be bumped to fit a freelist entry (one pointer)");

	void* a = pool.alloc();
	pool.dealloc(a);
	void* b = pool.alloc();
	test::eq(a, b);
}

fn void test_alignment_is_honored() @test
{
	// MempoolTestPair is only 8 bytes: every block beyond the first would drift out of a
	// 64-byte alignment unless the pool rounds its block size up to a multiple of the
	// alignment.
	FixedBlockPool pool;
	pool.init(mem, MempoolTestPair::size, 4, 64);
	defer pool.free();

	// More than the initial capacity, to also cover blocks from a grown page.
	for (int i = 0; i < 10; i++)
	{
		void* p = pool.alloc();
		test::eq((uptr)p & 63, (uptr)0);
	}
}

fn void test_no_alignment_request_does_not_pad_blocks() @test
{
	FixedBlockPool pool;
	pool.init(mem, 1, 4);
	defer pool.free();

	// Just big enough to hold a FixedBlockPoolEntry (one pointer), nothing more.
	test::eq(pool.block_size, FixedBlockPoolEntry::size);
}

fn void test_requested_alignment_smaller_than_default_does_not_pad_blocks() @test
{
	FixedBlockPool pool;
	pool.init(mem, MempoolTestPair::size, 4, 4);
	defer pool.free();

	test::eq(pool.block_size, MempoolTestPair::size);

	void* a = pool.alloc();
	void* b = pool.alloc();
	test::eq((uptr)a % 4, (uptr)0);
	test::eq((uptr)b % 4, (uptr)0);
}

struct OveralignedFoo @align(32)
{
	int a;
}

fn void test_overaligned_type_is_honored_for_every_block() @test
{
	test::eq(OveralignedFoo::alignment, (sz)32);

	FixedBlockPool pool;
	pool.init_for_type(mem, OveralignedFoo, 4);
	defer pool.free();

	test::eq(pool.block_size, OveralignedFoo::size);

	// More than the initial capacity, to also cover blocks from a grown page.
	for (int i = 0; i < 10; i++)
	{
		void* p = pool.alloc();
		test::eq((uptr)p % OveralignedFoo::alignment, (uptr)0);
	}
}

fn void test_alignment_16bit_is_honored() @test
{
	FixedBlockPool pool;
	pool.init(mem, 3, 4, 2);
	defer pool.free();

	test::eq(pool.block_size, FixedBlockPoolEntry::size);

	for (int i = 0; i < 10; i++)
	{
		void* p = pool.alloc();
		test::eq((uptr)p % 2, (uptr)0);
	}
}


fn void test_dealloc_and_reuse_on_grown_page() @test
{
	// Force growth past the initial page, then dealloc/realloc a block that lives on the
	// grown (non-head) page, so fixedblockpool_check_ptr's multi-page walk is exercised
	// through the real dealloc contract, not just alloc().
	FixedBlockPool pool;
	pool.init(mem, MempoolTestPair::size, 4);
	defer pool.free();

	void*[10] blocks;
	for (int i = 0; i < 10; i++) blocks[i] = pool.alloc();

	void* grown_block = blocks[9];
	pool.dealloc(grown_block);
	test::eq(pool.used, 9);

	void* reused = pool.alloc();
	test::eq(reused, grown_block);
	test::eq(pool.used, 10);
}

fn void test_auto_capacity_derives_from_os_pagesize() @test
{
	// capacity = INITIAL_CAPACITY (0) should derive a capacity from the OS page size instead
	// of requiring the caller to pick one.
	FixedBlockPool pool;
	pool.init(mem, MempoolTestPair::size, mempool::INITIAL_CAPACITY);
	defer pool.free();

	sz expected = (mem::os_pagesize() - 128) / pool.block_size;
	if (!expected) expected = 1;
	test::eq(pool.allocated, expected);
	test::eq(pool.grow_capacity, expected);

	void* a = pool.alloc();
	void* b = pool.alloc();
	test::@check(a != null && b != null && a != b);
}
