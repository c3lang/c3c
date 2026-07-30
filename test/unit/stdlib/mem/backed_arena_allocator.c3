module backed_arena_allocator_test @test;
import std::core::mem::allocators;

fn void reset_after_pages_no_oob()
{
	BackedArenaAllocator* a = allocators::new_backed_allocator(16, mem)!!;
	for (int i = 0; i < 300; i++)
	{
		a.acquire(64, NO_ZERO, 0)!!;
	}
	a.reset(0);
	a.destroy();
}

fn void destroy_after_pages_no_oob()
{
	BackedArenaAllocator* a = allocators::new_backed_allocator(16, mem)!!;
	for (int i = 0; i < 300; i++)
	{
		a.acquire(64, NO_ZERO, 0)!!;
	}
	a.destroy();
}

fn void reset_mixed_inline_and_pages()
{
	BackedArenaAllocator* a = allocators::new_backed_allocator(128, mem)!!;
	a.acquire(8, NO_ZERO, 0)!!;
	for (int i = 0; i < 50; i++)
	{
		a.acquire(64, NO_ZERO, 0)!!;
		a.acquire(4, NO_ZERO, 0)!!;
	}
	a.reset(0);
	a.acquire(8, NO_ZERO, 0)!!;
	a.destroy();
}
