module runtime_test_tests @test;

import std::core::runtime @public;
import std::core::mem::allocator @public;
import std::math::random;


fn void tmem_shouldnt_leak_if_no_pool()
{
	Lcg64Random rand;
	random::seed(&rand, 0x1337_83fb_c1ac_1a20);

	for (usz i = 0; i < 1024; i++)
	{
		// Init 1KiB then scramble it. Writes will try to corrupt the space of other tests/data.
		char[] my_arr = mem::talloc_array(char, 1024);
		for (usz x = 0; x < my_arr.len; x++) my_arr[x] = rand.next_byte();
	}
}

fn void ensure_leak_check_works()
{
	if (!(runtime::test_context.check_leaks)) return;

	char[] my_arr = mem::alloc_array(char, 1024);
	test::@check(((TrackingAllocator*)allocator::thread_allocator).has_leaks());

	mem::free(my_arr);
	test::@check(!((TrackingAllocator*)allocator::thread_allocator).has_leaks());
}
