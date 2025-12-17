module runtime_test_tests @test;

import std::core::runtime @public;
import std::core::mem::allocator @public;
import std::math::random;


fn void tmem_shouldnt_leak_if_no_pool()
{
	for (usz i = 0; i < 256; i++)
	{
		char[] my_arr = mem::talloc_array(char, 1024);
        my_arr[..] = (char[*]){ [0..1023] = 0xA5 }[..];
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
