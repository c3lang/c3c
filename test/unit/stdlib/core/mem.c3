module std::core::mem_test @test;
import std;

fn void test_mem_equals_compares_full_slice_elements()
{
	uint[*] a = { 1, 2 };
	uint[*] b = { 1, 3 };

	assert(!mem::equals(a[..], b[..]));
}

fn void test_mem_equals_handles_byte_tails()
{
	char[*] a = { 1, 2, 3, 4, 5 };
	char[*] b = { 1, 2, 3, 4, 6 };

	assert(!mem::equals(a[..], b[..]));
}
