module xorshiro_test;
import std::math;

fn void test_vectors_xorshiro() @test
{
	Xorshiro128PPRandom a = { { 0x1, 0x2, 0x3, 0x4 } };
	test::eq(0x281, a.next_int());
	test::eq(0x180387, a.next_int());
	test::eq(0xc0183387, a.next_int());

	a = { { ~0x1, ~0x2, ~0x3, ~0x4 } };
	test::eq(0xfffffcfd, a.next_int());
	test::eq(0x17fbf8, a.next_int());
	test::eq(0x40183386, a.next_int());
}