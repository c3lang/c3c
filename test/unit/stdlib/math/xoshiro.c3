module xoshiro_tests @test;

import std::math::random;

fn void xoshiro128pp()
{
	Xoshiro{128, XoshiroOperation.PLUS_PLUS} x = { { 1, 2, 3, 4 } };
	test::eq(0x281, x.next_int());
	test::eq(0x180387, x.next_int());
	test::eq(0xc0183387, x.next_int());
	x = { { ~1u, ~2u, ~3u, ~4u } };
	test::eq(0xfffffcfd, x.next_int());
	test::eq(0x17fbf8, x.next_int());
	test::eq(0x40183386, x.next_int());
}
