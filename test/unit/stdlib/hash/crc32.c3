module crc32_test @test;

import std::hash::crc32;
import std::io;

fn void test_crc32_empty()
{
	const uint EXPECTED = 0x00000000;

	Crc32 crc;
	crc.init();
	crc.update("");

	uint final = crc.final();
	test::@check(final == EXPECTED, "Actual crc32: 0x%x == 0x%x", final, EXPECTED);
}

fn void test_crc32_a()
{
	const uint EXPECTED = 0xe8b7be43;

	Crc32 crc;
	crc.init();
	crc.updatec('a');

	uint final = crc.final();
	test::@check(final == EXPECTED, "Actual crc32: 0x%x == 0x%x", final, EXPECTED);
}

fn void test_crc32_abc()
{
	const uint EXPECTED = 0x352441c2;

	Crc32 crc;
	crc.init();
	crc.update("abc");

	uint final = crc.final();
	test::@check(final == EXPECTED, "Actual crc32: 0x%x == 0x%x", final, EXPECTED);
}

fn void test_crc32_longer()
{
	const uint EXPECTED = 0x656e49ed;

	Crc32 crc;
	crc.init();
	crc.update("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopqabcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");

	uint final = crc.final();
	test::@check(final == EXPECTED, "Actual crc32: 0x%x == 0x%x", final, EXPECTED);
}

fn void test_crc32_alphabet()
{
	const uint EXPECTED = 0x4c2750bd;

	Crc32 crc;
	crc.init();
	crc.update("abcdefghijklmnopqrstuvwxyz");

	uint final = crc.final();
	test::@check(final == EXPECTED, "Actual crc32: 0x%x == 0x%x", final, EXPECTED);
}
