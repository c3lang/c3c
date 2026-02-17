module adler32_test @test;

import std::hash::adler32;
import std::io;

fn void test_adler32_empty()
{
	const uint EXPECTED = 0x1;

	Adler32 adl;
	adl.init();
	adl.update("");

	uint final = adl.final();
	test::@check(final == EXPECTED, "Actual Adler32: 0x%x == 0x%x", final, EXPECTED);
}

fn void test_adler32_a()
{
	const uint EXPECTED = 0x00620062;

	Adler32 adl;
	adl.init();
	adl.updatec('a');

	uint final = adl.final();
	test::@check(final == EXPECTED, "Actual Adler32: 0x%x == 0x%x", final, EXPECTED);
}

fn void test_adler32_abc()
{
	const uint EXPECTED = 0x024d0127;

	Adler32 adl;
	adl.init();
	adl.update("abc");

	uint final = adl.final();
	test::@check(final == EXPECTED, "Actual Adler32: 0x%x == 0x%x", final, EXPECTED);
}

fn void test_adler32_longer()
{
	const uint EXPECTED = 0x07822df1;

	Adler32 adl;
	adl.init();
	adl.update("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopqabcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");

	uint final = adl.final();
	test::@check(final == EXPECTED, "Actual Adler32: 0x%x == 0x%x", final, EXPECTED);
}

fn void test_adler32_alphabet()
{
	const uint EXPECTED = 0x90860b20;

	Adler32 adl;
	adl.init();
	adl.update("abcdefghijklmnopqrstuvwxyz");

	uint final = adl.final();
	test::@check(final == EXPECTED, "Actual Adler32: 0x%x == 0x%x", final, EXPECTED);
}
