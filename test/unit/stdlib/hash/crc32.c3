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
	crc.update_byte('a');

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

fn void test_crc32_large_known_values()
{
	// Reference values from zlib.crc32 over bytes (i * 131 + 7) & 0xFF.
	char[1000] data;
	foreach (i, &c : data) *c = (char)(i * 131 + 7);
	assert(crc32::hash(data[..]) == 0x1ed57bb9);
	assert(crc32::hash(data[:37]) == 0x6add4778);
	assert(crc32::hash(data[3:800]) == 0x5a4817f6);
}

fn void test_crc32_matches_bytewise_for_all_lengths_and_alignments()
{
	char[600] data;
	ulong x = 88172645463325252;
	foreach (&c : data)
	{
		x ^= x << 13;
		x ^= x >> 7;
		x ^= x << 17;
		*c = (char)(x >> 24);
	}
	for (sz offset = 0; offset < 8; offset++)
	{
		for (sz len = 0; len <= 300; len++)
		{
			char[] part = data[offset:len];
			Crc32 reference;
			reference.init();
			foreach (c : part) reference.update_byte(c);
			Crc32 fast;
			fast.init();
			fast.update(part);
			assert(fast.final() == reference.final(), "Wide update differs from bytewise update");
			assert(crc32::hash(part) == reference.final(), "hash differs from bytewise update");
		}
	}
}

fn void test_crc32_chunked_updates()
{
	char[4000] data;
	foreach (i, &c : data) *c = (char)(i * 7 + (i >> 5));
	uint whole = crc32::hash(data[..]);
	foreach (sz step : { 1, 3, 15, 16, 17, 31, 32, 33, 100, 1000 })
	{
		Crc32 crc;
		crc.init();
		sz pos = 0;
		while (pos < data.len)
		{
			sz n = step < data.len - pos ? step : data.len - pos;
			crc.update(data[pos:n]);
			pos += n;
		}
		assert(crc.final() == whole, "Chunked update differs");
	}
}
