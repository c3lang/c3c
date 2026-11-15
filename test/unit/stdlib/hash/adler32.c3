module adler32_test @test;

import std::hash::adler32;
import std::io, std::math;

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
	adl.update_byte('a');

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

// Deterministic pseudo random bytes; the expected values below were computed with zlib.adler32.
macro char[] adler_test_data(Allocator allocator, sz len)
{
	char[] data = alloc::alloc_array(allocator, char, len);
	uint state = 12345;
	for (sz i = 0; i < len; i++)
	{
		state = state * 1103515245 + 12345;
		data[i] = (char)(state >> 16);
	}
	return data;
}

struct AdlerCase @private
{
	sz len;
	uint aligned;
	uint offset3;
}

fn void test_adler32_block_boundaries()
{
	@pool()
	{
		AdlerCase[14] cases = {
		{ 1, 0x00dd00dd, 0x00ab00ab },
		{ 15, 0x36770649, 0x35f10659 },
		{ 16, 0x3d3006b9, 0x3d470756 },
		{ 17, 0x44620732, 0x44ad0766 },
		{ 31, 0xd94a0c9d, 0xd6f70c89 },
		{ 32, 0xe62d0ce3, 0xe3ba0cc3 },
		{ 33, 0xf3880d5b, 0xf16f0db5 },
		{ 255, 0x57a682a5, 0x9d4f8357 },
		{ 256, 0xdb15836f, 0x20d18373 },
		{ 5551, 0x6095e70f, 0x8b7ee68d },
		{ 5552, 0x47bde719, 0x72fde770 },
		{ 5553, 0x2f70e7a4, 0x5b75e869 },
		{ 11104, 0x510abbed, 0x724cbcb7 },
		{ 70000, 0xa6bc1f17, 0xca691e39 }
		};
		char[] data = adler_test_data(tmem, 70003);
		foreach (c : cases)
		{
			uint got = adler32::hash(data[:c.len]);
			test::@check(got == c.aligned, "len %d: 0x%x == 0x%x", c.len, got, c.aligned);
			got = adler32::hash(data[3:c.len]);
			test::@check(got == c.offset3, "len %d at offset 3: 0x%x == 0x%x", c.len, got, c.offset3);
		}
	};
}

struct AdlerFill @private
{
	sz len;
	uint expected;
}

fn void test_adler32_worst_case_fill()
{
	@pool()
	{
		AdlerFill[5] ones = {
		{ 5551, 0x56039a8d },
		{ 5552, 0xf18f9b8c },
		{ 5553, 0x8e299c8b },
		{ 16656, 0x29a0d2b1 },
		{ 100000, 0x149a302c }
		};
		AdlerFill[2] zeros = {
		{ 5552, 0x15b00001 },
		{ 100000, 0x86af0001 }
		};
		foreach (c : ones)
		{
			char[] data = alloc::alloc_array(tmem, char, c.len);
			data[..] = 0xFF;
			uint got = adler32::hash(data);
			test::@check(got == c.expected, "0xFF x %d: 0x%x == 0x%x", c.len, got, c.expected);
		}
		foreach (c : zeros)
		{
			char[] data = alloc::alloc_array(tmem, char, c.len);
			data[..] = 0;
			uint got = adler32::hash(data);
			test::@check(got == c.expected, "0x00 x %d: 0x%x == 0x%x", c.len, got, c.expected);
		}
	};
}

fn void test_adler32_chunked_update()
{
	@pool()
	{
		char[] data = adler_test_data(tmem, 20000);
		const uint EXPECTED = 0x64a8f5ff;
		sz[6] steps = { 1, 7, 16, 17, 5552, 9999 };
		foreach (step : steps)
		{
			Adler32 adl;
			adl.init();
			sz pos = 0;
			while (pos < data.len)
			{
				sz n = math::min(step, data.len - pos);
				adl.update(data[pos:n]);
				pos += n;
			}
			uint got = adl.final();
			test::@check(got == EXPECTED, "chunk %d: 0x%x == 0x%x", step, got, EXPECTED);
		}
	};
}

fn void test_adler32_maximal_state()
{
	// a and b start at their largest valid value.
	@pool()
	{
		char[] data = alloc::alloc_array(tmem, char, 20000);
		data[..] = 0xFF;
		Adler32 adl = { 65520, 65520 };
		adl.update(data);
		const uint EXPECTED = 0x0310d662;
		uint got = adl.final();
		test::@check(got == EXPECTED, "Actual Adler32: 0x%x == 0x%x", got, EXPECTED);
	};
}
