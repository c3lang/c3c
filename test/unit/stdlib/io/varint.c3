module std::io::varint @test;
import std::io;

fn void write_read()
{
	ByteBuffer buf;
	buf.tinit(16);
	sz n;
	uint x;
	uint y;

	n = io::write_varint(&buf, 123u)!!;
	assert(n == 1, "got %d; want 1", n);
	io::read_varint(&buf, &y)!!;
	assert(y == 123, "got %d; want 123", y);

	n = io::write_varint(&buf, 123456789u)!!;
	assert(n == 4, "got %d; want 4", n);
	io::read_varint(&buf, &y)!!;
	assert(y == 123456789u, "got %d; want 123456789", y);

	const uint128 TEST_VAL = 29382938129343348392839ull;
	uint128 val = TEST_VAL;
	n = io::write_varint(&buf, val)!!;
	test::eq(n, 11);
	val = 0;
	io::read_varint(&buf, &val)!!;
	test::eq(val, TEST_VAL);

}

struct VarIntTest
{
	uint in;
	char[] bytes;
}

fn void samples()
{
	VarIntTest[] tcases = {
		{ 0,          { 0x00 } },
        { 100,        { 0x64 } },
        { 127,        { 0x7F } },
        { 128,        { 0x80, 0x01 } },
        { 16271,      { 0x8F, 0x7F } },
        { 16383,      { 0xFF, 0x7F } },
        { 16384,      { 0x80, 0x80, 0x01 } },
        { 1048576,    { 0x80, 0x80, 0x40 } },
        { 2097151,    { 0xFF, 0xFF, 0x7F } },
        { 2097152,    { 0x80, 0x80, 0x80, 0x01 } },
        { 2147483648, { 0x80, 0x80, 0x80, 0x80, 0x08 } },
        { 4294967295, { 0xFF, 0xFF, 0xFF, 0xFF, 0x0F } },
	};
	foreach (tc : tcases)
	{
		ByteWriter bw;
		bw.tinit();
		sz n = io::write_varint(&bw, tc.in)!!;
		assert(n == tc.bytes.len, "got %d; want %d", n, tc.bytes.len);
		char[] bytes = bw.bytes[:bw.index];
		assert(bytes == tc.bytes, "got %d; want %d", bytes, tc.bytes);
	}
}

fn void roundtrip_uint_max()
{
	ByteBuffer buf;
	buf.tinit(16);
	uint val = uint::max;
	uint y;
	sz n = io::write_varint(&buf, val)!!;
	test::eq(n, 5);
	io::read_varint(&buf, &y)!!;
	test::eq(y, val);
}

fn void roundtrip_uint_high_values()
{
	uint[*] cases = { 2147483648, 3000000000, 4000000000, uint::max };
	foreach (val : cases)
	{
		ByteBuffer buf;
		buf.tinit(16);
		uint y;
		io::write_varint(&buf, val)!!;
		io::read_varint(&buf, &y)!!;
		test::eq(y, val);
	}
}

fn void roundtrip_ushort_max()
{
	ByteBuffer buf;
	buf.tinit(16);
	ushort val = ushort::max;
	ushort y;
	sz n = io::write_varint(&buf, val)!!;
	test::eq(n, 3);
	io::read_varint(&buf, &y)!!;
	test::eq(y, val);
}

fn void roundtrip_ushort_high_values()
{
	ushort[*] cases = { 256, 16384, 32768, ushort::max };
	foreach (val : cases)
	{
		ByteBuffer buf;
		buf.tinit(16);
		ushort y;
		io::write_varint(&buf, val)!!;
		io::read_varint(&buf, &y)!!;
		test::eq(y, val);
	}
}

fn void roundtrip_uint128_max()
{
	ByteBuffer buf;
	buf.tinit(32);
	uint128 val = uint128::max;
	uint128 y;
	io::write_varint(&buf, val)!!;
	io::read_varint(&buf, &y)!!;
	test::eq(y, val);
}

fn void roundtrip_int_negative_one()
{
	ByteBuffer buf;
	buf.tinit(16);
	int x = -1;
	int y;
	io::write_varint(&buf, x)!!;
	io::read_varint(&buf, &y)!!;
	test::eq(y, x);
}

fn void roundtrip_int_negative_values()
{
	int[*] cases = { -1, -42, -128, -16384, -1000000, int::min };
	foreach (val : cases)
	{
		ByteBuffer buf;
		buf.tinit(16);
		int y;
		io::write_varint(&buf, val)!!;
		io::read_varint(&buf, &y)!!;
		test::eq(y, val);
	}
}

fn void roundtrip_int_positive_values()
{
	int[*] cases = { 0, 1, 42, 127, 128, 16384, 1000000, int::max };
	foreach (val : cases)
	{
		ByteBuffer buf;
		buf.tinit(16);
		int y;
		io::write_varint(&buf, val)!!;
		io::read_varint(&buf, &y)!!;
		test::eq(y, val);
	}
}

fn void roundtrip_long_extremes()
{
	long[*] cases = { 0, 1, -1, long::min, long::max };
	foreach (val : cases)
	{
		ByteBuffer buf;
		buf.tinit(32);
		long y;
		io::write_varint(&buf, val)!!;
		io::read_varint(&buf, &y)!!;
		test::eq(y, val);
	}
}

fn void roundtrip_short_extremes()
{
	short[*] cases = { 0, 1, -1, short::min, short::max };
	foreach (val : cases)
	{
		ByteBuffer buf;
		buf.tinit(16);
		short y;
		io::write_varint(&buf, val)!!;
		io::read_varint(&buf, &y)!!;
		test::eq(y, val);
	}
}