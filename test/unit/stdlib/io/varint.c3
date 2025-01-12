module std::io::varint @test;
import std::io;

fn void write_read()
{
	ByteBuffer buf;
	buf.temp_init(16)!!;
	usz n;
	uint x;
	uint y;

	n = io::write_varint(&buf, 123)!!;
	assert(n == 1, "got %d; want 1", n);
	io::read_varint(&buf, &y)!!;
	assert(y == 123, "got %d; want 123", y);

	n = io::write_varint(&buf, 123456789)!!;
	assert(n == 4, "got %d; want 4", n);
	io::read_varint(&buf, &y)!!;
	assert(y == 123456789, "got %d; want 123456789", y);
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
		bw.temp_init();
		usz n = io::write_varint(&bw, tc.in)!!;
		assert(n == tc.bytes.len, "got %d; want %d", n, tc.bytes.len);
		char[] bytes = bw.bytes[:bw.index];
		assert(bytes == tc.bytes, "got %d; want %d", bytes, tc.bytes);
	}
}