module bitstruct_designated_endian @test;

// Nested designators (`.field.member = runtime`) into an endian bitstruct
// must byteswap like a plain assignment does.

bitstruct ShortBe : ushort @bigendian    { ushort val : 0..15; }
bitstruct ShortLe : ushort @littleendian { ushort val : 0..15; }
bitstruct IntBe : uint @bigendian        { uint val : 0..31; }
bitstruct IntLe : uint @littleendian     { uint val : 0..31; }
bitstruct SplitBe : uint @bigendian      { ushort hi : 16..31; ushort lo : 0..15; }

struct Pair @packed { ShortBe a; IntBe b; }
struct PairLe @packed { ShortLe a; IntLe b; }
struct Split { SplitBe s; }

fn void runtime_nested_designated_be()
{
	ushort a = 0x0102;
	uint b = 0x03040506;
	Pair p = { .a.val = a, .b.val = b };
	test::eq(*(char[6]*)&p, x'010203040506');
	assert(p.a.val == 0x0102);
	assert(p.b.val == 0x03040506);
}

fn void mixed_const_and_runtime_designated_be()
{
	ushort a = 1;
	Pair p = { .a.val = a, .b.val = 0x01020304 };
	test::eq(*(char[6]*)&p, x'000101020304');
}

fn void runtime_nested_designated_le()
{
	ushort a = 0x0102;
	uint b = 0x03040506;
	PairLe p = { .a.val = a, .b.val = b };
	test::eq(*(char[6]*)&p, x'020106050403');
}

fn void runtime_nested_designated_be_partial_members()
{
	ushort hi = 0xAABB;
	ushort lo = 0xCCDD;
	Split s = { .s.hi = hi, .s.lo = lo };
	Split expected;
	expected.s.hi = hi;
	expected.s.lo = lo;
	test::eq(*(char[4]*)&s, *(char[4]*)&expected);
	assert(s.s.hi == 0xAABB);
	assert(s.s.lo == 0xCCDD);
}
