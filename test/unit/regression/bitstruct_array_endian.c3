module bitstruct_array_endian @test;


bitstruct BeLow : char[4] @bigendian    { ushort a : 0..15;  }
bitstruct LeLow : char[4] @littleendian { ushort a : 0..15;  }
bitstruct BeMid : char[4] @bigendian    { ushort a : 8..23;  }
bitstruct LeMid : char[4] @littleendian { ushort a : 8..23;  }

bitstruct Be12 : char[3] @bigendian    { uint a : 0..11; }
bitstruct Le12 : char[3] @littleendian { uint a : 0..11; }

bitstruct Be15 : char[4] @bigendian    { ushort a : 0..14; }
bitstruct Le15 : char[4] @littleendian { ushort a : 0..14; }

fn void test_byte_aligned_low_runtime()
{
	BeLow be;
	be.a = 0xABCD;
	test::eq((char[4])be, x'ABCD0000');
	assert(be.a == 0xABCD, "BE bits 0..15 round-trip: got %x", (uint)be.a);

	LeLow le;
	le.a = 0xABCD;
	test::eq((char[4])le, x'CDAB0000');
	assert(le.a == 0xABCD, "LE bits 0..15 round-trip: got %x", (uint)le.a);
}

fn void test_byte_aligned_mid_runtime()
{
	BeMid be;
	be.a = 0xABCD;
	test::eq((char[4])be, x'00ABCD00');
	assert(be.a == 0xABCD, "BE bits 8..23 round-trip: got %x", (uint)be.a);

	LeMid le;
	le.a = 0xABCD;
	test::eq((char[4])le, x'00CDAB00');
	assert(le.a == 0xABCD, "LE bits 8..23 round-trip: got %x", (uint)le.a);
}

fn void test_sub_byte_aligned_runtime()
{
	Be12 be;
	be.a = 0xABC;
	test::eq((char[3])be, x'AB0C00');
	assert(be.a == 0xABC, "BE 12-bit round-trip: got %x", (uint)be.a);

	Le12 le;
	le.a = 0xABC;
	test::eq((char[3])le, x'BC0A00');
	assert(le.a == 0xABC, "LE 12-bit round-trip: got %x", (uint)le.a);
}

fn void test_byte_aligned_low_const()
{
	BeLow be = { .a = 0xABCD };
	test::eq((char[4])be, x'ABCD0000');

	BeLow be2 = { .a = 0x7869 };
	test::eq((char[4])be2, x'78690000');

	LeLow le = { .a = 0xABCD };
	test::eq((char[4])le, x'CDAB0000');

	LeLow le2 = { .a = 0x7869 };
	test::eq((char[4])le2, x'69780000');

}

fn void test_byte_aligned_mid_const()
{
	BeMid be = { .a = 0xABCD };
	test::eq((char[4])be, x'00ABCD00');

	LeMid le = { .a = 0xABCD };
	test::eq((char[4])le, x'00CDAB00');

}

fn void test_sub_byte_aligned_const()
{
	Be12 be = { .a = 0xABC };
	test::eq((char[3])be, x'AB0C00');

	Le12 le = { .a = 0xABC };
	test::eq((char[3])le, x'BC0A00');

}

fn void test_15_bit_runtime()
{
	// Field bits 0..14, value 0x7FFF.
	Be15 be;
	be.a = 0x7FFF;
	test::eq((char[4])be, x'FF7F0000');
	assert(be.a == 0x7FFF, "BE 15-bit round-trip: got %x", (uint)be.a);

	Le15 le;
	le.a = 0x7FFF;
	test::eq((char[4])le, x'FF7F0000');
	assert(le.a == 0x7FFF, "LE 15-bit round-trip: got %x", (uint)le.a);
}

fn void test_15_bit_const()
{
	Be15 be = { .a = 0x7FFF };
	test::eq((char[4])be, x'FF7F0000');

	Le15 le = { .a = 0x7FFF };
	test::eq((char[4])le, x'FF7F0000');
}

bitstruct NoSpill : char[2] @bigendian
{
	ushort a : 0..14;
	bool   b : 15;
}

fn void test_no_spill_adjacent_field()
{
	NoSpill f;
	f.b = true;
	assert(f.b, "b should be true after assignment");
	test::eq((char[2])f, x'0080');

	// Writing `a` must leave `b` alone.
	f.a = 0;
	assert(f.b, "b corrupted by writing a = 0");
	test::eq((char[2])f, x'0080');

	f.a = 0x7FFF;
	assert(f.b, "b corrupted by writing a = 0x7FFF");
	test::eq((char[2])f, x'FFFF');

	f.a = 0x1234;
	assert(f.b, "b corrupted by writing a = 0x1234");

	// And `b = false` must leave `a` alone.
	f.a = 0x7FFF;
	f.b = false;
	assert(!f.b, "b not cleared");
	assert(f.a == 0x7FFF, "a corrupted by writing b = false: got %x", (uint)f.a);
}
