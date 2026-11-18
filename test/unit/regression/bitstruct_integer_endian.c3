module bitstruct_integer_endian;

bitstruct Be32 : uint @bigendian
{
	uint a : 3..20;
	int b : 21..31;
}

bitstruct Be64 : ulong @bigendian
{
	ulong a : 0..63;
}

bitstruct Le32 : uint @littleendian
{
	int a : 1..9;
	uint b : 10..31;
}

fn void big_endian_fields() @test
{
	Be32 x;
	x.a = 0x2BCDE;
	x.b = -1000;
	assert(x.a == 0x2BCDE && x.b == -1000 && (uint)x == 0xf0e61583);
	x.a = 1;
	assert((uint)x == 0x8000083 && x.b == -1000);
	Be64 y;
	y.a = 0x0123456789ABCDEF;
	assert(y.a == 0x0123456789ABCDEF && (ulong)y == 0xefcdab8967452301);
}

fn void little_endian_fields() @test
{
	Le32 x;
	x.a = -200;
	x.b = 0x3FFFFF;
	assert(x.a == -200 && x.b == 0x3FFFFF && (uint)x == 0xfffffe70);
	x.a = 255;
	assert(x.a == 255 && x.b == 0x3FFFFF);
}
