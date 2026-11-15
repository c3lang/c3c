module designated_bitstruct_runtime;

bitstruct Bits : char
{
	bool x : 0;
	bool y : 1;
	char n : 4..7;
}

bitstruct Wide : char[3]
{
	bool a : 0;
	ushort mid : 4..19;
}

bitstruct Big : ushort @bigendian
{
	char lo : 0..7;
	char hi : 8..15;
}

struct Holder
{
	int k;
	Bits bs;
	Bits[2] arr;
}

union BitsUnion
{
	Bits bs;
	char raw;
}

fn void array_element() @test
{
	bool t = true;
	Bits[2] b = { [1].x = t };
	test::eq((char)b[0], 0);
	test::eq(b[1].x, true);
	test::eq(b[1].y, false);
}

fn void several_fields_same_element() @test
{
	bool t = true;
	char v = 9;
	Bits[3] b = { [2].y = t, [2].n = v, [0].x = t };
	test::eq(b[0].x, true);
	test::eq((char)b[1], 0);
	test::eq(b[2].x, false);
	test::eq(b[2].y, true);
	test::eq(b[2].n, 9);
}

fn void range_of_elements() @test
{
	char v = 5;
	Bits[4] b = { [1..2].n = v };
	test::eq((char)b[0], 0);
	test::eq(b[1].n, 5);
	test::eq(b[2].n, 5);
	test::eq((char)b[3], 0);
}

fn void array_backed_bitstruct_element() @test
{
	ushort m = 0xABCD;
	bool t = true;
	Wide[2] w = { [1].mid = m, [0].a = t };
	test::eq(w[0].a, true);
	test::eq(w[0].mid, 0);
	test::eq(w[1].a, false);
	test::eq(w[1].mid, 0xABCD);
}

fn void bigendian_element() @test
{
	char h = 0x12;
	char l = 0x34;
	Big[2] g = { [1].hi = h, [1].lo = l };
	test::eq(g[1].hi, 0x12);
	test::eq(g[1].lo, 0x34);
	test::eq((ushort)g[0], 0);
}

fn void struct_member_paths() @test
{
	bool t = true;
	char v = 3;
	Holder h = { .k = 1, .bs.y = t, .arr[1].n = v, .arr[0].x = t };
	test::eq(h.k, 1);
	test::eq(h.bs.y, true);
	test::eq(h.bs.x, false);
	test::eq(h.arr[0].x, true);
	test::eq(h.arr[1].n, 3);
	Holder[2] hs = { [1].arr[0..1].y = t };
	test::eq(hs[1].arr[0].y, true);
	test::eq(hs[1].arr[1].y, true);
	test::eq((char)hs[0].arr[0], 0);
}

fn void union_member() @test
{
	bool t = true;
	BitsUnion u = { .bs.y = t };
	test::eq(u.raw, 2);
}

fn void constant_values_unchanged() @test
{
	Bits[2] b = { [1].x = true, [0].n = 15 };
	test::eq(b[1].x, true);
	test::eq(b[0].n, 15);
}
