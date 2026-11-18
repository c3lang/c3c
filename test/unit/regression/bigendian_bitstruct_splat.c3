module bigendian_bitstruct_splat;
bitstruct Be : uint @bigendian { int x : 4..11; int y : 12..19; bool f : 20; }
bitstruct Le : uint { int x : 4..11; int y : 12..19; bool f : 20; }
struct Holder { int k; Be b; }
fn void bigendian_splat_initializer() @test
{
	Be base;
	base.x = 0x12;
	base.y = 0x34;
	Be s = { ...base, .f = true };
	Be t = { ...base, .y = 0x56 };
	assert(s.x == 0x12 && s.y == 0x34 && s.f);
	assert(t.x == 0x12 && t.y == 0x56 && !t.f);
	Holder h = { .k = 1, .b = { ...base, .f = true } };
	Be[2] arr = { [0..1] = { ...base, .f = true } };
	assert(h.b.x == 0x12 && h.b.y == 0x34 && h.b.f);
	assert(arr[0].x == 0x12 && arr[1].x == 0x12 && arr[1].y == 0x34 && arr[1].f);
	Le lb; lb.x = 0x12; lb.y = 0x34;
	Le l = { ...lb, .f = true };
	assert(l.x == 0x12 && l.y == 0x34 && l.f);
}
