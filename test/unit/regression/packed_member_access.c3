module packed_member_access;

bitstruct Flags : uint
{
	bool a : 0;
	uint b : 4..19;
}
bitstruct Op : ushort @align(1) { uint v : 2..9; }
struct Inner { short s; int x; long y; }
struct Packed @packed
{
	char tag;
	long l;
	Inner inner;
	long[2] ls;
	Flags flags;
	Op op;
	int128 big;
	float[<4>] v;
}
union Un @packed { char c; long l; }
struct Holder @packed { char c; Un u; }

fn long sum(Packed* p) => p.ls[0] + p.ls[1] + p.inner.x;
fn Inner pass(Inner i) => i;

fn void packed_members() @test
{
	Packed[2] ps;
	Packed* p = &ps[1];
	usz one = 1;
	p.l = 0x1122334455667788;
	p.inner = { 1, 8, 9 };
	p.inner.y += 3;
	p.ls[one] = 2;
	p.ls[0] = 1;
	p.ls[one] *= 3;
	p.flags.b = 0xABC;
	p.flags.a = true;
	p.op.v = 200;
	p.op.v += 3;
	p.big = (int128)1 << 100;
	p.big += 5;
	p.v = { 1, 2, 3, 4 };
	p.v.y = 9;
	p.v.xz = { 7, 8 };
	ps[0] = *p;
	Inner i = pass(ps[0].inner);
	assert(p.l == 0x1122334455667788);
	assert(i.x == 8 && i.y == 12);
	assert(sum(p) == 15);
	assert(p.flags.b == 0xABC && p.flags.a);
	assert(p.op.v == 203);
	assert(p.big == ((int128)1 << 100) + 5);
	assert(p.v.x == 7 && p.v.y == 9 && p.v.z == 8 && p.v.w == 4);
	Holder h;
	h.u.l = 77;
	h.u.l++;
	assert(h.u.l == 78);
}

int target = 5;
struct WideRef @align(16) { int* p; }
struct HoldsRef @packed { char c; WideRef r; }
HoldsRef g_ref = { 1, { &target } };
union RefUnion @packed { char c; WideRef r; }
RefUnion g_ref_union = { .r = { &target } };

fn void packed_member_address() @test
{
	assert(g_ref.c == 1 && *g_ref.r.p == 5);
	assert(*g_ref_union.r.p == 5);
	HoldsRef local = { 2, { &target } };
	assert(local.c == 2 && local.r.p == &target);
}
