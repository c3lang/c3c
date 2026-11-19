module packed_union_member_init;

union Aa { bool b; int x; }
union Ba @packed { Aa x; }
struct Sp @packed { char c; Aa u; }
union Cu @packed { Sp s; long l; }

fn void union_member_init_in_packed() @test
{
	bool flag = true;
	int value = 7;
	Ba a = (Ba){ { .b = flag } };
	Ba b = { { .x = value } };
	Sp c = { 1, { .x = value } };
	Sp d = { .c = 3, .u.x = value + 1 };
	Sp[2] e = { [1] = { 1, { .x = value + 2 } } };
	Cu f = { .s = { 2, { .x = value + 3 } } };
	assert(a.x.b);
	assert(b.x.x == 7);
	assert(c.c == 1 && c.u.x == 7);
	assert(d.c == 3 && d.u.x == 8);
	assert(e[1].u.x == 9);
	assert(f.s.c == 2 && f.s.u.x == 10);
}
