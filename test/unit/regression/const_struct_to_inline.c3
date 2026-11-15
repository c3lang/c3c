module const_struct_to_inline;

struct Point { int x, y; }
struct Wrap { inline Point p; int z; }
struct Wrap2 { inline Wrap w; int q; }
struct IntW { inline int v; int z; }
union Un { int a; float f; }
struct UInl { inline Un u; int k; }

const Wrap CW = { { 1, 2 }, 3 };
const Wrap CZ = {};
const Wrap2 CW2 = { { { 4, 5 }, 6 }, 7 };
const IntW CI = { 9, 10 };
const UInl CU = { { .f = 1.5 }, 2 };

const Point CP = CW;
const int CII = CI;
Point gp = CW;
Point gz = CZ;
Wrap gw = CW2;
Point gp2 = CW2;
int gi = CI;
Un gu = CU;

fn void const_struct_to_inline() @test
{
	$assert CP.x == 1 && CP.y == 2;
	$assert CII == 9;
	Point lp = CW;
	assert(gp.x == 1 && gp.y == 2);
	assert(gz.x == 0 && gz.y == 0);
	assert(gw.x == 4 && gw.y == 5 && gw.z == 6);
	assert(gp2.x == 4 && gp2.y == 5);
	assert(gi == 9);
	assert(gu.f == 1.5f);
	assert(lp.x == 1 && lp.y == 2);
}
