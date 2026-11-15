module vector_overflow_builtins;
import std::math;

fn void vector_overflow_mul() @test
{
	int[<2>] a = { 2, int::max };
	int[<2>] b = { 3, 2 };
	int[<2>] r;
	bool[<2>] f = math::overflow_mul(a, b, &r);
	assert(f == { false, true });
	assert(r == { 6, -2 });
}

fn void vector_overflow_mul_lanes() @test
{
	int[<4>] a = { 1, int::max, int::min, 5 };
	int[<4>] b = { 1, 2, -1, 5 };
	int[<4>] r;
	bool[<4>] f = math::overflow_mul(a, b, &r);
	assert(f == { false, true, true, false });
	assert(r[0] == 1 && r[3] == 25);
	uint[<2>] u = { 0x8000_0000, 7 };
	uint[<2>] ur;
	bool[<2>] uf = math::overflow_mul(u, (uint[<2>]){ 2, 3 }, &ur);
	assert(uf == { true, false });
	assert(ur == { 0, 21 });
}

fn void scalar_overflow_still_works() @test
{
	int r;
	assert(math::overflow_mul(int::max, 2, &r));
	assert(!math::overflow_mul(3, 2, &r));
	assert(r == 6);
}

fn void vector_overflow_add_sub() @test
{
	int[<4>] a = { 1, int::max, int::min, 5 };
	int[<4>] b = { 1, 1, 1, 5 };
	int[<4>] r;
	bool[<4>] f = math::overflow_add(a, b, &r);
	assert(f == { false, true, false, false });
	assert(r[0] == 2 && r[3] == 10);
	f = math::overflow_sub(a, b, &r);
	assert(f == { false, false, true, false });
	assert(r[0] == 0 && r[3] == 0);
	char[<2>] c = { 200, 10 };
	char[<2>] cr;
	bool[<2>] cf = math::overflow_add(c, (char[<2>]){ 100, 10 }, &cr);
	assert(cf == { true, false });
	assert(cr == { 44, 20 });
	cf = math::overflow_sub(c, (char[<2>]){ 100, 11 }, &cr);
	assert(cf == { false, true });
	assert(cr == { 100, 255 });
}

fn void scalar_overflow_add_sub() @test
{
	int r;
	assert(math::overflow_add(int::max, 1, &r));
	assert(!math::overflow_add(1, 2, &r));
	assert(r == 3);
	assert(math::overflow_sub(int::min, 1, &r));
	assert(!math::overflow_sub(5, 2, &r));
	assert(r == 3);
}
