// Bool vectors from `!`, from int/pointer vectors, comparisons and bitwise logic, and back to int vectors.
module vector_bool_ops;
import std::math;

fn void not_vectors() @test
{
	int[<4>] a = { 1, 0, 5, 0 };
	bool[<4>] bb = !!a;
	assert(bb[0] && !bb[1] && bb[2] && !bb[3]);
	char[<4>] ch = { 1, 0, 2, 0 };
	bool[<4>] nch = !ch;
	assert(!nch[0] && nch[1] && !nch[2] && nch[3]);
	bool[<4>] c = { true, false, true, false };
	bool[<4>] nc = !c;
	assert(!nc[0] && nc[1] && !nc[2] && nc[3]);
}

fn void to_and_from_bool_vectors() @test
{
	int xd;
	void*[<2>] p = { &xd, null };
	bool[<2>] pb = (bool[<2>])p;
	assert(pb[0] && !pb[1]);
	int[<4>] a = { 3, 0, -2, 0 };
	bool[<4>] ab = (bool[<4>])a;
	assert(ab[0] && !ab[1] && ab[2] && !ab[3]);
	bool[<4>] c = { true, false, true, false };
	int[<4>] w = !c;
	assert(w == (int[<4>]){ 0, -1, 0, -1 });
	int[<4>] ci = (int[<4>])c;
	assert(ci == (int[<4>]){ -1, 0, -1, 0 });
}

fn void comparisons_and_logic() @test
{
	int[<4>] x = { 1, 2, 3, 4 };
	int[<4>] y = { 1, 0, 3, 0 };
	bool[<4>] eq = x.comp_eq(y);
	assert(eq == (bool[<4>]){ true, false, true, false });
	bool[<4>] lt = x.comp_lt(y);
	assert(lt == (bool[<4>]){ false, false, false, false });
	bool[<4>] t = { true, true, false, false };
	bool[<4>] u = { true, false, true, false };
	assert((t & u) == (bool[<4>]){ true, false, false, false });
	assert((t | u) == (bool[<4>]){ true, true, true, false });
	assert((t ^ u) == (bool[<4>]){ false, true, true, false });
	assert(t.and() == false && t.or() == true);
}

// Overflow builtins on vectors compute per lane.
fn void overflow_builtins() @test
{
	int[<2>] a = { 2, 7 };
	int[<2>] b = { 3, 2 };
	int[<2>] r;
	(void)math::overflow_mul(a, b, &r);
	assert(r == (int[<2>]){ 6, 14 });
	int x = 2147483647;
	int y;
	assert(math::overflow_add(x, 1, &y));
	assert(!math::overflow_add(1, 1, &y) && y == 2);
}
