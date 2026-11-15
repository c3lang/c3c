module optional_inline_member;

struct Inner { int x; int y; }
fn int Inner.sum(&self) => self.x + self.y;

struct Top { inline Inner in; int c; }
fn int Top.triple(&self) => self.c * 3;

struct Outer { inline Top top; int d; }

typedef Wrapped = inline Top;

faultdef BAD;

fn Top? make_top(bool ok) @noinline
{
	if (!ok) return BAD~;
	return { { 1, 2 }, 3 };
}

fn int? sum_through(bool ok) @noinline => make_top(ok).sum();

fn void inline_member_through_optional() @test
{
	Top? ot = { { 1, 2 }, 3 };
	int? x = ot.x;
	int? y = ot.y;
	int? c = ot.c;
	int? s = ot.sum();
	int? t = ot.triple();
	assert(x!! == 1 && y!! == 2 && c!! == 3);
	assert(s!! == 3 && t!! == 9);
	assert(make_top(true).x!! == 1);
	assert(sum_through(true)!! == 3);
	assert($defined(ot.x) && $defined(ot.sum()));
}

fn void nested_and_typedef_inline() @test
{
	Outer? oo = { { { 4, 5 }, 6 }, 7 };
	assert(oo.x!! == 4);
	assert(oo.c!! == 6);
	assert(oo.sum()!! == 9);
	assert(oo.triple()!! == 18);
	Wrapped? w = (Wrapped){ { 8, 9 }, 10 };
	assert(w.y!! == 9);
	assert(w.sum()!! == 17);
}

fn void fault_propagates() @test
{
	Top? bad = BAD~;
	int? bx = bad.x;
	assert(@catch(bx) == BAD);
	assert(@catch(bad.sum()) == BAD);
	assert(@catch(sum_through(false)) == BAD);
	Outer? obad = BAD~;
	assert(@catch(obad.y) == BAD);
}
