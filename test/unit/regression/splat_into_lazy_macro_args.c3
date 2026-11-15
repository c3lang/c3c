module splat_into_lazy_macro_args;

struct Pt { int x, y; }
int calls;
fn Pt mk() { calls++; return { 5, 6 }; }

macro @hh(#a, #b) => #a * 10 + #b;
macro @rev(#a, #b) => #b * 10 + #a;
macro @skip(#a, #b) => #b;
macro @never(#a, #b) => 0;
macro @vv(...) => $vaarg[0] * 10 + $vaarg[1];
macro @norm(a, b) => a * 10 + b;
macro @mixed(a, #b, c) => a * 100 + #b * 10 + c;
macro @four(#a, b, #c, d) => a(#a) * 1000 + b * 100 + #c * 10 + d;
macro a(x) => x;

fn void splat_into_lazy_macro_args() @test
{
	Pt p = { 5, 6 };
	int[2] arr = { 5, 6 };
	Pt q = { 3, 4 };
	assert(@hh(...p) == 56);
	assert(@hh(...arr) == 56);
	assert(@rev(...p) == 65);
	assert(@skip(...arr) == 6);
	assert(@vv(...p) == 56);
	assert(@norm(...p) == 56);
	assert(@mixed(1, ...p) == 156);
	assert(@four(...p, ...q) == 5634);
	calls = 0;
	assert(@hh(...mk()) == 56);
	assert(@never(...mk()) == 0);
	assert(calls == 2);
}
