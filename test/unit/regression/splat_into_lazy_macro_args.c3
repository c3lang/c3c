module splat_into_lazy_macro_args;

faultdef SPLAT_ORDER_FAIL;
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

int order;
int[32] seen;
fn int note(int v) { seen[order++] = v; return v; }
fn int[1] mk_one(int v) { note(v); return { v }; }
fn int[2] mk_arr(int v) { note(v); return { v, v }; }
fn int[<2>] mk_vec(int v) { note(v); return { v, v }; }
fn int[2]? maybe_arr(int v, bool fail)
{
	note(v);
	if (fail) return SPLAT_ORDER_FAIL~;
	return { v, v };
}
fn Pt mk_pt(int v) { note(v); return { v, v }; }
fn Pt? maybe_pt(int v, bool fail)
{
	note(v);
	if (fail) return SPLAT_ORDER_FAIL~;
	return { v, v };
}
<* @require a > 0 *>
fn int typed_fn(int a, int... values, int z = 0) => a + values[0] + z;

macro @eager_then_lazy(int a, #b, #c) => a * 100 + #b * 10 + #c;
macro @repeat_lazy(#a, #b) => #a + #b + #a + #b;
macro @splat_starts_eager(int a, #b) => a + #b;
macro @splat_ends_eager(#a, int b) => #a + b;
macro @sandwich(int a, #b, int c) => a + #b + c;
macro @two_splats(int a, #b, #c, int d, #e, #f) => a + #b + #c + d + #e + #f;
macro @two_splats_tail(int a, #b, #c, int d, #e, #f, int g) => a + #b + #c + d + #e + #f + g;
macro @raw_after(int a, ...) => a * 10 + $vaarg[0];
macro @named_tail(int a, #b, #c, int z = 0) => a + #b + #c + z;
macro @typed_tail(int a, int... values, int z = 0) => a + values[0] + z;
macro @typed_len(int a, int... values, int z = 0) => a + values.len + z;
macro @default_tail(int a, #b, #c, int z = note(3)) => a + #b + #c + z;
macro @ignore_lazy(#value) => 0;
macro @forward_splat(#values) => @never(...#values);
struct Counter { int v; }
fn Counter mk_counter(int marker) { note(marker); return { 100 }; }
macro Counter.@meth(&self, int a, #b, #c) => self.v + a + #b + #c;

fn void splat_keeps_argument_order() @test
{
	order = 0;
	assert(@eager_then_lazy(note(1), ...mk_arr(2)) == 122);
	assert(seen[:2] == { 1, 2 });
	order = 0;
	assert(@eager_then_lazy(note(1), ...mk_pt(2)) == 122);
	assert(seen[:2] == { 1, 2 });
	order = 0;
	assert(@eager_then_lazy(note(1), ...mk_vec(2)) == 122);
	assert(seen[:2] == { 1, 2 });
	order = 0;
	assert(@eager_then_lazy(note(1), ...mk_arr(2)[:2]) == 122);
	assert(seen[:2] == { 1, 2 });
	order = 0;
	assert(@repeat_lazy(...mk_arr(1)) == 4);
	assert(seen[:1] == { 1 });
	order = 0;
	assert(@splat_starts_eager(...mk_arr(1)) == 2);
	assert(seen[:1] == { 1 });
	order = 0;
	assert(@splat_ends_eager(...mk_arr(1)) == 2);
	assert(seen[:1] == { 1 });
	order = 0;
	assert(@sandwich(note(1), ...mk_one(2), note(3)) == 6);
	assert(seen[:3] == { 1, 2, 3 });
	order = 0;
	assert(@two_splats(note(1), ...mk_arr(2), note(3), ...mk_pt(4)) == 16);
	assert(seen[:4] == { 1, 2, 3, 4 });
	order = 0;
	assert(@raw_after(note(1), ...mk_pt(2)) == 12);
	assert(seen[:2] == { 1, 2 });
	order = 0;
	assert(@raw_after(note(1), ...mk_arr(2)) == 12);
	assert(seen[:2] == { 1, 2 });
	order = 0;
	assert(@named_tail(note(1), ...mk_arr(2), z: note(3)) == 8);
	assert(seen[:3] == { 1, 2, 3 });
	order = 0;
	assert(@typed_tail(note(1), note(2), z: note(3)) == 6);
	assert(seen[:3] == { 1, 2, 3 });
	order = 0;
	assert(@typed_len(note(1), z: note(2)) == 3);
	assert(seen[:2] == { 1, 2 });
	order = 0;
	assert(@typed_tail(note(1), ...mk_arr(2), z: note(3)) == 6);
	assert(seen[:3] == { 1, 2, 3 });
	order = 0;
	assert(typed_fn(note(1), ...mk_arr(2), z: note(3)) == 6);
	assert(seen[:3] == { 1, 2, 3 });
	order = 0;
	assert(@default_tail(note(1), ...mk_arr(2)) == 8);
	assert(seen[:3] == { 1, 2, 3 });
	order = 0;
	assert((@eager_then_lazy(note(1), ...maybe_arr(2, false)) ?? -1) == 122);
	assert(seen[:2] == { 1, 2 });
	order = 0;
	assert((@named_tail(note(1), ...maybe_arr(2, true), z: note(3)) ?? -1) == -1);
	assert(seen[:2] == { 1, 2 });
	order = 0;
	assert((@named_tail(note(1), ...maybe_pt(2, true), z: note(3)) ?? -1) == -1);
	assert(seen[:2] == { 1, 2 });
	order = 0;
	assert((@splat_starts_eager(...maybe_arr(1, true)) ?? -1) == -1);
	assert(seen[:1] == { 1 });
	order = 0;
	assert((@typed_tail(note(1), ...maybe_arr(2, true), z: note(3)) ?? -1) == -1);
	assert(seen[:2] == { 1, 2 });
	order = 0;
	assert((@two_splats_tail(note(1), ...maybe_arr(2, false), note(3), ...maybe_pt(4, true), note(5)) ?? -1) == -1);
	assert(seen[:4] == { 1, 2, 3, 4 });
	order = 0;
	assert(@ignore_lazy(note(1)) == 0);
	assert(order == 0);
	assert(@never(...{ 1, 2 }) == 0);
	assert(order == 0);
	assert(@forward_splat(mk_arr(1)) == 0);
	assert(seen[:1] == { 1 });
	order = 0;
	assert(mk_counter(1).@meth(note(2), ...mk_arr(3)) == 108);
	assert(seen[:3] == { 1, 2, 3 });
}
