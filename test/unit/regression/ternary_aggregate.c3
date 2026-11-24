module ternary_aggregate;

union PaddedRepUnion
{
	struct padded { ulong lo; ushort hi; }
	ulong[2] wide;
}

union GapRepUnion
{
	struct gap { char a; ulong b; }
	ulong[2] wide;
}

struct Tagged
{
	char tag;
	PaddedRepUnion u;
}

struct NestedPad { ulong lo; ushort hi; }
struct NestedRep { NestedPad[2] values; }

union NestedArrayUnion
{
	NestedRep rep;
	ulong[4] wide;
}

struct Big { long a; long b; long c; }

alias UnionPair = PaddedRepUnion[2];

faultdef BAD;

fn PaddedRepUnion pick(bool c, PaddedRepUnion a, PaddedRepUnion b) @noinline => c ? a : b;
fn PaddedRepUnion pick_const(bool c) @noinline => c ? (PaddedRepUnion){ .wide = { 1, 0xAAAA_BBBB_CCCC_DDDD } } : (PaddedRepUnion){ .wide = { 2, 0x1111_2222_3333_4444 } };
fn GapRepUnion pick_gap(bool c, GapRepUnion a, GapRepUnion b) @noinline => c ? a : b;
fn Tagged pick_tagged(bool c, Tagged a, Tagged b) @noinline => c ? a : b;
fn UnionPair pick_pair(bool c, UnionPair a, UnionPair b) @noinline => c ? a : b;
fn NestedArrayUnion pick_nested_array(bool c, NestedArrayUnion a, NestedArrayUnion b) @noinline => c ? a : b;
fn Big? some_or_fault(Big* p) @noinline => p ? *p : BAD~;
fn Big? fault_or_some(Big* p, Big* q) @noinline => p ? BAD~ : *q;

fn Big with_defer(bool c) @noinline
{
	Big x = { 1, 2, 3 };
	Big y = { 4, 5, 6 };
	defer { x.a = 100; y.a = 200; }
	return c ? x : y;
}

macro Big @pick_local(bool c)
{
	Big p = { 7, 8, 9 };
	Big q = { 10, 11, 12 };
	return c ? p : q;
}

fn long sum(Big v, long extra) @noinline => v.a + v.b + v.c + extra;
fn long first_a(Big v, long ignored) @noinline => v.a;

fn void union_padded_representative() @test
{
	PaddedRepUnion a = { .wide = { 1, 0xAAAA_BBBB_CCCC_DDDD } };
	PaddedRepUnion b = { .wide = { 2, 0x1111_2222_3333_4444 } };
	assert(pick(true, a, b).wide[1] == 0xAAAA_BBBB_CCCC_DDDD);
	assert(pick(false, a, b).wide[1] == 0x1111_2222_3333_4444);
	bool t = a.wide[0] == 1;
	PaddedRepUnion local = t ? a : b;
	assert(local.wide[1] == 0xAAAA_BBBB_CCCC_DDDD);
}

fn void union_constant_operands() @test
{
	assert(pick_const(true).wide[1] == 0xAAAA_BBBB_CCCC_DDDD);
	assert(pick_const(false).wide[1] == 0x1111_2222_3333_4444);
}

fn void union_gap_in_representative() @test
{
	GapRepUnion a = { .wide = { 0xAAAA_BBBB_CCCC_DDDD, 1 } };
	GapRepUnion b = { .wide = { 0x1111_2222_3333_4444, 2 } };
	assert(pick_gap(true, a, b).wide[0] == 0xAAAA_BBBB_CCCC_DDDD);
	assert(pick_gap(false, a, b).wide[0] == 0x1111_2222_3333_4444);
}

fn void union_nested_in_struct() @test
{
	Tagged a = { .tag = 1, .u.wide = { 1, 0xAAAA_BBBB_CCCC_DDDD } };
	Tagged b = { .tag = 2, .u.wide = { 2, 0x1111_2222_3333_4444 } };
	assert(pick_tagged(true, a, b).u.wide[1] == 0xAAAA_BBBB_CCCC_DDDD);
	assert(pick_tagged(false, a, b).u.wide[1] == 0x1111_2222_3333_4444);
}

fn void union_array() @test
{
	UnionPair a = { { .wide = { 1, 0xAAAA_BBBB_CCCC_DDDD } }, { .wide = { 3, 0xAAAA_BBBB_CCCC_DDDD } } };
	UnionPair b = { { .wide = { 2, 0x1111_2222_3333_4444 } }, { .wide = { 4, 0x1111_2222_3333_4444 } } };
	assert(pick_pair(true, a, b)[1].wide[1] == 0xAAAA_BBBB_CCCC_DDDD);
	assert(pick_pair(false, a, b)[0].wide[1] == 0x1111_2222_3333_4444);
}

fn void padding_nested_through_array() @test
{
	NestedArrayUnion a = { .wide = { 1, 2, 3, 4 } };
	NestedArrayUnion b = { .wide = { 5, 6, 7, 8 } };
	assert(pick_nested_array(true, a, b).wide == { 1, 2, 3, 4 });
	assert(pick_nested_array(false, a, b).wide == { 5, 6, 7, 8 });
}

fn void fault_in_one_branch() @test
{
	Big x = { 1, 2, 3 };
	assert(some_or_fault(&x)!!.c == 3);
	assert(@catch(some_or_fault(null)) == BAD);
	assert(@catch(fault_or_some(&x, &x)) == BAD);
}

fn void result_is_a_copy() @test
{
	bool t = true;
	bool f = false;
	Big a = { 1, 1, 1 };
	Big b = { 2, 2, 2 };
	a = f ? b : a;
	assert(a.a == 1);
	a = t ? b : a;
	assert(a.a == 2);
	Big[2] arr = { { 5, 5, 5 }, { 6, 6, 6 } };
	Big picked = t ? arr[1] : arr[0];
	arr[1].a = 99;
	assert(picked.a == 6);
	assert(sum(t ? arr[0] : arr[1], 1) == 16);
	Big? o = f ? a : arr[0];
	assert(o!!.b == 5);
	assert(first_a(t ? a : b, a.a = 50) == 2);
	assert(a.a == 50);
}

fn void defer_and_macro_locals() @test
{
	assert(with_defer(true).a == 1);
	assert(with_defer(false).a == 4);
	bool t = true;
	assert(@pick_local(t).c == 9);
	assert(@pick_local(!t).c == 12);
}
