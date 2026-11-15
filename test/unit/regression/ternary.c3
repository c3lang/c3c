module test;

union TernaryUnion
{
	struct padded { ulong lo; ushort hi; } // first largest member: picked as the union's LLVM type
	ulong[2] wide;
}

union TernaryGapUnion
{
	struct gap { char a; ulong b; } // padding between `a` and `b` overlaps wide[0]
	ulong[2] wide;
}

struct TernaryTagged
{
	char tag;
	TernaryUnion u;
}

alias TernaryPair = TernaryUnion[2];

fn TernaryUnion pick(bool c, TernaryUnion a, TernaryUnion b) @noinline => c ? a : b;
fn TernaryGapUnion pick_gap(bool c, TernaryGapUnion a, TernaryGapUnion b) @noinline => c ? a : b;
fn TernaryTagged pick_tagged(bool c, TernaryTagged a, TernaryTagged b) @noinline => c ? a : b;
fn TernaryPair pick_pair(bool c, TernaryPair a, TernaryPair b) @noinline => c ? a : b;


fn void const_ternary()  @test
{
	int foo = 1;
    const int FOO = 1;
    assert((foo ?: 2) == 1);
    assert((FOO ?: 2) == 1);
    int bar = 2;
    assert((FOO ?: bar) == 1);
}

fn void test_ternary_temp_lifetime()  @test
{
    bool c = true;
    int* p = c ? &&5 : &&7;
    int* p2 = c ? &&999 : &&888;
    assert(*p == 5);
    assert(*p2 == 999);
}

fn void test_ternary_slice_temp_lifetime()  @test
{
    bool c = true;
    int[] s1 = c ? (int[]){ 10, 20 } : (int[]){ 30, 40 };
    int[] s2 = c ? (int[]){ 100, 200 } : (int[]){ 300, 400 };
    assert(s1[0] == 10 && s1[1] == 20);
    assert(s2[0] == 100 && s2[1] == 200);
}

fn void test_ternary_any_lifetime()  @test
{
    bool c = true;
    any a = c ? &&5 : &&7;
    assert(*(int*)a.ptr == 5);
}

fn void ternary_union_with_padded_rep()  @test
{
	// Bytes past `padded.hi` are padding in the representative member's type,
	// a by-value phi of the aggregate must not drop them.
	TernaryUnion a = { .wide = { 1, 0xAAAA_BBBB_CCCC_DDDD } };
	TernaryUnion b = { .wide = { 2, 0x1111_2222_3333_4444 } };
	assert(pick(true, a, b).wide[1] == 0xAAAA_BBBB_CCCC_DDDD);
	assert(pick(false, a, b).wide[1] == 0x1111_2222_3333_4444);
}

fn void ternary_union_with_gap_in_rep()  @test
{
	TernaryGapUnion a = { .wide = { 0xAAAA_BBBB_CCCC_DDDD, 1 } };
	TernaryGapUnion b = { .wide = { 0x1111_2222_3333_4444, 2 } };
	assert(pick_gap(true, a, b).wide[0] == 0xAAAA_BBBB_CCCC_DDDD);
	assert(pick_gap(false, a, b).wide[0] == 0x1111_2222_3333_4444);
}

fn void ternary_union_nested_in_struct()  @test
{
	TernaryTagged a = { .tag = 1, .u.wide = { 1, 0xAAAA_BBBB_CCCC_DDDD } };
	TernaryTagged b = { .tag = 2, .u.wide = { 2, 0x1111_2222_3333_4444 } };
	assert(pick_tagged(true, a, b).u.wide[1] == 0xAAAA_BBBB_CCCC_DDDD);
	assert(pick_tagged(false, a, b).u.wide[1] == 0x1111_2222_3333_4444);
}

fn void ternary_union_array()  @test
{
	TernaryPair a = { { .wide = { 1, 0xAAAA_BBBB_CCCC_DDDD } }, { .wide = { 3, 0xAAAA_BBBB_CCCC_DDDD } } };
	TernaryPair b = { { .wide = { 2, 0x1111_2222_3333_4444 } }, { .wide = { 4, 0x1111_2222_3333_4444 } } };
	assert(pick_pair(true, a, b)[1].wide[1] == 0xAAAA_BBBB_CCCC_DDDD);
	assert(pick_pair(false, a, b)[0].wide[1] == 0x1111_2222_3333_4444);
}
