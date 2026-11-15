module test @test;

fn void const_ternary()
{
	int foo = 1;
    const int FOO = 1;
    assert((foo ?: 2) == 1);
    assert((FOO ?: 2) == 1);
    int bar = 2;
    assert((FOO ?: bar) == 1);
}


union TernaryUnion
{
	struct padded { ulong lo; ushort hi; } // first largest member: picked as the union's LLVM type
	ulong[2] wide;
}

fn TernaryUnion pick(bool c, TernaryUnion a, TernaryUnion b) @noinline => c ? a : b;

fn void ternary_union_with_padded_rep()
{
	// Bytes past `padded.hi` are padding in the representative member's type,
	// a by-value phi of the aggregate must not drop them.
	TernaryUnion a = { .wide = { 1, 0xAAAA_BBBB_CCCC_DDDD } };
	TernaryUnion b = { .wide = { 2, 0x1111_2222_3333_4444 } };
	assert(test_ternary_union::pick(true, a, b).wide[1] == 0xAAAA_BBBB_CCCC_DDDD);
	assert(test_ternary_union::pick(false, a, b).wide[1] == 0x1111_2222_3333_4444);
}

fn void test_ternary_temp_lifetime()
{
    bool c = true;
    int* p = c ? &&5 : &&7;
    int* p2 = c ? &&999 : &&888;
    assert(*p == 5);
    assert(*p2 == 999);
}

fn void test_ternary_slice_temp_lifetime()
{
    bool c = true;
    int[] s1 = c ? (int[]){ 10, 20 } : (int[]){ 30, 40 };
    int[] s2 = c ? (int[]){ 100, 200 } : (int[]){ 300, 400 };
    assert(s1[0] == 10 && s1[1] == 20);
    assert(s2[0] == 100 && s2[1] == 200);
}

fn void test_ternary_any_lifetime()
{
    bool c = true;
    any a = c ? &&5 : &&7;
    assert(*(int*)a.ptr == 5);
}

