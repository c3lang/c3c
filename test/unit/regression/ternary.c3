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

module test_ternary_union;

union TernaryUnion
{
	struct padded { ulong lo; ushort hi; } // first largest member: picked as the union's LLVM type
	ulong[2] wide;
}

fn TernaryUnion pick(bool c, TernaryUnion a, TernaryUnion b) @noinline => c ? a : b;

module test @test;
import test_ternary_union;

fn void ternary_union_with_padded_rep()
{
	// Bytes past `padded.hi` are padding in the representative member's type,
	// a by-value phi of the aggregate must not drop them.
	TernaryUnion a = { .wide = { 1, 0xAAAA_BBBB_CCCC_DDDD } };
	TernaryUnion b = { .wide = { 2, 0x1111_2222_3333_4444 } };
	assert(test_ternary_union::pick(true, a, b).wide[1] == 0xAAAA_BBBB_CCCC_DDDD);
	assert(test_ternary_union::pick(false, a, b).wide[1] == 0x1111_2222_3333_4444);
}
