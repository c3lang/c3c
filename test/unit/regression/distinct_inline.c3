module distinct_inline @test;

distinct Foo = inline int;
distinct Bar = inline Foo;
distinct Baz = inline Foo;
distinct Abc = inline Baz;
distinct Def = inline Abc;
distinct Other = inline int;
distinct Other2 = inline Other;
fn void test_binary()
{
	assert($typeof((Foo)1 + 1).typeid == Foo.typeid);
	assert($typeof((Foo)1 + (Bar)1).typeid == Foo.typeid);
	assert($typeof((Baz)1 + (Bar)1).typeid == Foo.typeid);
	assert($typeof((Def)1 + (Bar)1).typeid == Foo.typeid);
	assert($typeof((Other2)1 + (Def)1).typeid == int.typeid);
}

distinct DistinctInt = inline int;
distinct DistinctUInt = inline uint;

fn void test_comparison()
{
	char v_char;
	ichar v_ichar;
	short v_short;
	short v_ushort;
	int v_int;
	uint v_uint;
	long v_long;
	ulong v_ulong;
	assert((DistinctInt)0 == v_ichar);
	assert((DistinctInt)0 == v_char);
	assert((DistinctInt)0 == v_short);
	assert((DistinctInt)0 == v_ushort);
	assert((DistinctInt)0 == v_int);
	assert((DistinctInt)0 == v_uint);
	assert((DistinctInt)0 == v_long);
	assert((DistinctInt)0 == v_ulong);

	assert((DistinctUInt)0 == v_ichar);
	assert((DistinctUInt)0 == v_char);
	assert((DistinctUInt)0 == v_short);
	assert((DistinctUInt)0 == v_ushort);
	assert((DistinctUInt)0 == v_int);
	assert((DistinctUInt)0 == v_uint);
	assert((DistinctUInt)0 == v_long);
	assert((DistinctUInt)0 == v_ulong);
}