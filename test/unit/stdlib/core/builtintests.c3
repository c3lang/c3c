module std::core::builtins @test;

fn void test_anycast()
{
	int a;
	any b = &a;
	assert(anycast(b, int)!! == &a);
	assert(@catch(anycast(b, double)) == CastResult.TYPE_MISMATCH);
}

fn void test_bitcast()
{
	int a = 123;
	float z = bitcast(a, float);
	assert(bitcast(z, int) == a);
}

enum Tester
{
	ABC,
	DEF,
}

fn void test_enum_by_name()
{
	assert(enum_by_name(Tester, "ABC")!! == Tester.ABC);
	assert(enum_by_name(Tester, "DEF")!! == Tester.DEF);
	assert(@catch(enum_by_name(Tester, "GHI")) == SearchResult.MISSING);
}

fn void test_likely()
{
	int a = 2;
	int b = 43;
	if (@likely(a > b, 0.5)) a++;
	if (@likely(a < b)) b++;
}

fn void test_unlikely()
{
	int a = 2;
	int b = 43;
	if (@unlikely(a > b, 0.5)) a++;
	if (@unlikely(a < b)) b++;
}

fn void test_expect()
{
	int a = 2;
	int b = 43;
	int c = @expect(a, 2, 0.5);
	int d = @expect(b, 2u8);
}


int abc;

fn void test_prefetch()
{
	@prefetch(&abc);
}

fn void test_hash()
{
	char{}.hash();
	ichar{}.hash();
	short{}.hash();
	ushort{}.hash();
	int{}.hash();
	uint{}.hash();
	long{}.hash();
	ulong{}.hash();
	int128{}.hash();
	uint128{}.hash();
	String x = "abc";
	char[] y = "abc";
	assert(x.hash() == y.hash());
	assert(int.typeid.hash());
}