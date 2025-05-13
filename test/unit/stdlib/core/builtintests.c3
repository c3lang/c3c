module std::core::builtins @test;

fn void test_anycast()
{
	int a;
	any b = &a;
	assert(anycast(b, int)!! == &a);
	assert(@catch(anycast(b, double)) == TYPE_MISMATCH);
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
	assert(@catch(enum_by_name(Tester, "GHI")) == NOT_FOUND);
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
	int d = @expect(b, 2);
}


int abc;

fn void test_prefetch()
{
	@prefetch(&abc);
}

fn void test_hash()
{
	(char){}.hash();
	(char[<100>]){}.hash();
	(char[100]){}.hash();
	(ichar){}.hash();
	(ichar[<100>]){}.hash();
	(ichar[100]){}.hash();
	(short){}.hash();
	(short[<100>]){}.hash();
	(short[100]){}.hash();
	(ushort){}.hash();
	(ushort[<100>]){}.hash();
	(ushort[100]){}.hash();
	(int){}.hash();
	(int[<100>]){}.hash();
	(int[100]){}.hash();
	(uint){}.hash();
	(uint[<100>]){}.hash();
	(uint[100]){}.hash();
	(long){}.hash();
	(long[<100>]){}.hash();
	(long[100]){}.hash();
	(ulong){}.hash();
	(ulong[<100>]){}.hash();
	(ulong[100]){}.hash();
	(int128){}.hash();
	(int128[<100>]){}.hash();
	(int128[100]){}.hash();
	(uint128){}.hash();
	(uint128[<100>]){}.hash();
	(uint128[100]){}.hash();
	(bool){}.hash();
	(bool[<100>]){}.hash();
	(bool[100]){}.hash();
	String x = "abc";
	char[] y = "abc";
	assert(x.hash() == y.hash());
	assert(int.typeid.hash());
}

fn void test_hash_repeat()
{
	assert((char){}.hash() == (char){}.hash());
	assert((char[<100>]){}.hash() == (char[<100>]){}.hash());
	assert((char[100]){}.hash() == (char[100]){}.hash());
	assert((ichar){}.hash() == (ichar){}.hash());
	assert((ichar[<100>]){}.hash() == (ichar[<100>]){}.hash());
	assert((ichar[100]){}.hash() == (ichar[100]){}.hash());
	assert((short){}.hash() == (short){}.hash());
	assert((short[<100>]){}.hash() == (short[<100>]){}.hash());
	assert((short[100]){}.hash() == (short[100]){}.hash());
	assert((ushort){}.hash() == (ushort){}.hash());
	assert((ushort[<100>]){}.hash() == (ushort[<100>]){}.hash());
	assert((ushort[100]){}.hash() == (ushort[100]){}.hash());
	assert((int){}.hash() == (int){}.hash());
	assert((int[<100>]){}.hash() == (int[<100>]){}.hash());
	assert((int[100]){}.hash() == (int[100]){}.hash());
	assert((uint){}.hash() == (uint){}.hash());
	assert((uint[<100>]){}.hash() == (uint[<100>]){}.hash());
	assert((uint[100]){}.hash() == (uint[100]){}.hash());
	assert((long){}.hash() == (long){}.hash());
	assert((long[<100>]){}.hash() == (long[<100>]){}.hash());
	assert((long[100]){}.hash() == (long[100]){}.hash());
	assert((ulong){}.hash() == (ulong){}.hash());
	assert((ulong[<100>]){}.hash() == (ulong[<100>]){}.hash());
	assert((ulong[100]){}.hash() == (ulong[100]){}.hash());
	assert((int128){}.hash() == (int128){}.hash());
	assert((int128[<100>]){}.hash() == (int128[<100>]){}.hash());
	assert((int128[100]){}.hash() == (int128[100]){}.hash());
	assert((uint128){}.hash() == (uint128){}.hash());
	assert((uint128[<100>]){}.hash() == (uint128[<100>]){}.hash());
	assert((uint128[100]){}.hash() == (uint128[100]){}.hash());
	assert((bool){}.hash() == (bool){}.hash());
	assert((bool[<100>]){}.hash() == (bool[<100>]){}.hash());
	assert((bool[100]){}.hash() == (bool[100]){}.hash());
	assert(int.typeid.hash() == int.typeid.hash());
}
