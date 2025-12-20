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


faultdef SOME_FAULT, ABC_FAULT;

fn void test_try_catch()
{
	int val;
	int? x = ABC_FAULT?;
	assert(@try_catch(val, x, ABC_FAULT)!!);
	assert(val == 0);
	assert(!@catch(@try_catch(val, x, ABC_FAULT)));
	x = SOME_FAULT?;
	assert(@catch(@try_catch(val, x, ABC_FAULT)) == SOME_FAULT);
	x = 3;
	assert(!@try_catch(val, x, ABC_FAULT)!!);
	assert(val == 3);
}

fn void test_try_set()
{
	assert(enum_by_name(Tester, "ABC")!! == Tester.ABC);

	Tester val;
	if (catch @try(val, enum_by_name(Tester, "ABC"))) abort("Test failure");
	assert(val == Tester.ABC);

	Tester another;
	if (catch err = @try(another, enum_by_name(Tester, "GHI")))
	{
		assert(err == NOT_FOUND);
		return;
	}

	abort("Test failure");
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
	(long[<20>]){}.hash();
	(long[100]){}.hash();
	(ulong){}.hash();
	(ulong[<20>]){}.hash();
	(ulong[100]){}.hash();
	(int128){}.hash();
	(int128[<20>]){}.hash();
	(int128[100]){}.hash();
	(uint128){}.hash();
	(uint128[<20>]){}.hash();
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
	assert((long[<20>]){}.hash() == (long[<20>]){}.hash());
	assert((long[100]){}.hash() == (long[100]){}.hash());
	assert((ulong){}.hash() == (ulong){}.hash());
	assert((ulong[<20>]){}.hash() == (ulong[<20>]){}.hash());
	assert((ulong[100]){}.hash() == (ulong[100]){}.hash());
	assert((int128){}.hash() == (int128){}.hash());
	assert((int128[<20>]){}.hash() == (int128[<20>]){}.hash());
	assert((int128[100]){}.hash() == (int128[100]){}.hash());
	assert((uint128){}.hash() == (uint128){}.hash());
	assert((uint128[<20>]){}.hash() == (uint128[<20>]){}.hash());
	assert((uint128[100]){}.hash() == (uint128[100]){}.hash());
	assert((bool){}.hash() == (bool){}.hash());
	assert((bool[<100>]){}.hash() == (bool[<100>]){}.hash());
	assert((bool[100]){}.hash() == (bool[100]){}.hash());
	assert(int.typeid.hash() == int.typeid.hash());
}

fn void test_builtin_string_hashing() => @pool()
{
	var $x = "";
    ulong l;
    $for var $i = 0; $i < 65; ++$i:   // 65 is a good length to reliably test all branches w/o being excessive
        l = string::tformat("%s%s", $x, $i).hash();
        var $r = $$str_hash(@sprintf("%s%s", $x, $i));
        assert((uint)l == (uint)$r, "Builtin $$str_hash mismatch against String.hash()");
        $x = $x +++ "a";
    $endfor
}

fn void test_ct_clz()
{
	assert(@clz((ulong)0) == ulong.sizeof * 8);
	assert(@clz((char)1) == (char.sizeof * 8) - 1);
	assert(@clz((uint)0x8000_0000) == 0);
	assert(@clz((ushort)0x0100) == 7);
	assert(@clz((long)-1) == 0);
	assert(@clz((uint128)0x87) == 120);
	assert(@clz((char)(0x44 - 0x40)) == 5);
}

fn void test_bitsizeof()
{
	assert(bitsizeof(uint128) == 128);
	assert(bitsizeof(ulong) == 64);
	assert(bitsizeof(int) == 32);
	assert(bitsizeof(short) == 16);
	assert(bitsizeof(char) == 8);
	assert(bitsizeof(char[200]) == 1600);

	// Checks that they may be converted
	int x = bitsizeof(char);
	int y = @bitsizeof(1);

	assert(@bitsizeof((char)0x07) == 8);
	assert(@bitsizeof(0x1000ul) == 64);
	assert(@bitsizeof(0) == 32);
	assert(@bitsizeof((char[*])"abcdefghi") == 72);
}

fn void test_ct_min_max()
{
	assert(@min(4, 5) == 4);
	assert(@max(1.234, 1.2345) == 1.2345);
	assert(@min(4, 5, 6, 7, 8.90, 3.14) == 3.14);
	assert(@max(0, 0, 1.234, 1.2345, 0.2) == 1.2345);
	assert(@max(127.9999999, 45 + 46, bitsizeof(uint128)) == 128);
}

const usz[] MY_OK_VALUES = { 128, 256, 384, 512 };
fn void test_in()
{
    $assert @in(384, ...MY_OK_VALUES);
    $assert !@in(123, ...MY_OK_VALUES);
    $assert @in(384, 128, 256, 384, 512);
    $assert @in("love", "joy", "cheer", "love", "friend");
    $assert !@in("hate", "joy", "cheer", "love", "friend");
}
