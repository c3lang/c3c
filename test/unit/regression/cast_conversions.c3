// Each cast expression kind does its own conversion.
module cast_conversions;

faultdef OOPS;
enum Color : char { RED, GREEN, BLUE }

fn void casts() @test
{
	// int <-> float, ext/trunc
	double d = 3.75;
	int i = (int)d;
	assert(i == 3);
	float f = (float)i;
	assert(f == 3.0f);
	long l = -5;
	char ch = (char)l;
	assert(ch == 251);
	short s = (short)(int)-2;
	assert((long)s == -2);
	// int <-> pointer
	int x = 7;
	uptr p = (uptr)&x;
	int* xp = (int*)p;
	assert(*xp == 7);
	// typeid and fault through integers
	typeid t = int::typeid;
	ulong tv = (ulong)t;
	assert(tv != 0 && tv == (ulong)int::typeid);
	fault e = OOPS;
	uptr ev = (uptr)e;
	assert(ev != 0 && ev == (uptr)OOPS);
	// enum from ordinal
	int ord = 2;
	Color c = (Color)ord;
	assert(c == BLUE);
	// bit recast
	float one = 1.0f;
	uint bits = bitcast(one, uint);
	assert(bits == 0x3f800000);
	int[] ints = { 1, 2 };
	uint[] uints = (uint[])ints;
	assert(uints[1] == 2);
}
