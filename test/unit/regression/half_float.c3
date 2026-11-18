module half_float;
import std::atomic, std::math;

struct Pair { float16 a, b; }
struct Mixed { bfloat a; char c; bfloat b; }

float16 g_half = 1.5;
Pair[2] g_pairs = { { 1, 2 }, { 3, -0.25 } };

fn float16 add(float16 a, float16 b) @noinline => a + b;
fn bfloat to_bfloat(long x) @noinline => (bfloat)x;
fn float16 to_float16(double x) @noinline => (float16)x;

fn void layout() @test
{
	assert(Pair::size == 4);
	assert(Mixed::size == 6);
	Pair[4] pairs;
	assert((char*)&pairs[3].b - (char*)&pairs[0] == 14);
	assert(float16[<4>]::size == 8);
	assert(bitcast(g_half, ushort) == 0x3e00);
	assert(bitcast(g_pairs[1].b, ushort) == 0xb400);
}

fn void arithmetic() @test
{
	// Rounded once to float16: 1000.3 is 1000.5 in float16, then 1000.5 + 1.5 is exact.
	assert(bitcast(add(to_float16(1000.3), 1.5), ushort) == 0x63d4);
	// float16 steps by 2 above 2048: 2051 is halfway and rounds to the even 2052.
	float16 x = 2050;
	x++;
	assert(x == 2052);
	x -= 2;
	assert(bitcast(x, ushort) == 0x6801);
	assert(-x == -2050);
	assert((float16)7 % (float16)1.5 == 1);
	bfloat b = 3;
	b *= b;
	assert(bitcast(b, ushort) == 0x4110);
	assert(b > 8 && b != 8 && (bool)b);
}

fn void conversions() @test
{
	assert(bitcast(to_bfloat(100003), ushort) == 0x47c3);
	assert(bitcast(to_bfloat(-7), ushort) == 0xc0e0);
	assert(bitcast(to_float16(65520), ushort) == 0x7c00);
	assert(bitcast(to_float16(65519), ushort) == 0x7bff);
	assert(bitcast(to_float16(1e-6), ushort) == 0x0011);
	assert((int)to_float16(-1000.5) == -1000);
	assert((float)(bfloat)to_float16(3.140625) == 3.140625f);
}

fn void vectors() @test
{
	float16[<4>] v = { 1, 2.5, -3, 100 };
	float16[<4>] w = v * v + v;
	assert(w == (float16[<4>]){ 2, 8.75, 6, 10096 });
	float16[<4>] r = math::sqrt(math::abs(v));
	assert(bitcast(r[1], ushort) == 0x3e53);
}

fn void atomics() @test
{
	bfloat a = 1;
	atomic::fetch_add(&a, (bfloat)2);
	atomic::fetch_max(&a, (bfloat)7);
	assert(a == 7);
}
