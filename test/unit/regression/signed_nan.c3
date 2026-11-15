module signed_nan;
import std::math;

fn void test_nan_sign() @test
{
	float f1 = float::nan;
	assert(math::signbit(f1) == 0);
	assert(math::signbit(-f1) == 1);
	float f2 = -float::nan;
	assert(math::signbit(f2) == 1);
	assert(math::signbit(-f2) == 0);
	double d1 = double::nan;
	assert(math::signbit(d1) == 0);
	assert(math::signbit(-d1) == 1);
	double d2 = -double::nan;
	assert(math::signbit(d2) == 1);
	assert(math::signbit(-d2) == 0);
}
