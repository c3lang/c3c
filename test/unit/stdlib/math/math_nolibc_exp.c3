module std::math::nolibc::exp_test @test @feat(NO_LIBC, C3_MATH);
import std::math::nolibc;

struct DoubleCase { double x; double expected; }
struct FloatCase { float x; float expected; }

// Expected results are bit exact; all were checked against musl and the macOS libm.
const DoubleCase[*] EXP_CASES = {
	{ 0.0, 1.0 },
	{ -0.0, 1.0 },
	{ 0x1p-54, 1.0 },
	{ 0x1.56e1fc2f8f359p-997, 1.0 },                      // 1e-300
	{ 1.0, 0x1.5bf0a8b145769p+1 },
	{ -1.0, 0x1.78b56362cef38p-2 },
	{ 0.5, 0x1.a61298e1e069cp+0 },
	{ 10.0, 0x1.5829dcf95056p+14 },
	{ -10.0, 0x1.7cd79b5647c9bp-15 },
	{ 100.0, 0x1.3494a9b171bf5p+144 },
	// Overflow threshold.
	{ 0x1.62e42fefa39efp+9, 0x1.fffffffffff2ap+1023 },
	{ 0x1.62e42fefa39f0p+9, double::inf },
	{ 1024.0, double::inf },
	// Smallest normal result, then subnormal results.
	{ -0x1.6232bdd7abcd2p+9, 0x1.000000000007cp-1022 },
	{ -0x1.6232bdd7abcd3p+9, 0x1.ffffffffffcf8p-1023 },
	{ -740.0, 0x1.54p-1068 },
	// Underflow threshold.
	{ -0x1.74910d52d3051p+9, 0x1p-1074 },
	{ -0x1.74910d52d3052p+9, 0.0 },
	{ -1100.0, 0.0 },
	{ double::inf, double::inf },
	{ -double::inf, 0.0 },
};

const FloatCase[*] EXPF_CASES = {
	{ 0.0f, 1.0f },
	{ -0.0f, 1.0f },
	{ 0x1.4484cp-100f, 1.0f },                            // 1e-30
	{ 1.0f, 0x1.5bf0a8p+1f },
	{ -1.0f, 0x1.78b564p-2f },
	{ 0.5f, 0x1.a61298p+0f },
	{ 10.0f, 0x1.5829dcp+14f },
	{ -10.0f, 0x1.7cd79cp-15f },
	// Overflow threshold.
	{ 0x1.62e42ep+6f, 0x1.ffff08p+127f },
	{ 0x1.62e430p+6f, float::inf },
	{ 128.0f, float::inf },
	// Smallest normal result, then subnormal results.
	{ -0x1.5d589ep+6f, 0x1.00004cp-126f },
	{ -0x1.5d58a0p+6f, 0x1.ffff98p-127f },
	{ -100.0f, 0x1.bp-145f },
	// Underflow threshold.
	{ -0x1.9fe368p+6f, 0x1p-149f },
	{ -0x1.9fe36ap+6f, 0.0f },
	{ -150.0f, 0.0f },
	{ float::inf, float::inf },
	{ -float::inf, 0.0f },
};

fn void exp_bit_exact()
{
	foreach (c : EXP_CASES)
	{
		double x = c.x;
		double got = nolibc::exp(@volatile_load(x));
		assert(bitcast(got, ulong) == bitcast(c.expected, ulong), "exp(%a) = %a, expected %a", c.x, got, c.expected);
	}
	double nan = double::nan;
	assert(math::is_nan(nolibc::exp(@volatile_load(nan))));
}

fn void expf_bit_exact()
{
	foreach (c : EXPF_CASES)
	{
		float x = c.x;
		float got = nolibc::expf(@volatile_load(x));
		assert(bitcast(got, uint) == bitcast(c.expected, uint), "expf(%a) = %a, expected %a", c.x, got, c.expected);
	}
	float nan = float::nan;
	assert(math::is_nan(nolibc::expf(@volatile_load(nan))));
}
