// The C3 implementations of log and logf, used when there is no C library (wasm32, freestanding) or with -D C3_MATH.
// c3c compile-test test/unit/stdlib/math/math_nolibc_log.c3 -D C3_MATH
module std::math::nolibc::log_test @test @feat(NO_LIBC, C3_MATH);
import std::math::nolibc;

// Natural logarithms, correctly rounded, of values on both sides of 1, both sides of sqrt(2), and far from 1.
const double[*] X = { 0.001, 0.75, 1.1, 1.5, 3.0, 10.0, 12345.678, 1e100 };
const double[*] LOG_X = {
	-6.907755278982137, -0.2876820724517809, 0.09531017980432493, 0.4054651081081644,
	1.0986122886681098, 2.302585092994046, 9.421061321291832, 230.25850929940458
};

fn void log_is_accurate()
{
	foreach (i, x : X)
	{
		double got = nolibc::log(x);
		assert(math::abs(got - LOG_X[i]) <= math::abs(LOG_X[i]) * 4e-16, "log(%s) = %s, expected %s", x, got, LOG_X[i]);
	}
}

fn void logf_is_accurate()
{
	foreach (i, x : X[:7])
	{
		float got = nolibc::logf((float)x);
		assert(math::abs(got - LOG_X[i]) <= math::abs(LOG_X[i]) * 3e-7, "logf(%s) = %s, expected %s", x, got, LOG_X[i]);
	}
}

struct DoubleCase { double x; double expected; }
struct FloatCase { float x; float expected; }

// Expected results are bit exact; all were checked against musl and the macOS libm.
const DoubleCase[*] LOG_CASES = {
	{ 1.0, 0.0 },
	{ 2.0, 0x1.62e42fefa39efp-1 },
	{ 0.5, -0x1.62e42fefa39efp-1 },
	{ 1024.0, 0x1.bb9d3beb8c86bp+2 },
	{ 0x1p1023, 0x1.628b76e3a7b61p+9 },
	{ 0x1p-1022, -0x1.6232bdd7abcd2p+9 },
	{ 0x1.fffffffffffffp+1023, 0x1.62e42fefa39efp+9 },
	{ 0x1.5bf0a8b145769p+1, 1.0 },                        // e
	{ 3.0, 0x1.193ea7aad030bp+0 },
	{ 10.0, 0x1.26bb1bbb55516p+1 },
	{ 0.75, -0x1.269621134db92p-2 },
	{ 0x1.249ad2594c37dp+332, 0x1.cc845b54b54f2p+7 },     // 1e100
	{ 0x1.0624dd2f1a9fcp-10, -0x1.ba18a998fffap+2 },      // 0.001
	// Close to 1, both sides.
	{ 0x1.199999999999ap+0, 0x1.8663f793c46ccp-4 },       // 1.1
	{ 0x1.e666666666666p-1, -0x1.a431d5bcc193ep-5 },      // 0.95
	{ 0x1.0cccccccccccdp+0, 0x1.8fb063ef2c7fp-5 },        // 1.05
	{ 0x1.fffffffffffffp-1, -0x1p-53 },
	{ 0x1.0000000000001p+0, 0x1.fffffffffffffp-53 },
	// Subnormal inputs.
	{ 0x1p-1074, -0x1.74385446d71c3p+9 },
	{ 0x1.2344p-1060, -0x1.6f4db0a45b22dp+9 },
	{ 0.0, -double::inf },
	{ -0.0, -double::inf },
	{ double::inf, double::inf },
};

const FloatCase[*] LOGF_CASES = {
	{ 1.0f, 0.0f },
	{ 2.0f, 0x1.62e43p-1f },
	{ 0.5f, -0x1.62e43p-1f },
	{ 1024.0f, 0x1.bb9d3cp+2f },
	{ 0x1p127f, 0x1.601e68p+6f },
	{ 0x1p-126f, -0x1.5d58ap+6f },
	{ 0x1.fffffep+127f, 0x1.62e43p+6f },
	{ 0x1.5bf0a8p+1f, 0x1.fffffep-1f },                   // e
	{ 10.0f, 0x1.26bb1cp+1f },
	{ 0.75f, -0x1.269622p-2f },
	{ 0x1.93e594p+99f, 0x1.144f6ap+6f },                  // 1e30
	{ 0x1.0624dep-10f, -0x1.ba18aap+2f },                 // 0.001
	// Close to 1, both sides.
	{ 0x1.19999ap+0f, 0x1.8663fep-4f },                   // 1.1
	{ 0x1.e66666p-1f, -0x1.a431dcp-5f },                  // 0.95
	{ 0x1.0cccccp+0f, 0x1.8fb04cp-5f },                   // 1.05
	{ 0x1.fffffep-1f, -0x1p-24f },
	{ 0x1.000002p+0f, 0x1.fffffep-24f },
	// Subnormal inputs.
	{ 0x1p-149f, -0x1.9d1dap+6f },
	{ 0x1.238p-140f, -0x1.83a49ap+6f },
	{ 0.0f, -float::inf },
	{ -0.0f, -float::inf },
	{ float::inf, float::inf },
};

const double[*] LOG_NAN_INPUTS = { -1.0, -0x1p-1074, -double::inf, double::nan };
const float[*] LOGF_NAN_INPUTS = { -1.0f, -0x1p-149f, -float::inf, float::nan };

fn void log_bit_exact()
{
	foreach (c : LOG_CASES)
	{
		double x = c.x;
		double got = nolibc::log(@volatile_load(x));
		assert(bitcast(got, ulong) == bitcast(c.expected, ulong), "log(%a) = %a, expected %a", c.x, got, c.expected);
	}
	foreach (x : LOG_NAN_INPUTS)
	{
		assert(math::is_nan(nolibc::log(@volatile_load(x))), "log(%a) should be nan", x);
	}
}

fn void logf_bit_exact()
{
	foreach (c : LOGF_CASES)
	{
		float x = c.x;
		float got = nolibc::logf(@volatile_load(x));
		assert(bitcast(got, uint) == bitcast(c.expected, uint), "logf(%a) = %a, expected %a", c.x, got, c.expected);
	}
	foreach (x : LOGF_NAN_INPUTS)
	{
		assert(math::is_nan(nolibc::logf(@volatile_load(x))), "logf(%a) should be nan", x);
	}
}
