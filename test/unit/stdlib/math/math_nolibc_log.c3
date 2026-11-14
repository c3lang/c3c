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
