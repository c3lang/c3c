// Underflow and subnormal cases follow the IEEE edge coverage in GCC's
// gcc.c-torture/execute/ieee/mul-subnormal-single-1.c.
// c3c compile-test test/unit/stdlib/math/math_nolibc_ldexp.c3 -D C3_MATH
module std::math::nolibc::ldexp_test @test @feat(NO_LIBC, C3_MATH);
import std::math::nolibc;

fn void ldexp_double_rounds_subnormals()
{
	assert(bitcast(nolibc::ldexp(1.5, -1074), ulong) == 0x2);
	assert(bitcast(nolibc::ldexp(1.5, -1073), ulong) == 0x3);
	assert(bitcast(nolibc::ldexp(1.0, -1075), ulong) == 0x0);
}

fn void ldexpf_rounds_subnormals()
{
	assert(bitcast(nolibc::ldexpf(1.5f, -149), uint) == 0x2);
	assert(bitcast(nolibc::ldexpf(1.5f, -148), uint) == 0x3);
	assert(bitcast(nolibc::ldexpf(1.0f, -150), uint) == 0x0);
}

fn void ldexp_handles_overflow_and_signed_zero()
{
	assert(bitcast(nolibc::ldexp(1.5, 1024), ulong) == 0x7ff0_0000_0000_0000);
	assert(bitcast(nolibc::ldexp(-0.0, 1024), ulong) == 0x8000_0000_0000_0000);
	assert(bitcast(nolibc::ldexp(-1.0, int::min), ulong) == 0x8000_0000_0000_0000);
	assert(bitcast(nolibc::ldexp(-1.0, int::max), ulong) == 0xfff0_0000_0000_0000);
	assert(bitcast(nolibc::ldexpf(-1.0f, int::min), uint) == 0x8000_0000);
	assert(bitcast(nolibc::ldexpf(-1.0f, int::max), uint) == 0xff80_0000);
}

fn void ldexp_scales_subnormal_inputs()
{
	assert(nolibc::ldexp(bitcast(1UL, double), 1074) == 1.0);
	assert(nolibc::ldexpf(bitcast(1U, float), 149) == 1.0f);
	assert(bitcast(nolibc::ldexp(bitcast(3UL, double), -1), ulong) == 2);
	assert(bitcast(nolibc::ldexpf(bitcast(3U, float), -1), uint) == 2);
}

fn void ldexp_rounds_to_smallest_normal()
{
	assert(bitcast(nolibc::ldexp(0x1.fffffffffffffp-1, -1022), ulong) == 0x0010_0000_0000_0000);
	assert(bitcast(nolibc::ldexpf(0x1.fffffep-1f, -126), uint) == 0x0080_0000);
}

fn void ldexp_preserves_special_values()
{
	assert(bitcast(nolibc::ldexp(0.0, int::max), ulong) == 0);
	assert(bitcast(nolibc::ldexpf(-0.0f, int::max), uint) == 0x8000_0000);
	assert(nolibc::ldexp(double::inf, int::min) == double::inf);
	assert(nolibc::ldexpf(-float::inf, int::max) == -float::inf);
	assert(bitcast(nolibc::ldexp(bitcast(0x7ff8_0000_0000_0123UL, double), 1), ulong) == 0x7ff8_0000_0000_0123);
	assert(bitcast(nolibc::ldexpf(bitcast(0x7fc0_0123U, float), 1), uint) == 0x7fc0_0123);
}

fn void ldexp_scales_exact_values()
{
	assert(nolibc::ldexp(1.5, 4) == 24.0);
	assert(nolibc::ldexpf(-1.5f, 4) == -24.0f);
	assert(nolibc::ldexp(1.5, -4) == 0.09375);
	assert(nolibc::ldexpf(-1.5f, -4) == -0.09375f);
	assert(bitcast(nolibc::ldexp(bitcast(1UL, double), 3), ulong) == 8);
	assert(bitcast(nolibc::ldexpf(bitcast(8U, float), -2), uint) == 2);
}

fn void ldexp_rounds_ties_to_even()
{
	assert(bitcast(nolibc::ldexp(1.25, -1073), ulong) == 2);
	assert(bitcast(nolibc::ldexp(1.75, -1073), ulong) == 4);
	assert(bitcast(nolibc::ldexpf(1.25f, -148), uint) == 2);
	assert(bitcast(nolibc::ldexpf(1.75f, -148), uint) == 4);
	assert(bitcast(nolibc::ldexp(-1.0, -1075), ulong) == 0x8000_0000_0000_0000);
	assert(bitcast(nolibc::ldexpf(-1.0f, -150), uint) == 0x8000_0000);
}
