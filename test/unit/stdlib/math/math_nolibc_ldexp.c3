module std::math::nolibc::ldexp_test @test @feat(NO_LIBC, C3_MATH);
import std::math::nolibc;

struct DoubleCase { ulong x; int e; ulong expected; }
struct FloatCase { uint x; int e; uint expected; }

// Bit patterns, checked against the macOS libc ldexp and ldexpf.
const DoubleCase[*] DOUBLE_CASES = {
	// Exact scaling of normal numbers.
	{ 0x3ff8_0000_0000_0000, 4, 0x4038_0000_0000_0000 },          // 1.5 * 2^4 = 24
	{ 0xbff8_0000_0000_0000, -4, 0xbfb8_0000_0000_0000 },         // -1.5 * 2^-4
	{ 0x3ff0_0000_0000_0000, 1023, 0x7fe0_0000_0000_0000 },       // 2^1023
	{ 0x3ff0_0000_0000_0000, -1022, 0x0010_0000_0000_0000 },      // smallest normal
	{ 0x0010_0000_0000_0000, 2045, 0x7fe0_0000_0000_0000 },       // smallest normal to 2^1023 in one step
	{ 0x7fe0_0000_0000_0000, -2045, 0x0010_0000_0000_0000 },      // and back
	// Overflow.
	{ 0x3ff8_0000_0000_0000, 1024, 0x7ff0_0000_0000_0000 },
	{ 0xbff0_0000_0000_0000, 1024, 0xfff0_0000_0000_0000 },
	{ 0x7fef_ffff_ffff_ffff, 1, 0x7ff0_0000_0000_0000 },
	{ 0xbff0_0000_0000_0000, int::max, 0xfff0_0000_0000_0000 },
	{ 0x0000_0000_0000_0001, int::max, 0x7ff0_0000_0000_0000 },
	// Results that are subnormal, exact.
	{ 0x3ff0_0000_0000_0000, -1074, 0x0000_0000_0000_0001 },      // smallest subnormal
	{ 0x3ff0_0000_0000_0000, -1023, 0x0008_0000_0000_0000 },
	{ 0x3ff8_0000_0000_0000, -1073, 0x0000_0000_0000_0003 },
	// Results that are subnormal, rounded.
	{ 0x3ff8_0000_0000_0000, -1074, 0x0000_0000_0000_0002 },      // 1.5 ulp -> 2
	{ 0x3ff4_0000_0000_0000, -1073, 0x0000_0000_0000_0002 },      // 2.5 ulp -> 2 (tie to even)
	{ 0x3ffc_0000_0000_0000, -1073, 0x0000_0000_0000_0004 },      // 3.5 ulp -> 4 (tie to even)
	{ 0x3ffa_0000_0000_0000, -1073, 0x0000_0000_0000_0003 },      // 3.25 ulp -> 3
	{ 0x3fef_ffff_ffff_ffff, -1022, 0x0010_0000_0000_0000 },      // rounds up to smallest normal
	{ 0x3fff_ffff_ffff_ffff, -1074, 0x0000_0000_0000_0002 },
	// Underflow to zero or to the smallest subnormal.
	{ 0x3ff0_0000_0000_0000, -1075, 0x0000_0000_0000_0000 },      // exactly half, odd exponent: tie to 0
	{ 0x4000_0000_0000_0000, -1076, 0x0000_0000_0000_0000 },      // exactly half, even exponent: tie to 0
	{ 0x4020_0000_0000_0000, -1078, 0x0000_0000_0000_0000 },      // exactly half, 8.0
	{ 0xc000_0000_0000_0000, -1076, 0x8000_0000_0000_0000 },
	{ 0x3ff0_0000_0000_0001, -1075, 0x0000_0000_0000_0001 },      // just above half
	{ 0x3fff_ffff_ffff_ffff, -1076, 0x0000_0000_0000_0000 },      // just below half
	{ 0x3ff0_0000_0000_0000, -1076, 0x0000_0000_0000_0000 },
	{ 0xbff0_0000_0000_0000, int::min, 0x8000_0000_0000_0000 },
	{ 0x7fef_ffff_ffff_ffff, int::min, 0x0000_0000_0000_0000 },
	// Subnormal inputs.
	{ 0x0000_0000_0000_0001, 1074, 0x3ff0_0000_0000_0000 },
	{ 0x0000_0000_0000_0001, 3, 0x0000_0000_0000_0008 },
	{ 0x0000_0000_0000_0001, 52, 0x0010_0000_0000_0000 },
	{ 0x000f_ffff_ffff_ffff, 1, 0x001f_ffff_ffff_fffe },
	{ 0x0000_0000_0000_0008, -2, 0x0000_0000_0000_0002 },
	{ 0x0000_0000_0000_0003, -1, 0x0000_0000_0000_0002 },         // 1.5 ulp -> 2
	{ 0x0000_0000_0000_0005, -1, 0x0000_0000_0000_0002 },         // 2.5 ulp -> 2
	{ 0x8000_0000_0000_0001, -1, 0x8000_0000_0000_0000 },
	{ 0x0000_0000_0000_0001, 2097, 0x7fe0_0000_0000_0000 },
	{ 0x0000_0000_0000_0001, 2098, 0x7ff0_0000_0000_0000 },
	// Zero, infinity and NaN pass through.
	{ 0x0000_0000_0000_0000, int::max, 0x0000_0000_0000_0000 },
	{ 0x8000_0000_0000_0000, 1024, 0x8000_0000_0000_0000 },
	{ 0x8000_0000_0000_0000, int::min, 0x8000_0000_0000_0000 },
	{ 0x7ff0_0000_0000_0000, int::min, 0x7ff0_0000_0000_0000 },
	{ 0xfff0_0000_0000_0000, -5, 0xfff0_0000_0000_0000 },
};

const FloatCase[*] FLOAT_CASES = {
	// Exact scaling of normal numbers.
	{ 0x3fc0_0000, 4, 0x41c0_0000 },             // 1.5 * 2^4 = 24
	{ 0xbfc0_0000, -4, 0xbdc0_0000 },            // -1.5 * 2^-4
	{ 0x3f80_0000, 127, 0x7f00_0000 },           // 2^127
	{ 0x3f80_0000, -126, 0x0080_0000 },          // smallest normal
	{ 0x0080_0000, 253, 0x7f00_0000 },           // smallest normal to 2^127 in one step
	{ 0x7f00_0000, -253, 0x0080_0000 },          // and back
	// Overflow.
	{ 0x3fc0_0000, 128, 0x7f80_0000 },
	{ 0xbf80_0000, 128, 0xff80_0000 },
	{ 0x7f7f_ffff, 1, 0x7f80_0000 },
	{ 0xbf80_0000, int::max, 0xff80_0000 },
	{ 0x0000_0001, int::max, 0x7f80_0000 },
	// Results that are subnormal, exact.
	{ 0x3f80_0000, -149, 0x0000_0001 },          // smallest subnormal
	{ 0x3f80_0000, -127, 0x0040_0000 },
	{ 0x3fc0_0000, -148, 0x0000_0003 },
	// Results that are subnormal, rounded.
	{ 0x3fc0_0000, -149, 0x0000_0002 },          // 1.5 ulp -> 2
	{ 0x3fa0_0000, -148, 0x0000_0002 },          // 2.5 ulp -> 2 (tie to even)
	{ 0x3fe0_0000, -148, 0x0000_0004 },          // 3.5 ulp -> 4 (tie to even)
	{ 0x3fd0_0000, -148, 0x0000_0003 },          // 3.25 ulp -> 3
	{ 0x3f7f_ffff, -126, 0x0080_0000 },          // rounds up to smallest normal
	{ 0x3fff_ffff, -149, 0x0000_0002 },
	// Underflow to zero or to the smallest subnormal.
	{ 0x3f80_0000, -150, 0x0000_0000 },          // exactly half, odd exponent: tie to 0
	{ 0x4000_0000, -151, 0x0000_0000 },          // exactly half, even exponent: tie to 0
	{ 0x4100_0000, -153, 0x0000_0000 },          // exactly half, 8.0
	{ 0xc000_0000, -151, 0x8000_0000 },
	{ 0x3f80_0001, -150, 0x0000_0001 },          // just above half
	{ 0x3fff_ffff, -151, 0x0000_0000 },          // just below half
	{ 0x3f80_0000, -151, 0x0000_0000 },
	{ 0xbf80_0000, int::min, 0x8000_0000 },
	{ 0x7f7f_ffff, int::min, 0x0000_0000 },
	// Subnormal inputs.
	{ 0x0000_0001, 149, 0x3f80_0000 },
	{ 0x0000_0001, 3, 0x0000_0008 },
	{ 0x0000_0001, 23, 0x0080_0000 },
	{ 0x007f_ffff, 1, 0x00ff_fffe },
	{ 0x0000_0008, -2, 0x0000_0002 },
	{ 0x0000_0003, -1, 0x0000_0002 },            // 1.5 ulp -> 2
	{ 0x0000_0005, -1, 0x0000_0002 },            // 2.5 ulp -> 2
	{ 0x8000_0001, -1, 0x8000_0000 },
	{ 0x0000_0001, 276, 0x7f00_0000 },
	{ 0x0000_0001, 277, 0x7f80_0000 },
	// Zero, infinity and NaN pass through.
	{ 0x0000_0000, int::max, 0x0000_0000 },
	{ 0x8000_0000, 128, 0x8000_0000 },
	{ 0x8000_0000, int::min, 0x8000_0000 },
	{ 0x7f80_0000, int::min, 0x7f80_0000 },
	{ 0xff80_0000, -5, 0xff80_0000 },
};

fn void ldexp_matches_ieee()
{
	foreach (c : DOUBLE_CASES)
	{
		ulong got = bitcast(nolibc::ldexp(bitcast(c.x, double), c.e), ulong);
		assert(got == c.expected, "ldexp(%x, %d) = %x, expected %x", c.x, c.e, got, c.expected);
	}
}

fn void scalbn_matches_ieee()
{
	foreach (c : DOUBLE_CASES)
	{
		ulong got = bitcast(nolibc::_scalbn(bitcast(c.x, double), c.e), ulong);
		assert(got == c.expected, "scalbn(%x, %d) = %x, expected %x", c.x, c.e, got, c.expected);
	}
}

fn void ldexpf_matches_ieee()
{
	foreach (c : FLOAT_CASES)
	{
		uint got = bitcast(nolibc::ldexpf(bitcast(c.x, float), c.e), uint);
		assert(got == c.expected, "ldexpf(%x, %d) = %x, expected %x", c.x, c.e, got, c.expected);
	}
}

fn void ldexp_keeps_nan()
{
	assert(math::is_nan(nolibc::ldexp(double::nan, 10)));
	assert(math::is_nan(nolibc::ldexp(double::nan, int::min)));
	assert(math::is_nan(nolibc::ldexpf(float::nan, 10)));
	assert(math::is_nan(nolibc::ldexpf(float::nan, int::max)));
}

fn void ldexp_inverts()
{
	foreach (double x : { 1.0, -3.75, 0x1.23456789abcdep-500, 0x1.fffffffffffffp900 })
	{
		for (int e = -40; e <= 40; e += 7)
		{
			assert(nolibc::ldexp(nolibc::ldexp(x, e), -e) == x, "ldexp(ldexp(%a, %d), %d) != %a", x, e, -e, x);
		}
	}
	foreach (float x : { 1.0f, -3.75f, 0x1.234566p-60f, 0x1.fffffep100f })
	{
		for (int e = -20; e <= 20; e += 3)
		{
			assert(nolibc::ldexpf(nolibc::ldexpf(x, e), -e) == x, "ldexpf(ldexpf(%a, %d), %d) != %a", x, e, -e, x);
		}
	}
}
