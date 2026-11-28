module std::math::nolibc::misc_test @test @feat(NO_LIBC, C3_MATH);
import std::math::nolibc;

struct DoubleCase { ulong x; ulong expected; }
struct FloatCase { uint x; uint expected; }

const DoubleCase[*] EXP2_CASES = {
	{ 0x3fe0000000000000, 0x3ff6a09e667f3bcd }, // 0x1p-1
	{ 0xbfe0000000000000, 0x3fe6a09e667f3bcd }, // -0x1p-1
	{ 0x4008000000000000, 0x4020000000000000 }, // 0x1.8p+1
	{ 0x3bc79ca10c924223, 0x3ff0000000000000 }, // 0x1.79ca10c924223p-67
	{ 0x408fff3333333333, 0x7feddb680117aa8e }, // 0x1.fff3333333333p+9
	{ 0x4090000000000000, 0x7ff0000000000000 }, // 0x1p+10
	{ 0xc08ff40000000000, 0x000b504f333f9de6 }, // -0x1.ff4p+9
	{ 0xc090913333333333, 0x00000000000033fc }, // -0x1.0913333333333p+10
	{ 0xc090c80000000000, 0x0000000000000001 }, // -0x1.0c8p+10
	{ 0xc090ca0000000000, 0x0000000000000001 }, // -0x1.0cap+10
	{ 0xc090cc0000000000, 0x0000000000000000 }, // -0x1.0ccp+10
	{ 0xc091300000000000, 0x0000000000000000 }, // -0x1.13p+10
	{ 0x409f400000000000, 0x7ff0000000000000 }, // 0x1.f4p+10
	{ 0xc09f400000000000, 0x0000000000000000 }, // -0x1.f4p+10
};

const FloatCase[*] EXP2F_CASES = {
	{ 0x3f000000, 0x3fb504f3 }, // 0x1p-1
	{ 0xbf000000, 0x3f3504f3 }, // -0x1p-1
	{ 0x40400000, 0x41000000 }, // 0x1.8p+1
	{ 0x42ffcccd, 0x7f6edb51 }, // 0x1.ff999ap+6
	{ 0x43000000, 0x7f800000 }, // 0x1p+7
	{ 0xc2fd0000, 0x005a827a }, // -0x1.fap+6
	{ 0xc3150000, 0x00000001 }, // -0x1.2ap+7
	{ 0xc3158000, 0x00000001 }, // -0x1.2bp+7
	{ 0xc3160000, 0x00000000 }, // -0x1.2cp+7
	{ 0x44320000, 0x7f800000 }, // 0x1.64p+9
	{ 0xc4320000, 0x00000000 }, // -0x1.64p+9
};

const DoubleCase[*] ROUND_CASES = {
	{ 0x8000000000000000, 0x8000000000000000 }, // -0x0p+0
	{ 0xbfd3333333333333, 0x8000000000000000 }, // -0x1.3333333333333p-2
	{ 0xbfe0000000000000, 0xbff0000000000000 }, // -0x1p-1
	{ 0x3fe0000000000000, 0x3ff0000000000000 }, // 0x1p-1
	{ 0x4004000000000000, 0x4008000000000000 }, // 0x1.4p+1
	{ 0xc004000000000000, 0xc008000000000000 }, // -0x1.4p+1
	{ 0x3fdfffffffffffff, 0x0000000000000000 }, // 0x1.fffffffffffffp-2
	{ 0x4330000000000001, 0x4330000000000001 }, // 0x1.0000000000001p+52
	{ 0x8000000000000001, 0x8000000000000000 }, // -0x1p-1074
};

const FloatCase[*] ROUNDF_CASES = {
	{ 0x80000000, 0x80000000 }, // -0x0p+0
	{ 0xbe99999a, 0x80000000 }, // -0x1.333334p-2
	{ 0xbf000000, 0xbf800000 }, // -0x1p-1
	{ 0x3f000000, 0x3f800000 }, // 0x1p-1
	{ 0x40200000, 0x40400000 }, // 0x1.4p+1
	{ 0xc0200000, 0xc0400000 }, // -0x1.4p+1
	{ 0x3effffff, 0x00000000 }, // 0x1.fffffep-2
	{ 0x80000001, 0x80000000 }, // -0x1p-149
};

const FloatCase[*] FLOORF_CASES = {
	{ 0xbf000000, 0xbf800000 }, // -0x1p-1
	{ 0x3f000000, 0x00000000 }, // 0x1p-1
	{ 0x80000000, 0x80000000 }, // -0x0p+0
	{ 0x000116c2, 0x00000000 }, // 0x1.16c2p-133
	{ 0x800116c2, 0xbf800000 }, // -0x1.16c2p-133
	{ 0xbfc00000, 0xc0000000 }, // -0x1.8p+0
};

const FloatCase[*] CEILF_CASES = {
	{ 0xbf000000, 0x80000000 }, // -0x1p-1
	{ 0x3f000000, 0x3f800000 }, // 0x1p-1
	{ 0x80000000, 0x80000000 }, // -0x0p+0
	{ 0x000116c2, 0x3f800000 }, // 0x1.16c2p-133
	{ 0x800116c2, 0x80000000 }, // -0x1.16c2p-133
	{ 0x3fc00000, 0x40000000 }, // 0x1.8p+0
};

fn void exp2_matches_musl()
{
	foreach (c : EXP2_CASES)
	{
		ulong x = c.x;
		ulong got = bitcast(nolibc::_exp2(bitcast(@volatile_load(x), double)), ulong);
		if (math::is_nan(bitcast(c.expected, double)) && math::is_nan(bitcast(got, double))) continue;
		assert(got == c.expected, "exp2(%a) = %x, expected %x", bitcast(c.x, double), got, c.expected);
	}
}

fn void exp2f_matches_musl()
{
	foreach (c : EXP2F_CASES)
	{
		uint x = c.x;
		uint got = bitcast(nolibc::_exp2f(bitcast(@volatile_load(x), float)), uint);
		if (math::is_nan(bitcast(c.expected, float)) && math::is_nan(bitcast(got, float))) continue;
		assert(got == c.expected, "exp2f(%a) = %x, expected %x", bitcast(c.x, float), got, c.expected);
	}
}

fn void round_matches_musl()
{
	foreach (c : ROUND_CASES)
	{
		ulong x = c.x;
		ulong got = bitcast(nolibc::_round(bitcast(@volatile_load(x), double)), ulong);
		if (math::is_nan(bitcast(c.expected, double)) && math::is_nan(bitcast(got, double))) continue;
		assert(got == c.expected, "round(%a) = %x, expected %x", bitcast(c.x, double), got, c.expected);
	}
}

fn void roundf_matches_musl()
{
	foreach (c : ROUNDF_CASES)
	{
		uint x = c.x;
		uint got = bitcast(nolibc::_roundf(bitcast(@volatile_load(x), float)), uint);
		if (math::is_nan(bitcast(c.expected, float)) && math::is_nan(bitcast(got, float))) continue;
		assert(got == c.expected, "roundf(%a) = %x, expected %x", bitcast(c.x, float), got, c.expected);
	}
}

fn void floorf_matches_musl()
{
	foreach (c : FLOORF_CASES)
	{
		uint x = c.x;
		uint got = bitcast(nolibc::_floorf(bitcast(@volatile_load(x), float)), uint);
		if (math::is_nan(bitcast(c.expected, float)) && math::is_nan(bitcast(got, float))) continue;
		assert(got == c.expected, "floorf(%a) = %x, expected %x", bitcast(c.x, float), got, c.expected);
	}
}

fn void ceilf_matches_musl()
{
	foreach (c : CEILF_CASES)
	{
		uint x = c.x;
		uint got = bitcast(nolibc::_ceilf(bitcast(@volatile_load(x), float)), uint);
		if (math::is_nan(bitcast(c.expected, float)) && math::is_nan(bitcast(got, float))) continue;
		assert(got == c.expected, "ceilf(%a) = %x, expected %x", bitcast(c.x, float), got, c.expected);
	}
}

fn void frexp_splits_subnormals()
{
	int e;
	double m = nolibc::frexp(0x1p-1074, &e);
	assert(m == 0.5 && e == -1073, "frexp(0x1p-1074) = %a, %d", m, e);
	m = nolibc::frexp(-3 * 0x1p-1074, &e);
	assert(m == -0.75 && e == -1072, "frexp(-3 * 0x1p-1074) = %a, %d", m, e);
	m = nolibc::frexp(0x1.ffffffffffffep-1023, &e);
	assert(m == 0x1.ffffffffffffep-1 && e == -1022, "frexp(largest subnormal) = %a, %d", m, e);
	m = nolibc::frexp(0x1.8p-1050 + 0x1p-1074, &e);
	assert(m == 0x1.800001p-1 && e == -1049, "frexp(0x1.800001p-1050) = %a, %d", m, e);
	float mf = nolibc::frexpf(0x1p-149f, &e);
	assert(mf == 0.5f && e == -148, "frexpf(0x1p-149) = %a, %d", mf, e);
	mf = nolibc::frexpf(-3 * 0x1p-149f, &e);
	assert(mf == -0.75f && e == -147, "frexpf(-3 * 0x1p-149) = %a, %d", mf, e);
}

fn void frexp_special_values()
{
	int e = 7;
	assert(bitcast(nolibc::frexp(-0.0, &e), ulong) == 0x8000_0000_0000_0000 && e == 0);
	assert(nolibc::frexp(double::inf, &e) == double::inf);
	assert(math::is_nan(nolibc::frexp(double::nan, &e)));
	assert(nolibc::frexp(1.0, &e) == 0.5 && e == 1);
	assert(nolibc::frexpf(-8.0f, &e) == -0.5f && e == 4);
}

fn void log1p_poles_and_domain()
{
	assert(nolibc::_log1p(-1.0) == -double::inf);
	assert(nolibc::_log1pf(-1.0f) == -float::inf);
	assert(math::is_nan(nolibc::_log1p(-2.0)));
	assert(math::is_nan(nolibc::_log1pf(-2.0f)));
}
